
#' Take a gsp in tibble form and make a list suitable for gene dropping
#'
#' Just a simple function that makes a list-based data structure
#' that makes it easy to gene drop chromosome segments down the
#' gsp.  The basic idea is to get everyone into a data structure
#' in which they are ordered such that by the time you get to segregating
#' genes from anyone, you already have the genes segregated into them.
#' This works by each individual having a list of gametes (post-recombination)
#' coming out of them, and a list of "uniters" which are the gametes coming
#' into them.  We just point the uniters to gametes in the previous generation
#' and then make sure that we shuffle the order of the gametes when they come
#' out of someone.
#' @param gsp A tibble that holds the gsp
#' @export
prep_gsp_for_hap_dropping <- function(gsp) {
  founders <- gsp %>%
    filter(is.na(par1) | is.na(par2)) %>%
    pull(ind)

  # now we make a vector of which round each ind gets produced in
  rounds <- rep(1, length(founders))
  names(rounds) <- founders
  tmp <- gsp %>%
    filter(!(as.character(ind) %in% names(rounds)))
  n <- 1
  while(nrow(tmp) > 0) {
    n <- n + 1
    nexties <- tmp %>%
      filter((as.character(par1) %in% names(rounds)) &
               as.character(par2) %in% names(rounds)) %>%
      pull(ind)

    t2 <- rep(n, length(nexties))
    names(t2) <- nexties

    rounds <- c(rounds, t2)

    tmp <- gsp %>%
      filter(!(as.character(ind) %in% names(rounds)))
  }

  # That tells us in what order we should handle individuals when we are
  # haplotype dropping.  So, now, we should be able to put them all into a
  # list structure that keeps some info on each.  Namely for each individual
  # we will name the list element by as.character(ind) and then we will have
  # these other fields:
  #    1. isFounder logical
  #    2. If isFounder = TRUE we have hpop1 and hpop2, and we also keep track of the
  #       index of the founding haplotype within each of those populations/species,
  #       with the variables fh_idx1 and fh_idx2.
  #    3. If isFounder = FALSE we have two lists: par1 and par2.  Each
  #       of those is a list with element par = the character name of the
  #       indiv who is that parent, and then gam_idx, an integer vector giving the
  #       index of the gamete segregated from that parent.
  #    4. nGamete: the number of gametes segregated from this individual. This
  #       is used whilst figuring out the index of gametes in parents in 3,
  #       above. Note that nGamete should always be <= 2 for founders
  #    5. gam_idx, tells us which gamete in the parents gets segregated to the
  #       individual.
  glist <- vector("list", length(rounds))
  names(glist) <- names(rounds)

  # reorder gsp along the lines of the rounds and make sure
  # every column is a character vector
  gre <- enframe(rounds, name = "ind", value = "round") %>%
    left_join(., gsp %>% mutate(ind = as.character(ind)), by = "ind") %>%
    mutate_all(as.character)

  # now, we just walk our way down it with a for loop and fill our glist:
  # first we make some initializations
  fh_Counter <- list()
  for(i in 1:nrow(gre)) {
    ind <- gre$ind[i]
    round <- gre$round[i]
    glist[[ind]]$isSample <- FALSE
    if(!is.na(gre$sample[i]) & !is.na(gre$osample[i])) {
      # Now, if this individual (ind) has isSample == TRUE, we will have to
      # note that and also to say how many samples we want from it
      glist[[ind]]$isSample <- TRUE
      glist[[ind]]$nSamples <- gre$osample[i]
    }
    if(round == 1) {
      glist[[ind]]$isFounder <- TRUE
      glist[[ind]]$hpop1 <- gre$hpop1[i]
      glist[[ind]]$hpop2 <- gre$hpop2[i]

      if(is.null(fh_Counter[[glist[[ind]]$hpop1]])) {
        fh_Counter[[glist[[ind]]$hpop1]] <- 0
      }
      if(is.null(fh_Counter[[glist[[ind]]$hpop2]])) {
        fh_Counter[[glist[[ind]]$hpop2]] <- 0
      }

      # increment the founding haplotype counters and then assign to each haplotype
      fh_Counter[[glist[[ind]]$hpop1]] <- fh_Counter[[glist[[ind]]$hpop1]] + 1
      glist[[ind]]$fh_idx1 <- fh_Counter[[glist[[ind]]$hpop1]]

      fh_Counter[[glist[[ind]]$hpop2]] <- fh_Counter[[glist[[ind]]$hpop2]] + 1
      glist[[ind]]$fh_idx2 <- fh_Counter[[glist[[ind]]$hpop2]]



    } else {
      glist[[ind]]$isFounder <- FALSE
    }
    glist[[ind]]$nGamete <- 0  # we initialize this to 0 to count them up later
  }

  # now, we cycle through that again and set up our pointers to gametes.
  # Note that this is based on the number of incoming gametes to each individual
  # which in turn increments the number of outgoing gametes (nGamete) in the
  # parents.  One consequence of this is that incoming gametes that are to be used
  # for samples will be available for that use, but that is not explicitly recorded
  # in any other variables (other than the surfeit of incoming gametes relative to
  # outgoing gametes.)
  for(i in 1:nrow(gre)) {
    ind <- gre$ind[i]
    if(glist[[ind]]$isFounder == FALSE) {

      # do it for parent 1
      glist[[ind]]$par1$par <- gre$par1[i]
      ipar1 <- gre$ipar1[i]
      glist[[ind]]$par1$gam_idx <- integer(ipar1)
      for(j in 1:ipar1) {
        glist[[gre$par1[i]]]$nGamete <- glist[[gre$par1[i]]]$nGamete + 1
        glist[[ind]]$par1$gam_idx[j] <- glist[[gre$par1[i]]]$nGamete
      }

      # and then again for parent 2
      glist[[ind]]$par2$par <- gre$par2[i]
      ipar2 <- gre$ipar2[i]
      glist[[ind]]$par2$gam_idx <- integer(ipar2)
      for(j in 1:ipar2) {
        glist[[gre$par2[i]]]$nGamete <- glist[[gre$par2[i]]]$nGamete + 1
        glist[[ind]]$par2$gam_idx[j] <- glist[[gre$par2[i]]]$nGamete
      }

    }

  }

  glist
}
