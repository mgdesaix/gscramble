
#' Take a gsp in tibble form and make a list suitable for gene dropping
#'
#' Just a simple function that makes a list-based data structure
#' that makes it easy to gene-drop chromosome segments down the
#' gsp.  The basic idea is to get everyone into a data structure
#' in which they are ordered such that by the time you get to segregating
#' segments _from_ anyone, you already have the segments segregated _into_ them.
#' This works by each individual having a list of gametes (post-recombination)
#' coming out of them, and a list of "uniters" which are the gametes coming
#' into them.  We just point the uniters to gametes in the previous generation
#' and then make sure that we shuffle the order of the gametes when they come
#' out of someone.
#' @param gsp A tibble that holds the genome simulation pedigree (GSP).
#' This is a tibble in which each row specifies an individual in the
#' GSP.  The columns of the tibble are:
#'
#' - `ind`: a numeric identifier for that row's indvidual.
#' - `par`: the numeric ID of the first parent of the individual (NA if the
#' individual is a founder of the pedigree).
#' - `par2`: the numeric ID of the second parent of the individual (NA if
#' the individual is a founder)
#' - `ipar1`: the number of gametes that `par1` must segregate "through" `ind` in
#' order to exhaust all the genetic material in the GSP.  These values are given
#' by the red numerals alongside the edge connecting parents to offspring in the
#' GSP images defined by `gsp2dot()`.  See the vignette `gscramble-tutorial`, for
#' an example. (NA if `ind` is a founder).
#' - `ipar2`: the number of gametes that `par2` must segregate through `ind`.
#' (NA if `ind` is a founder).
#' - `hap1`: a unique character label given to the first haplotype in `ind` if `ind` is a
#' founder.  If `ind` is not a founder, this must be NA.
#' - `hap2`: unique character label given to the second haplotype in `ind`. NA if `ind`
#' is not a founder.
#' - `hpop1`: character label giving the population of origin of the first haplotype
#' (i.e., `hap1`) in `ind`, if `ind` is a founder.  NA otherwise.
#' - `hpop2`: character label giving the population of origin of the second
#' haplotype (i.e., `hap2`) in `ind`.  NA if `ind` is not a founder.
#' - `sample`: unique character label for the outcoming diploid sample from the
#' pedigree member `ind`. NA if `ind` is not sampled.
#' - `osample`: the number of diploid samples that come out of `ind`.  NA if
#' `ind` is not sampled.
#' @return
#' This function returns a named list, which is a linked-list type of
#' structure that contains the same information that is in `gsp`, but
#' it makes it easier to access when traversing the pedigree.
#'
#' The length of the list is
#' `nrow(gsp)`. The names are the character versions of the `ind` column.
#' Each component of the list refers to an individual row from `gsp`.  All
#' of these list elements are themselves lists. (i.e., the information
#' about a single individual is stored in a list.) Every such individual
#' list has at least the two elements:
#'
#' - `isSample`: TRUE if samples are taken from the individual. FALSE otherwise.
#' - `isFounder`: TRUE if the individual is a founder.  FALSE otherwise.
#' - `nGamete`: The total number of gametes that will be segregated _out_
#' of this individual along edges to the _offspring_ of the individual in the pedigree.
#' This is the sum of all the red numbers alongside edges below the individual in the
#' GSP.
#'
#' If an individual's `isSample` is TRUE, then its list also contains the following elements:
#'
#' - `nSamples`: the number of diploid genomes sampled out of this individual.
#' This is the purple number along the edge to the sample node below the individual
#' in the GSP "picture".
#'
#' If an individual's, `isFounder` is TRUE then its list also contains
#' the following elements:
#'
#' - `hpop1`: the population from which haplotype 1 in this (founder) individual originated.
#' - `hpop2`: the population from which haplotype 2 in this (founder) individual originated.
#' - `fh_idx1`: this stands for "founding haplotype index 1.  It is a unique integer
#' that identifies haplotype one in this founder individual.  This integer is unique over
#' all haplotypes in all the founder individuals.
#' - `fh_idx2`' the unique integer identifier for haplotype two in this founder
#' individual.
#'
#' If an individual's `isFounder` is FALSE, then its list also contains
#' the following elements:
#'
#' - `par1` and `par2`. Each of these is a list with the elements:
#'     * `par`: the character identifier of the first (if in `par1`) of the
#'     second (if in `par2`) parent of the individual.
#'     * `gam_idx`: This tells us which of the gametes in the parent (1 or 2)
#'     depending on if this is in `par1` or `par2`, gets segregated to the
#'     individual.  NEED TO EXPLAIN MORE.  SINCE THINGS GET PERMUTED, ETC.
#' @export
#' @examples
#' # get the 13 member complex pedigree in tibble form as the
#' # package data object GSP and prep it:
#' GSP_list <- prep_gsp_for_hap_dropping(GSP)
prep_gsp_for_hap_dropping <- function(gsp) {

  # first check the gsp tibble to make sure that the column
  # types are correct and shout a warning if any element is
  # "NA" (as a character) rather than NA.
  if(!is.na(any(gsp == "NA")) && any(gsp == "NA")) {
    warning("WARNING in prep_gsp_for_hap_dropping(): some elements of gsp are \"NA\" strings, not NAs")
  }
  numeric_cols <- c(
    "ind",
    "par1",
    "par2",
    "ipar1",
    "ipar2",
    "osample"
  )
  char_cols <- c(
    "hap1",
    "hap2",
    "hpop1",
    "hpop2",
    "sample"
  )
  nc_tf <- sapply(gsp[numeric_cols], is.numeric)
  if(any(!nc_tf)) {
    stop(
      "Column(s): [",
      paste(names(nc_tf)[!nc_tf], collapse = ", "),
      "] in parameter gsp are not of numeric type"
    )
  }
  cc_tf <- sapply(gsp[char_cols], is.character)
  if(any(!cc_tf)) {
    stop(
      "Column(s): [",
      paste(names(cc_tf)[!cc_tf], collapse = ", "),
      "] in parameter gsp are not of character type"
    )
  }


  ## Done with that type-error catching.

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
      glist[[ind]]$nSamples <- as.integer(gre$osample[i])
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

  # now, before we return this thing, we check to make sure that it
  # 1. Does not have inbreeding loops in it.  (That's an error!)
  # 2. Does not "consume" more genetic material than is available
  #    at any step. (That's an error!)
  # 3. Each individual passes on as much genetic material as
  #    comes into it. (If not, that's a warning!)
  # 4. The amount of genetic material sampled is the same
  #    as the amount coming in from the founders. (If not, that's a warning!)

  # 1 is taken care of by this function
  check_pedigree_for_inbreeding(glist)

  # 2-4 are checked by this function.     STILL HAVE TO WRITE IT!!
  check_gsp_for_validity_and_saturation(glist)




  glist
}
