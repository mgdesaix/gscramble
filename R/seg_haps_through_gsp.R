
#' Segregate haplotypes through a genome simulation pedigree list
#'
#' The main input into this is the list prepared by prep_gsp_for_hap_dropping.
#' This returns a list of the same form, but with a few extra variables in there,
#' the most important of which for future use will be the Samples, which are
#' lists that hold the founder segments that made it into the sampled individuals.
#'
#' This is a low-level function that the user will not typically use.
#' @param G a genome simulation pedigree as a list, ready to go
#' @param M map information.  Should be a tibble with start_pos end_pos
#' and rec_prob
#' @param chrom_len  The length of the chromosome in base pairs
#' @param recomb_model  Not used for now, but we will want to add that
#' here, eventually, to specify the kind of recombination model we are using,
#' and then modify recomb_point to accommodate the different choices.
#' @param pop_idx_sep The string to use to separate the index of the pop
#' and the index of the founder for naming where chromosomal segments
#' come from.  It is not recommended that this be changed.
#' @keywords internal
seg_haps_through_gsp <- function(G, M, chrom_len, pop_idx_sep = "--%--", recomb_model = "always one") {

  # make a copy of G and initialize gametes in the founders and segregate
  # them elsewhere
  ret <- G
  for(i in seq_along(ret)) {
    if(ret[[i]]$isFounder == TRUE) {
      # make a list of the gametes
      init1 <- c(0, chrom_len)
      names(init1) <- paste(rep(ret[[i]]$hpop1, 2), ret[[i]]$fh_idx1, sep = pop_idx_sep)
      init2 <- c(0, chrom_len)
      names(init2) <- paste(rep(ret[[i]]$hpop2, 2), ret[[i]]$fh_idx2, sep = pop_idx_sep)
      ret[[i]]$gametes <- list(init1, init2)[sample(1:2)]  # note that we sample these to randomize their order when they get segregated into the offspring
    } else {  # otherwise we segregate stuff into each individual, and then segregate
      # it back out
      if(length(ret[[i]]$par1$gam_idx) != length(ret[[i]]$par2$gam_idx)) {
        stop("Mismatching number of gametes segregated into indiv ", i, " from its two parents")
      }
      tmplist <- list()
      outgoing_idx <- 0 # set to eventually start indexing the outgoing gametes
      sample_idx <- 0 # set to start indexing the samples made from the individual
      if(ret[[i]]$isSample == TRUE) {
        ret[[i]]$Samples = list()
      }
      for(g in 1:length(ret[[i]]$par1$gam_idx)) {

        # now, we cycle over the incoming gametes and we get them as V1 and V2.
        # Then, if we are taking samples from this individual, we put them
        # into to the sample.  If not, or if we have already fulfilled all the samples,
        # then we recombine them and put them on the outgoing gamete list.

        # then collect information about the parental genotypes
        p1 <- ret[[i]]$par1$par
        p2 <- ret[[i]]$par2$par
        g1 <- ret[[i]]$par1$gam_idx[g]
        g2 <- ret[[i]]$par2$gam_idx[g]

        # these form the pair of incoming gametes (one from each parent)
        V1 <- ret[[p1]]$gametes[[g1]]
        V2 <- ret[[p2]]$gametes[[g2]]

        # Now we unite those gametes into a sample (if samples are taken from this individual).
        # If we have fulfilled all those samples, then we recombine and segregate the gametes
        # to be available for the next generation
        if(ret[[i]]$isSample == TRUE && g <= ret[[i]]$nSamples) {
          sample_idx <- sample_idx + 1
          ret[[i]]$Samples[[sample_idx]] <- list(V1, V2)
        } else {
          outgoing_idx <- outgoing_idx + 1
          # choose the recomb point:
          R <- recomb_point(M)
          tmplist[[outgoing_idx]] <- xover(V1, V2, R)  # this does the recombination and complementary segregation

          tmp_gametes <- do.call(c, tmplist)
          ret[[i]]$gametes <- tmp_gametes[sample(1:length(tmp_gametes))]
        }


      } # closes loop over g (incoming gamete pairs)

    }  # closes else
  }  # closes loop over the G list

  ret
}
