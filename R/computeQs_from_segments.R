#' return the admixture fractions of sampled individuals
#'
#' This operates on the output of segregate to add up the lengths
#' of all the segments segregated to different individuals to
#' thus compute the admixture fractions of each sampled
#' individual.
#' @param S the tibble output from \code{\link{segregate}}
#' @param check_total_length TRUE means it checks the total
#' genome length in each individual to make sure it checks out.
#' @return
#' This function returns a tibble with the following columns:
#' - `gpp`: the genomic simulation pedigree within which the individual sample was
#'     simulated.
#' - `index`: the index which gives which instance of the GSP the sample is from
#' - `ped_sample_id`: the id number of that the sampled individual had in the
#'     genomic simulation pedigree.
#' - `samp_index`: the index of the sample taken.  Some individuals in some
#'     genomic simulation pedigrees can produce more than one sample. This number
#'     tells you which sample it is.
#' - `pop_origin`: the "pedigree" population of origin of the segments that contributed
#'     to the `group_length`.  These are
#'     the simple "A", "B", "C", etc. designations given in the genomic simulation
#'     pedigree.
#' - `group_origin`: Which group of samples the segments contributing to
#'     the `group_length` originated from.  These are the groups of samples
#'     that were mapped onto the simple pedigree `pop_origin`s by the reppop
#'     request.
#' - `group_length`: the total length of segments from this group in this individual
#'     in this reppop index from this gpp (in bases).
#' - `tot_length`: the total number of bases from all origins carried by this
#'     individual.
#' - `admixture_fraction`: the fraction of all bases in the simulated individual
#'     that originate from the group in `group_origin`.
#' @export
#' @examples
#' #### Get output from segregate to use as input ####
#' # We construct an example here where we will request segregation
#' # down a GSP with two F1s and F1B backcrosses between two hypothetical
#' # populations, A and B.
#' gsp_f1f1b <- create_GSP("A", "B", F1 = TRUE, F1B = TRUE)
#'
#' # We will imagine that in our marker data there are three groups
#' # labelled "grp1", "grp2", and "grp3", and we want to create the F1Bs with backcrossing
#' # only to grp3.
#' reppop <- tibble::tibble(
#'   index = as.integer(c(1, 1, 2, 2)),
#'   pop = c("A", "B", "A", "B"),
#'   group = c("grp3", "grp1", "grp3", "grp2")
#' )
#'
#' # combine those into a request
#' request <- tibble::tibble(
#'   gpp = list(gsp_f1f1b),
#'   reppop = list(reppop)
#' )
#'
#' # now run it through segregate()
#' set.seed(5)  # just for reproducibility in example...
#' simSegs <- segregate(request, RecRates)
#'
#' #### Now we can run those through computeQs_from_segments() ####
#' Qs <- computeQs_from_segments(simSegs)
#'
#' Qs
computeQs_from_segments <- function(S, check_total_length = TRUE) {
  tmp <- S %>%
    group_by(gpp, index, ped_sample_id, samp_index, pop_origin, group_origin) %>%
    summarise(group_length = sum(end - start)) %>%
    group_by(gpp, index, ped_sample_id, samp_index) %>%
    mutate(tot_length = sum(group_length))

  if(check_total_length == TRUE) {
    chrom_lengths <- S %>%
      group_by(chrom) %>%
      summarise(ml = max(end))

    tot_len_by_max <- sum(chrom_lengths$ml) * 2

    problems <- tmp %>%
      filter(!near(tot_length, tot_len_by_max, tol = 0.0001))

    if(nrow(problems) > 0) {
      message("Mismatch in total genome length and sum of max chrom lengths:")
      problems %>%
        mutate(expected_to_be = tot_len_by_max)
      stop("Bailing out!")
    }
  }

  tmp %>%
    ungroup() %>%
    mutate(admixture_fraction = group_length / tot_length)
}
