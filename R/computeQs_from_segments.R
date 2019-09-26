#' return the admixture fractions of sampled individuals
#'
#' This operates on the output of segregate to add up the lengths
#' of all the segments segregated to different individuals to
#' thus compute the admixture fractions of each sampled
#' individual.
#' @param S the tibble output from \code{\link{segregate}}
#' @param check_total_length TRUE means it checks the total
#' genome length in each individual to make sure it checks out.
#' @export
computeQs_from_segments <- function(S, check_total_length = TRUE) {
  tmp <- S %>%
    group_by(gpp, rep, ped_sample_id, samp_index, pop_origin, group_origin) %>%
    summarise(group_length = sum(end - start)) %>%
    group_by(gpp, rep, ped_sample_id, samp_index) %>%
    mutate(tot_length = sum(group_length))

  if(check_total_length == TRUE) {
    chrom_lengths <- S %>%
      group_by(chrom) %>%
      summarise(ml = max(end))

    tot_len_by_max <- sum(chrom_lengths$ml) * 2

    problems <- tmp %>%
      filter(tot_length != tot_len_by_max)

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
