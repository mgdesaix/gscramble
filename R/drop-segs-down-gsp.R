#' High level function for dropping segments down a GSP
#'
#' This one asks for the number of reps to do, and it also
#' automatically does it over chromosomes and returns the results
#' in a nice tidy tibble.
#' @param GSP the pedigree to use for the simulation, in the format of
#' the package data \code{\link{GSP}}.
#' @param RR the recombination rates in the format of the package data
#' \code{\link{RecRates}}
#' @param Reps the number of times to do the simulation.  Different replicates
#' are denoted by the rep column in the output tibble.
#' @export
#' @examples
#' simSegs <- drop_segs_down_gsp(GSP, RecRates, 4)
#'
#'
drop_segs_down_gsp <- function(GSP, RR, Reps) {

  # TODO: add function call to test GSP to make sure it is valid

  # Convert GSP into a list structure that allows seg-dropping
  Plist <- prep_gsp_for_hap_dropping(GSP)

  # to do it over chromosomes, we break up the RR over chromosomes
  # and attach the Plist from above to each one
  RR_by_chrom <- RR %>%
    group_by(chrom) %>%
    nest(map_stuff = chrom_len:rec_prob) %>%
    mutate(chrom_len = map_dbl(map_stuff, function(x) x$chrom_len[1])) %>%
    mutate(gsp_init = list(Plist))

  # now, cycle over the Reps and drop segments for each replicate.
  # At the end we bind_rows them all together.
  rvec <- 1:Reps
  names(rvec) <- rvec

  Res1 <- suppressWarnings(  # doing this cuz of the "Vectorizing 'vctrs_list_of' elements may not preserve their attributes" warning
    lapply(rvec, function(r) {
      RR_by_chrom %>%
        mutate(segged = pmap(.l = list(G = gsp_init, M = map_stuff, C = chrom_len),
                             .f = function(G, M, C) seg_haps_through_gsp(G = G, M = M, chrom_len = C))
        )
    }) %>%
      bind_rows(.id = "rep") %>%
      mutate(rep = as.integer(rep))
  )

  # now we tidy up the segged elements, to have a column where we retain just the samples.
  # Note that we might like to have Res1 for other types of analysis, but now we strip this
  # thing down to what we will need, ultimately, to propagate variants back.
  Res2 <- tidy_up_sampled_haplos(Res1)

  # and now we prune that down and unnest the ped_samples, then expand the segments in them
  # so that each segment gets its own row so that we don't have any more list
  # columns and we can easily see all the segments in the samples.
  # This is the data frame that we will return.
  Res2 %>%
    select(rep, chrom, ped_samples) %>%
    unnest(ped_samples) %>%
    mutate(gam_tibs = map(gamete_segments, seg2tib)) %>%
    unnest(gam_tibs) %>%
    separate(tmp_seg_names, into = c("pop_origin", "rs_founder_haplo")) %>%
    mutate(rs_founder_haplo = as.integer(rs_founder_haplo)) %>%
    arrange(rep, ped_sample_id, samp_index, gamete_index, chrom, start)

}
