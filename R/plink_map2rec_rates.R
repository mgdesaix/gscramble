
#' Convert a PLINK map file to gscramble RecRates bins in a tibble
#'
#' This is a convenience function to convert PLINK map format to
#' the format used in the gscramble `RecRates` object.  By default,
#' this function will use the positions of the markers and assume
#' recombination rates of 1 cM per megabase.  If the marker positions
#' are also available in Morgans in the PLINK map file, the these can be
#' used by setting `use_morgans` to TRUE.
#' @param map path to the plink `.map` file holding information about
#' the markers.  This file can be gzipped.
#' @param use_morgans logical. IF true, the third column in the PLINK map
#' file (assumed to have the position of the markers in Morgans) will be used
#' to calculate the `rec_probs` in the bins of the RecRates object.
#' @param cM_per_Mb numeric. If `use_morgans` is `FALSE`, physical positions
#' will be converted to recombination fractions as `cM_per_Mb` centiMorgans
#' per megabase.  Default is 1. This is also used to determine the recombination
#' probability on the last segment of the chromosome (beyond the last marker)
#' if `chrom_lengths` is used.
#' @param chrom_lengths if you know the full length of each chromosome, you can
#' add those in a tibble with columns `chrom` and `bp` where `chrom`
#' _must be a character vector_ (Don't leave them in as numerics) and `bp`
#' must be a numeric vector of the number of base pairs of length of each
#' chromosome.
#' @details
#' For simplicity, this function will assume that the length of the chromosome
#' is just one base pair beyond the last marker.  That is typically not correct
#' but will have no effect, since there are no markers to be typed out beyond
#' that point.  However, if you know the lengths of the chromosomes and want to
#' add those in there, then pass them into the `chrom_lengths` option.
#' @export
#' @examples
#' mapfile <- system.file(
#'     "extdata/example-plink-with-morgans.map.gz",
#'     package = "gscramble"
#'  )
#'
#' # get a rec-rates tibble from the positions of the markers,
#' # assuming 1 cM per megabase.
#' rec_rates_from_positions <- plink_map2rec_rates(mapfile)
#'
#' # get a rec-rates tibble from the positions of the markers,
#' # assuming 1.5 cM per megabase.
#' rec_rates_from_positions_1.5 <- plink_map2rec_rates(
#'     mapfile,
#'     cM_per_Mb = 1.5
#' )
#'
#' # get a rec-rates tibble from the cumulative Morgans position
#' # in the plink map file
#' rec_rates_from_positions_Morg <- plink_map2rec_rates(
#'     mapfile,
#'     use_morgans = TRUE
#' )
#'
#' # get a rec-rates tibble from the cumulative Morgans position
#' # in the plink map file, and extend it out to the full length
#' # of the chromosome (assuming for that last part of the chromosome
#' # a map of 1.2 cM per megabase.)
#' rec_rates_from_positions_Morg_fl <- plink_map2rec_rates(
#'     mapfile,
#'     use_morgans = TRUE,
#'     cM_per_Mb = 1.2,
#'     chrom_lengths = example_chrom_lengths
#' )
plink_map2rec_rates <- function(
    map,
    use_morgans = FALSE,
    cM_per_Mb = 1,
    chrom_lengths = NULL
) {

  # read the plink map file in:
  pmap <- readr::read_tsv(
    map,
    col_names = c("chrom", "id", "morgans", "bp")
  )

  # if use_morgans is FALSE, then we calculate the Morgans
  # by cM_per_Mb
  if(use_morgans == FALSE) {
    pmap <- pmap %>%
      mutate(morgans = cM_per_Mb * (bp / 1e6) / 100)
  }

  # now, we just use the morgans column for the recombination
  # rates. This should be an easy process of making bins
  # that have the markers just inside the end of each bin.
  ret <- pmap %>%
    group_by(chrom) %>%
    mutate(
      start_pos = lag(bp + 1, default = 1),
      end_pos = bp,
      rec_prob = morgans - lag(morgans, default = 0),
      chrom_len = max(bp) - 1
    ) %>%
    ungroup() %>%
    select(chrom, chrom_len, start_pos, end_pos, rec_prob) %>%
    mutate(chrom = as.character(chrom))  # at the end, make sure that these are characters

  if(!is.null(chrom_lengths)) {
    tib_list <- ret %>%
      left_join(chrom_lengths, by = "chrom")

    # bark an error if we don't have lengths for everything
    miss_bp <- tib_list %>%
      filter(is.na(bp))

    if(nrow(miss_bp) > 0) {
      mc <- paste(unique(miss_bp$chrom), collapse = ", ")
      stop(paste("No bp specified in chrom_lengths argument for chroms: ", mc))
    }

    # also bark an error if the chrom_length is less than any of the positions
    too_short <- tib_list %>%
      filter(end_pos > bp)

    if(nrow(too_short) > 0) {
      stop("chrom_length too short in ", nrow(too_short), "instances.")
    }


    # now we nest these things and add a row to each
    tib_list <- tib_list %>%
      mutate(chrom_len = bp) %>%
      select(-bp) %>%
      group_by(chrom) %>%
      nest()



    # here is a function to make add that final row from a tibble
    add_final_row <- function(tib) {
      cl <- max(tib$chrom_len)
      max_end <- cl + 1
      last_start <- max(tib$end_pos) + 1
      rec <- cM_per_Mb * ((max_end - last_start) / 1e6) / 100

      bind_rows(
        tib,
        tibble(
          chrom_len = cl,
          start_pos = last_start,
          end_pos = max_end,
          rec_prob = rec
        )
      )
    }

    # and here we use that function, and unnest the results
    ret <- tib_list %>%
      mutate(
        new_tib = map(.x = data, .f = add_final_row)
      ) %>%
      select(-data) %>%
      unnest(cols = c(chrom, new_tib))

  }

  # return the rec_rates tibble
  ret
}
