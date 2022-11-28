
#' check to make sure that the recombination rates tibble is formatted properly
#'
#' This is used internally when the rr object is about to be used.
#' @param rr The tibble of recombination-rate bins like RecRates
#' @keywords internal
check_rec_rates_formatting <- function(rr) {

  Flag <- FALSE
  msg <- ""
  # check that max(end_pos) chrom_len+1
  cl_tib <- rr %>%
    group_by(chrom, chrom_len) %>%
    summarise(max_end_pos = max(end_pos), .groups = "drop") %>%
    filter(!(chrom_len == max_end_pos - 1))

  # check that the next start position is one more than the last
  # ending position.
  tmp <- rr %>%
    group_by(chrom) %>%
    mutate(next_start = lead(start_pos)) %>%
    filter(!is.na(next_start))

  start_end <- tmp %>%
    filter(end_pos != next_start - 1)

  # and check that the first bin of each chromosome starts at 1
  one_starts <- tmp %>%
    slice(1) %>%
    filter(start_pos != 1)

  if(nrow(cl_tib) > 0) {
    stringy <- paste("\t- Chrom:", cl_tib$chrom, "\n")
    err <- paste("* end_pos of right-most bin not 1 greater than chromosome length for chromosomes: \n",
                 paste(stringy, collapse = ""))
    msg <- paste(msg, err, sep = "")
    Flag <- TRUE
  }
  if(nrow(start_end) > 0) {
    stringy <- paste("\t- Chrom:", start_end$chrom, "start_pos:", start_end$start_pos, "\n")
    err <- paste("* end_pos of these bins is not at 1 less than the next bin start position: \n", paste(stringy, collapse = ""))
    msg <- paste(msg, err, sep = "")
    Flag <- TRUE
  }
  if(nrow(one_starts) > 0) {
    stringy <- paste("\t- Chrom:", one_starts$chrom, "\n")
    err <- paste("* Left-most bin start position is not 1 for these chromosomes: \n", paste(stringy, collapse = ""))
    msg <- paste(msg, err, sep = "")
    Flag <- TRUE
  }

  if (Flag == TRUE) {
    stop(paste("Problems with recombination rates:\n", msg, sep = ""))
  }

}
