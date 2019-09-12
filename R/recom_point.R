
#' chooses positions of recombination points
#'
#' It is based on a map M.
#' @param M a tibble that has the columns start_pos, end_pos, and rec_prob (where rec_prob is the
#' probability (or intensity) of recombinations in the interval defined by start_pos and end_pos.
#' @keywords internal
#' @export
recomb_point <- function(M, always_one = TRUE) {
  if(always_one == TRUE) {  # in this case, always have one recombination. Just doing
    # this as a simple thing to do while developing it.  Will
    # do more as time goes on.
    ival <- sample(1:nrow(M), size = 1, prob = M$rec_prob) # get the map interval in which the recomb occurs

    # now, chose a point uniformly within that interval
    ret <- runif(length(ival), min = M$start_pos[ival], max = M$end_pos[ival])
  }
  ret
}
