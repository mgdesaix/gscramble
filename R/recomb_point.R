
#' Randomly sample the positions of recombinations on a chromosome
#'
#' This function uses the observed recombination fractions such as those
#' in the data object [RecRates].  These are observed recombination fractions
#' in a series of adjacent small bins that are defined by a start position
#' `start_pos` and ending position `end_pos`.  This function operates on the
#' recombination rates for only a single chromosome at a time, so it will
#' typically be wrapped up inside a `purrr::map()` function to operate over
#' multiple chromosomes.
#' @details There are two main modes by which this function operates. If
#' `at_least_one == TRUE`, then the chromosome will always have at least
#' one recombination.  In this case, the position of that first recombination
#' is chosen according to the recombination rates.  Subsequently, the remaining
#' number of recombinations is chosen by the random variable Y, which is
#' the greater of zero and X - 1, where X is a
#' Poisson r.v. with mean given by the sum of the
#' recombination fractions.  These additional recombinations are placed
#' randomly according to the `rec_probs` but without interference.
#'
#' If `at_least_one == FALSE` then the total number of recombinations is
#' simulated as a Poisson r.v. with mean equal to the sum of the
#' recombination fractions. Again, their placement assumes no interference.
#'
#' Locations within each bin are chosen uniformly.  These locations are represented
#' as real numbers (rather than as integers) and those are used for describing segments,
#' as well.  This simplifies matters such as condensing information about multiple recombinations
#' that occurred at the same base pair.  In practice, this will have negligible effects, since
#' it is so unlikely that a recombination will ever occur in the same place.
#'
#' If no recombination occurs, this just returns a zero-length numeric vector.
#'
#' @param M a tibble that has the columns start_pos, end_pos, and rec_prob (where rec_prob is the
#' probability of a recombination occurring during meiosis within the interval defined by start_pos and end_pos.
#' @export
#' @examples
#' # for an example, create a tibble of bins, roughly 1 Mb each,
#' # on a chromosome of length roughly 150 Mb, and we assign each
#' # a rec_prob around 0.01
#' ends <- seq(1e6, 150e6, by = 1e6)
#' ends <- ends + floor(runif(length(ends), min = -1e4, max = 1e4))
#' set.seed(5)
#' M <- tibble::tibble(
#'     start_pos = c(0, ends[-length(ends)] + 1),
#'     end_pos = ends,
#'     rec_prob = abs(rnorm(length(ends), 0.01, 0.004))
#' )
#' recomb_point(M)
recomb_point <- function(M, at_least_one = TRUE) {

  # regardless of the value of at_least_one, we always simulate
  # a Poisson number of recombinations
  X <- rpois(n = 1, lambda = sum(M$rec_prob))

  if(at_least_one == TRUE) {
    Y <- max(c(1, X))
  } else {
    Y <- X
  }

  if(Y == 0) return(numeric(0))

  ivals <- sort(sample(1:nrow(M), size = Y, prob = M$rec_prob)) # get the map interval in which the recomb occurs

  # now, chose a point uniformly within that interval
  ret <- runif(length(ivals), min = M$start_pos[ivals], max = M$end_pos[ivals])

  ret
}
