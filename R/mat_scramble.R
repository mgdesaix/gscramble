#` Scramble a matrix of genotype data
#'
#' Assumes that M is a matrix with L rows (number of markers) and
#' 2 * N (N = number of individuals) columns.  Holds position of
#' missing data constant.
#' @param M a matrix with L rows (number of markers) and 2 * N columns
#' where N is the number of individuals.
#' @param missing The value used to denote missing data in the matrix.
#' @export
mat_scramble <- function(M, missing = "0") {
  apply(M, 1, function(x) {
    x[!(x == missing)] <- sample(x[!(x == missing)])
    x
  }) %>%
    t()
}
