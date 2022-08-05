#` Scramble a matrix of genotype data
#'
#' This function assumes that M is a matrix with L rows (number of markers) and
#' 2 * N (N = number of individuals) columns.
#' There are two ways that the data might be permuted.  In the first,
#' obtained with `preserve_haplotypes = FALSE`,
#' the position of missing data within the matrix is held constant, but all
#' non-missing sites within a row (i.e. all gene copies at a locus) get
#' scrambled amongst the samples.  In the second way, just the columns are
#' permuted.  This preserves haplotypes in the data, if there are any.
#' The second approach should only be used if haplotypes are inferred in
#' the individuals.
#' @param M a matrix with L rows (number of markers) and 2 * N columns
#' where N is the number of individuals. Missing data must be coded
#' as NA
#' @param preserve_haplotypes logical indicating whether the haplotypes
# should be preserved.  If `row_groups` is not null, this is automatically
#' set to be TRUE
#' @param row_groups if not NULL must be a list of indexes of adjacent rows
#' that are all in the same groups.  For example: `list(1:10, 11:15, 16:30)`.
#' They should be in order and complete.
#' In practice, these should correspond to the indexes of markers on different
#' chromosomes.
#' @export
#' @examples
#' # make a matrix with alleles named as I.M.g, where I is individual
#' # number, M is marker number, and g is either "a" or "b" depending
#' # on which gene copy in the diploid it is.  4 indivs and 7 markers...
#' Mat <- matrix(
#'  paste(
#'    rep(1:4, each = 7 * 2),
#'    rep(1:7, 4 * 2),
#'    rep(c("a", "b"), each = 7),
#'    sep = "."
#'  ),
#'  nrow = 7
#' )
#'
#' # without preserving haplotypes
#' S1 <- mat_scramble(Mat)
#'
#' # preserving haplotypes with markers 1-7 all on one chromosome
#' S2 <- mat_scramble(Mat, preserve_haplotypes = TRUE)
#'
#' # preserving haplotypes with markers 1-3 on one chromosome and 4-7 on another
#' S3 <- mat_scramble(Mat, row_groups = list(1:3, 4:7))
mat_scramble <- function(M, preserve_haplotypes = !is.null(row_groups), row_groups = NULL) {
  if(!is.null(row_groups)) {
    idxs <- unlist(row_groups)
    if(any(duplicated(idxs))) stop("Duplicated values in row_groups.")
    if(!all( (1:nrow(M)) %in% idxs)) stop("row_groups does not contain all indexes.")
  }

  if(preserve_haplotypes == FALSE) {
    ret <- apply(M, 1, function(x) {
      x[!is.na(x)] <- sample(x[!is.na(x)])
      x
    }) %>%
      t()
  } else {
    if(is.null(row_groups)) {
      ret <- M[, sample(1:ncol(M))]
    } else {
      ret <- lapply(row_groups, function(x) {
        M[x, sample(1:ncol(M))]
      }) %>%
        do.call(rbind, args = .)
    }
  }
  return(ret)
}
