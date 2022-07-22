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
