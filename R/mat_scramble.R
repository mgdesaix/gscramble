#' Scramble a matrix of genotype data
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
#'
#' There is now an additional way of permuting:  if
#' `preserve_individuals = TRUE`, then entire individuals are permuted.
#' If `preserve_haplotypes = FALSE`, then the gene copies at each locus
#' are randomly ordered within each individual before permuating them.
#' If `preserve_haplotypes = TRUE` then that initial permutation is not
#' done.  This should only be done if the individuals are phased and that
#' phasing is reprented in how the genotypes are stored in the matrix.
#' @param M a matrix with L rows (number of markers) and 2 * N columns
#' where N is the number of individuals. Missing data must be coded
#' as NA
#' @param preserve_haplotypes logical indicating whether the haplotypes
# should be preserved.  If `row_groups` is not null, this is automatically
#' set to be TRUE
#' @param preserve_individuals logical indicating whether the genes within
#' each individual should stay togeter.
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
mat_scramble <- function(M, preserve_haplotypes = !is.null(row_groups), row_groups = NULL, preserve_individuals = FALSE) {
  if(!is.null(row_groups)) {
    idxs <- unlist(row_groups)
    if(any(duplicated(idxs))) stop("Duplicated values in row_groups.")
    if(!all( (1:nrow(M)) %in% idxs)) stop("row_groups does not contain all indexes.")
  }
  # here is a block that will return if preserve_individuals is TRUE
  if(preserve_individuals == TRUE) {
    if(preserve_haplotypes == FALSE) {
      # cycle over individuals and sample the gene copies at each locus within them
      M2 <- lapply(seq(1, ncol(M), by = 2), function(i) {  # cycle over individuals
        apply(M[, c(i, i+1)], 1, sample)
      }) %>%
        do.call(rbind, .) %>%
        t()
    } else {  # if preserve haplotypes == TRUE, we don't do that step
      M2 <- M
    }

    # now, we permute the individuals around M2 and return
    num_ind <- ncol(M2) / 2
    ind_ord <- sample(1:num_ind)
    ind_pick <- (2 * rep(ind_ord, each = 2)) - c(1, 0)
    return(M2[, ind_pick])
  }


  # This only happens if preserve_individuals was FALSE
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
