
#` scramble a matrix of genetic data
#'
#' Assumes that M is a matrix with L rows (number of markers) and
#' 2 * N (N = number of individuals) columns.  Holds position of
#' missing data constant.
mat_scramble <- function(M, missing = "0") {
  apply(M, 1, function(x) {
    x[!(x == missing)] <- sample(x[!(x == missing)])
    x
  }) %>%
    t()
}


#' Take the output of rearrange_genos and permute everyone by population
#'
#' This was put together pretty hastily
#' @param GS the tibble that is the output from rearrange_genos
#' @export
perm_gs_by_pops <- function(GS) {

  scrambit <- GS$I[[1]] %>%
    group_by(group) %>%
    summarise(pop_mat = list(mat_scramble(GS$G[[1]][, abs_column]))) %>%
    pull(pop_mat) %>%
    do.call(what = cbind, args = .)

  GS$G_permed <- list(scrambit)

  GS
}
