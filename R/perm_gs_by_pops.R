#' Take the output of rearrange_genos and permute everyone by population
#'
#' This was put together pretty hastily
#' @param GS the tibble that is the output from rearrange_genos
#' @param preserve_haplotypes If true then the Geno data is assumed phased
#' (first allele at an individual on one haplotype and second allele on the
#' other) and those haplotypes are preserved in this permutation of
#' genomic material amongst the founders.
#' @export
perm_gs_by_pops <- function(GS, preserve_haplotypes = FALSE) {

  row_groups <- NULL

  if(preserve_haplotypes == TRUE) {
    Mm = GS$M[[1]]
    row_groups <- Mm %>%
      ungroup() %>%
      mutate(idx = 1:n()) %>%
      group_by(chrom) %>%
      summarise(rows = list(idx)) %>%
      pull(rows)
  }

  scrambit <- GS$I[[1]] %>%
    group_by(group) %>%
    summarise(pop_mat = list(mat_scramble(GS$G[[1]][, abs_column], row_groups = row_groups))) %>%
    pull(pop_mat) %>%
    do.call(what = cbind, args = .)

  GS$G_permed <- list(scrambit)

  GS
}
