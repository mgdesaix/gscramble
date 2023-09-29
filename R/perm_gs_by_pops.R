#' Take the output of rearrange_genos and permute everyone by population
#'
#' This is done prior to assign random genomic fragments of individuals in the
#' sample to the founders of the GSP, to be dropped to the samples.
#' @param GS the tibble that is the output from rearrange_genos
#' @param preserve_haplotypes If true then the Geno data is assumed phased
#' (first allele at an individual on one haplotype and second allele on the
#' other) and those haplotypes are preserved in this permutation of
#' genomic material amongst the founders.
#' @param preserve_individuals If true then whole individuals are permuted
#' around the data set and the two gene copies at each locus are randomly
#' permuted within each individual.  (If `preserve_haplotypes = TRUE` then
#' the gene copies are not permuted within individuals. You should only ever
#' use `preserve_haplotypes = TRUE` if you have phased data.)
#' @export
#' @examples
#' # first get the output of rearrange_genos
#' RG <- rearrange_genos(Geno, I_meta, M_meta)
#'
#' # then permute by the populations
#' PG <- perm_gs_by_pops(RG)
perm_gs_by_pops <- function(GS, preserve_haplotypes = FALSE, preserve_individuals = FALSE) {

  row_groups <- NULL

  if(preserve_individuals == FALSE && preserve_haplotypes == TRUE) {
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
    summarise(pop_mat = list(
      mat_scramble(
        GS$G[[1]][, abs_column],
        row_groups = row_groups,
        preserve_individuals = preserve_individuals,
        preserve_haplotypes = preserve_haplotypes
      ))) %>%
    pull(pop_mat) %>%
    do.call(what = cbind, args = .)

  GS$G_permed <- list(scrambit)

  GS
}
