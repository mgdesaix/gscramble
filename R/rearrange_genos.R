#' rearrange genotypes into separate columns for each haplotype.
#'
#' This function first reorders individuals in the columns of
#' the matrix so that every population is together.  Then it
#' rearranges genotypes into separate columns for each haplotype
#' (or "halflotype" if they are unphased.)  This prepares the matrix
#' for different kinds of a permutation (within or between populations,
#' for example).
#'
#' It returns a list.  One component is the matrix, another is the
#' updated individual meta data, and the third is the marker meta
#' data.
#' @param G the genotype matrix (N rows and 2L columns)
#' @param Im the meta data for the N samples.
#' @param Mm the meta data for the L markers.
#' @export
rearrange_genos <- function(G, Im, Mm) {
  # check that Im, Mm, and the dimensions of G line up correctly
  stopifnot(ncol(G) / 2 == nrow(Mm))
  stopifnot(nrow(G) == nrow(Im))

  # now, reorder the individuals in Im and then G, and while we
  # are at it, transpose G so that markers are in the rows
  I2 <- Im %>%
    ungroup() %>%
    mutate(orig_index__ = 1:n()) %>%
    arrange(group) %>%
    mutate(new_index__ = 1:n())


  G2 <- t(G[I2$orig_index__, ])

  # now, modify G2 so that
  # each column is a haplotype (or halflotype) of an individual,
  gmatH <- rbind(
    G2[seq(1, nrow(G2), by = 2), ],
    G2[seq(2, nrow(G2), by = 2), ]
  ) %>%
    matrix(nrow = nrow(Mm))

  # and create a new tibble that calls out the positions of the haflotypes
  # in each individual
  ImetaH <- tibble(
    group = rep(I2$group, each = 2),
    indiv = rep(I2$indiv, each = 2),
    indiv_idx = rep(I2$new_index__, each = 2),
    haplo = rep(c(1, 2), length.out = nrow(I2) * 2),
    abs_column = 1:(nrow(I2) * 2), # absolute column
  ) %>%
    group_by(group) %>%
    mutate(gs_column = 1:n()) %>%  # this is the "group-specific column" (i.e. which column within the group)
    ungroup()

  # and here at the end we just return that as a tibble with one row.
  # This feels nicer than as a list, if you print it, you don't get
  # something that bombs your screen.
  tibble(G = list(gmatH), I = list(ImetaH), M = list(Mm))
}
