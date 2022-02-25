#' read plink-formatted .map and .ped files into gscramble format
#'
#' This will read .ped and .map files (which can be gzipped, but
#' cannot be the binary .bed or .bim plink format).  The population
#' specifier of each individual is assumed to be the first column
#' (the FID column) in the .ped file.
#' @param map path to the plink `.map` file holding information about
#' the markers.  This file can be gzipped
#' @param ped path to the plink `.ped` file holding information about the
#' individuals and their genotypes.  This file can also be gzipped.
#' The function assumes that the second column of this file is unique across
#' all family IDs. If this is not the case, the function throws a warning.
#' It is assumed that missing genotypes are denoted by 0's in this file.
#' @return A list with three components:
#'   - `I_meta`: meta data about the individuals in the file.  This will
#'   include the columns of `group` (value of the first column of the
#'   ped file) and `indiv` (the ID of the individual stored in
#'   second column of the ped file). And wil also include the other four
#'   columns of the plink ped specification, named as follows: `pa`
#'   `ma`, `sex_code`, `pheno`.
#'  - `M_meta`: meta data about the markers.  A tibble with the columns
#'   `chrom`, `pos`, and `variant_id` and `link_pos`.  The `link_pos` column
#'   holds the information about marker position in Morgans or cM that was
#'   included in the `map` file.
#'   - `Geno`: a character matrix of genotypes with number-of-indviduals rows
#'   and number-of-markers * 2 columns. Missing genotypes in this matrix
#'   are coded as `NA`.
#' @export
#' @examples
#' map_plink <- system.file("extdata/example-plink.map.gz", package = "gscramble")
#' ped_plink <- system.file("extdata/example-plink.ped.gz", package = "gscramble")
#'
#' result <- plink2gscramble(map_plink, ped_plink)

plink2gscramble <- function(map, ped) {

  # read the map file
  maptib <- readr::read_tsv(
    map,
    col_names = c("chrom", "variant_id", "link_pos", "pos"),
    col_types = "ccnn"
  ) %>%
    select(chrom, pos, variant_id, link_pos)

  # read the ped file and make a matrix out of it
  full_gmat <- scan(ped, character()) %>%
    matrix(byrow = TRUE, ncol = 6 + 2 * nrow(maptib))

  # break it into the .fam part
  imeta <- as_tibble(
    full_gmat[, 1:6],
    .name_repair = "minimal"
  ) %>%
    setNames(c("group", "indiv", "pa", "ma", "sex_code", "pheno"))

  # and then make a matrix of the genotypes, and recode "0" as NA
  geno <- full_gmat[, -(1:6)]
  geno[geno == "0"] <- NA


  if((ncol(geno) / 2) != nrow(maptib)) {
    stop("Mismatch between number of markers in map and ped files.")
  }
  if(any(duplicated(imeta$indiv))) {
    warning("Duplicate individual IDs in column indiv")
  }

  # return
  list(
    I_meta = imeta,
    M_meta = maptib,
    Geno = geno
  )

}
