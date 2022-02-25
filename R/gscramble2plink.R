#' Write gscramble I_meta, M_meta, and Geno to a plink file
#'
#' Writes genetic and individual information in gscramble's
#' I_meta, M_meta, and Geno like objects into a uncompressed plink
#' `.ped` and `.map` files.
#' @param I_meta a tibble of individual meta data
#' with at least the columns of `group` and `indiv`.
#' @param M_meta a tibble of marker meta data with at least the columns
#' of `chrom`, `pos`, and `variant_id`.
#' @param Geno a character matrix of genotypes.  Num-indivs rows and
#' num-markers * 2 columns, with missing denoted denoted by NA.
#' @param prefix the file path and prefix into which to write out the files
#' @return Returns TRUE if successful.
#' @export
#' @examples
#' gscramble2plink(I_meta, M_meta, Geno)
gscramble2plink <- function(I_meta, M_meta, Geno, prefix = tempfile()) {

  # some error checking
  if (length(intersect(names(I_meta), c("group", "indiv"))) != 2) {
    stop("I_meta must have at least two columns named group and indiv")
  }
  # format I_meta into the 6 columns we want:
  if (!("pa" %in% names(I_meta)))
    I_meta <- I_meta %>% mutate(pa = "0")
  if (!("ma" %in% names(I_meta)))
    I_meta <- I_meta %>% mutate(ma = "0")
  if (!("sex_code" %in% names(I_meta)))
    I_meta <- I_meta %>% mutate(sex_code = "0")
  if (!("pheno" %in% names(I_meta)))
    I_meta <- I_meta %>% mutate(pheno = "-9")

  Imfinal <- I_meta %>%
    select(group, indiv, pa, ma, sex_code, pheno)


  # dealing with M_meta
  if(length(intersect(names(M_meta), c("chrom", "pos", "variant_id") )) != 3) {
    stop("M_meta must include at least the columns of chrom, pos, and variant_id.")
  }
  if(!("link_pos" %in% names(M_meta)))
    M_meta <- M_meta %>% mutate(link_pos = 0)

  Mmfinal <- M_meta %>%
    select(chrom, variant_id, link_pos, pos)

  # dealing with Geno
  Geno[is.na(Geno)] <- "0"

  # now, write them out
  mapfile <- paste0(prefix, ".map")
  pedfile <- paste0(prefix, ".ped")

  write.table(
    cbind(as.matrix(Imfinal), Geno),
    file = pedfile,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )

  message("pedfile written to ", pedfile)

  write.table(
    Mmfinal,
    file = mapfile,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )

  message("mapfile written to ", mapfile)

  TRUE
}
