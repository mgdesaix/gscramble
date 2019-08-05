

#' Genotype matrix of 329 individuals and 25165 SNP markers
#'
#' This is a data set from feral pigs.  It is a standard "two-column"
#' format genotype data matrix for a diploid species.
#'
#' Need to document it a little more....
#' @format A character matrix.
#' @source Must add...
#' @docType data
#' @name Geno
NULL


#' Meta data for 329 individuals
#'
#' Individuals IDs and group specifications for 329 individuals
#' whose genotypes are in \code{\link{Geno}}
#'
#' @format A tibble with three columns: chrom, pos, and variant_id.
#' @source Must add...
#' @docType data
#' @name I_meta
NULL


#' Meta data for 25165 molecular markers
#'
#' Chromosome, position, and ID for 25156 molecular
#' markers in \code{\link{Geno}}
#'
#' @format A tibble with two columns: group and indiv.
#' @source Must add...
#' @docType data
#' @name M_meta
NULL


#' Recombination rate data for many roughly 1 Mb bins
#'
#' Chromosome, start position and end position and probability of recombination
#' within the bin for chromosomes in pigs.
#'
#' @format A tibble with four columns: chrom, chrom_len, start_pos, end_pos, and rec_prob.
#' @source Must add, inluding how I computed it from Torterau.
#' @docType data
#' @name RecRates
NULL
