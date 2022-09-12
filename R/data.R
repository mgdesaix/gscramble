

#' Genotype matrix of 78 individuals and 100 SNP markers
#'
#' This dataset represents 3 distinct populations of feral swine in the United States.
#' To make this dataset computationally efficient, only 3 chromosomes (12, 17, 18) from the pig genome were used.
#' Further, loci were reduced to the 100 most informative loci for distinguishing the 3 populations.
#' The genotype matrix is in a standard "two-column"
#' format genotype data matrix for a diploid species
#' where two adjacent columns make up a locus and each column of a locus contains an allele.
#' Genotype data is stored in the character class.
#' Missing data is represented by NAs.
#' Individual IDs and population assignment can be found in \code{\link{I_meta}}
#' Locus and chromosome information for genotypes can be found in \code{\link{M_meta}}
#'
#' Need to document it a little more....
#' @format A character matrix.Each row represents an individual and
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
#' @source These rates were estimated in: Tortereau, Flavie, et al. "A high density
#' recombination map of the pig reveals a correlation between sex-specific
#' recombination and GC content." BMC genomics 13.1 (2012): 1-12. It is available
#' for download from \url{https://static-content.springer.com/esm/art%3A10.1186%2F1471-2164-13-586/MediaObjects/12864_2012_4363_MOESM1_ESM.txt}.
#' After downloading the data were processed to remove inconsistencies with the
#' marker data set used for M_meta and Geno.
#' @docType data
#' @name RecRates
NULL

#' Tibble holding specification for a 13 member genomic simulation pedigree
#'
#' The CSV version of this is in `extdata/13-member-ped.csv`.
#'
#' Need to document it a little more....
#' @format A tibble
#' @source I just made it up!
#' @docType data
#' @name GSP
NULL



#' Tibble holding specification for a 7 member genomic permutation pedigree.
#'
#' This has 4 founders, each one from a different population, and it provides
#' four samples that are the product of F1 matings (an A x B F1 crossing with
#' a C x D F1.)
#'
#' Need to document it a little more....
#' @format A tibble
#' @source I just made it up!
#' @docType data
#' @name gsp4
NULL

#' Tibble holding specification for a 5 member genomic permutation pedigree.
#'
#' This has 3 founders
#'
#' Need to document it a little more....
#' @format A tibble
#' @source Tim made it up!
#' @docType data
#' @name gsp3
NULL

#' A list of tibbles specifying the pedigrees available from `createGSP()`
#'
#' This is a list that is used by the function `createGSP()`.  There are 15
#' different genomic permutation pedigrees, specified as tibbles, in this
#' list.
#'
#' @format A list of 15 tibbles
#' @source Written by package authors Rachael and Tim
#' @docType data
#' @name GSP_opts
NULL



#' A reppop tibble for an example
#'
#' Needs more explanation
#' @format A tibble...
#' @source eric
#' @docType data
#' @name RepPop1
NULL


#' A reppop tibble for an example
#'
#' Needs more explanation
#' @format A tibble...
#' @source eric
#' @docType data
#' @name RepPop4
NULL




#' Example of a segments tibble
#'
#' This is the Segments object from the gscramble tutorial.
#' It is included as a data object to use in the example for
#' the function `plot_simulated_chromosome_segments()`.
#' @format A tibble like that produced by the `segregate()` function.
#' @source Made from package functions
#' @docType data
#' @name example_segments
NULL
