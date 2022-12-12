

#' Genotype matrix of 78 individuals and 100 SNP markers
#'
#' This dataset represents 3 distinct populations of feral swine in the United States.
#' To make this dataset computationally efficient, only 3 chromosomes (12, 17, 18) from the pig genome were used.
#' Further, loci were reduced to the 100 most informative loci for distinguishing the 3 populations.
#' Each row of the genotype matrix includes the genotypes for a single individual.
#' The genotype matrix is in a standard "two-column" format for a diploid species
#' where two adjacent columns make up a locus and each column of a locus contains an allele.
#' Genotype data is stored in the character class.
#' Missing data is represented by NAs.
#' Individual IDs and population assignment information can be found in \code{\link{I_meta}}
#' Locus and chromosome information for genotypes can be found in \code{\link{M_meta}}
#'
#' @format A character matrix. Each row represents an individual and columns contain the genotypes for individuals
#' in a "two-column" format where two adjacent columns make up a locus with an allele in each column.
#' @source USDA-APHIS-WS-National Wildlife Research Center Feral Swine Genetic Archive
#' @docType data
#' @name Geno
NULL


#' Metadata for 78 individuals
#'
#' The tibble 'I_meta' contains the Individuals IDs and group specifications for 78 individuals. Each row of 'I_meta'
#' contains the metadata for an individual. 'I_meta' has two columns, a 'group' column with the group or population assignment
#' for a given individual and an 'indiv' column with the individual sample IDs. The information in each column are stored as characters.
#' The order of the individual IDs corresponds to the genotypes found in \code{\link{Geno}}.
#'
#' @format A tibble with two columns: group and indiv.
#' @source USDA-APHIS-WS-National Wildlife Research Center Feral Swine Genetic Archive
#' @docType data
#' @name I_meta
NULL


#' Metadata for 100 molecular markers
#'
#' This data set contains the metadata for the 100 most divergent SNP loci for three feral swine populations
#' sampled in the United States. To make the dataset more computationally efficient, only 3 chromosomes were
#' used (12,17, and 18). The metadata for the SNP loci is in a tibble with three columns:
#' chrom (character), pos (numeric), variant_id (character).
#' The column 'chrom' contains the chromosome ID where the SNP is located, the column 'pos' gives the base pair
#' location on the chromosome, and the column 'variant_id' gives the name of the SNP.
#' Each row of the metadata tibble contains the metadata for a given SNP locus.
#' The individual genotypes for each of these SNP loci can be found in \code{\link{Geno}}.
#'
#' @format A tibble with three columns: chrom, pos, and variant_id.
#' @source Chromosome, position, and variant IDs are from the Sus scrofa 10.2 genome \url{https://www.ncbi.nlm.nih.gov/assembly/GCF_000003025.5/}.
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


#' Lengths of the three chromosomes used in the example data set
#'
#' These are for the example of how to use `plink_map2rec_rates()`.
#'
#' @format A tibble with two columns: `chrom` and `bp`.
#' @source These lengths were taken from the maximal values in
#' Tortereau, Flavie, et al. "A high density
#' recombination map of the pig reveals a correlation between sex-specific
#' recombination and GC content." BMC genomics 13.1 (2012): 1-12. It is available
#' for download from \url{https://static-content.springer.com/esm/art%3A10.1186%2F1471-2164-13-586/MediaObjects/12864_2012_4363_MOESM1_ESM.txt}.
#' After downloading the data were processed to remove inconsistencies with the
#' marker data set used for M_meta and Geno.
#' @docType data
#' @name example_chrom_lengths
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
