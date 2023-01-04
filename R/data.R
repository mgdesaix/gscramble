

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




#' Example Genomic Simulation Pedigree (GSP) with 13 members
#'
#' A GSP is a pedigree with no inbreeding which specifies
#' This is a relatively complex GSP.  The tibble `GSP` specifies its
#' structure using the following columns:
#' - `ind`: the numeric identifier for the individual specific to the row
#'   (we will call that the "focal individual").
#' - `par1`: numeric identifier of the first parent of the focal individual.
#'    Must be NA for pedigree founders.
#' - `par2`: numeric identifier of the second parent of the focal individual.
#'    Must be NA for pedigree founders.
#' - `ipar1`: the number of gametes that will be incoming from the first parent
#'    to the focal individual. Must be NA for pedigree founders. Note that this
#'    must be equal to `ipar2`.
#' - `ipar2`: the number of gametes that will be incoming from the second parent
#'    to the focal individual. Must be NA for pedigree founders. Note that this
#'    must be equal to `ipar1`.
#' - `hap1`: character name for the first haplotype of a founder (if the focal
#'    individual is a founder).  Must be NA for pedigree non-founders.
#' - `hap2`: character name for the second haplotype of a founder (if the focal
#'    individual is a founder).  Must be NA for pedigree non-founders.
#' - `hpop1`: character ID of the population from which haplotype 1 comes from.
#'    Must be NA for pedigree non-founders.
#' - `hpop2`: character ID of the population from which haplotype 2 comes from.
#'    Must be NA for pedigree non-founders.
#' - `sample`: character ID for the sample from the focal individual. NA if no samples
#'    are taken from the focal individual, and must be NA for any pedigree founders.
#' - `osample`: numeric value giving the number or samples that are taken from this
#'    individual.  `osample` must be less than or equal to `ipar1` and `ipar2`. If
#'    `osample` is less than `ipar1` and `ipar2`, then some gametes must get passed
#'    on to descendants of the focal individual.
#'
#' The CSV version of this is in `extdata/13-member-ped.csv`.
#'
#' @format A tibble
#' @source Created by the developers.
#' @docType data
#' @name GSP
NULL


#' Tibble holding specification for a 7 member genomic permutation pedigree.
#'
#' This has 4 founders, each one from a different population, and it provides
#' four samples that are the product of F1 matings (an A x B F1 crossing with
#' a C x D F1.)
#'
#' For details on the columns, see the documentation for \code{\link{GSP}}.
#' @format A tibble
#' @source Created by the developers.
#' @docType data
#' @name gsp4
NULL

#' Tibble holding specification for a 5 member genomic permutation pedigree.
#'
#' This has 3 founders
#'
#' For details on the columns, see the documentation for \code{\link{GSP}}.
#' @format A tibble
#' @source Created by the developers.
#' @docType data
#' @name gsp3
NULL

#' A list of tibbles specifying the pedigrees available from `createGSP()`
#'
#' This is a list that is used by the function `createGSP()`.  There are 15
#' different genomic simulation pedigrees, specified as tibbles, in this
#' list.
#'
#' @format A list of 15 tibbles
#' @source Written by package authors Rachael and Tim
#' @docType data
#' @name GSP_opts
NULL



#' A simple example of a reppop table
#'
#' A `reppop` table in gscramble is used to define how the founder populations
#' in a GSP (typically named something like "A", "B", etc.) are mapped to the
#' groups/populations of individuals, as specified in the individual meta data
#' (an example of which is found in `I_meta`).
#'
#' This particular version shows a situation where individuals from group Pop1 will be
#' sampled as founders from A and from group Pop2 will be sampled as founders from
#' B into the GSP.  Since this `RepPop1` example has two values of index: 1 and 2,
#' it specifies that individuals from the populations will be sampled without replacment,
#' two times into the founders on the pedigree.
#' @format A tibble with three columns: `index`, which must be of type integer,
#' `pop`, and `group` of type character.
#' @source The developers created this.
#' @docType data
#' @name RepPop1
NULL


#' Another simple example of a reppop table
#'
#' A `reppop` table in gscramble is used to define how the founder populations
#' in a GSP (typically named something like "A", "B", etc.) are mapped to the
#' groups/populations of individuals, as specified in the individual meta data
#' (an example of which is found in `I_meta`).
#'
#' This particular version shows a situation where individuals from four different
#' groups (Pop1, Pop2, Pop3, and Pop4) get mapped to four different founder groups
#' (A, B, C, D) in the GSP.
#' Since this `RepPop4` example has three values of index: 1, 2, and 3,
#' it specifies that there will be three rounds of sampling of individuals
#' from the populations to be the founders on the pedigree.  That will be done
#' entirely without replacement (individuals are not replaced after each round!)
#' @format A tibble with three columns: `index`, which must be of type integer,
#' `pop`, and `group` of type character.
#' @source The developers created this.
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
