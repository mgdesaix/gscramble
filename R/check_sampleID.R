#' Check that individual IDs are in the same order in the genotype and the individual metadata tibbles.
#'
#' This function will check that the order of individuals is the same in the genotype tibble and the individual metadata tibble.
#' There are two inputs. The first input is a tibble (see 'Geno' for an example) that contains the genotype information
#' for each individual in a row and has the individual IDs as the row names. The second input is a tibble with individual
#' metadata information (see 'I_meta' for an example) with the group or population name in the first column and the individual ID in the
#' second column. The function will also make sure that the number of individuals in each input tibble is the same.
#'
#'
#' @param geno_data a tibble with genotypes for each individual.
#' The tibble must consist of 2X the number of loci (each allele is in it's own column and two adjacent columns make a locus).
#' Rownames should be sample IDs.
#' @param ind_meta a tibble with meta data for individuals. It must consist of the columns (group, indiv).
#' @return This function will return a message stateing whether the sample IDs for the genotype and individual metadata tibbles are in the same order
#' and will return an error message if each tibble has a different number of individuals.
#' @export
#' @examples
#'
#'  # check that the order of sample IDs is the same in the genotype input and the individual metadata input tibble
#'
#' check_sampleID(Geno,I_meta)
#'


check_sampleID<-function(geno_data,ind_meta){

if(setequal(rownames(geno_data),ind_meta[[2]]) == TRUE) print("IDs in genotype data object match individual metadata IDs.")
if(setequal(rownames(geno_data),ind_meta[[2]]) == FALSE) print("ERROR: IDs in genotype data object do not match individual metadata IDs. Check that the order of individuals matches in these two data objects and that your genotype data object has rownames.")
if(nrow(geno_data) != nrow(ind_meta)) print("ERROR: There number of individuals in the genotype data object differs from the number of individuals in the individual meta data file.")

}
