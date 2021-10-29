#' Create a GSP from user inputs about what type of hybrids from which populations to create hybrids
#'
#' This function allows the user to choose two populations and create a GSP input for gscramble.
#' The input requires two different population IDs of class character as well as at least one TRUE statement for one of
#' the hybrid parameters (F1, F2, F1B, F1B2). The GSP will indicate hybrid individuals that will be sampled based on
#' which F1, F2, F1B, and F1B2 parameters are TRUE. F1= TRUE means F1 hybrids will be sampled. F2=TRUE means F2 hybrids
#' will be sampled. F1B2=TRUE means F1 backcross hybrids will be sampled. F1B2=TRUE means F1 backcross 2 hybrids will be sampled.
#' Default setting for these parameters is FALSE. The function then outputs a GSP in tibble format that can be used for other
#' functions in gscramble including "check_pedigree_for_inbreeding" and "prep_gsp_for_hap_dropping".
#'
#'
#' @param pop1 character name for population 1
#' @param pop2 character name for population 2
#' @param F1 logical indicating whether you would like to have sampled F1 hybrids in the output.
#' @param F2 logical indicating whether you would like to have sampled F2 hybrids in the output.
#' @param F1B logical indicating whether you would like to have sampled F1 backcross hybrids in the output.
#' @param F1B2 logical indicating whether you would like to have sampled F1 backcross 2 hybrids in the output.
#' @return This function returns a GSP in tibble format with the user argument for pop1 and pop2 autopopulated
#' in the hpop1 and hpop2 columns.
#' @export
#' @examples
#' # create a GSP that generates hybrids and samples F1s and F1B's from pops A and B
#' gsp <- create_GSP("A", "B", F1 = TRUE, F1B = TRUE)
#'
#' @examples
#' #create a GSP that generates hybrids and samples F1s, F2s, F1Bs, F1B2s from pops A and B
#' gsp <- create_GSP("A", "B", F1 = TRUE, F1=TRUE, F1B = TRUE, F1B2=TRUE)
#'
create_GSP<-function(pop1,pop2,F1=F,F2=F,F1B=F,F1B2=F){

  Mateparams<-c(F1,F2,F1B,F1B2)
  GSP_index<-sum(Mateparams*c(1,2,4,8))

  if(GSP_index == 0)stop("Error: At least one parameter (F1, F2, F1B, or F1B2) must be TRUE")
  if(pop1 == pop2)stop("Error: You must select two different populations for the pop1 and pop2 parameters")

  GSP<-GSP_opts[[GSP_index]]
  GSP[,c("hpop1", "hpop2")] <- lapply(GSP[,c("hpop1", "hpop2")], gsub, pattern = "p1", replacement = pop1)
  GSP[,c("hpop1", "hpop2")] <- lapply(GSP[,c("hpop1", "hpop2")], gsub, pattern = "p2", replacement = pop2)
  return(GSP)
}
