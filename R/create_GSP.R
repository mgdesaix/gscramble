#' Create a GSP from user inputs about what type of hybrids from which populations to create hybrids,
#'
#' what types of individuals they would like to sample
#' in terms of F1, F2, F1 backcross (F1B), or F1 backross 2 (F1B2) individuals.
#' Pop IDs must be of class character and at least one parameter from the list (F1,F2,F1B, or F1B2) must be TRUE.
#' Default setting for these parameters is FALSE.
#' @param pop1 character name for population 1
#' @param pop2 character name for population 2
#' @param F1 logical indicating whether you would like to have sampled F1 hybrids in the output.
#' @param F2 logical indicating whether you would like to have sampled F2 hybrids in the output.
#' Needs more documenting.
#' @export
#' @examples
#' # create a GSP that produces F1s and F1B's from pops A and B
#' gsp <- create_GSP("A", "B", F1 = TRUE, F1B = TRUE)
#'
create_GSP<-function(pop1,pop2,F1=F,F2=F,F1B=F,F1B2=F){

  Mateparams<-c(F1,F2,F1B,F1B2)
  GSP_index<-sum(Mateparams*c(1,2,4,8))

  if(GSP_index == 0)stop("Error: At least one parameter (F1, F2, F1B, or F1B2) must be TRUE")

  GSP<-GSP_opts[[GSP_index]]
  GSP[,c("hpop1", "hpop2")] <- lapply(GSP[,c("hpop1", "hpop2")], gsub, pattern = "p1", replacement = pop1)
  GSP[,c("hpop1", "hpop2")] <- lapply(GSP[,c("hpop1", "hpop2")], gsub, pattern = "p2", replacement = pop2)
  return(GSP)
}
