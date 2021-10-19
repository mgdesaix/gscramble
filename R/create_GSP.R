#' Creates a GSP based on user inputs for which populations from which to create hybrids, what types of individuals they would like to sample
#' in terms of F1, F2, F1 backcross (F1B), or F1 backross 2 (F1B2) individuals.
#' Pop IDs must be of class character and at least one parameter from the list (F1,F2,F1B, or F1B2) must be TRUE.
#' Default setting for these parameters is FALSE.
#'
#' Needs more documenting.
#' @export
create_GSP<-function(pop1,pop2,F1=F,F2=F,F1B=F,F1B2=F){

  Mateparams<-c(F1,F2,F1B,F1B2)
  GSP_index<-sum(Mateparams*c(1,2,4,8))

  if(GSP_index == 0)stop("Error: At least one parameter (F1, F2, F1B, or F1B2) must be TRUE")

  GSP<-GSP_opts[[GSP_index]]
  GSP[,c("hpop1", "hpop2")] <- lapply(GSP[,c("hpop1", "hpop2")], gsub, pattern = "p1", replacement = pop1)
  GSP[,c("hpop1", "hpop2")] <- lapply(GSP[,c("hpop1", "hpop2")], gsub, pattern = "p2", replacement = pop2)
  return(GSP)
}
