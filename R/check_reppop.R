#' check that the RepPop is formatted correctly
#'
#' The input 'RepPop' is used in the segregate() function to create admixed individuals based on defined pedigrees (GSP).
#' In order for segregate() to work, the RepPop defined by the user must be formatted correctly. The function check_repop()
#' checks the RepPop for errors that would result in errors while using segregate(). Specifically, it checks that you have
#' three columns with the names ('index','pop', and 'group'), that the class of each column is correct, and that your indices
#' (column 'index') starts with 1 and all other indices are in consecutive order.
#'
#' @param reppop a tibble with the information used to define how individuals among groups combine to form
#' hybrids. It must consist of the columns (index, pop, group).
#' @return This function will return an error message if reppop is not formatted properly. If no issues exist,
#' a message will be returned stating that the reppop is formatted properly.
#' @export
#' @examples
#' # The example uses the built in dataset RepPop1
#' # to use the check_reppop function
#'
#' check_reppop(RepPop1)
#'


check_reppop<-function(reppop){
  Flag<-FALSE
  #check that three columns exist 'index', 'pop', 'group'
  if(!setequal(colnames(reppop), c('index','pop','group'))){
    print("ERROR: There should be 3 columns with the headings 'index', 'pop', and 'group'.")
    Flag<-TRUE
  }
  #check that columns are "integer", "character", "character"
  if(!setequal(sapply(reppop,function(x) class(x)),c("integer","character","character"))){
    print("ERROR: The class of each column in RepPop should be 'integer','character','character'.")
    Flag<-TRUE
  }
  #check that the index starts at one and goes in numerical order
  if(!setequal(unique(reppop$index),1:length(unique(reppop$index)))){
    print("ERROR: The index in RepPop must start with 1 and integers must be in consecutive order.")
    Flag<-TRUE
  }
  if(Flag == FALSE){
    print("Your RepPop is formatted correctly and ready to go!")
  }
}
