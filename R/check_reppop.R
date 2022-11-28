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
#' @param request_idx The index of the request
#' @return This function will return an error message if reppop is not formatted properly. If no issues exist,
#' a message will be returned stating that the reppop is formatted properly.
#' @keywords internal


check_reppop <- function(reppop, request_idx) {

  Flag<-FALSE
  msg <- ""

  #check to see that reppop is a tibble
  if (!is_tibble(reppop)){
    msg <- paste(msg, "\tRepPop needs to be a tibble.", sep = "\n")
    Flag <- TRUE
  }
  #check that three columns exist 'index', 'pop', 'group'
  if (!setequal(colnames(reppop), c('index','pop','group'))){
    msg <- paste(msg, "\tThere should be 3 columns with the headings 'index', 'pop', and 'group'.", sep = "\n")
    Flag <- TRUE
  }
  if (!is.integer(reppop$index)) {
    msg <- paste(msg, "\treppop$index must be an integer vector", sep = "\n")
    Flag <- TRUE
  }
  #check that the index starts at one and goes in numerical order
  if (!identical(unique(reppop$index),1:length(unique(reppop$index)))) {
    msg <- paste(msg, "\tThe index in RepPop must start with 1 and integers must be in consecutive order.", sep = "\n")
    Flag <- TRUE
  }
  if (Flag == TRUE) {
    stop(paste("RepPop not formatted correctly in row  ", request_idx, " of the request tibble:",  msg))
  }
}
