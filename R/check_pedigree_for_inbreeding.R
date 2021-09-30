
#' Check the a GSP (in prepped list format) for inbreeding loops
#'
#' After a GSP has been passed through `prep_gsp_for_hap_dropping()` it
#' is in a list format with the individuals ordered in such a way that
#' it should be easy to check for any inbreeding loops in it (which
#' are not allowed!).  This version uses a simple recursive
#' approach to compute the ancestry vector for each individual, and it
#' detects inbreeding the the occurrence of the same ID in the ancestry
#' vector more than once.  This might be slow on large pedigrees, but for
#' most that people would use, this should be fine.
#'
#' Note that the ancestry vector produced by this is not ordered the
#' way the ancestry vectors are in my package CKMRpop---for simplicity
#' I just get a list of ancestors in whatever order they happen to be
#' reached.
#' @param GP A gsp in list format as produced by the function
#' `prep_gsp_for_hap_dropping()`.  See the documentation for the return
#' object of `prep_gsp_for_hap_dropping()` for a description.
#' @export
#' @examples
#' # get the 13 member complex pedigree in tibble form
#' file <- system.file("extdata/ped-13-complex.csv", package = "gscramble")
#' complex13 <- readr::read_csv(file)
#'
#' # turn it into a list
#' GP <- prep_gsp_for_hap_dropping(complex13)
#'
#' # check it for inbreeding. (There is none)
#' check_pedigree_for_inbreeding(GP)
#'
#' \dontrun{
#' # To see what happens if there are inbreeding loops, make some
#' GP_inbred <-  GP
#' # make 12 be inbred trough individual 6
#' GP_inbred$`12`$par1$par = "13"
#' # make 8 inbred (because both of its founder parents are the same!)
#' GP_inbred$`8`$par2$par = "4"
#'
#' # now try that:
#' check_pedigree_for_inbreeding(GP_inbred)
#'
#' }
check_pedigree_for_inbreeding <- function(GP) {

  # x is the ID on an individual in the pedigree
  # a is a vector that starts empty (or should be initialized to x)
  # and gets added to each time a node is visited on the way up
  search_ancestors <- function(x, a) {
    a <- c(x, a)
    # message("Calling with: x = ", x, " and a = ", paste(a, collapse = " "))
    if(GP[[x]]$isFounder == TRUE) {
      return(a);
    } else {
      a = search_ancestors(GP[[x]]$par1$par, a)
      a = search_ancestors(GP[[x]]$par2$par, a)
    }
    return(a)
  }

  # apply that recursive function to every node in the pedigree
  ancs <- lapply(names(GP), function(x) search_ancestors(x, character(0)))

  # then name the output list according to the node IDs
  names(ancs) <- names(GP)

  # see if any of those nodes have the same individual more than once
  # in their ancestry.
  dup_ancs <- lapply(ancs, function(x) x[duplicated(x)])

  dup_ancs <- dup_ancs[sapply(dup_ancs, function(x) length(x) > 0)]

  if(length(dup_ancs) > 0) {
    for(i in names(dup_ancs)) {
      message("ID ",
              i,
              " in the GSP has the same individual(s) more than once in its ancestry: ",
              paste(dup_ancs[[i]], collapse = ", ")
              )
    }
    message("Such duplicated IDs imply a pedigree with inbreeding.  Not allowed.")
    stop("Error: the GSP has inbreeding loops")
  }
}
