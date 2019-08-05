
#' internal function to do crossovers and create two recombinations
#'
#' This doesn't choose the recombination points.  That has to be done
#' and the results passed into this function.
#' @param V1 integer vector of breakpoints already existing on the
#' first incoming gamete.  Its names are the names of the
#' founder haplotype that the left end originates from (i.e. it is from the named
#' haplotype up until it changes at each point).  For example
#' c(A = 0, B = 12890, B = 30000) would work for a 30 Kb chromosome in which there
#' is a single recombination at 12890.  Note that these vectors have to have a first
#' value of 0 and a final value of the chromosome length.
#' @param V2 integer vector of breakpoints of the second incoming gamete.  Format
#' is just like it is for V1.
#' @param R a vector of new breakpoints to insert into the existing ones on each gamete.
#' @return This sends back two updated gametes, V1 and V2, but with the new points
#' of recombination stuck in there.  Note, for two incoming gametes there are two
#' outgoing gametes, but we aren't "re-using" any genomic sequence.
#' @export
xover <- function(V1, V2, R) {
  if(length(R) < 1) {  # no recombinations
    return(list(V1 = V1, V2 = V2))
  }

  for(r in R) {
    i1 <- as.integer(cut(r, V1))
    i2 <- as.integer(cut(r, V2))

    left1 <- V1[1:i1]
    left2 <- V2[1:i2]
    right1 <- V1[(i1 + 1):length(V1)]
    right2 <- V2[(i2 + 1):length(V2)]

    mid1 <- r
    names(mid1) <- names(V2)[i2]
    mid2 <- r
    names(mid2) <- names(V1)[i1]

    V1 <- c(left1, mid1, right2)
    V2 <- c(left2, mid2, right1)
  }

  list(V1, V2)  # return a list with two components
}

