
#' internal function to do crossovers and create recombinations
#'
#' This function doesn't choose the recombination points.  That is done with
#' the function `recomb_point()`,
#' and the results are passed into this function. The two inputs `V1` and
#' `V2` represent the two gametes coming into an individual on the pedigree.
#' Recombination occurs within that individual, and the two resulting gametes
#' from that recombination are the output.  Typically this is not the way
#' things happen, of course.  Generally, only one of the two resulting gametes
#' from the recombination will be segregated to a surviving offspring. But,
#' since we are interested in segregating genetic material without duplicating
#' or destroying any of it, we keep track of both gametes that result
#' from the meiosis/recombination.
#' @param V1 integer vector of recombination points already existing on the
#' first incoming gamete.  Its names are the names of the
#' founder haplotype that the left end originates from (i.e. it is from the named
#' haplotype up until it changes at each point).  For example
#' c(A = 0, B = 12890, B = 30000) would work for a 30 Kb chromosome in which there
#' is a single recombination just to the right of the point 12890.
#' (In that example, positions 1 through 12890 are from founder haplotype A, while positions
#' 12891 to 30000 are from founder haplotype B.) Note that these
#' vectors have to have a first
#' value of 0 and a final value of the chromosome length.
#' @param V2 integer vector of breakpoints of the second incoming gamete.  Format
#' is just like it is for V1.
#' @param R a vector of new breakpoints to insert into the existing ones on each gamete.
#' This is usually returned from the function `recomb_point()`.
#' @return This sends back two updated gametes, V1 and V2, but with the new points
#' of recombination stuck in there.  Note, for two incoming gametes there are two
#' outgoing gametes, but we aren't "re-using" any genomic sequence.
#' @export
#' @examples
#' #' # make the two gametes/chromosomes coming into the function.
#' #' # Each one has length 30000 and a single existing recombination
#' V1 <- c(A = 0, B = 10000, B = 30000)
#' V2 <- c(C = 0, D = 20000, D = 30000)
#'
#' # now, set a new recombination point at position 15000
#' xover(V1, V2, R = 15000)
#'
#' # set three recombination points at 5,000, 15,000, and 25,000:
#' xover(V1, V2, R = c(5000, 15000, 25000))
#'
#' # no recombinations (R is a zero length numeric vector)
#' xover(V1, V2, R = numeric(0))
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

