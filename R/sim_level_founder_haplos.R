#' computes the simulation-level founder haplotype index for each founder haplo
#'
#' This takes the output of `segregate()` and deals with the multiple gpp's and reps
#' to come up with a unique index for each found haplotype, so that those haplotypes
#' can all, eventually, be accessed easily out of the genotype matrix.
#' Along the way, this function does some light checking to make sure that the
#' `rs_founder_haplo` values are dense within `gpp` and `index` as they should be.
#' @return This function returns a result that is basically the output of `segregate()` with
#' an additional column added to it:  `sim_level_founder_haplo`.  This is the index
#' of the haplotype within each `group_origin` that should be used. For details of the
#' other columns in the output tibble, see the documentation for \code{\link{segregate}}.
#'

#' @param S tibble of segments like that produced by \code{\link{segregate}}.
#' @export
#' @examples
#' #### Get output from segregate to use as input ####
#' # We construct an example here where we will request segregation
#' # down a GSP with two F1s and F1B backcrosses between two hypothetical
#' # populations, A and B.
#' gsp_f1f1b <- create_GSP("A", "B", F1 = TRUE, F1B = TRUE)
#'
#' # We will imagine that in our marker data there are three groups
#' # labelled "grp1", "grp2", and "grp3", and we want to create the F1Bs with backcrossing
#' # only to grp3.
#' reppop <- tibble::tibble(
#'   index = as.integer(c(1, 1, 2, 2)),
#'   pop = c("A", "B", "A", "B"),
#'   group = c("grp3", "grp1", "grp3", "grp2")
#' )
#'
#' # combine those into a request
#' request <- tibble::tibble(
#'   gpp = list(gsp_f1f1b),
#'   reppop = list(reppop)
#' )
#'
#' # now run it through segregate()
#' set.seed(5)  # just for reproducibility in example...
#' simSegs <- segregate(request, RecRates)
#'
#' #### Now we can run those through sim_level_founder_haplos() ####
#' fh <- sim_level_founder_haplos(simSegs)
#' fh
sim_level_founder_haplos <- function(S) {
  tmp <- S %>%
    group_by(group_origin, gpp, index, rs_founder_haplo) %>%
    tally() %>%
    select(-n)

  # now we check to make sure the rs_founder_haplo indices are dense within each grouping
  problemos <- tmp %>%
    filter(rs_founder_haplo != 1:n())
  if(nrow(problemos) > 0) {
    warning("Bad news!  rs_founder_haplos are not dense within group_origin, gpp, and index!")
    print(problemos)
    stop("Bailing out!")
  }

  new_vals <- tmp %>%
    group_by(group_origin) %>%
    mutate(sim_level_founder_haplo = 1:n()) %>%
    ungroup()

  # and now we return those by joining them onto the original S
  S %>%
    left_join(new_vals, by = c("gpp", "index", "group_origin", "rs_founder_haplo")) %>%
    ungroup()
}
