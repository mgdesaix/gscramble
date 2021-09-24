#' computes the simulation-level founder haplotype index for each founder haplo
#'
#' This takes the output of segregate and deals with the multiple gpp's and reps
#' to come up with a unique index for each found haplotype, so that those haplotypes
#' can all, eventually, be accessed easily out of the genotype matrix.
#'
#' Note that the result one gets here is basically a the output of segregate with
#' an additional column added to it:  sim_level_founder_haplo.  This is the index
#' of the haplotype within each group_origin that should be used.
#'
#' Along the way, this function does some light checking to make sure that the
#' rs_founder_haplo values are dense with gpp and rep as they should be.
#' @param S tibble of segments like that produced by \code{\link{segregate}}.
#' @export
sim_level_founder_haplos <- function(S) {
  tmp <- S %>%
    group_by(group_origin, gpp, rep, rs_founder_haplo) %>%
    tally() %>%
    select(-n)

  # now we check to make sure the rs_founder_haplo indices are dense within each grouping
  problemos <- tmp %>%
    filter(rs_founder_haplo != 1:n())
  if(nrow(problemos) > 0) {
    warning("Bad news!  rs_founder_haplos are not dense within group_origin, gpp, and rep!")
    print(problemos)
    stop("Bailing out!")
  }

  new_vals <- tmp %>%
    group_by(group_origin) %>%
    mutate(sim_level_founder_haplo = 1:n()) %>%
    ungroup()

  # and now we return those by joining them onto the original S
  S %>%
    left_join(new_vals, by = c("gpp", "rep", "group_origin", "rs_founder_haplo")) %>%
    ungroup()
}
