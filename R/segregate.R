#' Segregate segments down genomic simlation pedigrees
#'
#' Given a collection of genomic simulation pedigrees and requests
#' for how many simulations should be done (in the `request` input),
#' as well as recombination rates this simulates the segregation
#' of segments down through the pedigrees
#' @param request a tibble with list columns "gpp" and "reppop".
#' Each element of the gpp column is a tibble giving a genomic simulation
#' pedigree as documented as the input for `prep_gsp_for_hap_dropping()`.
#' Each element of the "reppop" column is a tibble  a tibble with columns
#' `rep`, `pop`, `group`, to indicate which of the founding
#' populations ("A", "B", etc.) correspond to the different groups
#' (from the `group` column in, for example, the meta data for individuals
#' in your genotype data set, like the data object `I_meta`.
#' Because it is quite likely that you might wish to iterate
#' the segregation procedure multiple
#' times in a single simulation, you can specify that by doing multiple
#' "reps" (replicates) of the procedure.
#' @param RR the recombination rates in the format of the package data
#' @param MM the marker meta data tibble (like M_meta).  If this is NULL it
#' is fine.  If not, then it uses the order of the markers in MM to define
#' the levels of a chrom_f column so that we can sort the rows of the output
#' correctly, with respect to markers in the Genotype data frame.  This will
#' let us more efficiently subscript the markers out of the matrix.
#' \code{\link{RecRates}}
#' @export
#' @examples
#' # We construct an example here where we will request segregation
#' # down a GSP with two F1s and F1B backcrosses between two hypothetical
#' # populations, A and B.
#' gsp_f1f1b <- create_GSP("A", "B", F1 = TRUE, F1B = TRUE)
#'
#' # We will imagine that in our marker data there are three groups
#' # labelled "grp1", "grp2", and "grp3", and we want to create the F1Bs with backcrossing
#' # only to grp3.
#' reppop <- tibble::tibble(
#'     rep = c(1, 1, 2, 2),
#'     pop = c("A", "B", "A", "B"),
#'     group = c("grp3", "grp1", "grp3", "grp2")
#' )
#'
#' # combine those into a request
#' request <- tibble::tibble(
#'    gpp = list(gsp_f1f1b),
#'    reppop = list(reppop)
#' )
segregate <- function(request, RR, MM = NULL) {

  # ERROR CHECKING (gotta do)
  # Write a function to do a variety of things...
    # is tibble
    # check that reps requested are dense (no missing numbers in there...)
    # etc.

  # just lapply over the rows of request and bind_rows() the outputs together
  idx <- 1:nrow(request)
  names(idx) <- idx

  ret <- lapply(idx, function(i) {
    # number of reps to do is the maximum number listed in the request

    Reps <- max(request$reppop[[i]]$rep)
    tmp <- drop_segs_down_gsp(GSP = request$gpp[[i]],
                       RR = RR,
                       Reps = Reps)

    # now, we join the group specification on there
    left_join(tmp, request$reppop[[i]], by = c("rep" = "rep", "pop_origin" = "pop")) %>%
      rename(group_origin = group)
  }) %>%
    bind_rows(.id = "gpp") %>%
    mutate(gpp = as.integer(gpp))

  # ERROR CHECKING on the result
    # any missing groups? etc.

  ret2 <- ret %>%
    ungroup() %>%
    sim_level_founder_haplos()  # on this line we also compute the simulation-level founder haplos!

  if(!is.null(MM)) {
    ret2 <- ret2 %>%
      mutate(chrom_f = factor(chrom, levels = unique(MM$chrom))) %>%
      select(chrom_f, everything()) %>%
      arrange(gpp, rep, ped_sample_id, samp_index, gamete_index, chrom_f, start)
  }

  ret2
}


