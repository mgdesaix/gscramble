


#' must document this one fully
#'
#' Gonna do that later
#' @param request a tibble with list columns "gpp" and "reppop".
#' @param RR the recombination rates in the format of the package data
#' \code{\link{RecRates}}
#' @export
#'
segregate <- function(request, RR) {

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

  ret
}


