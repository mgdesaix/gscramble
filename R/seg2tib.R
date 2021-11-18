#' Takes a gamete in segment format and returns a tibble with Pop and indiv_index
#'
#' A small helper function.
#' @param s a gamete in segment format
#' @export
seg2tib <- function(s) {
  L <- length(s)
  tibble(tmp_seg_names = names(s)[-L],
         start = s[-L],
         end = s[-1])
}
