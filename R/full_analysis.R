#' A full analysis of the included feral pig data
#'
#' This is put in here as a function so that it makes it easier to
#' make a dependency graph of all the different functions in
#' the package. It doesn't actually do anything except
#' call the high-level functions that one would call in
#' a typical full analysis. Then we can make a function dependency
#' graph like this:
#' ```
#' library(DependenciesGraphs)
#' deps <- funDependencies("package:gscramble", "full_analysis")
#' plot(deps)
#' ```
#' @export
full_analysis <- function() {
  gsp2dot()
  segregate()
  rearrange_genos()
  big_wrapper()
}
