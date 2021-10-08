
#' Check that GSP does not reuse genetic material and yet uses all of it
#'
#' These conditions can be checked for a GSP with no inbreeding loops
#' simply by ensuring that the amount of genetic material coming into
#' each individual is the same as the amount going out (either as
#' segregated gametes or as samples).  If the amount of material coming
#' out of any individual in the GSP is greater than the amount coming
#' in, then an error is thrown.  If the amount coming out is less than
#' the amount coming in, then a warning about the GSP is thrown.
#' Messages printed via `message()` and `warning()` indicate which
#' individuals in the GSP are problematic.  All problematic individuals
#' are listed before an error is thrown with `stop()`.
#' @param GP A gsp in list format as produced by the function
#' `prep_gsp_for_hap_dropping()`.  See the documentation for the return
#' object of `prep_gsp_for_hap_dropping()` for a description.
#' @return This function does not return anything.
#' @export
#' @examples
#' # get the 13 member pedigree in tibble form as the package
#' # and turn it into a list
#' GP <- prep_gsp_for_hap_dropping(GSP)
#'
#' # check it. (This passes)
#' check_gsp_for_validity_and_saturation(GP)
#'
#' \dontrun{
#' # Read in a gsp with errors and then make sure all the
#' # error in it are caught
#' bad <- read_csv("inst/extdata/13-member-ped-with-errors.csv")
#' badL <- prep_gsp_for_hap_dropping(bad)
#' check_gsp_for_validity_and_saturation(badL)
#' }
#'
check_gsp_for_validity_and_saturation <- function(GP) {
  WARN <- FALSE
  ERROR <- FALSE

  for(i in names(GP)) {
    G <- GP[[i]]

    # first, special treatment for founders
    if(G$isFounder == TRUE) {
      if(G$isSample == TRUE) {
        message("Error! Founder with ID ", i, " cannot also be a sample")
        ERROR <- TRUE
      }
      if(is.na(G$hpop1) || is.na(G$hpop2)) {
        message("Error! Founder with ID ", i, " missing founder haplotype population information")
        ERROR <- TRUE
      }
      if(is.na(G$hpop1) || is.na(G$hpop2)) {
        message("Error! Founder with ID ", i, " missing valid founder haplotype index")
        ERROR <- TRUE
      }
      if(G$nGamete < 2) {
        message("Warning! Founder with ID ", i, " is segregating ", G$nGamete, " gametes, which is < 2.  This leaves genetic material unused")
        WARN <- TRUE
      }
      if(G$nGamete > 2) {
        message("Error! Founder with ID ", i, " is segregating ", G$nGamete, " gametes, which is > 2.  No founder can segregate more than two gametes")
        ERROR <- TRUE
      }
    } else {  # handle all the non-founders
      # first, check to make sure that the number of outgoing gametes
      # is the same as the number of incoming gametes.

      # The incoming number of gametes from each parent must be the same, because
      # otherwise there wouldn't be an appropriate chance of recombination for
      # each segregated gamete.  So we check that too.
      if(length(G$par1$gam_idx) != length(G$par2$gam_idx)) {
        message("Error! Non-founder with ID ", i, " has different numbers of gametes coming in from its two parents: from parent ID ",
                G$par1$par, ", number of gametes is ",
                length(G$par1$gam_idx),
                ". From parent ID ",
                G$par2$par, ", number of gametes is ",
                length(G$par2$gam_idx))
        ERROR <- TRUE
      }

      # If the total number of outgoing gametes (that is for samples and
      # for offspring) is > the number of incoming gametes.  That is an
      # error, and if it is less than, that is a warning.
      IncGam = length(G$par1$gam_idx) + length(G$par2$gam_idx)
      OffGam = G$nGamete  # number of gametes that will be segregated to offspring
      if(G$isSample == TRUE) {
        SamGam <- G$nSamples * 2  # number of gametes that will be used in making the samples.
        nSamples <- G$nSamples
      } else {
        SamGam <- 0
        nSamples <- 0
      }
      if(IncGam < OffGam + SamGam) {
        message("Error! Non-founder with ID ", i, " has ", IncGam, " incoming gametes, but is requested to use ",
                OffGam + SamGam, " gametes. ", OffGam, " for its descendants and ", SamGam, " for its ", nSamples, " samples.")
        ERROR <- TRUE
      }
      if(IncGam > OffGam + SamGam) {
        message("Warning! Non-founder with ID ", i, " has ", IncGam, " incoming gametes, and is requested to use only ",
                OffGam + SamGam, " gametes. ", OffGam, " for its descendants and ", SamGam, " for its ", nSamples, " samples.")
        WARN <- TRUE
      }
    }
  }

  if(WARN) warning("There were warnings. See them in the above messages.", immediate. = TRUE)
  if(ERROR) stop("There were errors. See them in the above messages. Exiting...")
}
