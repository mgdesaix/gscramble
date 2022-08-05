#' takes the waka_waka table and returns a simple tibble with results for each sampled "hybridized" gamete from each chromosome
#' #'
#' Not yet documented
#' @param W a tibble.  see how it is used in code below.
#' @keywords internal
tidy_up_sampled_haplos <- function(W) {

  # here we pick out only those elements that have samples:
  W %>%
    mutate(ped_samples =
             map(segged, function(x) {
               map(x, "Samples") %>%
                 keep(~!is.null(.x)) %>%
                 map_dfr(function(z) tibble(
                   samp_index = rep(1:length(z), each = 2),
                   gamete_index = rep(c(1,2), times = length(z)),
                   gamete_segments = flatten(z)
                 ),
                 .id = "ped_sample_id")
             })
    )
}
