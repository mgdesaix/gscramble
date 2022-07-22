
#' Plot the simulated chromosomes of an individual
#'
#' This function uses the information in the tibble about segments
#' dropped down a genome simulation pedigree to plot the
#' chromomomes of an individual colored by either
#' the population of origin of each segment or the founding
#' haplotype of origin of each segment.
#' @param Segs a tibble of segments
#' @param RR a tibble of recombination rates in bins in the format
#' of [RecRates]. If this is
#' included, the recombination rates in cM/Mb are plotted atop the chromosomes
#' as a little sparkline. If it is not included, then the there are
#' no little sparklines above the chromsomes.
#' @param rel_heights a vector the the relative heights of the different
#' elements of each chromosomal unit of the plot.  This is a named vector
#' with the following elements, listed in order of the bottom of each
#' unit to the top:
#' - `chrom_ht`: the height of the bars for each of the two chromosomes of the
#' pair in a chromosome unit.
#' - `chrom_gap`: The gap between the two homologous chromosomes of the individual.
#' - `spark_gap`: the gap between the top chromosome and the sparkline box
#' for recombination rates.
#' - `spark_box`: height of the box within which the sparkline goes.  Note that
#' the sparkline itself will be scaled so that the highest rate anywhere within
#' the genome will correspond to the top of the spark box.
#' - `unit_gap`: The relative height of the gap between one chromosome unit and
#' the next.
#' @param bottom_gap the y value of the bottom chromosome unit.  Basically the
#' absolute distance between the y=0 line and the start of the plotted material.
#' Should typically be between 0 and 1.
#' @export
#' @examples
#' # must add some
plot_simulated_chromomsome_segments <- function(
  Segs,
  RR = NULL,
  rel_heights = c(
    chrom_ht = 4,
    chrom_gap = 0.8,
    spark_gap = 0.6 * !is.null(RR),
    spark_box = 2.2 * !is.null(RR),
    unit_gap = 4
  ),
  bottom_gap = 0.3
) {

  rh <- rel_heights

  NORM <- (rh["chrom_ht"] * 2) +
    rh["chrom_gap"] +
    rh["spark_gap"] +
    rh["spark_box"] +
    rh["unit_gap"]

  # get the absolute heights
  ah = rh / NORM

  # get the number of chromosomes
  C <- length(levels(Segs$chrom_f))

  # put the "bottom_y" value in for each chromosome unit
  S <- Segs %>%
    mutate(BY = bottom_gap + C - as.integer(chrom_f))

  # get a tibble of the central points of each chromosome unit, where
  # we will want to put tick marks and plot their name. I think we
  # want to put those in the middle of the gap between the chromosomes.
  chrom_ticks <- S %>%
    distinct(chrom_f, BY) %>%
    mutate(tick_y = BY + ah["chrom_ht"] + 0.5 * ah["chrom_gap"])

  # now, fill out the y values for all the chromosomes in the data.
  # We use the gamete index to figure out which chromosome of the pair
  # we are dealing with.  gamete_index = 1 goes on the bottom, while
  # gamete_index = 2 goes on the top of the pair.
  S2 <- S %>%
    mutate(
      unit = paste0("Rep ", rep, ", ID: ", ped_sample_id, "[", samp_index, "]"),
      chr_ymin = BY + ((gamete_index == 2) * (ah["chrom_ht"] + ah["chrom_gap"])),
      chr_ymax = BY + ah["chrom_ht"] + ((gamete_index == 2) * (ah["chrom_ht"] + ah["chrom_gap"])),
      chr_xmin = start,
      chr_xmax = end
    )


  ggplot() +
    geom_rect(
      data = S2 %>% filter(rep <= 4),
      mapping = aes(
        xmin = chr_xmin/1e6,
        xmax = chr_xmax/1e6,
        ymin = chr_ymin,
        ymax = chr_ymax,
        fill = pop_origin
      )
    ) +
    xlab("bp in Megabases") +
    scale_y_continuous(
      name = "Chromosome",
      breaks = chrom_ticks$tick_y,
      labels = as.character(chrom_ticks$chrom_f),
      minor_breaks = NULL
    ) +
    facet_wrap(~ unit)

}
