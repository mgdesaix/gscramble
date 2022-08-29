
#' Plot the simulated chromosomes of an individual
#'
#' This function uses the information in the tibble about segments
#' dropped down a genome simulation pedigree to plot the
#' chromomomes of an individual colored by
#' the population of origin of each segment.
#' @param Segs a tibble of segments
#' @param RR a tibble of recombination rates in bins in the format
#' of [RecRates]. If this is
#' included, the recombination rates in cM/Mb are plotted atop the chromosomes
#' as a little sparkline. If it is not included, then the there are
#' no little sparklines above the chromosomes.
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
#' @param spark_thick thickness of the line that draws the recombination rate
#' sparkline.
#' @param spark_splat fraction by which the unit gap should be reduced when
#' there are sparklines being drawn.
#' @export
#' @return This function returns a ggplot object.  Each facet of the plot shows
#' the chromosomes of a different sampled individual from a particular replicate
#' simulation from a particular genome simulation pedigree.  The facets are titled
#' like: `GSP 1, Rep 2, ID 8[3]`, which means that the chromosomes shown in the panel
#' are from the third sampled set of chromosomes from the individual with ID 8 from the
#' second replicate simulation from genome simulation pedigree 1.
#' @examples
#' s <- example_segments
#' rr <- RecRates
#' g <- plot_simulated_chromomsome_segments(s)
#' g_with_sparklines <- plot_simulated_chromomsome_segments(s, rr)
plot_simulated_chromomsome_segments <- function(
  Segs,
  RR = NULL,
  rel_heights = c(
    chrom_ht = 4,
    chrom_gap = 0.8,
    spark_gap = 0.2 * !is.null(RR),
    spark_box = 2.6 * !is.null(RR),
    unit_gap = 4
  ),
  bottom_gap = 0.3,
  spark_thick = 0.2,
  spark_splat = 0.25
) {

  rh <- rel_heights

  # make the chrom gap smaller if there are sparklines

  if(!is.null(RR)) rh["unit_gap"] <- rh["unit_gap"] * spark_splat

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
      unit = paste0("GSP ", gpp, ", Rep ", rep, ", ID: ", ped_sample_id, "[", samp_index, "]"),
      chr_ymin = BY + ((gamete_index == 2) * (ah["chrom_ht"] + ah["chrom_gap"])),
      chr_ymax = BY + ah["chrom_ht"] + ((gamete_index == 2) * (ah["chrom_ht"] + ah["chrom_gap"])),
      chr_xmin = start,
      chr_xmax = end
    )

  g <- ggplot() +
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
    facet_wrap(~ unit) +
    theme_bw()

  # add the sparklines to them, if indicated
  if(!is.null(RR)) {
    # make a data set that is facetable that has the information we
    # need for making the sparklines
    Sp1 <- S2 %>%
      distinct(chrom_f, chrom, unit, BY) %>%
      left_join(RR, by = "chrom") %>%
      ungroup() %>%
      mutate(
        mid_pos = (start_pos + end_pos) / 2,
        max_rec = max(rec_prob),
        yval = BY + ah["chrom_ht"] +  ah["chrom_gap"] + ah["chrom_ht"] + ah["spark_gap"] + (rec_prob / max_rec) * ah["spark_box"]
      )

    # then add that to the ggplot object
    g <- g +
      geom_line(
        data = Sp1,
        mapping = aes(x = mid_pos / 1e6, y = yval, group = chrom_f),
        size = spark_thick
      )

  }


  g

}
