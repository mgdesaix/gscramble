

#### Import the pipe operator from magrittr ####
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @noRd
NULL





#' @importFrom dplyr arrange bind_rows case_when distinct everything filter group_by left_join mutate mutate_all n near pull rename select summarise tally ungroup
#' @importFrom ggplot2 aes facet_wrap geom_line geom_rect ggplot scale_y_continuous theme_bw xlab
#' @importFrom glue glue
#' @importFrom purrr flatten keep map map_dbl map_dfr pmap
#' @importFrom rlang .data
#' @importFrom stats rpois runif setNames
#' @importFrom stringr str_c
#' @importFrom tibble as_tibble enframe is_tibble tibble
#' @importFrom tidyr separate nest unite unnest
#' @importFrom utils write.table
NULL



# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      ".",
      "BY",
      "GSP_opts",
      "abs_column",
      "chr_xmax",
      "chr_xmin",
      "chr_ymax",
      "chr_ymin",
      "chrom",
      "chrom_f",
      "chrom_len",
      "end",
      "end_pos",
      "gam_tibs",
      "gamete_index",
      "gamete_segments",
      "gpp",
      "group",
      "group_length",
      "group_origin",
      "gs_column",
      "gsp_init",
      "h",
      "hap1",
      "hap2",
      "haplo",
      "id",
      "idx",
      "index",
      "indiv",
      "link_pos",
      "m_subscript_matrix",
      "ma",
      "map_stuff",
      "matrix_row",
      "max_rec",
      "mid_pos",
      "pa",
      "par1",
      "par2",
      "ped_sample_id",
      "ped_samples",
      "pheno",
      "pop",
      "pop_mat",
      "pop_origin",
      "pos",
      "rec_prob",
      "rows",
      "rs_founder_haplo",
      "samp_index",
      "segged",
      "sex_code",
      "start",
      "start_pos",
      "tmp_seg_names",
      "tot_length",
      "unit",
      "variant_id",
      "yval"
    )
  )
}
