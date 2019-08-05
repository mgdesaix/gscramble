

#' Write a dot file to represent a genome scramble pedigree
#'
#' g <- read_csv("inst/extdata/13-member-ped.csv")
#' @export
gsp2dot <- function(
  g,
  file = stdout(),
  edge_label_font_size = 18,
  haplo_origin_colors = c("lightblue", "orange"),
  sam_node_color = "violet",
  sample_edge_label_color = "purple",
  parent_edge_label_color = "red"
  ) {

  #### GET THE NODE SPECIFICATIONS ALL SET UP IN A TIBBLE ####
  # define shapes and such for different nodes
  shape_stuff <- tibble(
    node_type = c("hap", "ind", "sam"),
    shape_text = c("shape=invtriangle, regular=1, height=0.56, fixedsize=true",
             "shape=box, regular=1, height=0.86, fixedsize=true",
             glue("shape=hexagon, regular=1, height=0.86, fixedsize=true, style=filled, fillcolor={sam_node_color}"))
  )

  # get a tibble of all the different types of nodes, and add the
  # labels that they should have
  node_tib <- list(
    hap = tibble(id = as.character(unique(c(g$hap1, g$hap2)))),
    ind = tibble(id = as.character(unique(c(g$ind, g$par1, g$par2)))),
    sam = tibble(id = as.character(g$sample))
  ) %>% bind_rows(.id = "node_type") %>%
    filter(!is.na(id)) %>%
    left_join(shape_stuff, by = "node_type")

  # now, pull stuff together to make the labels for the haplos
  tmp <- g %>%
    filter(!(is.na(hap1) & is.na(hap2)))
  haplos <- tibble(
    node_type = "hap",
    id = c(tmp$hap1, tmp$hap2),
    pop = c(tmp$hpop1, tmp$hpop2)
  )

  node_tib2 <- node_tib %>%
    left_join(haplos, by = c("node_type", "id")) %>%
    mutate(pop_f = factor(pop)) %>%
    mutate(label_text = case_when(
      node_type == "hap" ~ glue("label={pop}, style=filled, fillcolor={haplo_origin_colors[pop_f]}"),
      TRUE ~ str_c("label=", id)
    )) %>%
    mutate(node_text = glue("\"{id}\" [{shape_text}, {label_text}];"))

  #### NOW, GET THE EDGE SPECS ALL SET UP IN A TIBBLE ####
  hap_edges <- g %>%
    filter(!(is.na(hap1) & is.na(hap2))) %>%
    mutate(edges = glue("\"{hap1}\" -> \"{ind}\" [dir=none, style=solid];
                         \"{hap2}\" -> \"{ind}\" [dir=none, style=solid];"))

  par_edges <- g %>%
    filter(!is.na(par1) & !is.na(par2)) %>%
    mutate(edges = glue("\"{par1}\" -> \"{ind}\" [dir=none, style=solid, label = \" {ipar1}\", fontsize={edge_label_font_size}, fontcolor={parent_edge_label_color}];
                         \"{par2}\" -> \"{ind}\" [dir=none, style=solid, label = \" {ipar2}\", fontsize={edge_label_font_size}, fontcolor={parent_edge_label_color}];"))


  sam_edges <- g %>%
    filter(!is.na(sample)) %>%
      mutate(edges = glue("\"{ind}\" -> \"{sample}\" [dir=none, style=solid, label = \" {osample}\", fontsize={edge_label_font_size}, fontcolor={sample_edge_label_color}];"))

  #### NOW, JUST WRITE THAT OUT ####
  cat(
    "digraph xxx {\nlabel = \"  \"\nranksep = 1\nnodesep = 1\ncompress = false",
    node_tib2$node_text,
    hap_edges$edges,
    par_edges$edges,
    sam_edges$edges,
    file = file,
    sep = "\n",
    "}"
    )
}
