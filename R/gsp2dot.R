

#' Write a dot file to represent a genome simulation pedigree
#'
#' This takes the tibble representation of a GSP and writes it to
#' a dot file to be rendered into a graph using the dot command
#' from the GraphViz package.  You can easily get GraphViz using Miniconda
#' or check out the GraphViz downloads page.  If you have the dot
#' executable in your PATH, then dot will be run on the dot file
#' and an SVG and a PNG image of the graph.
#' @param g a GSP tibble.
#' @param path the path to the file prefix to use (to this will be appended
#' .dot, and .png or .svg, if dot is on your system). By default these
#' paths are in a temporary directory, because packages are not allowed to
#' write to user home directories by default.  Supply a path with prefix,
#' like `my_path/myfile` to get the output file `mypath.myfile.dot`
#' @param edge_label_font_size The font size of the number annotations along the edges.
#' @param haplo_origin_colors The colors for different origins of haplotypes.
#' By default there are only four.  If you have more populations that founders
#' may come from, you should add some more.
#' @param sam_node_color The color given to the sample nodes in the GSP.
#' @param sample_edge_label_color Color for the numeric annotations along the
#' edges leading to samples.
#' @param parent_edge_label_color Color for the numeric annotations along the
#' edges leading from parents to offspring.
#' @details It can be tricky knowing whether or not R or Rstudio will read
#' your Unix rc files and populate your paths appropriately.  If you want to
#' test whether `dot` in on your PATH as it is when running under R, try:
#' `Sys.which("dot")` at your R console.  If that returns an empty string,
#' (`""`), then you need to do something else to make sure R can find `dot`
#' on your system.
#' @return A vector of file paths.  The first is the path of the dot
#' file that was produced.  The second and third are only present if
#' dot was found in the PATH.  They are the paths of the png and svg
#' files that were produced.
#' @export
#' @examples
#' gsp_file <- system.file("extdata/13-member-ped.csv", package = "gscramble")
#' g <- readr::read_csv(gsp_file)
#'
#' paths <- gsp2dot(g)
#' paths
#'
#' \dontrun{
#' paths <- gsp2dot(
#'     g = g,
#'     path = "gsp_figs/13-member"
#' )
#' paths
#' }
gsp2dot <- function(
  g,
  path = file.path(tempfile(), "file_prefix"),
  edge_label_font_size = 18,
  haplo_origin_colors = c("lightblue", "orange", "blue", "green"),
  sam_node_color = "violet",
  sample_edge_label_color = "purple",
  parent_edge_label_color = "red"
  ) {

  # get the path to the output files and make the directory if necessary
  DIR <- dirname(path)
  BASE <- basename(path)
  file <- path
  dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

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
  ret <- paste0(file, ".dot")
  names(ret) <- "dot"
  cat(
    "digraph xxx {\nlabel = \"  \"\nranksep = 1\nnodesep = 1\ncompress = false",
    node_tib2$node_text,
    hap_edges$edges,
    par_edges$edges,
    sam_edges$edges,
    file = ret,
    sep = "\n",
    "}"
    )

  ### Now, if dot is on the system make the png and the svg
  if(Sys.which("dot") != "") {
    PNG <- paste0(BASE, ".png")
    SVG <- paste0(BASE, ".svg")
    dot <- basename(ret)
    CALL <- paste(
      "cd ", DIR,
      "; dot -Tpng ", dot, " > ", PNG,
      "; dot -Tsvg ", dot, " > ", SVG
    )
    system(CALL)

    ret[2] <- paste0(file, ".png")
    ret[3] <- paste0(file, ".svg")
    names(ret) <- c("dot", "png", "svg")
  }

  return(ret)

}
