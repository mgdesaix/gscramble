#' Map alleles from scrambled founders to the sampled segments from a GSP.
#'
#' @param Segs the simulated segments. A tibble like that returned from
#' [segregate()].
#' @param Im the individual meta data, like that in \code{\link{I_meta}}. A tibble with
#' columns `group` and `indiv`.
#' @param Mm the marker meta data formatted like that in \code{\link{M_meta}}. A tibble
#' with columns `chrom`, `pos`, and `variant_id`.
#' @param G the marker genotype data as a matrix like \code{\link{Geno}}.  This is
#' a character matrix.  Each row is an individual, and each pair of columns are the
#' alleles at a locus.  Thus it is N x 2L where N is the number of individuals
#' and L is the number of markers.
#' @export
#' @examples
#' #### First, get input segments for the function ####
#' # We construct an example here where we will request segregation
#' # down a GSP with two F1s and F1B backcrosses between two hypothetical
#' # populations, A and B.
#' set.seed(5)
#' gsp_f1f1b <- create_GSP("A", "B", F1 = TRUE, F1B = TRUE)
#'
#' # We will imagine that in our marker data there are three groups
#' # labelled "Pop1", "Pop2", and "Pop3", and we want to create the F1Bs with backcrossing
#' # only to Pop3.
#' reppop <- tibble::tibble(
#'     index = as.integer(c(1, 1, 2, 2)),
#'     pop = c("A", "B", "A", "B"),
#'     group = c("Pop3", "Pop1", "Pop3", "Pop2")
#' )
#'
#' # combine those into a request
#' request <- tibble::tibble(
#'    gpp = list(gsp_f1f1b),
#'    reppop = list(reppop)
#' )
#'
#' # now segegate segments.  Explicitly pass the markers
#' # in M_meta so that the order of the markers is set efficiently.
#' segs <- segregate(request, RecRates, M_meta)
#'
#' #### Now, use segs in an example with segments2markers() ####
#' # this uses several package data objects that are there for examples
#' # and illustration.
#' s2m_result <- segments2markers(segs, I_meta, M_meta, Geno)
segments2markers <- function(Segs, Im, Mm, G) {


  # first off, make sure that the request makes sense in terms of
  # population/group labels.  In other words, if there is a group origin
  # in Segs that does not correspond to a group in Im, then we throw an error.
  groups_in_Seqs <- dplyr::distinct(Segs, group_origin) %>% dplyr::pull(group_origin)
  groups_in_Im <- unique(Im$group)
  wrongos <- setdiff(groups_in_Seqs, groups_in_Im)
  if(length(wrongos) > 0) {
    stop(
      paste("Error. These group_origins exist in the Segs, but not in the Im:",
            paste(wrongos, collapse = ", "),
            sep = " "
      )
    )
  }

  # also, check to make sure that the rownames of G correspond to Im
  if(!identical(rownames(G), Im$indiv)) {
    stop("rownames of G not concordant with indiv column in Im.  They must be identical")
  }



  GS_input = rearrange_genos(G, Im, Mm)

  # compute the true Q values for the ped-sampled individuals
  trueQs <- computeQs_from_segments(Segs)

  # scramble by pops
  GS <- perm_gs_by_pops(GS_input)

  # do this if TEMPORARILY NOT-SCRAMBLING WHILE TESTING
  #GS <- GS_input
  #GS$G_permed <- list(GS$G[[1]])

  # join so that we have the absolute columns of the founders
  Segs2 <- GS$I[[1]] %>%
    select(group, gs_column, abs_column) %>%
    left_join(Segs, ., by = c("group_origin" = "group", "sim_level_founder_haplo" = "gs_column"))

  # break the markers up into a list of chromosomes
  m_list <- Mm %>%
    mutate(chrom_f = factor(chrom, levels = unique(chrom)), idx = 1:n()) %>%
    select(-variant_id) %>%
    split(., f = .$chrom_f)


  # sprinkle the variants in there:
  full_pick_mat <- Segs2 %>%
    group_by(gpp, index, ped_sample_id, samp_index, gamete_index) %>%
    summarise(
      m_subscript_matrix = list(
        make_subscript_matrix(
          n = n(),
          chrom = chrom,
          start = start,
          end =  end,
          abs_column = abs_column,
          m_list = m_list,
          num_markers = nrow(GS$G[[1]])
        )
      )
    ) %>%
    pull(m_subscript_matrix) %>%
    do.call(rbind, .)

  # and now we slurp the genos out of GS$G_permed into a big matrix
  ped_sampled_indivs <- GS$G_permed[[1]][full_pick_mat] %>%
    matrix(nrow = dim(GS$G_permed[[1]]))

  # now, cycle over the columns and make missing data within a locus
  # consistent at the two haplotypes.  Assumes missing data is NA.  This
  # is likely not the fastest, but it gets the job done
  for(i in seq(from = 1, to = ncol(ped_sampled_indivs), by = 2)) {
    ped_sampled_indivs[, i+1] <- ifelse(is.na(ped_sampled_indivs[, i]), NA_character_, ped_sampled_indivs[, i+1])
    ped_sampled_indivs[, i] <- ifelse(is.na(ped_sampled_indivs[, i+1]), NA_character_, ped_sampled_indivs[, i])
  }

  # now we just have some bookkeeping to do.
  # here is a function to plink-ize a matrix (i.e. interleave it and make
  # it have 2 * L columns and N rows)
  plinkize <- function(M) {
    tM <- t(M)
    ret <- lapply(seq(1, nrow(tM), by = 2), function(i) matrix(tM[c(i, i + 1), ], nrow = 1)) %>%
      do.call(rbind, .)
    ret
  }

  pPSI <- plinkize(ped_sampled_indivs)

  # first, get the IDs of the gametes in ped_sampled_indivs
  # the format here is h-gpp-index-ped_sample_id-samp_index-matrix_row
  psi_IDs <- Segs2 %>%
    ungroup() %>%
    select(gpp, index, ped_sample_id, samp_index) %>%
    distinct() %>%
    mutate(matrix_row = 1:n(),
           h = "h") %>%
    unite(col = "indiv", h, gpp:matrix_row, sep = "-") %>%
    mutate(group = "ped_hybs") %>%
    select(group, indiv)

  # now, make a column like that in the trueQs
  trueQs2 <- trueQs %>%
    ungroup() %>%
    distinct(gpp, index, ped_sample_id, samp_index) %>%
    mutate(matrix_row = 1:n()) %>%
    left_join(trueQs, ., by = c("gpp", "index", "ped_sample_id", "samp_index")) %>%
    mutate(h = "h") %>%
    unite(col = "indiv", h, gpp:samp_index, matrix_row, sep = "-", remove = FALSE) %>%
    select(-h) %>%
    select(indiv, everything()) %>%
    arrange(matrix_row) %>%
    select(-group_length, -tot_length, -matrix_row)

    # OK, those hyb indivs are now ready to ship back out

    # now we just need to pack up the permed individuals that were not consumed
    # in the process of making the hybrid indivs.
    consumed_columns <- sort(unique(Segs2$abs_column))

    remaining_haplos <- GS$I[[1]] %>%
      filter(!(abs_column %in% consumed_columns))

    # here is a quick error check. Every indiv should have two haplos
    stopifnot(all(table(remaining_haplos$indiv_idx) == 2))

    # pull those out of the permed matrix
    rem_genos <- GS$G_permed[[1]][, remaining_haplos$abs_column] %>%
      plinkize()

    # now, make IDs for them based on whom they used to be
    rem_IDs <- remaining_haplos %>%
      filter(haplo == 1) %>%
      select(group, indiv) %>%
      mutate(indiv = paste("permed_", indiv, sep = ""))

    # hell, now we just bung those matrices and IDs together and we should be good to return
    # what we need in a list
    list(
      ret_geno = rbind(pPSI, rem_genos),
      ret_ids = bind_rows(psi_IDs, rem_IDs),
      hyb_Qs = trueQs2
    )

}
