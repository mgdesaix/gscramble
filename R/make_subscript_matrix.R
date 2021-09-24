
#' makes a two-column matrix for subscripting alleles out of a genotype matrix
#'
#' I need to fully document this still.
#' @export
make_subscript_matrix <- function(n, chrom, start, end, abs_column, m_list, num_markers) {
  ret <- lapply(1:n, function(i) {
    the_chrom <- chrom[i]
    idxs <- m_list[[the_chrom]]$idx[ m_list[[the_chrom]]$pos > start[i] & m_list[[the_chrom]]$pos <= end[i] ]
    cbind(idxs, abs_column[i])
  })

  # here is a quick hack to deal with empty segments (i.e. those that have no markers in them)
  ret <- ret[sapply(ret, ncol) == 2]

  ret <- do.call(rbind, args = ret)

  # now do a quick check to make sure that every position is in there in the
  # correct order
  stopifnot(all(ret[,1] == 1:num_markers))

  ret
}
