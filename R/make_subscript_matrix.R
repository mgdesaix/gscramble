
#' makes a two-column matrix for subscripting alleles out of a genotype matrix
#'
#' This is intended to be applied to a grouped tibble that
#' has all the columns that are arguments to the function.
#' @param n the number of rows in the group of the tibble
#' @param chrom chromosome names
#' @param start starting position of the segment
#' @param end ending position of th segment
#' @param abs_column the absolute column index of the founder
#' @param m_list a list column of markers
#' @param num_markers  the number of markers
#' @keywords internal
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
