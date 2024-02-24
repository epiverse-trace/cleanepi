#' Standardize column names of a data frame or linelist
#'
#' @param data A data frame or linelist.
#' @param keep A vector of column names to maintain. The Default
#' is `NULL`.
#'
#' @return A data frame with easy to work with column names.
#'
#' @export
#' @examples
#' cleaned_data <- standardize_column_names(
#'   data = readRDS(system.file("extdata", "test_df.RDS",
#'     package = "cleanepi"
#'   )),
#'   keep = c("Sex", "Country")
#' )
#'
standardize_column_names <- function(data, keep = NULL) {
  # checkmate::assert data
  # checkmate::assert keep
  # if they're anything apart from ASCII e.g. arabic, throw error
  before <- colnames(data)
  # TODO replace snakecase with fixed list of diacritics swapable to English
  # TODO e.g. é,ê,è = e
  after <- make.unique(
    snakecase::to_snake_case(before, transliterations = "Latin-ASCII"),
    sep = "_"
  )
  kept <- which(before %in% keep)
  after[kept] <- before[kept]
  colnames(data) <- after

  colnames_info <- data.frame(before, after)
  data <- add_to_report(data, "colnames", colnames_info)
  print(colnames_info)
  data
}
