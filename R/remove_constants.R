#' Remove empty rows and columns and constant column
#'
#' @param data The input data frame or linelist
#' @param cutoff The cut-off for empty rows and columns removal. If provided,
#'    only rows and columns where the percent of missing data is greater than
#'    this cut-off will removed.
#'
#' @returns The input dataset without the empty rows and columns and the
#'    constant columns.
#' @export
#'
#' @examples
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#'
#' # introduce an empty column
#' data$empty_column <- NA
#'
#' # remove the constant columns, empty rows and columns
#' dat <- remove_constants(
#'   data   = data,
#'   cutoff = 1
#' )
#'
#' # check the report to see what has happened
#' report <- attr(dat, "report")
#' summary(report)
remove_constants <- function(data, cutoff = 1L) {
  checkmate::assert_number(cutoff, lower = 0.0, upper = 1.0, na.ok = FALSE,
                           finite = TRUE, null.ok = FALSE)
  report  <- attr(data, "report")
  # remove the empty rows and columns
  dat     <- data %>%
    janitor::remove_empty(which = c("rows", "cols"), cutoff = cutoff)

  # report empty columns if found
  removed <- setdiff(colnames(data), names(dat))
  if (length(removed) > 0L) {
    add_this <- toString(removed)
    report[["empty_columns"]] <- add_this
  }

  # report empty rows if found
  if (nrow(data) > nrow(dat)) {
    add_this <- summary(
      arsenal::comparedf(data, dat)
    )[["obs.table"]][["observation"]]
    report[["empty_rows"]] <- add_this
  }

  # remove constant columns
  data    <- dat
  dat     <- data %>% janitor::remove_constant()
  removed <- setdiff(colnames(data), names(dat))
  if (length(removed) > 0L) {
    add_this <- toString(removed)
    report[["constant_columns"]] <- add_this
  }

  attr(dat, "report") <- report
  return(dat)
}
