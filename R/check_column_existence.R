#' Check if date column exists in given data frame
#'
#' @param data the input data frame
#' @param date_column_name the date column of interest
check_column_existence <- function(data, date_column_name) {
  # check the column name
  if (is.null(date_column_name)) {
    idx <- which(names(data) %in% c("Date", "DATE", "date"))
    if (length(idx) == 0) {
      stop("Could not find column named as ",
           glue::glue_collapse(c("Date", "DATE", "date"), sep = " or "),
           "\nPlease specify the date column name.")
    }
    date_column_name <- names(data)[idx]
  }

  # check whether the provided column name belong to the data
  if (!all(date_column_name %in% names(data))) {
    idx <- which(!(date_column_name %in% names(data)))
    stop("Can't find column: ", date_column_name[idx])
  }
  date_column_name
}
