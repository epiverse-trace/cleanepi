#' clean column names of a data frame
#'
#' @param x the input data frame
#'
#' @return the input data frame with a knit column names
#'
clean_col_names <- function(x) {
  col_names <- colnames(x)
  # standardize the column names
  col_names <- snakecase::to_snake_case(col_names)
  cleaned_names <- epitrix::clean_labels(col_names)

  # make column name unique
  unique_names <- make.unique(cleaned_names, sep = "_")

  # detect modified column names from the previous command
  xx <- data.frame(cbind(current_name = cleaned_names, new_name = unique_names))
  idx <- which(xx$current_name != xx$new_name)
  if (length(idx) > 0) {
    warning("The following variable names have changed due to duplication after
            cleaning:\n")
    print(xx[idx, ])
  }


  # save the original column names in comment
  colnames(x) <- unique_names
  names(col_names) <- unique_names
  comment(x) <- c(comment(x), col_names)
  x
}
