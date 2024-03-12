#' Common strings representing missing values
#'
#'This vector contains common values of NA (missing) and is intended for
#' use within \{cleanepi\} functions [replace_missing_values()].
#' The current list of strings used can be found by printing out
#' `common_na_strings`. It serves as a helpful tool to explore your data
#' for possible missing values. However, I strongly caution against using
#'  this to replace `NA` values without meticulously examining the
#'  incidence for each case. Please note that `common_na_strings` utilizes
#'  `\\` around the "?", ".", and "*" characters to prevent their wildcard
#'
#' @source This vector is a combination of `naniar::common_na_strings`
#'   (\url{https://github.com/njtierney/naniar/}) and other strings found in the
#'   literature.
#' @format A vector of 35 character strings.
#' @name common_na_strings
#' @docType data
#'
"common_na_strings"
