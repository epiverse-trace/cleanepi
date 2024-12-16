#' Flag out what message will be translated using the \pkg{potools} package
#'
#' @param ... A character string. This represents the message to be translated
#'
#' @return The input object
#' @keywords internal
#' @details
#' This function was copied from the `Translation for package developers`
#' vignette of the \pkg{potools} package.
#'
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-cleanepi"))
}
