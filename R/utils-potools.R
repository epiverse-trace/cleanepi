#' Flag out what message will be translated using {potools}
#'
#' @param ... A character string. This represents the message to be translated
#'
#' @return The input object
#' @keywords internal
#' @details
#' This function was taken from the `Translation for package developers`
#' vignette of the \code{{potools}} package.
#'
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-cleanepi"))
}
