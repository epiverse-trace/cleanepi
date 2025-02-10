#' Clean spelling mistakes detected with approximate string matching and
#' replaced with `wordlist`
#'
#' @inheritParams clean_data
#' @param wordlist A `character` vector or list of words to match to cells in
#' `data`.
#' @inheritParams base::agrep
#'
#' @return The cleaned input date according to the user-specified `wordlist`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   case_type = c("confirmed", "confermed", "probable", "susspected"),
#'   outcome = c("died", "recoverd", "did", "recovered")
#' )
#' df
#' clean_spelling_mistakes(
#'   data = df,
#'   wordlist = c("confirmed", "probable", "suspected", "died", "recovered")
#' )
clean_spelling_mistakes <- function(data,
                                    wordlist,
                                    max.distance = 1,
                                    ignore.case = FALSE) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  # convert wordlist to vector is supplied as list
  wordlist <- unlist(wordlist)

  for (col in seq_len(ncol(data))) {
    for (word in seq_along(wordlist)) {
      idx <- agrepl(
        pattern = wordlist[word],
        x = data[, col],
        max.distance = max.distance,
        ignore.case = ignore.case
      )
      data[, col][idx] <- wordlist[word]
    }
  }

  return(data)
}
