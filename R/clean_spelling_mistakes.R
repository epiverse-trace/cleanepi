#' Clean spelling mistakes detected with approximate string matching and
#' replaced with `wordlist`
#'
#' @details
#' When used interactively (see [interactive()]) the user is presented a menu
#' to ensure that the words detected using approximate string matching are not
#' false positives and the user can decided whether to proceed with the
#' spelling corrections. In non-interactive sessions all spelling mistakes
#' detected with approximate string matching are replaced by the `wordlist`
#' word.
#'
#' @inheritParams clean_data
#' @param wordlist A `character` vector of words to match to cells in `data`.
#' @inheritParams base::agrep
#' @param confirm A `logical` boolean to determine whether to show the user a
#' menu of spelling corrections. If `TRUE` and using \R interactively then the
#' user will have the option to review the proposed spelling changes. (This
#' argument is useful for turning off the [menu()] when
#' [rlang::is_interactive()] returns `TRUE` but not wanting to prompt the user
#' e.g. `devtools::run_examples()`).
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
#'   wordlist = c("confirmed", "probable", "suspected", "died", "recovered"),
#'   confirm = FALSE
#' )
clean_spelling_mistakes <- function(data,
                                    wordlist,
                                    max.distance = 1,
                                    ignore.case = FALSE,
                                    confirm = TRUE) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_character(wordlist, any.missing = FALSE)
  checkmate::assert_logical(confirm, any.missing = FALSE, len = 1)

  for (col in seq_len(ncol(data))) {
    for (word in seq_along(wordlist)) {
      # only check and fix char columns
      if (is.character(data[, col])) {
        data[, col] <- fix_spelling_mistakes(
          df_col = data[, col],
          word = wordlist[word],
          max.distance = max.distance,
          ignore.case = ignore.case,
          confirm = confirm
        )
      }
    }
  }
  return(data)
}

fix_spelling_mistakes <- function(df_col,
                                  word,
                                  max.distance,
                                  ignore.case,
                                  confirm) {
  idx <- agrepl(
    pattern = word,
    x = df_col,
    max.distance = max.distance,
    ignore.case = ignore.case
  )
  # remove correct spelling from matches
  idx[df_col == word] <- FALSE
  if (any(idx)) {
    # only show user menu when interactive
    if (rlang::is_interactive() && confirm) {
      # only print each misspelled word once
      misspelled <- unique(df_col[idx])
      menu_title <- paste(
        "The following words will be corrected:",
        toString(paste("\n -", misspelled, "->", word)),
        "\n\n (0 to exit)"
      )
      # ask user to change spelling if fuzzy matched
      user_choice <- utils::menu(
        choices = c("Yes", "No"),
        title = menu_title
      )
      if (user_choice == 1) df_col[idx] <- word
    } else {
      df_col[idx] <- word
    }
  }
  return(df_col)
}
