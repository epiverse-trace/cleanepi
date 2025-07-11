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
#' @inheritParams convert_numeric_to_date
#' @param wordlist A `character` vector of words to match to cells in `data`.
#' @param max.distance An `integer` for the maximum distance allowed for a
#' detecting a spelling mistakes from the `wordlist`. The distance is the
#' generalized Levenshtein edit distance (see [adist()]). Default is `1`.
#' @param confirm A `logical` boolean to determine whether to show the user a
#' menu of spelling corrections. If `TRUE` and using \R interactively then the
#' user will have the option to review the proposed spelling changes. (This
#' argument is useful for turning off the [menu()] when
#' [rlang::is_interactive()] returns `TRUE` but not wanting to prompt the user
#' e.g. `devtools::run_examples()`).
#' @param ... [dots] Extra arguments to pass to [adist()].
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
#'   target_columns = c("case_type", "outcome"),
#'   wordlist = c("confirmed", "probable", "suspected", "died", "recovered"),
#'   confirm = FALSE
#' )
clean_spelling_mistakes <- function(data,
                                    target_columns,
                                    wordlist,
                                    max.distance = 1,
                                    confirm = rlang::is_interactive(),
                                    ...) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_vector(
    target_columns, min.len = 1, max.len = ncol(data), null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_character(wordlist, any.missing = FALSE)
  checkmate::assert_integerish(max.distance, any.missing = FALSE)
  checkmate::assert_logical(confirm, any.missing = FALSE, len = 1)

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  for (col in target_columns) {
    # only check and fix char columns
    if (is.character(data[, col])) {
      word_dist <- utils::adist(data[, col], wordlist)
      # ignore correct spelling from matches
      misspell_idx <- word_dist <= max.distance & word_dist > 0 &
        !is.na(word_dist)
      data[, col] <- fix_spelling_mistakes(
        df_col = data[, col],
        misspell_idx = misspell_idx,
        wordlist = wordlist,
        confirm = confirm
      )
    } else {
      cli::cli_inform(c(
        "!" = tr_("The {.val {col}} column given to {.code target_columns} does not contain {.code character}s so cannot be spell checked.") # nolint: line_length_linter
      ))
    }
  }
  return(data)
}

fix_spelling_mistakes <- function(df_col,
                                  misspell_idx,
                                  wordlist,
                                  confirm) {
  for (idx in seq_along(wordlist)) {
    if (any(misspell_idx[, idx])) {
      # only show user menu when interactive
      if (rlang::is_interactive() && confirm) {
        # only print each misspelled word once
        misspelled <- unique(df_col[misspell_idx[, idx]])
        menu_title <- paste(
          "The following words will be corrected:",
          toString(paste("\n -", misspelled, "->", wordlist[idx])),
          "\n\n (0 to exit)"
        )
        # ask user to change spelling if fuzzy matched
        user_choice <- utils::menu(
          choices = c("Yes", "No"),
          title = menu_title
        )
        if (user_choice == 1) df_col[misspell_idx[, idx]] <- wordlist[idx]
      } else {
        df_col[misspell_idx[, idx]] <- wordlist[idx]
      }
    }
  }
  return(df_col)
}
