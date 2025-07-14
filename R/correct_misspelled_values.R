#' Correct misspelled values by using approximate string matching techniques to
#' compare them against the expected values.
#'
#' @details
#' When used interactively (see [interactive()]) the user is presented a menu
#' to ensure that the words detected using approximate string matching are not
#' false positives and the user can decided whether to proceed with the
#' spelling corrections. In non-interactive sessions all misspelled values are
#' replaced by their closest values within the provided vector of expected
#' values.
#'
<<<<<<< HEAD
#' If multiple words supplied in the `wordlist` equally match a word in the
#' data and `confirm` is `TRUE` the user is presented a menu to choose the
#' replacement word. If it is not used interactively multiple equal matches
#' throws a warning.
#'
=======
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
#' @inheritParams clean_data
#' @param target_columns A \code{<vector>} of the target column names. When the
#'    input data is a \code{<linelist>} object, this parameter can be set to
#'    \code{linelist_tags} to apply the fuzzy matching exclusively to the
#'    tagged columns.
#' @param wordlist A \code{<vector>} of characters with the words to match to
#'    the detected misspelled values.
<<<<<<< HEAD
#' @param max.distance An `integer` for the maximum distance allowed for a
#'    detecting a spelling mistakes from the `wordlist`. The distance is the
#'    generalized Levenshtein edit distance (see [adist()]). Default is `1`.
=======
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
#' @param confirm A `logical` that determines whether to show the user a menu of
#'    spelling corrections. If `TRUE` and using \R interactively then the user
#'    will have the option to review the proposed spelling corrections. This
#'    argument is useful for turning off the [menu()] when
#'    [rlang::is_interactive()] returns `TRUE` but not wanting to prompt the
#'    user e.g. `devtools::run_examples()`.
#' @param ... [dots] Extra arguments to pass to [adist()].
#'
#' @return The corrected input data according to the user-specified `wordlist`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   case_type = c("confirmed", "confermed", "probable", "susspected"),
#'   outcome = c("died", "recoverd", "did", "recovered")
#' )
#' df
<<<<<<< HEAD
#' correct_misspelled_values(
=======
#' correct_spelling_mistakes(
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
#'   data = df,
#'   target_columns = c("case_type", "outcome"),
#'   wordlist = c("confirmed", "probable", "suspected", "died", "recovered"),
#'   confirm = FALSE
#' )
correct_misspelled_values <- function(data,
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

      # find and warn if there are multiple words in wordlist that equally match
<<<<<<< HEAD
      multi_match <- vector(mode = "numeric", length = nrow(word_dist))
      wordlist_idx <- matrix(nrow = nrow(word_dist), ncol = ncol(word_dist))
      for (i in seq_len(nrow(word_dist))) {
        # ignore multiple matches for correct spelling
        wordlist_idx[i, ] <- word_dist[i, ] == min(word_dist[i, ]) &
          min(word_dist[i, ]) != 0
        multi_match[i] <- anyDuplicated(word_dist[i, ][wordlist_idx[i, ]])
      }
      multi_match_idx <- multi_match > 0L
      if (any(multi_match_idx)) {
        # only show user menu when interactive
        if (rlang::is_interactive() && confirm) {
          menu_title <- paste0(
            "'", toString(data[, col][multi_match_idx]), "'",
            " matched equally with multiple words in the `wordlist`.\n",
            "Select the spelling fix:\n",
            "\n\n (0 to exit)"
          )
          # ask user to change spelling if fuzzy matched
          user_choice <- utils::menu(
            choices = wordlist[wordlist_idx[multi_match_idx, ]],
            title = menu_title
          )
          # remove word(s) not chosen by user, important to keep the same order
          # due to word_dist
          rm_words <- setdiff(
            wordlist[wordlist_idx[multi_match_idx, ]],
            wordlist[wordlist_idx[multi_match_idx, ]][user_choice]
          )
          rm_words_idx <- !wordlist %in% rm_words

          # make copies of wordlist and word_dist to not overwrite outside loop
          wordlist_ <- wordlist[rm_words_idx]
          word_dist_ <- word_dist[, rm_words_idx]
        } else {
          warning(
            "'", toString(data[, col][multi_match_idx]), "'",
            " matched equally with multiple words in the `wordlist`.\n",
            "Using the first matched word in the `wordlist`.",
            call. = FALSE
          )

          # remove word(s) not chosen by user, important to keep the same order
          # due to word_dist
          rm_words <- setdiff(
            wordlist[wordlist_idx[multi_match_idx, ]],
            wordlist[wordlist_idx[multi_match_idx, ]][1]
          )
          rm_words_idx <- !wordlist %in% rm_words

          # make copies of wordlist and word_dist to not overwrite outside loop
          wordlist_ <- wordlist[rm_words_idx]
          word_dist_ <- word_dist[, rm_words_idx]
        }
      } else {
        # make copy of wordlist and word_dist to match multi-match case
        wordlist_ <- wordlist
        word_dist_ <- word_dist
=======
      multi_match <- apply(word_dist, MARGIN = 1, FUN = function(x) {
        # ignore multiple matches for correct spelling
        idx <- x == min(x) & min(x) != 0
        anyDuplicated(x[idx])
      })
      multi_match_idx <- multi_match > 0L
      if (any(multi_match_idx)) {
        warning(
          "'", toString(data[, col][multi_match_idx]), "'",
          " matched equally with multiple words in the `wordlist`.\n",
          "Using the first matched word in the `wordlist`."
        )
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
      }

      data[, col] <- fix_spelling_mistakes(
        df_col = data[, col],
<<<<<<< HEAD
        wordlist = wordlist_,
        max.distance = max.distance,
        confirm = confirm,
        word_dist = word_dist_
=======
        wordlist = wordlist,
        max.distance = max.distance,
        confirm = confirm,
        word_dist = word_dist
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
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
                                  wordlist,
                                  max.distance,
                                  confirm,
                                  word_dist) {
  for (i in seq_len(nrow(word_dist))) {
<<<<<<< HEAD
    # check for misspelling within max.distance and no NA or zeros
    misspelled <- any(word_dist[i, ] > 0L & word_dist[i, ] <= max.distance &
                        !is.na(word_dist[i, ])) && !any(word_dist[i, ] == 0)
=======
    misspelled <- all(word_dist[i, ] > 0L & word_dist[i, ] <= max.distance &
                        !is.na(word_dist[i, ]))
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
    if (misspelled) {
      # only show user menu when interactive
      if (rlang::is_interactive() && confirm) {
        menu_title <- paste(
          "The following words will be corrected:",
<<<<<<< HEAD
          toString(paste(
            "\n -", df_col[i], "->", wordlist[which.min(word_dist[i, ])]
          )),
=======
          toString(paste("\n -", df_col[i], "->", wordlist[which.min(word_dist[i, ])])),
>>>>>>> cebaa7d (rename correct_spelling_mistakes to correct_misspelled_values and update documentation)
          "\n\n (0 to exit)"
        )
        # ask user to change spelling if fuzzy matched
        user_choice <- utils::menu(
          choices = c("Yes", "No"),
          title = menu_title
        )
        if (user_choice == 1) df_col[i] <- wordlist[which.min(word_dist[i, ])]
      } else {
        # replace with word that is nearest match (first if multiple)
        df_col[i] <- wordlist[which.min(word_dist[i, ])]
      }
    }
  }
  return(df_col)
}
