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
#' @inheritParams clean_data
#' @param target_columns A \code{<vector>} of the target column names. When the
#'    input data is a \code{<linelist>} object, this parameter can be set to
#'    \code{linelist_tags} to apply the fuzzy matching exclusively to the
#'    tagged columns.
#' @param wordlist A \code{<vector>} of characters with the words to match to
#'    the detected misspelled values.
#' @param confirm A `logical` that determines whether to show the user a menu of
#'    spelling corrections. If `TRUE` and using \R interactively then the user
#'    will have the option to review the proposed spelling corrections. This
#'    argument is useful for turning off the [menu()] when
#'    [rlang::is_interactive()] returns `TRUE` but not wanting to prompt the
#'    user e.g. `devtools::run_examples()`.
#' @param ... Any extra argument that can be taken by the \code{base::agrep()}
#'    function. Type \code{?base::agrep} for more details.
#'
#' @return The cleaned input data according to the user-specified `wordlist`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   case_type = c("confirmed", "confermed", "probable", "susspected"),
#'   outcome = c("died", "recoverd", "did", "recovered")
#' )
#' df
#'
#' corrected_df <- correct_misspelled_values(
#'   data = df,
#'   wordlist = c("confirmed", "probable", "suspected", "died", "recovered"),
#'   confirm = FALSE
#' )
#' corrected_df
correct_misspelled_values <- function(data,
                                      target_columns = NULL,
                                      wordlist,
                                      confirm = TRUE,
                                      ...) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_logical(confirm, any.missing = FALSE, len = 1)
  checkmate::assert_vector(wordlist, min.len = 1, null.ok = FALSE,
                           any.missing = FALSE)
  # get the list of the extra parameters
  extra_params <- get_extra_args(list(...))
  checkmate::check_names(
    extra_params,
    subset.of = c(
      "max.distance", "ignore.case", "costs", "value", "fixed", "useBytes"
    )
  )

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  for (col in target_columns) {
    for (word in seq_along(wordlist)) {
      # only check and fix char columns
      if (is.character(data[[col]])) {
        data[[col]] <- fix_spelling_mistakes(
          df_col = data[[col]],
          word = wordlist[word],
          max.distance = extra_params[["max.distance"]],
          ignore.case = extra_params[["ignore.case"]],
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
  distance_matrix <- adist(df_col, word)
  find_min_dist <- function(x) {
    return(which(x == min(x)))
  }
  mins <- apply(distance_matrix, 1, find_min_dist)
  lengths(mins)


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

#' Get and update the extra arguments to the [correct_misspelled_values()]
#' function.
#'
#' @param args_list A list with the extra arguments. Default is NULL.
#'
#' @return A list with the arguments needed by the [base::agrep()] function.
#' @keywords internal
get_extra_args <- function(args_list) {
  # set default to base::agrep() default values
  defaults <- list(
    max.distance = 0.1,
    ignore.case = FALSE,
    costs = NULL,
    value = FALSE,
    fixed = TRUE,
    useBytes = FALSE
  )

  if (length(args_list) == 0) {
    return(defaults)
  }

  return(utils::modifyList(defaults, args_list, keep.null = TRUE))
}
