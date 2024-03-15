data <- readRDS(system.file("extdata", "test_linelist.RDS",
                            package = "cleanepi"))

test_that("remove_duplicates works with 'linelist_tags'", {
  no_dups <- remove_duplicates(
    data             = data,
    target_columns   = "linelist_tags",
    remove           = NULL,
    rm_empty_rows    = TRUE,
    rm_empty_cols    = TRUE,
    rm_constant_cols = TRUE
  )
  report <- attr(no_dups, "report")
  expect_type(report, "list")
  expect_length(report, 5L)
  expect_named(report, c("empty_columns", "constant_columns", "duplicated_rows",
                         "duplicates_checked_from", "removed_duplicates"))
  expect_true(inherits(report[["duplicated_rows"]], "data.frame"))
  expect_true(inherits(report[["removed_duplicates"]], "data.frame"))
  expect_type(report[["duplicates_checked_from"]], "character")
  expect_true(all(c("row_id", "group_id") %in%
                    colnames(report[["duplicated_rows"]])))
  expect_false("group_id" %in% colnames(report[["removed_duplicates"]]))
})

test_that("remove_duplicates works with 'linelist_tags'", {
  no_dups <- remove_duplicates(
    data             = data,
    target_columns   = c("dt_onset", "dt_report", "sex", "outcome"),
    remove           = NULL,
    rm_empty_rows    = TRUE,
    rm_empty_cols    = TRUE,
    rm_constant_cols = TRUE
  )
  report <- attr(no_dups, "report")
  expect_type(report, "list")
  expect_length(report, 5L)
  expect_named(report, c("empty_columns", "constant_columns", "duplicated_rows",
                         "duplicates_checked_from", "removed_duplicates"))
  expect_true(inherits(report[["duplicated_rows"]], "data.frame"))
  expect_true(inherits(report[["removed_duplicates"]], "data.frame"))
  expect_type(report[["duplicates_checked_from"]], "character")
  expect_true(all(c("row_id", "group_id") %in%
                    colnames(report[["duplicated_rows"]])))
  expect_false("group_id" %in% colnames(report[["removed_duplicates"]]))
})

test_that("remove_duplicates works when removing specific duplicated rows", {
  no_dups <- remove_duplicates(
    data             = data,
    target_columns   = c("dt_onset", "dt_report", "sex", "outcome"),
    remove           = c(26L, 62L, 23L, 105L, 31L),
    rm_empty_rows    = TRUE,
    rm_empty_cols    = TRUE,
    rm_constant_cols = TRUE
  )
  report <- attr(no_dups, "report")
  expect_type(report, "list")
  expect_length(report, 5L)
  expect_named(report, c("empty_columns", "constant_columns", "duplicated_rows",
                         "duplicates_checked_from", "removed_duplicates"))
  expect_true(inherits(report[["duplicated_rows"]], "data.frame"))
  expect_true(inherits(report[["removed_duplicates"]], "data.frame"))
  expect_type(report[["duplicates_checked_from"]], "character")
  expect_true(all(c("row_id", "group_id") %in%
                    colnames(report[["duplicated_rows"]])))
  expect_false("group_id" %in% colnames(report[["removed_duplicates"]]))
  expect_identical(nrow(report[["removed_duplicates"]]), 5L)
  expect_true(identical(report[["removed_duplicates"]][["row_id"]],
                        c(26L, 62L, 23L, 105L, 31L)))
})

test_that("remove_duplicates works when removing specific duplicated rows", {
  no_dups <- remove_duplicates(
    data             = data,
    target_columns   = c("dt_onset", "dt_report", "sex", "outcome"),
    remove           = c(26L, 62L, 23L, 105L, 31L),
    rm_empty_rows    = FALSE,
    rm_empty_cols    = FALSE,
    rm_constant_cols = FALSE
  )
  report <- attr(no_dups, "report")
  expect_type(report, "list")
  expect_length(report, 5L)
  expect_named(report, c("empty_columns", "constant_columns", "duplicated_rows",
                         "duplicates_checked_from", "removed_duplicates"))
  expect_true(inherits(report[["duplicated_rows"]], "data.frame"))
  expect_true(inherits(report[["removed_duplicates"]], "data.frame"))
  expect_type(report[["duplicates_checked_from"]], "character")
  expect_true(all(c("row_id", "group_id") %in%
                    colnames(report[["duplicated_rows"]])))
  expect_false("group_id" %in% colnames(report[["removed_duplicates"]]))
  expect_identical(nrow(report[["removed_duplicates"]]), 5L)
  expect_true(identical(report[["removed_duplicates"]][["row_id"]],
                        c(26L, 62L, 23L, 105L, 31L)))
})




test_that("find_duplicates works with a vector of column names", {
  dups <- find_duplicates(
    data           = data,
    target_columns = c("dt_onset", "dt_report", "sex", "outcome")
  )
  report <- attr(dups, "report")
  expect_type(report, "list")
  expect_length(report, 2L)
  expect_true(inherits(report[["duplicated_rows"]], "data.frame"))
  expect_type(report[["duplicates_checked_from"]], "character")
  expect_true(all(c("row_id", "group_id") %in%
                    colnames(report[["duplicated_rows"]])))
})

test_that("find_duplicates works with 'linelist_tags'", {
  dups <- find_duplicates(
    data           = data,
    target_columns = "linelist_tags"
  )
  report <- attr(dups, "report")
  expect_type(report, "list")
  expect_length(report, 2L)
  expect_true(inherits(report[["duplicated_rows"]], "data.frame"))
  expect_type(report[["duplicates_checked_from"]], "character")
  expect_true(all(c("row_id", "group_id") %in%
                    colnames(report[["duplicated_rows"]])))
})

test_that("find_duplicates works when target_columns = NULL", {
  dups <- find_duplicates(
    data           = data,
    target_columns = NULL
  )
  report <- attr(dups, "report")
  expect_null(report)
})

test_that("find_duplicates sends a messages when duplicates are found", {
  expect_message(
    find_duplicates(
      data           = data,
      target_columns = "linelist_tags"
    ),
    "Found 57 duplicated rows. Please consult the report for more details."
  )
})