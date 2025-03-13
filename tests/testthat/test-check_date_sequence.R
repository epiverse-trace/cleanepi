# import the data
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

# standardize the date values
data <- data %>%
  standardize_dates(
    target_columns = c("date_first_pcr_positive_test", "date.of.admission"),
    error_tolerance = 0.4,
    format = NULL,
    timeframe = NULL
  )

test_that("is_date_sequence_ordered works as expected", {
  x <- as.Date(c("2023-10-02", "2023-10-10", "2023-10-20"))
  res <- is_date_sequence_ordered(x)
  expect_true(res)
})

test_that("is_date_sequence_ordered works as expected", {
  x <- as.Date(c("2023-10-02", "2023-10-20", "2023-10-10"))
  res <- is_date_sequence_ordered(x)
  expect_false(res)
})

test_that("check_date_sequence sends a warning when incorrect column nams are
          found", {
            expect_message(
              check_date_sequence(
                data = data,
                target_columns = c(
                  "date_first_pcr_positive_test", "date.of.admission",
                  "fake_name"
                )
              ),
              regexp = cat("Found the following unrecognised column name:
                           `fake_name`")
            )

            expect_message(
              check_date_sequence(
                data = data,
                target_columns = c(
                  "date_first_pcr_positive_test", "date.of.admission"
                )
              ),
              regexp = cat("Detected 2 incorrect date sequences at
                           lines: `6, 8`")
            )

            expect_error(
              check_date_sequence(
                data = data,
                target_columns = c(
                  "date_first_pcr_positive_test", "fake_name")
                ),
              regexp = cat("Insufficient number of columns to compare.")
            )
})

test_that("check_date_sequence works as expected when target_column is provided
          as a vector", {
              good_date_sequence <- check_date_sequence(
                data = data,
                target_columns = c(
                  "date_first_pcr_positive_test", "date.of.admission"
                )
              )
              report <- attr(good_date_sequence, "report")
              expect_s3_class(good_date_sequence, "data.frame")
              expect_identical(nrow(good_date_sequence), 10L)
              expect_type(report, "list")
              expect_named(
                report,
                c("date_standardization", "incorrect_date_sequence")
              )
              expect_s3_class(report[["incorrect_date_sequence"]], "data.frame")
              expect_identical(nrow(report[["incorrect_date_sequence"]]), 2L)
              expect_identical(ncol(report[["incorrect_date_sequence"]]), 3L)
              expect_identical(
                names(report[["incorrect_date_sequence"]]),
                c("date_first_pcr_positive_test", "date.of.admission", "row_id")
              )
})

test_that("check_date_sequence sends a message when no incorrect incorrec
          sequence of date was found", {
            data_good <- tibble::tribble(
              ~case_id, ~date_of_infection, ~date_of_onset,
              "53371b",       "2014-04-09",   "2014-04-15",
              "f5c3d8",       "2014-04-18",   "2014-04-21",
              "0f58c4",       "2014-04-22",   "2014-04-26"
            )

            expect_message(
              check_date_sequence(
                data = data_good,
                target_columns = c("date_of_infection", "date_of_onset")
              ),
              regexp = cat("No incorrect date sequence was detected.")
            )
})
