test_that("is_order works as expected", {
  x   <- as.Date(c("2023-10-02", "2023-10-10", "2023-10-20"))
  res <- is_order(x)
  expect_true(res)
})

test_that("is_order works as expected", {
  x   <- as.Date(c("2023-10-02", "2023-10-20", "2023-10-10"))
  res <- is_order(x)
  expect_false(res)
})

# test_that("is_order fails when input values are not of type Date", {
#   x   <- c(-1, 0, 10)
#   expect_error(
#     is_order(x),
#     regexp = cat("Incorrect values supllied. Input values should be of type
#                  'Date' or 'character.'")
#   )
# })

# -----

# test_that("check_date_sequence sends a warning when incorrect sequence is
#           found", {
#             expect_warning(
#               check_date_sequence(
#                 data           = readRDS(system.file("extdata", "test_df.RDS",
#                                                      package = "cleanepi")),
#                 target_columns = c("date_first_pcr_positive_test",
#                                    "date.of.admission"),
#                 remove_bad_seq = FALSE
#               ),
#               "Detected 2 incorrect date sequences at line(s): 6, 8"
#             )
# })

test_that("check_date_sequence works as expected when target_column is provided
          as a vector", {
  good_date_sequence <- check_date_sequence(
    data           = readRDS(system.file("extdata", "test_df.RDS",
                                         package = "cleanepi")),
    target_columns = c("date_first_pcr_positive_test", "date.of.admission"),
    remove         = FALSE
  )
  expect_s3_class(good_date_sequence, "data.frame")
  expect_identical(nrow(good_date_sequence), 10L)
})

test_that("check_date_sequence works as expected when target_column is provided
          as a comma-separated list of column names", {
            good_date_sequence <- check_date_sequence(
              data           = readRDS(system.file("extdata", "test_df.RDS",
                                                   package = "cleanepi")),
              target_columns = "date_first_pcr_positive_test, date.of.admission", # nolint: line_length_linters
              remove         = FALSE
            )
            expect_s3_class(good_date_sequence, "data.frame")
            expect_identical(nrow(good_date_sequence), 10L)
          })

test_that("check_date_sequence works as expected", {
  good_date_sequence <- check_date_sequence(
    data           = readRDS(system.file("extdata", "test_df.RDS",
                                         package = "cleanepi")),
    target_columns = c("date_first_pcr_positive_test", "date.of.admission"),
    remove         = TRUE
  )
  expect_s3_class(good_date_sequence, "data.frame")
  expect_identical(nrow(good_date_sequence), 8L)
})
