test_that("is_date_sequence_ordered works as expected", {
  x   <- as.Date(c("2023-10-02", "2023-10-10", "2023-10-20"))
  res <- is_date_sequence_ordered(x)
  expect_true(res)
})

test_that("is_date_sequence_ordered works as expected", {
  x   <- as.Date(c("2023-10-02", "2023-10-20", "2023-10-10"))
  res <- is_date_sequence_ordered(x)
  expect_false(res)
})

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
