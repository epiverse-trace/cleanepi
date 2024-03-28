dat <- readRDS(system.file("extdata", "test_df1.RDS", package = "cleanepi"))
test_that("convert_numeric_to_date works with forward = TRUE", {
  dat <- convert_numeric_to_date(
    data           = dat,
    target_columns = "recruted_on_day",
    ref_date       = as.Date("2022-10-13"),
    forward        = TRUE
  )
  expect_s3_class(dat, "data.frame")
  expect_identical(ncol(dat), 3L)
  expect_true(inherits(dat[["recruted_on_day"]], "Date"))
})

test_that("convert_numeric_to_date works with forward = FALSE", {
  dat <- convert_numeric_to_date(
    data           = dat,
    target_columns = "recruted_on_day",
    ref_date       = as.Date("2022-10-13"),
    forward        = FALSE
  )
  expect_s3_class(dat, "data.frame")
  expect_identical(ncol(dat), 3L)
  expect_true(inherits(dat[["recruted_on_day"]], "Date"))
})

test_that("convert_numeric_to_date works with a vector of column names", {
  dat <- convert_numeric_to_date(
    data           = dat,
    target_columns = c("recruted_on_day", "removed_on_day"),
    ref_date       = as.Date("2022-10-13"),
    forward        = FALSE
  )
  expect_s3_class(dat, "data.frame")
  expect_identical(ncol(dat), 3L)
  expect_true(inherits(dat[["recruted_on_day"]], "Date"))
  expect_true(inherits(dat[["removed_on_day"]], "Date"))
})

test_that("convert_numeric_to_date works with a comma-separated list of column
          names", {
            dat <- convert_numeric_to_date(
              data           = dat,
              target_columns = "recruted_on_day, removed_on_day",
              ref_date       = as.Date("2022-10-13"),
              forward        = FALSE
            )
            expect_s3_class(dat, "data.frame")
            expect_identical(ncol(dat), 3L)
            expect_true(inherits(dat[["recruted_on_day"]], "Date"))
            expect_true(inherits(dat[["removed_on_day"]], "Date"))
})

test_that("convert_numeric_to_date fails as expected", {
  expect_error(
    convert_numeric_to_date(
      data           = dat,
      target_columns = "fake_column_name",
      ref_date       = as.Date("2022-10-13"),
      forward        = FALSE
    ),
    regexp = cat("Supplied incorrect target column name")
  )

  expect_error(
    convert_numeric_to_date(
      data           = dat,
      target_columns = "recruted_on_day",
      ref_date       = "fake_column_name",
      forward        = FALSE
    ),
    regexp = cat("'fake_column_name' not found.")
  )
})
