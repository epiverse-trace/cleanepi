data <- readRDS(system.file("extdata", "messy_data.RDS", package = "cleanepi"))
test_that("convert_to_numeric works", {
  dat <- convert_to_numeric(
    data           = data,
    target_columns = "age"
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["age"]], "numeric"))

  dat <- convert_to_numeric(
    data           = data,
    target_columns = NULL
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["age"]], "numeric"))
})

test_that("convert_to_numeric sends a warning when no column is provided and
          scan_data() does not find a target column", {
            expect_warning(
              convert_to_numeric(
                data           = data,
                target_columns = NULL
              ),
              regexp = cat("'gender' column has similar number of numeric and
                           character values.")
            )
          })

scan_res <- scan_data(data)
test_that("detect_to_numeric_columns works", {
  to_numeric <- detect_to_numeric_columns(scan_res = scan_res)
  expect_type(to_numeric, "character")
  expect_identical(to_numeric, "age")
})

test_that("detect_to_numeric_columns sends a warning when no column is provided
          and scan_data() does not find a target column", {
            expect_warning(
              detect_to_numeric_columns(
                scan_res = scan_res
              ),
              regexp = cat("'gender' column has similar number of numeric and
                           character values.")
            )
          })

data <- readRDS(system.file("extdata", "test_df1.RDS", package = "cleanepi"))
test_that("detect_to_numeric_columns sends a warning when no column is provided
          and scan_data() does not find a target column", {
            expect_error(
              convert_to_numeric(
                data           = data,
                target_columns = NULL
              ),
              regexp = cat("target_columns not specified and could not be
                           identified from scan_data() function.")
            )
          })
