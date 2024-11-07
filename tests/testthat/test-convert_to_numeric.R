data <- readRDS(system.file("extdata", "messy_data.RDS", package = "cleanepi"))
test_that("convert_to_numeric works", {
  # test if it works fine when the target column names is specified
  dat <- convert_to_numeric(
    data           = data,
    target_columns = "age",
    lang           = "en"
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["age"]], "numeric"))

  # test if it works when the target column names are not specified but inferred
  # from the scan_data() result.
  dat <- convert_to_numeric(
    data           = data,
    target_columns = NULL,
    lang           = "en"
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["age"]], "numeric"))
})

test_that("convert_to_numeric sends a warning when no column is provided and
          scan_data() does not find a target column", {
            expect_message(
              convert_to_numeric(
                data           = data,
                target_columns = NULL
              ),
              regexp = cat("Found `3750` numeric values in `test`. Consider
                           converting characters into numeric or replacing the
                           numeric values by `NA` using the
                           `replace_missing_values()` function.")
            )
          })

test_that("convert_to_numeric returns NA when the specified language is not
          appropriate.", {
            dat <- data[1L:10L, ]
            idx <- which(is.na(suppressWarnings(as.numeric(dat$age))))
            dat <- convert_to_numeric(
              data           = dat,
              target_columns = "age",
              lang           = "fr"
            )
            expect_s3_class(dat, "data.frame")
            expect_identical(nrow(dat), 10L)
            expect_true(all(is.na(dat$age[idx])))
          })

scan_res <- scan_data(data)
test_that("detect_to_numeric_columns works", {
  to_numeric <- detect_to_numeric_columns(scan_res = scan_res, data)
  expect_type(to_numeric, "character")
  expect_identical(to_numeric, "age")
})

test_that("detect_to_numeric_columns sends a warning when no column is provided
          and scan_data() does not find a target column", {
            expect_message(
              detect_to_numeric_columns(
                scan_res = scan_res,
                data = data
              ),
              regexp = cat("Found `3750` numeric values in `test`. Consider
                           converting characters into numeric or replacing the
                           numeric values by `NA` using the
                           `replace_missing_values()` function.")
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
