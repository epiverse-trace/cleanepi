data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

test_that("replace_missing_values works", {
  cleaned_data <- replace_missing_values(
    data           = data,
    target_columns = "sex",
    na_strings     = "-99"
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_false("-99" %in% cleaned_data[["sex"]])
  expect_true(anyNA(cleaned_data[["sex"]]))
})

test_that("replace_missing_values works", {
  cleaned_data <- replace_missing_values(
    data           = data,
    target_columns = NULL,
    na_strings     = "-99"
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_false("-99" %in% cleaned_data[["sex"]])
  expect_false("-99" %in% cleaned_data[["dateOfBirth"]])
  expect_true(anyNA(cleaned_data[["sex"]]))
  expect_true(anyNA(cleaned_data[["dateOfBirth"]]))
})

test_that("replace_missing_values fails as expected", {
  expect_error(
    replace_missing_values(
      data           = data,
      target_columns = "sex",
      na_strings     = "missing"
    ),
    regexp = cat("Assertion on',na_strings,'failed: the provided missing value
                 strings must be part of the columns in the data.")
  )
})
