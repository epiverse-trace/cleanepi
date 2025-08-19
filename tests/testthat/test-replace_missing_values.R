data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_that("replace_missing_values works when both target_columns and na_strings
          are provided", {
            cleaned_data <- replace_missing_values(
              data = data,
              target_columns = "sex",
              na_strings = "-99"
            )
            expect_s3_class(cleaned_data, "data.frame")
            expect_false("-99" %in% cleaned_data[["sex"]])
            expect_true(anyNA(cleaned_data[["sex"]]))
            expect_true(all(
              is.na(cleaned_data[["sex"]][data[["sex"]] == "-99"])
            ))
})

test_that("replace_missing_values works when target_columns is set to NULL", {
  cleaned_data <- replace_missing_values(
    data = data,
    target_columns = NULL,
    na_strings = "-99"
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_false("-99" %in% cleaned_data[["sex"]])
  expect_false("-99" %in% cleaned_data[["dateOfBirth"]])
  expect_true(all(is.na(cleaned_data[["sex"]][data[["sex"]] == "-99"])))
  expect_true(
    all(is.na(cleaned_data[["dateOfBirth"]][data[["dateOfBirth"]] == "-99"]))
  )
})

test_that("replace_missing_values is case and whitespace insensitive", {
  data$country_name[1] <- "Not avaiLablE"
  data$country_name[2] <- " Not available "
  cleaned_data <- replace_missing_values(data = data)
  expect_s3_class(cleaned_data, "data.frame")
  expect_false("Not avaiLablE" %in% cleaned_data[["country_name"]])
  expect_true(anyNA(cleaned_data[["country_name"]]))
})

test_that("replace_missing_values fails as expected", {
  expect_message(
    replace_missing_values(
      data = data,
      target_columns = "sex",
      na_strings = "missing"
    ),
    regexp = cat("Could not detect the provided missing value character.")
  )
})
