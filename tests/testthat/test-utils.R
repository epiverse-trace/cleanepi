test_that("get_target_column_names works with
          target_columns = 'linelist_tags'", {
            target_columns <- get_target_column_names(
              data = readRDS(
                system.file("extdata", "test_linelist.RDS",
                            package = "cleanepi")
              ),
              target_columns = "linelist_tags",
              cols = NULL
            )
            expect_type(target_columns, "character")
            expect_length(target_columns, 4L)
            expect_identical(target_columns, c("dt_onset", "dt_report", "sex",
                                               "outcome"))
          })

test_that("get_target_column_names works with
          target_columns as a vector of column names", {
            target_columns <- get_target_column_names(
              data = readRDS(
                system.file("extdata", "test_df.RDS", package = "cleanepi")
              ),
              target_columns = c("dateOfBirth", "sex"),
              cols = NULL
            )
            expect_type(target_columns, "character")
            expect_length(target_columns, 2L)
            expect_identical(target_columns, c("dateOfBirth", "sex"))
          })

test_that("get_target_column_names works with
          target_columns as a vector of numeric values", {
            target_columns <- get_target_column_names(
              data = readRDS(
                system.file("extdata", "test_df.RDS", package = "cleanepi")
              ),
              target_columns = c(6L, 8L),
              cols = NULL
            )
            expect_type(target_columns, "character")
            expect_length(target_columns, 2L)
            expect_identical(target_columns, c("dateOfBirth", "sex"))
          })

test_that("get_target_column_names works with target_columns and cols", {
  target_columns <- get_target_column_names(
    data = readRDS(
      system.file("extdata", "test_df.RDS", package = "cleanepi")
    ),
    target_columns = c("dateOfBirth", "sex", "country_name"),
    cols = "country_name"
  )
  expect_type(target_columns, "character")
  expect_length(target_columns, 2L)
  expect_identical(target_columns, c("dateOfBirth", "sex"))
})

test_that("get_target_column_names fails as expected", {
  expect_error(
    get_target_column_names(
      data = readRDS(
        system.file("extdata", "test_df.RDS", package = "cleanepi")
      ),
      target_columns = "country_name",
      cols = "country_name"
    ),
    regexp = cat("Assertion on',target_columns,'failed: all specified target
                 columns will be ignored because they are either empty or
                 constant.")
  )
})

test_that("get_target_column_names fails as expected", {
  expect_error(
    get_target_column_names(
      data = readRDS(
        system.file("extdata", "test_df.RDS", package = "cleanepi")
      ),
      target_columns = "linelist_tags",
      cols = NULL
    ),
    regexp = cat("Assertion on',keep,'failed: usage of 'linelist_tags'
                 is only reserved for 'linelist' type of data.")
  )
})

data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
scan_res <- scan_data(data)

# Perform data cleaning
cleaned_data <- data %>%
  replace_missing_values(target_columns = "sex", na_strings = "-99") %>%
  convert_to_numeric(target_columns = "sex", lang = "en")

test_that("add_to_report works as expected", {
  cleaned_data <- add_to_report(
    x = cleaned_data,
    key = "scanning_result",
    value = scan_res
  )
  report <- attr(cleaned_data, "report")
  expect_type(report, "list")
  expect_length(report, 3L)
  expect_named(report, c("missing_values_replaced_at", "converted_into_numeric",
                         "scanning_result"))
  expect_identical(report[["missing_values_replaced_at"]], "sex")
  expect_identical(report[["converted_into_numeric"]], "sex")

  data <- add_to_report(
    x = data,
    key = "scanning_result",
    value = scan_res
  )
  report <- attr(data, "report")
  expect_type(report, "list")
  expect_length(report, 1L)
  expect_named(report, "scanning_result")
  expect_named(report[["scanning_result"]],
               c("Field_names", "missing", "numeric", "date", "character",
                 "logical"))
  expect_identical(nrow(report[["scanning_result"]]), 6L)
})


# test internal function
test_that("date_get_part1 works as expected", {
  res <- date_get_part1(NA, sep = "/")
  expect_identical(res, NA)

  res <- date_get_part1("2024/03/28", sep = "/")
  expect_identical(res, "2024")
})

test_that("date_get_part2 works as expected", {
  res <- date_get_part2(NA, sep = "/")
  expect_identical(res, NA)

  res <- date_get_part2("2024/03/28", sep = "/")
  expect_identical(res, "03")
})

test_that("date_get_part3 works as expected", {
  res <- date_get_part3(NA, sep = "/")
  expect_identical(res, NA)

  res <- date_get_part3("2024/03/28", sep = "/")
  expect_identical(res, "28")
})

test_that("get_target_column_names fails as expected", {
  data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
  expect_error(
    get_target_column_names(data, target_columns = c(2L, 3L, 10L), cols = NULL),
    regexp = cat("Some specified column names indices are out of bound.")
  )
})
