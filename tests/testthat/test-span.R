data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")) |>
  replace_missing_values(target_columns = "dateOfBirth",
                         na_strings     = "-99") |>
  standardize_dates(target_columns  = c("dateOfBirth",
                                        "date_first_pcr_positive_test"),
                    error_tolerance = 0.0)

test_that("timespan works when the time span is calculated in months and
          remainder is sent in days", {
            time_span <- timespan(
              data                = data,
              target_column       = "dateOfBirth",
              end_date            = Sys.Date(),
              span_unit           = "months",
              span_column_name    = "age_in_months",
              span_remainder_unit = "days"
            )
            expect_s3_class(time_span, "data.frame")
            expect_identical(ncol(time_span), 10L)
            expect_identical(names(time_span)[9L:10L], c("age_in_months",
                                                         "remainder_days"))
          })

test_that("timespan works when the time span is calculated in months and
          remainder is sent in weeks", {
            time_span <- timespan(
              data                = data,
              target_column       = "dateOfBirth",
              end_date            = Sys.Date(),
              span_unit           = "months",
              span_column_name    = "age_in_months",
              span_remainder_unit = "weeks"
            )
            expect_s3_class(time_span, "data.frame")
            expect_identical(ncol(time_span), 10L)
            expect_identical(names(time_span)[9L:10L], c("age_in_months",
                                                         "remainder_weeks"))
          })

test_that("timespan works when the time span is calculated in years and
          remainder is sent in months", {
            time_span <- timespan(
              data                = data,
              target_column       = "dateOfBirth",
              end_date            = Sys.Date(),
              span_unit           = "years",
              span_column_name    = "age_in_years",
              span_remainder_unit = "months"
            )
            expect_s3_class(time_span, "data.frame")
            expect_identical(ncol(time_span), 10L)
            expect_identical(names(time_span)[9L:10L],
                             c("age_in_years", "remainder_months"))
          })

test_that("timespan works when the time span is calculated in years and
          remainder is sent in weeks", {
            time_span <- timespan(
              data                = data,
              target_column       = "dateOfBirth",
              end_date            = Sys.Date(),
              span_unit           = "years",
              span_column_name    = "age_in_years",
              span_remainder_unit = "weeks"
            )
            expect_s3_class(time_span, "data.frame")
            expect_identical(ncol(time_span), 10L)
            expect_identical(names(time_span)[9L:10L],
                             c("age_in_years", "remainder_weeks"))

          })

test_that("timespan works when the time span is calculated in years and
          remainder is sent in days", {
            time_span <- timespan(
              data                = data,
              target_column       = "dateOfBirth",
              end_date            = Sys.Date(),
              span_unit           = "years",
              span_column_name    = "age_in_years",
              span_remainder_unit = "days"
            )
            expect_s3_class(time_span, "data.frame")
            expect_identical(ncol(time_span), 10L)
            expect_identical(names(time_span)[9L:10L],
                             c("age_in_years", "remainder_days"))
          })

test_that("timespan works when the time span is calculated in days", {
  time_span <- timespan(
    data                = data,
    target_column       = "dateOfBirth",
    end_date            = Sys.Date(),
    span_unit           = "days",
    span_column_name    = "age_in_days",
    span_remainder_unit = NULL
  )
  expect_s3_class(time_span, "data.frame")
  expect_identical(ncol(time_span), 9L)
  expect_identical(names(time_span)[9L], "age_in_days")
})

test_that("timespan works when the time span is returned in decimal", {
  time_span <- timespan(
    data                = data,
    target_column       = "dateOfBirth",
    end_date            = Sys.Date(),
    span_unit           = "years",
    span_column_name    = "age_in_years",
    span_remainder_unit = NULL
  )
  expect_s3_class(time_span, "data.frame")
  expect_identical(ncol(time_span), 9L)
  expect_identical(names(time_span)[9L], "age_in_years")
  expect_identical(typeof(time_span[["age_in_years"]]), "double")

  time_span <- timespan(
    data                = data,
    target_column       = "dateOfBirth",
    end_date            = Sys.Date(),
    span_unit           = "months",
    span_column_name    = "age_in_months",
    span_remainder_unit = NULL
  )
  expect_s3_class(time_span, "data.frame")
  expect_identical(ncol(time_span), 9L)
  expect_identical(names(time_span)[9L], "age_in_months")
  expect_identical(typeof(time_span[["age_in_months"]]), "double")

  time_span <- timespan(
    data                = data,
    target_column       = "dateOfBirth",
    end_date            = Sys.Date(),
    span_unit           = "weeks",
    span_column_name    = "age_in_weeks",
    span_remainder_unit = NULL
  )
  expect_s3_class(time_span, "data.frame")
  expect_identical(ncol(time_span), 9L)
  expect_identical(names(time_span)[9L], "age_in_weeks")
  expect_identical(typeof(time_span[["age_in_weeks"]]), "double")
})

test_that("timespan works when the time span is calculated between 2 columns", {
  time_span <- timespan(
    data                = data,
    target_column       = "dateOfBirth",
    end_date            = "date_first_pcr_positive_test",
    span_unit           = "years",
    span_column_name    = "elapsed_time",
    span_remainder_unit = NULL
  )
  expect_s3_class(time_span, "data.frame")
  expect_identical(ncol(time_span), 9L)
  expect_identical(names(time_span)[9L], "elapsed_time")
})

test_that("timespan works when the time span is calculated between a column of
          of the data frame and a vector of dates", {
            end_date <- data[["date_first_pcr_positive_test"]]
            time_span <- timespan(
              data                = data,
              target_column       = "dateOfBirth",
              end_date            = end_date,
              span_unit           = "years",
              span_column_name    = "elapsed_time",
              span_remainder_unit = NULL
            )
  expect_s3_class(time_span, "data.frame")
  expect_identical(ncol(time_span), 9L)
  expect_identical(names(time_span)[9L], "elapsed_time")
})
