data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_that("standardize_dates works with a data frame", {
  dat <- standardize_dates(
    data            = data,
    target_columns  = "date_first_pcr_positive_test",
    format          = NULL,
    timeframe       = NULL,
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))

  dat <- standardize_dates(
    data            = data,
    target_columns  = c("date_first_pcr_positive_test", "date.of.admission"),
    format          = NULL,
    timeframe       = NULL,
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))
  expect_true(inherits(dat[["date.of.admission"]], "Date"))

  dat <- standardize_dates(
    data            = data,
    target_columns  = "date_first_pcr_positive_test, date.of.admission",
    format          = NULL,
    timeframe       = NULL,
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))
  expect_true(inherits(dat[["date.of.admission"]], "Date"))

  dat <- data |>
    replace_missing_values(target_columns = "dateOfBirth",
                           na_strings = "-99") |>
    standardize_dates(
    target_columns  = NULL,
    format          = NULL,
    timeframe       = NULL,
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))
  expect_true(inherits(dat[["date.of.admission"]], "Date"))
  expect_true(inherits(dat[["dateOfBirth"]], "Date"))

  dat <- standardize_dates(
    data            = data,
    target_columns  = "date.of.admission",
    format          = "%d/%m/%Y",
    timeframe       = NULL,
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date.of.admission"]], "Date"))

  dat <- standardize_dates(
    data            = data,
    target_columns  = "date.of.admission",
    format          = "%d/%m/%Y",
    timeframe       = c(as.Date("2021-01-01"), as.Date("2021-03-01")),
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date.of.admission"]], "Date"))
  expect_true(all(is.na(dat[["date.of.admission"]][c(1L, 9L:10L)])))
})

data <- readRDS(system.file("extdata", "test_linelist.RDS",
                            package = "cleanepi")) |>
  linelist::make_linelist(date_onset     = "dt_onset",
                          date_reporting = "dt_report")
test_that("standardize_dates works with a linelist", {
  dat <- standardize_dates(
    data            = data,
    target_columns  = "linelist_tags",
    format          = NULL,
    timeframe       = NULL,
    error_tolerance = 0.5
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat, "linelist"))
  expect_true(inherits(dat[["dt_onset"]], "Date"))
  expect_true(inherits(dat[["dt_report"]], "Date"))
})
