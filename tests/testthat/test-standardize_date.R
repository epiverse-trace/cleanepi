
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")) %>%
  replace_missing_values(na_strings = "-99")
test_that("standardize_dates works with a data frame", {
  dat <- standardize_dates(
    data = data,
    target_columns = "date_first_pcr_positive_test",
    format = NULL,
    timeframe = as.Date(c("1973-05-29", "2023-05-29")),
    error_tolerance = 0.4,
    orders = list(
      world_named_months = c("Ybd", "dby"),
      world_digit_months = c("dmy", "Ymd"),
      US_formats = c("Omdy", "YOmd")
    )
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))

  dat <- standardize_dates(
    data = data,
    target_columns = c("date_first_pcr_positive_test", "date.of.admission"),
    format = NULL,
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(
      world_named_months = c("Ybd", "dby"),
      world_digit_months = c("dmy", "Ymd"),
      US_formats = c("Omdy", "YOmd")
    )
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))
  expect_true(inherits(dat[["date.of.admission"]], "Date"))

  dat <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")) %>%
    replace_missing_values(target_columns = "dateOfBirth",
                           na_strings = "-99") %>%
    standardize_dates(
    target_columns = NULL,
    format = NULL,
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(
      world_named_months = c("Ybd", "dby"),
      world_digit_months = c("dmy", "Ymd"),
      US_formats = c("Omdy", "YOmd")
    )
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date_first_pcr_positive_test"]], "Date"))
  expect_true(inherits(dat[["date.of.admission"]], "Date"))
  expect_true(inherits(dat[["dateOfBirth"]], "Date"))

  dat <- standardize_dates(
    data = data,
    target_columns = "date.of.admission",
    format = "%d/%m/%Y",
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(
      world_named_months = c("Ybd", "dby"),
      world_digit_months = c("dmy", "Ymd"),
      US_formats = c("Omdy", "YOmd")
    )
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date.of.admission"]], "Date"))

  dat <- standardize_dates(
    data = data,
    target_columns = "date.of.admission",
    format = "%d/%m/%Y",
    timeframe = as.Date(c("2021-01-01", "2021-03-01")),
    error_tolerance = 0.4,
    orders = list(
      world_named_months = c("Ybd", "dby"),
      world_digit_months = c("dmy", "Ymd"),
      US_formats = c("Omdy", "YOmd")
    )
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date.of.admission"]], "Date"))
  expect_true(all(is.na(dat[["date.of.admission"]][c(1L, 9L:10L)])))
})

test_that("standardize_dates works when the values are already in ISO format", {
  tmp_data <- standardize_dates(
    data = data,
    target_columns = "date.of.admission",
    format = NULL,
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(world_named_months = c("Ybd", "dby"),
                  world_digit_months = c("dmy", "Ymd"),
                  US_formats = c("Omdy", "YOmd"))
  )
  dat <- standardize_dates(
    data = tmp_data,
    target_columns = "date.of.admission",
    format = NULL,
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(world_named_months = c("Ybd", "dby"),
                  world_digit_months = c("dmy", "Ymd"),
                  US_formats = c("Omdy", "YOmd"))
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date.of.admission"]], "Date"))
})

test_that("standardize_dates works when the input column is a factor", {
  data[["date.of.admission"]] <- as.factor(data[["date.of.admission"]])
  dat <- standardize_dates(
    data = data,
    target_columns = "date.of.admission",
    format = NULL,
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(world_named_months = c("Ybd", "dby"),
                  world_digit_months = c("dmy", "Ymd"),
                  US_formats = c("Omdy", "YOmd"))
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat[["date.of.admission"]], "Date"))
})

data <- readRDS(
  system.file("extdata", "test_linelist.RDS", package = "cleanepi")
) %>%
  linelist::make_linelist(
    date_onset = "dt_onset",
    date_reporting = "dt_report"
  )
test_that("standardize_dates works with a linelist", {
  dat <- standardize_dates(
    data = data,
    target_columns = "linelist_tags",
    format = NULL,
    timeframe = NULL,
    error_tolerance = 0.4,
    orders = list(world_named_months = c("Ybd", "dby"),
                  world_digit_months = c("dmy", "Ymd"),
                  US_formats = c("Omdy", "YOmd"))
  )
  expect_s3_class(dat, "data.frame")
  expect_true(inherits(dat, "linelist"))
  expect_true(inherits(dat[["dt_onset"]], "Date"))
  expect_true(inherits(dat[["dt_report"]], "Date"))
})

data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_that("standardize_dates fails as expected", {
  expect_error(
    standardize_dates(
      data = data,
      target_columns = c("date_first_pcr_positive_test", "date.of.admission",
                         "dateOfBirth"),
      format = c("%d/%m/%Y", "%d/%m/%Y"),
      timeframe = NULL,
      error_tolerance = 0.4,
      orders = list(world_named_months = c("Ybd", "dby"),
                    world_digit_months = c("dmy", "Ymd"),
                    US_formats = c("Omdy", "YOmd"))
    ),
    regexp = cat("Need to specify one format if all target columns have the
    same format. Provide one format per target column, otherwise.e")
  )
})

test_that("date_guess works as expected", {
  data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
  res <- date_guess(x = data[["date.of.admission"]],
                    quiet = TRUE,
                    orders = "dmY",
                    column_name = "date.of.admission")
  expect_identical(res[["res"]],
                   as.Date(c("2020-12-01", "2021-01-28", "2021-02-15",
                             "2021-02-11", "2021-02-17", "2021-02-17",
                             "2021-02-28", "2021-02-22", "2021-03-02",
                             "2021-03-05")))
  expect_null(res[["multi_format"]])
  expect_false(res[["found_ambiguous"]])
})
