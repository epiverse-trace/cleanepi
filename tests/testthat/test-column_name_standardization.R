test_that("standardize_column_names works with rename argument", {
  cleaned_data <- standardize_column_names(
    data   = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
    rename = c(DOB = "dateOfBirth", gender = "sex")
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data, expected = c("study_id", "event_name",
                                          "country_code", "country_name",
                                          "date_of_admission", "DOB",
                                          "date_first_pcr_positive_test",
                                          "gender"))
})
test_that("standardize_column_names fails when rename argument contains existing column names", {
  expect_error(
    standardize_column_names(
      data   = readRDS(system.file("extdata", "test_df.RDS",
                                   package = "cleanepi")),
      rename = c(DOB = "dateOfBirth", dateOfBirth = "sex")
    ),
    regexp = cat("Replace column names already exists")
  )
})


test_that("standardize_column_names works with keep argument", {
  # TODO update this after Banky's suggestion as 'dateOfBirth' might change
  cleaned_data <- standardize_column_names(
    data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
    keep = as.character("date.of.admission")
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data,
               expected = c("study_id", "event_name", "country_code",
                            "country_name", "date.of.admission",
                            "date_of_birth", "date_first_pcr_positive_test",
                            "sex"))

  cleaned_data <- standardize_column_names(
    data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
    keep = c("date.of.admission", "dateOfBirth")
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data,
               expected = c("study_id", "event_name", "country_code",
                            "country_name", "date.of.admission",
                            "dateOfBirth", "date_first_pcr_positive_test",
                            "sex"))
})

test_that("standardize_column_names works with all argument", {
  cleaned_data <- standardize_column_names(
    data  = readRDS(system.file("extdata", "test_df.RDS",package = "cleanepi")),
    rename = c(DOB = "dateOfBirth", gender = "sex"),
    keep   = "date.of.admission"
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data, expected = c(
    "study_id", "event_name",
    "country_code", "country_name",
    "date.of.admission", "DOB",
    "date_first_pcr_positive_test",
    "gender"
  ))
})

test_that("standardize_column_names fails when 'linelist_tags' is provided when
          dealing with a data frame", {
  expect_error(
    standardize_column_names(
      data   = readRDS(system.file("extdata", "test_df.RDS",
                                   package = "cleanepi")),
      rename = c(DOB = "dateOfBirth", gender = "sex"),
      keep   = "linelist_tags"
    ),
    regexp = cat("Assertion on',keep,'failed: usage of 'linelist_tags'
                            is only reserved for 'linelist' type of data.")
  )
})

test_that("standardize_column_names fails when wrong column names are
          specified", {
  expect_error(
    standardize_column_names(
      data   = readRDS(system.file("extdata", "test_df.RDS",
                                   package = "cleanepi")),
      rename = c(DOB = "dateOfBirth", gender = "fake_name"),
      keep   = NULL
    ),
    regexp = cat("Assertion on',keep or rename,'failed: Only the
                           column names from the input data can be renamed or
                           kept.")
  )
})

test_that("standardize_column_names works as expected", {
  dat <- tibble::tibble(
    "...1" = -1,
    "1" = 0,
    "x1" = 0.5,
    "x 1" = 1,
    "x_1" = 2,
    "x  1" = 3,
  )
  test <- dat %>%
    cleanepi::standardize_column_names()

  cleaned_data <- dat %>%
    cleanepi::standardize_column_names(keep = "x_1")

  # without 'keep = "x_1"', this column would have been renamed as "x_1_2"
  # this demonstrate the use of 'make_unique_column_names' to preserve columns
  # in 'keep' and 'rename' in case duplicated names are generated during the
  # process.
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(
    cleaned_data,
    expected = c("x1", "x1_2", "x1_3", "x_1_1", "x_1", "x_1_2")
  )
  expect_false(identical(names(test), names(cleaned_data)))
})
