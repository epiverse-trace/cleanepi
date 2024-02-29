test_that("standardize_column_names works with rename argument", {
  cleaned_data <- standardize_column_names(
    data   = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
    rename = c("dateOfBirth = DOB, sex=gender")
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data, expected = c("study_id", "event_name",
                                          "country_code", "country_name",
                                          "date.of.admission", "DOB",
                                          "date_first_pcr_positive_test",
                                          "gender"))
})

test_that("standardize_column_names works with keep argument", {
  # TODO update this after Banky's implementation as 'dateOfBirth' might change
  cleaned_data <- standardize_column_names(
    data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
    keep = "date.of.admission"
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data, expected = c("study_id", "event_name",
                                          "country_code", "country_name",
                                          "date.of.admission", "dateOfBirth",
                                          "date_first_pcr_positive_test",
                                          "sex"))
})

test_that("standardize_column_names works with all argument", {
  cleaned_data <- standardize_column_names(
    data   = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
    rename = c("dateOfBirth = DOB, sex=gender"),
    keep   = "date.of.admission"
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_named(cleaned_data, expected = c("study_id", "event_name",
                                          "country_code", "country_name",
                                          "date.of.admission", "DOB",
                                          "date_first_pcr_positive_test",
                                          "gender"))
})

test_that("standardize_column_names fails when 'linelist_tags' is provided when
          dealing with a data frame", {
            expect_error(
              standardize_column_names(
                data   = readRDS(system.file("extdata", "test_df.RDS",
                                             package = "cleanepi")),
                rename = c("dateOfBirth = DOB, sex=gender"),
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
                rename = c("date of birth = DOB, sex=gender"),
                keep   = NULL
              ),
              regexp = cat("Assertion on',keep or rename,'failed: Only the
                           column names from the input data can be renamed or
                           kept.")
            )
          })
