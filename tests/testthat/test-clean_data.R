
test_data <- readRDS(
  system.file("extdata", "test_df.RDS", package = "cleanepi")
)

test_dictionary <- readRDS(
  system.file("extdata", "test_dictionary.RDS", package = "cleanepi")
)

test_that("clean_data works as expected with the default parameters", {
  cleaned_data <- clean_data(
    data = test_data
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_identical(nrow(cleaned_data), 10L)
  expect_identical(ncol(cleaned_data), 5L)
  expect_identical(
    names(cleaned_data),
    c("study_id", "date_of_admission", "date_of_birth",
      "date_first_pcr_positive_test", "sex")
  )
})

# DEFINING THE CLEANING PARAMETERS
replace_missing_values <- list(target_columns = NULL, na_strings = "-99")
standardize_column_names <- list(keep = NULL, rename = NULL)
remove_duplicates <- list(target_columns   = NULL)
standardize_dates <- list(
  target_columns = NULL,
  error_tolerance = 0.4,
  format = NULL,
  timeframe = as.Date(c("1973-05-29", "2023-05-29")),
  orders = list(
    named_months = c("Ybd", "dby"),
    digit_months = c("dmy", "Ymd"),
    US_formats = c("Omdy", "YOmd")
  )
)
standardize_subject_ids <- list(
  target_columns = "study_id",
  prefix = "PS",
  suffix = "P2",
  range = c(1L, 100L),
  nchar = 7L
)
to_numeric <- list(target_columns = "sex", lang = "en")
remove_constants <- list(cutoff = 1.0)

check_date_sequence <- list(
  target_columns = c("date_first_pcr_positive_test", "date.of.admission")
)

test_that("clean_data works as expected", {
  cleaned_data <- clean_data(
    data = test_data,
    standardize_column_names = standardize_column_names,
    remove_constants = list(cutoff = 1.0),
    replace_missing_values = replace_missing_values,
    remove_duplicates = remove_duplicates,
    standardize_dates = standardize_dates,
    standardize_subject_ids = standardize_subject_ids,
    to_numeric = to_numeric,
    dictionary = test_dictionary,
    check_date_sequence = check_date_sequence
  )
  expect_s3_class(cleaned_data, "data.frame")
  expect_identical(nrow(cleaned_data), 10L)
  expect_identical(ncol(cleaned_data), 5L)
  expect_false("-99" %in% as.vector(as.matrix(cleaned_data)))
})

test_that("cleaned_data works in a pipable way", {
  cleaned_data <- test_data %>%
    standardize_column_names(keep = NULL, rename = NULL) %>%
    replace_missing_values(target_columns = NULL, na_strings = "-99") %>%
    remove_constants(cutoff = 1.0) %>%
    remove_duplicates(target_columns = NULL) %>%
    standardize_dates(
      target_columns = NULL,
      error_tolerance = 0.4,
      format = NULL,
      timeframe = as.Date(c("1973-05-29", "2023-05-29"))
    ) %>%
    check_subject_ids(
      target_columns = "study_id",
      prefix = "PS",
      suffix = "P2",
      range = c(1L, 100L),
      nchar = 7L
    ) %>%
    convert_to_numeric(target_columns = "sex", lang = "en") %>%
    clean_using_dictionary(dictionary = test_dictionary)

  expect_s3_class(cleaned_data, "data.frame")
  expect_identical(nrow(cleaned_data), 10L)
  expect_identical(ncol(cleaned_data), 5L)
  expect_false("-99" %in% as.vector(as.matrix(cleaned_data)))
})

test_that("cleaned_data works in a pipable way even when old column names are
          used", {
            cleaned_data <- test_data %>%
              standardize_column_names(
                keep = NULL,
                rename = c(DOB = "dateOfBirth")
              ) %>%
              standardize_dates(
                target_columns = c("dateOfBirth", "date_of_admission")
              )

            expect_s3_class(cleaned_data, "data.frame")
            expect_identical(nrow(cleaned_data), 10L)
            expect_identical(class(cleaned_data[["DOB"]]), "Date")
            expect_identical(class(cleaned_data[["date_of_admission"]]), "Date")
})

test_that("clean_data fails as expected", {
  standardize_subject_ids[["target_columns"]] <- NULL
  expect_error(
    clean_data(
      data = test_data,
      standardize_subject_ids = standardize_subject_ids
    ),
    regexp = cat("'target_columns' must be provided.")
  )

  expect_error(
    test_data %>%
      standardize_column_names(
        keep = NULL,
        rename = "dateOfBirth = DOB"
      ) %>%
      standardize_dates(
        target_columns = c("dateOfBirth", "fake_column_name",
                           "date_of_admission")
      )

  )
})

