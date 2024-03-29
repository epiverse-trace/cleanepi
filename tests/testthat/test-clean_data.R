
test_data <- readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi"))

test_dictionary <- readRDS(system.file("extdata", "test_dictionary.RDS",
                                       package = "cleanepi"))

# DEFINING THE CLEANING PARAMETERS
use_na                  <- list(target_columns = NULL, na_strings = "-99")
standardize_col_names   <- list(keep = NULL, rename = NULL)
remove_duplicates       <- list(target_columns   = NULL,
                                rm_empty_rows    = TRUE,
                                rm_empty_cols    = TRUE,
                                rm_constant_cols = TRUE)
standardize_date        <- list(target_columns  = NULL,
                                error_tolerance = 0.4,
                                format          = NULL,
                                timeframe       = as.Date(c("1973-05-29",
                                                            "2023-05-29")))
standardize_subject_ids <- list(target_columns = "study_id",
                                prefix         = "PS",
                                suffix         = "P2",
                                range          = c(1L, 100L),
                                nchar          = 7L)
params <- list(
  standardize_column_names = standardize_col_names,
  remove_constant         = list(cutoff = 1.0),
  replace_missing_values   = use_na,
  remove_duplicates        = remove_duplicates,
  standardize_date         = standardize_date,
  standardize_subject_ids  = standardize_subject_ids,
  to_numeric               = "sex",
  dictionary               = test_dictionary
)

test_that("clean_data works as expected", {
  clean_data <- clean_data(
    data   = test_data,
    params = params
  )
  expect_s3_class(clean_data, "data.frame")
  expect_identical(nrow(clean_data), 10L)
  expect_identical(ncol(clean_data), 5L)
  expect_false("-99" %in% as.vector(as.matrix(clean_data)))
})

test_that("clean_data works in a pipable way", {
  clean_data <- test_data |>
    standardize_column_names(keep = NULL, rename = NULL) |>
    replace_missing_values(target_columns = NULL, na_strings = "-99") |>
    remove_constant(cutoff = 1.0) |>
    remove_duplicates(target_columns = NULL) |>
    standardize_dates(target_columns  = NULL,
                      error_tolerance = 0.4,
                      format          = NULL,
                      timeframe     = as.Date(c("1973-05-29", "2023-05-29"))) |>
    check_subject_ids(target_columns = "study_id",
                      prefix         = "PS",
                      suffix         = "P2",
                      range          = c(1L, 100L),
                      nchar          = 7L) |>
    convert_to_numeric(target_columns = "sex") |>
    clean_using_dictionary(dictionary = test_dictionary)

  expect_s3_class(clean_data, "data.frame")
  expect_identical(nrow(clean_data), 10L)
  expect_identical(ncol(clean_data), 5L)
  expect_false("-99" %in% as.vector(as.matrix(clean_data)))
})
