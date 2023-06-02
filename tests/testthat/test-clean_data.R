
params <- list(
  remove_duplicates = TRUE,
  duplicates_from = NULL,
  replace_missing = TRUE,
  na_comes_as = "-99",
  check_timeframe = TRUE,
  timeframe = as.Date(c("1973-05-29", "2023-05-29")),
  error_tolerance = 0.5,
  subject_id_col_name = "study_id",
  subject_id_format = "PS000P2",
  prefix = "PS",
  suffix = "P2",
  range = c(1, 100)
)

test_that("cleanepi works as expected", {
  clean_data <- clean_data(
    data = data.table::fread(system.file("extdata", "test.txt",
                                         package = "cleanepi")),
    params <- params
  )
  expect_type(clean_data, "list")
})
