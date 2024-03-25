data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

# introduce an empty column
data[["empty_column"]] <- NA

test_that("remove_constant works", {
  dat <- remove_constant(
    data   = data,
    cutoff = 1
  )
  expect_s3_class(dat, class = "data.frame")
  expect_identical(ncol(dat), 5L)
  expect_false(
    all(c("empty_column", "event_name", "country_code", "country_name")) %in%
      colnames(dat))

  report <- attr(dat, "report")
  expect_type(report, "list")
  expect_named(report, c("empty_columns", "constant_columns"))
  expect_identical(report[["empty_columns"]], "empty_column")
  expect_identical(report[["constant_columns"]],
                   "event_name, country_code, country_name")
})
