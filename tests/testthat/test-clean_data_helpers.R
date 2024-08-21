test_that("scan_data works as expected", {
  # using a dataset with character columns only
  dat <- readRDS(system.file("extdata", "messy_data.RDS", package = "cleanepi"))
  scan_result <- scan_data(data = dat)
  expect_s3_class(scan_result, "data.frame")
  expect_named(scan_result, c("Field_names", "missing", "numeric", "date",
                              "character", "logical"))
  expect_identical(ncol(scan_result), 6L)
  expect_identical(nrow(scan_result), ncol(dat))
  expect_identical(scan_result[["Field_names"]], names(dat))

  # using a dataset with no character column
  data(iris)
  iris[["fct"]]  <- as.factor(sample(c("gray", "orange"), nrow(iris),
                                    replace = TRUE))
  iris[["lgl"]]  <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
  iris[["date"]] <- as.Date(seq.Date(from = as.Date("2024-01-01"),
                                     to = as.Date("2024-08-30"),
                                     length.out = nrow(iris)))
  iris[["posit_ct"]] <- as.POSIXct(iris[["date"]])
  scan_result        <- scan_data(data = iris)
  expect_identical(scan_result, NA)
  expect_message(scan_data(data = iris),
                 "No character column found in the provided data.")

  # using a data with some character columns
  dat <- readRDS(system.file("extdata", "test_linelist.RDS",
                             package = "cleanepi"))
  scan_result <- scan_data(data = dat)
  expect_identical(ncol(scan_result), 6L)
  expect_identical(nrow(scan_result), 2L)
  expect_false(nrow(scan_result) == ncol(dat))
  expect_identical(scan_result[["Field_names"]], c("id", "age_class"))

  # use data where output is easily predictable. the data contains:
  # 1 character value
  # 1 date value
  # 1 numeric value which also corresponds to the date value above, hence the
  # warning about the presence of ambiguous data
  dat <- data.frame(col1 = c("20210702", "test"))
  scan_result <- scan_data(data = dat)
  expect_identical(as.numeric(scan_result[1L, -1L]), c(0, 0.5, 0.5, 0.5, 0))

  # use data where output is easily predictable. the data contains:
  # 1 character value
  # 2 date values (one of them `20210702` is also numeric)
  # 2 numeric values in which one is also a date value, hence the
  # warning about the presence of ambiguous data
  dat <- data.frame(col1 = c(c("20210702", "2021/07/03", "3"), "test"))
  scan_result <- scan_data(data = dat)
  expect_identical(as.numeric(scan_result[1L, -1L]), c(0, 0.5, 0.5, 0.25, 0))
})
