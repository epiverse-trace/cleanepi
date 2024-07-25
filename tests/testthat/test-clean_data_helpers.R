test_that("scan_data works as expected", {
  dat        <- readRDS(system.file("extdata", "messy_data.RDS",
                                    package = "cleanepi"))
  scan_result <- scan_data(data = dat)
  expect_s3_class(scan_result, "data.frame")
  expect_named(scan_result, c("Field_names", "missing", "numeric", "date",
                              "character", "logical"))
  expect_identical(ncol(scan_result), 6L)
  expect_identical(nrow(scan_result), ncol(dat))
  expect_identical(scan_result[["Field_names"]], names(dat))

  # using a dataset with many data types
  data(iris)
  iris[["fct"]]  <- as.factor(sample(c("gray", "orange"), nrow(iris),
                                    replace = TRUE))
  iris[["lgl"]]  <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
  iris[["date"]] <- as.Date(seq.Date(from = as.Date("2024-01-01"),
                                     to = as.Date("2024-08-30"),
                                     length.out = nrow(iris)))
  iris[["posit_ct"]] <- as.POSIXct(iris[["date"]])
  scan_result        <- scan_data(data = iris)
  expect_identical(ncol(scan_result), 6L)
  expect_identical(nrow(scan_result), ncol(iris))
  expect_identical(scan_result[["Field_names"]], names(iris))
  expect_identical(sum(scan_result[["numeric"]]), 4)
  expect_identical(sum(scan_result[["missing"]]), 0)
  expect_identical(sum(scan_result[["date"]]), 2)
  expect_identical(sum(scan_result[["character"]]), 2)
  expect_identical(sum(scan_result[["logical"]]), 1)
})
