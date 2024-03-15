test_that("scan_data works", {
  scan_result <- scan_data(
    data = readRDS(system.file("extdata", "messy_data.RDS",
                               package = "cleanepi"))
  )
  expect_s3_class(scan_result, "data.frame")
  expect_named(scan_result, c("Field_names", "missing", "numeric", "date",
                              "character", "logical"))
  expect_identical(ncol(scan_result), 6L)
  expect_identical(nrow(scan_result), 9L)
})
