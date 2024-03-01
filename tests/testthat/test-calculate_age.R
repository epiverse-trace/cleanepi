data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

test_that("calculate_age works when age is calculated in months and
          age_remainder_unit is days", {
            age <- calculate_age(
              data               = data,
              target_column      = "dateOfBirth",
              end_date           = Sys.Date(),
              age_in             = "months",
              age_remainder_unit = "days"
            )
            expect_s3_class(age, "data.frame")
            expect_identical(ncol(age), 10L)
            expect_identical(names(age)[9L:10L], c("age_in_months",
                                                   "remainder_days"))
})

test_that("calculate_age works when age is calculated in months and
          age_remainder_unit is weeks", {
            age <- calculate_age(
              data               = data,
              target_column      = "dateOfBirth",
              end_date           = Sys.Date(),
              age_in             = "months",
              age_remainder_unit = "weeks"
            )
            expect_s3_class(age, "data.frame")
            expect_identical(ncol(age), 10L)
            expect_identical(names(age)[9L:10L], c("age_in_months",
                                                   "remainder_weeks"))
})

test_that("calculate_age works when age is calculated in years and
          age_remainder_unit is months", {
            age <- calculate_age(
              data               = data,
              target_column      = "dateOfBirth",
              end_date           = Sys.Date(),
              age_in             = "years",
              age_remainder_unit = "months"
            )
            expect_s3_class(age, "data.frame")
            expect_identical(ncol(age), 10L)
            expect_identical(names(age)[9L:10L], c("age_in_years",
                                                   "remainder_months"))
})

test_that("calculate_age works when age is calculated in years and
          age_remainder_unit is weeks", {
            age <- calculate_age(
              data               = data,
              target_column      = "dateOfBirth",
              end_date           = Sys.Date(),
              age_in             = "years",
              age_remainder_unit = "weeks"
            )
            expect_s3_class(age, "data.frame")
            expect_identical(ncol(age), 10L)
            expect_identical(names(age)[9L:10L], c("age_in_years",
                                                   "remainder_weeks"))
})

test_that("calculate_age works when age is calculated in years and
          age_remainder_unit is days", {
            age <- calculate_age(
              data               = data,
              target_column      = "dateOfBirth",
              end_date           = Sys.Date(),
              age_in             = "years",
              age_remainder_unit = "days"
            )
            expect_s3_class(age, "data.frame")
            expect_identical(ncol(age), 10L)
            expect_identical(names(age)[9L:10L], c("age_in_years",
                                                   "remainder_days"))
})

test_that("calculate_age works when age is calculated in days", {
  age <- calculate_age(
    data               = data,
    target_column      = "dateOfBirth",
    end_date           = Sys.Date(),
    age_in             = "days"
  )
  expect_s3_class(age, "data.frame")
  expect_identical(ncol(age), 9L)
  expect_identical(names(age)[9L], "age_in_days")
})
