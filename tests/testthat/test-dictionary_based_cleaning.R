test_dictionary <- readRDS(
  system.file("extdata", "test_dict.RDS", package = "cleanepi")
)
test_that("add_to_dictionary works when adding only one element", {
  test <- add_to_dictionary(
    dictionary = test_dictionary,
    option = "ml",
    value = "male",
    grp = "gender",
    order = NULL
  )
  expect_s3_class(test, "data.frame")
  expect_false(identical(test_dictionary, test))
  expect_identical(nrow(test), 7L)
  expect_true("ml" %in% test[["options"]])
})

test_that("add_to_dictionary works when adding multiple elements", {
  test <- add_to_dictionary(
    dictionary = test_dictionary,
    option = c("homme", "femme"),
    value = c("male", "female"),
    grp = "gender",
    order = NULL
  )
  expect_s3_class(test, "data.frame")
  expect_false(identical(test_dictionary, test))
  expect_identical(nrow(test), 8L)
  expect_true(all(c("homme", "femme") %in% test[["options"]]))

  test <- add_to_dictionary(
    dictionary = test_dictionary,
    option = "homme",
    value = "male",
    grp = "gender",
    order = NULL
  )
  expect_s3_class(test, "data.frame")
  expect_false(identical(test_dictionary, test))
  expect_identical(nrow(test), 7L)
  expect_true("homme" %in% test[["options"]])
})

test_that("add_to_dictionary works when order is not NULL", {
  test <- add_to_dictionary(
    dictionary = test_dictionary,
    option = c("homme", "femme"),
    value = c("male", "female"),
    grp = "gender",
    order = 7L:8L
  )
  expect_s3_class(test, "data.frame")
  expect_false(identical(test_dictionary, test))
  expect_identical(nrow(test), 8L)
  expect_true(all(c("homme", "femme") %in% test[["options"]]))
})

data <- readRDS(system.file("extdata", "messy_data.RDS", package = "cleanepi"))
# introduce some noise
data[["gender"]][sample(1L:nrow(data), 10L, replace = FALSE)] <- "homme"

# update the data dictionary
test_dictionary <- add_to_dictionary(
  dictionary = test_dictionary,
  option = "homme",
  value = "male",
  grp = "gender",
  order = NULL
)
test_that("clean_using_dictionary works", {
  cleaned_df <- clean_using_dictionary(
    data = data,
    dictionary = test_dictionary
  )
  expect_s3_class(cleaned_df, "data.frame")
  expect_identical(ncol(data), ncol(cleaned_df))
  expect_false("homme" %in% cleaned_df[["gender"]])
})

# introduce a new option
data[["gender"]][2L] <- "femme"
test_that("clean_using_dictionary works with misspelled values", {
  cleaned_df <- clean_using_dictionary(
    data = data,
    dictionary = test_dictionary
  )
  expect_s3_class(cleaned_df, "data.frame")
  expect_identical(ncol(data), ncol(cleaned_df))
  expect_false("homme" %in% cleaned_df[["gender"]])
  expect_true("femme" %in% cleaned_df[["gender"]])
  expect_message(
    clean_using_dictionary(
      data = data,
      dictionary = test_dictionary
    ),
    regexp = cat("Can not replace the following values found in column ",
                 "`gender` but not defined in the dictionary: `femme`.")
  )
})

test_that("construct_misspelled_report works", {
  res <- construct_misspelled_report(
    misspelled_options = list(gender = 2L),
    data
  )
  expect_s3_class(res, "data.frame")
  expect_identical(ncol(res), 3L)
  expect_identical(nrow(res), 1L)
  expect_identical(res[["idx"]], 2L)
  expect_identical(res[["column"]], "gender")
  expect_identical(res[["value"]], "femme")
})
