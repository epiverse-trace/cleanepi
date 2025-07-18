data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

# introduce an empty column
data[["empty_column"]] <- NA

# introduce an empty row
data[7L, ] <- NA

test_that("remove_constants works", {
  dat <- remove_constants(
    data = data,
    cutoff = 1
  )
  expect_s3_class(dat, class = "data.frame")
  expect_identical(ncol(dat), 5L)
  expect_false(nrow(data) == nrow(dat))
  expect_false(ncol(data) == ncol(dat))
  expect_false(
    any(
      c("empty_column", "event_name", "country_code", "country_name") %in%
          colnames(dat)
    )
  )

  constant_data <- print_report(dat, "constant_data")
  expect_true(nrow(constant_data) == 1)
  expect_true(ncol(constant_data) == 4)
  expect_identical(
    names(constant_data),
    c("iteration", "empty_columns", "empty_rows", "constant_columns")
  )
  expect_identical(constant_data[["empty_columns"]], "empty_column")
  expect_identical(
    constant_data[["constant_columns"]],
    "event_name, country_code, country_name"
  )
  expect_identical(constant_data[["empty_rows"]], "7")
  expect_identical(constant_data[["iteration"]], 1L)
})


df <- tibble::tibble(
  x = c(1,2),
  y = c(1,3)
) %>%
  dplyr::mutate(invariant = rep("a",nrow(.))) %>%
  dplyr::mutate(invariant2 = rep("b",nrow(.))) %>%
  dplyr::mutate(empty1 = rep(lubridate::NA_Date_, nrow(.))) %>%
  dplyr::mutate(empty2 = rep(lubridate::NA_Date_, nrow(.))) %>%
  dplyr::mutate(empty3 = rep(lubridate::NA_Date_, nrow(.))) %>%
  dplyr::add_row(x = NA_integer_, invariant = "a") %>%
  dplyr::add_row(x = NA_integer_, invariant = "a") %>%
  dplyr::add_row(x = NA_integer_, invariant = "a") %>%
  dplyr::add_row(x = NA_integer_)

test_that("remove_constants works", {
  dat <- df %>%
  cleanepi::remove_constants()

  expect_s3_class(dat, class = "data.frame")
  expect_identical(ncol(dat), 2L)
  expect_true(nrow(dat) == 2L)
  tmp_dat <- data.frame(dat)
  expect_identical(
    tmp_dat,
    data.frame(
      x = as.double(1:2),
      y = as.double(c(1, 3))
    )
  )

  constant_data <- print_report(dat, "constant_data")
  expect_true(nrow(constant_data) == 2)
  expect_true(ncol(constant_data) == 4)
  expect_identical(
    names(constant_data),
    c("iteration", "empty_columns", "empty_rows", "constant_columns")
  )
  expect_identical(
    constant_data[["empty_columns"]],
    c("empty1, empty2, empty3", NA_character_)
  )
  expect_identical(
    constant_data[["constant_columns"]],
    c("invariant, invariant2", NA_character_)
  )
  expect_identical(constant_data[["empty_rows"]], c("6", "3, 4, 5"))
  expect_identical(constant_data[["iteration"]], c(1L, 2L))
})


test_that("remove_constants works as expected", {
  dat <- df %>%
    cleanepi::remove_constants(cutoff = 0.5)

  expect_s3_class(dat, class = "data.frame")
  expect_identical(ncol(dat), 2L)
  expect_true(nrow(dat) == 2L)
  tmp_dat <- data.frame(dat)
  expect_identical(
    tmp_dat,
    data.frame(
      x = as.double(1:2),
      y = as.double(c(1, 3))
    )
  )

  constant_data <- print_report(dat, "constant_data")
  expect_true(nrow(constant_data) == 1)
  expect_true(ncol(constant_data) == 4)
  expect_identical(
    names(constant_data),
    c("iteration", "empty_columns", "empty_rows", "constant_columns")
  )
  expect_identical(constant_data[["empty_columns"]], "empty1, empty2, empty3")
  expect_identical(constant_data[["constant_columns"]], "invariant, invariant2")
  expect_identical(constant_data[["empty_rows"]], "3, 4, 5, 6")
  expect_identical(constant_data[["iteration"]], 1L)
})

test_that("remove_constants returns the expected message", {
  expect_message(
    remove_constants(df),
    regexp = cat("Constant data was removed after 2 iterations.")
  )
})
