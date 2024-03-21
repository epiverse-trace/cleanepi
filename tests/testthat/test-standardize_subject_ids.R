data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

test_that("check_subject_ids works as expected when remove = FALSE", {
  dat <- check_subject_ids(
    data           = data,
    target_columns = "study_id",
    prefix         = "PS",
    suffix         = "P2",
    range          = c(1L, 100L),
    length         = NULL
  )
  expect_s3_class(dat, "data.frame")
  expect_false(identical(data, dat))
  expect_false(nrow(data) == nrow(dat))
  expect_false(all(c("PS004P2-1", "P0005P2", "PB500P2") %in% dat[["study_id"]]))
})

test_that("check_subject_ids fails as expected", {
  expect_error(
    check_subject_ids(
      data           = NULL,
      target_columns = "study_id",
      prefix         = "PS",
      suffix         = "P2",
      range          = c(1L, 100L),
      length         = NULL
    ),
    regexp = cat("Assertion on',data,'failed: input data frame must be
                 provided.")
  )

  expect_error(
    check_subject_ids(
      data           = readRDS(system.file("extdata", "test_df.RDS",
                                           package = "cleanepi")),
      target_columns = NA,
      prefix         = "PS",
      suffix         = "P2",
      range          = c(1L, 100L),
      length         = NULL
    ),
    regexp = cat("Assertion on',id_column_name,'failed: Missing value not
                 allowed for 'id_column_name'.")
  )

  expect_error(
    check_subject_ids(
      data           = readRDS(system.file("extdata", "test_df.RDS",
                                           package = "cleanepi")),
      target_columns = c("study_id", "event_name"),
      prefix         = "PS",
      suffix         = "P2",
      range          = c(1L, 100L),
      length         = NULL
    ),
    regexp = cat("Assertion on',id_column_name,'failed: Must be a character of
                 length 1.")
  )

  expect_error(
    check_subject_ids(
      data           = readRDS(system.file("extdata", "test_df.RDS",
                                           package = "cleanepi")),
      target_columns = "study_id",
      prefix         = "PS",
      suffix         = "P2",
      range          = c(1L, 100L),
      length         = NA
    ),
    regexp = cat("Assertion on',length,'failed: template sample IDs format
                 must be provided.")
  )
})

test_that("check_subject_ids sends a message when duplicated IDs are found", {
  data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
  data[["study_id"]][[10L]] <- data[["study_id"]][[1L]]
  expect_message(
    check_subject_ids(data           = data,
                      target_columns = "study_id",
                      prefix         = "PS",
                      suffix         = "P2",
                      range          = c(1L, 100L),
                      length         = NULL),
    "Found 2 duplicated rows. Please consult the report for more details."
  )
})

data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_that("check_subject_ids works when relying on the length argument", {
  dat <- check_subject_ids(data           = data,
                           target_columns = "study_id",
                           prefix         = NULL,
                           suffix         = NULL,
                           range          = NULL,
                           remove         = TRUE)
                           length         = 7L)
  expect_s3_class(dat, "data.frame")
  expect_false(nrow(data) == nrow(dat))
  expect_false("PS004P2-1" %in% dat[["study_id"]])

  dat <- check_subject_ids(data           = data,
                           target_columns = "study_id",
                           prefix         = NULL,
                           suffix         = NULL,
                           range          = c(1L, 100L),
                           length         = 7L)
  expect_s3_class(dat, "data.frame")
  expect_false(nrow(data) == nrow(dat))
  expect_false("PS004P2-1" %in% dat[["study_id"]])
  expect_false("PB500P2" %in% dat[["study_id"]])

  dat <- check_subject_ids(data           = data,
                           target_columns = "study_id",
                           prefix         = "PS",
                           suffix         = NULL,
                           range          = c(1L, 100L),
                           length         = 7L)
  expect_s3_class(dat, "data.frame")
  expect_false(nrow(data) == nrow(dat))
  expect_false("PS004P2-1" %in% dat[["study_id"]])
  expect_false("PB500P2" %in% dat[["study_id"]])
  expect_false("P0005P2" %in% dat[["study_id"]])
})

# testing correct_subject_ids()
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

test_that("correct_subject_ids works as expected", {
  # detect the incorrect subject ids
  dat <- check_subject_ids(
    data           = data,
    id_column_name = "study_id",
    format         = NULL,
    prefix         = "PS",
    suffix         = "P2",
    range          = c(1L, 100L),
    remove         = FALSE
  )
  report <- attr(dat, "report")

  # generate the correction table
  correction_table <- data.frame(
    from = c("P0005P2", "PB500P2", "PS004P2-1"),
    to   = c("PB005P2", "PB050P2", "PS004P2")
  )

  # perform the correction
  dat <- correct_subject_ids(
    data             = dat,
    id_column_name   = "study_id",
    correction_table = correction_table
  )
  expect_s3_class(dat, "data.frame")
  expect_false(identical(data, dat))
  expect_true(nrow(data) == nrow(dat))
  expect_false(all(c("PS004P2-1", "P0005P2", "PB500P2") %in% dat[["study_id"]]))
  expect_true(all(c("PS004P2", "PB005P2", "PB050P2") %in% dat[["study_id"]]))
  expect_true("incorrect_subject_id" %in% names(report))
  expect_true(all(c("PS004P2-1", "P0005P2", "PB500P2") %in%
                    report[["incorrect_subject_id"]][["ids"]]))
  expect_identical(ncol(report[["incorrect_subject_id"]]), 2L)
  expect_identical(nrow(report[["incorrect_subject_id"]]), 3L)
})

# generate the correction table
correction_table <- data.frame(
  from = c("P0005P2", "PB500P2", "PS004P2-1", "Karim"),
  to   = c("PB005P2", "PB050P2", "PS004P2", "PB075P2")
)
test_that("correct_subject_ids fails as expected", {
  expect_error(
    correct_subject_ids(
      data             = data,
      id_column_name   = "study_id",
      correction_table = correction_table
    ),
    regexp = cat("All subject ids in the correction table should be part of the
                 subject ids column of the input data.")
  )

})
