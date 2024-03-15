data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_that("check_subject_ids works as expected", {
  dat <- check_subject_ids(
    data           = data,
    id_column_name = "study_id",
    format         = NULL,
    prefix         = "PS",
    suffix         = "P2",
    range          = c(1L, 100L)
  )
  expect_s3_class(dat, "data.frame")
  expect_false(identical(data, dat))
  expect_false(nrow(data) == nrow(dat))
  expect_false(all(c("PS004P2-1", "P0005P2", "PB500P2") %in% dat[["study_id"]]))
})

test_that("check_subject_ids fails as expected", {
  expect_error(
    check_subject_ids(
      data = NULL,
      id_column_name = "study_id",
      format = "PS000P2",
      prefix = "PS",
      suffix = "P2",
      range = c(1L, 100L)
    ),
    regexp = cat("Assertion on',data,'failed: input data frame must be
                 provided.")
  )

  expect_error(
    check_subject_ids(
      data = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
      id_column_name = NA,
      format = "PS000P2",
      prefix = "PS",
      suffix = "P2",
      range = c(1L, 100L)
    ),
    regexp = cat("Assertion on',id_column_name,'failed: Missing value not
                 allowed for 'id_column_name'.")
  )

  expect_error(
    check_subject_ids(
      data = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
      id_column_name = c("study_id", "event_name"),
      format = "PS000P2",
      prefix = "PS",
      suffix = "P2",
      range = c(1L, 100L)
    ),
    regexp = cat("Assertion on',id_column_name,'failed: Must be a character of
                 length 1.")
  )

  expect_error(
    check_subject_ids(
      data = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
      id_column_name = "study_id",
      format = NA,
      prefix = "PS",
      suffix = "P2",
      range = c(1L, 100L)
    ),
    regexp = cat("Assertion on',format,'failed: template sample IDs format
                 must be provided.")
  )

  expect_error(
    check_subject_ids(
      data = readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi")),
      id_column_name = "study_id",
      format = c("PS000P2", "PS000P1"),
      prefix = "PS",
      suffix = "P2",
      range = c(1L, 100L)
    ),
    regexp = cat("Assertion on',format,'failed: Must be a character of length
                 1.")
  )
})

test_that("check_subject_ids sends a message when duplicated IDs are found", {
  data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
  data[["study_id"]][[10L]] <- data[["study_id"]][[1L]]
  expect_message(
    check_subject_ids(data           = data,
                      id_column_name = "study_id",
                      format         = NULL,
                      prefix         = "PS",
                      suffix         = "P2",
                      range          = c(1L, 100L)),
    "Found 2 duplicated rows. Please consult the report for more details."
  )
})

data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_that("check_subject_ids works when relying on the format argument", {
  dat <- check_subject_ids(data           = data,
                           id_column_name = "study_id",
                           format         = "PS100P2",
                           prefix         = NULL,
                           suffix         = NULL,
                           range          = NULL)
  expect_s3_class(dat, "data.frame")
  expect_false(nrow(data) == nrow(dat))
  expect_false("PS004P2-1" %in% dat[["study_id"]])

  dat <- check_subject_ids(data           = data,
                           id_column_name = "study_id",
                           format         = "PS100P2",
                           prefix         = NULL,
                           suffix         = NULL,
                           range          = c(1L, 100L))
  expect_s3_class(dat, "data.frame")
  expect_false(nrow(data) == nrow(dat))
  expect_false("PS004P2-1" %in% dat[["study_id"]])
  expect_false("PB500P2" %in% dat[["study_id"]])

  dat <- check_subject_ids(data           = data,
                           id_column_name = "study_id",
                           format         = "PS100P2",
                           prefix         = "PS",
                           suffix         = NULL,
                           range          = c(1L, 100L))
  expect_s3_class(dat, "data.frame")
  expect_false(nrow(data) == nrow(dat))
  expect_false("PS004P2-1" %in% dat[["study_id"]])
  expect_false("PB500P2" %in% dat[["study_id"]])
  expect_false("P0005P2" %in% dat[["study_id"]])
})
