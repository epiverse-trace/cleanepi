# test_that("check_subject_ids works as expected", {
#   dat <- check_subject_ids(
#     data           = readRDS(system.file("extdata", "test_df.RDS",
#                                          package = "cleanepi")),
#     id_column_name = "study_id",
#     format         = NULL,
#     prefix         = "PS",
#     suffix         = "P2",
#     range          = c(1L, 100L)
#   )
#   expect_type(dat, "data.frame")
# })
#
# test_that("check_subject_ids fails as expected", {
#   expect_error(
#     check_subject_ids(
#       data = NULL,
#       id_column_name = "study_id",
#       format = "PS000P2",
#       prefix = "PS",
#       suffix = "P2",
#       range = c(1L, 100L),
#       remove = FALSE,
#       verbose = TRUE,
#       report = list()
#     ),
#     regexp = cat("Assertion on',data,'failed: input data frame must be
#                  provided.")
#   )
#
#   expect_error(
#     check_subject_ids(
#       data = readRDS(system.file("extdata", "test_df.RDS",
#                                  package = "cleanepi")),
#       id_column_name = NA,
#       format = "PS000P2",
#       prefix = "PS",
#       suffix = "P2",
#       range = c(1L, 100L),
#       remove = FALSE,
#       verbose = TRUE,
#       report = list()
#     ),
#     regexp = cat("Assertion on',id_column_name,'failed: Missing value not
#                  allowed for 'id_column_name'.")
#   )
#
#   expect_error(
#     check_subject_ids(
#       data = readRDS(system.file("extdata", "test_df.RDS",
#                                  package = "cleanepi")),
#       id_column_name = c("study_id", "event_name"),
#       format = "PS000P2",
#       prefix = "PS",
#       suffix = "P2",
#       range = c(1L, 100L),
#       remove = FALSE,
#       verbose = TRUE,
#       report = list()
#     ),
#     regexp = cat("Assertion on',id_column_name,'failed: Must be a character of
#                  length 1.")
#   )
#
#   expect_error(
#     check_subject_ids(
#       data = readRDS(system.file("extdata", "test_df.RDS",
#                                  package = "cleanepi")),
#       id_column_name = "study_id",
#       format = NA,
#       prefix = "PS",
#       suffix = "P2",
#       range = c(1L, 100L),
#       remove = FALSE,
#       verbose = TRUE,
#       report = list()
#     ),
#     regexp = cat("Assertion on',format,'failed: template sample IDs format
#                  must be provided.")
#   )
#
#   expect_error(
#     check_subject_ids(
#       data = readRDS(system.file("extdata", "test_df.RDS",
#                                  package = "cleanepi")),
#       id_column_name = "study_id",
#       format = c("PS000P2", "PS000P1"),
#       prefix = "PS",
#       suffix = "P2",
#       range = c(1L, 100L),
#       remove = FALSE,
#       verbose = TRUE,
#       report = list()
#     ),
#     regexp = cat("Assertion on',format,'failed: Must be a character of length
#                  1.")
#   )
# })
