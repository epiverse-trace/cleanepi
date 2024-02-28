# test_that("clean_col_names() keeps names passed in `keep` as-is", {
#   x <- readRDS(
#     system.file("extdata", "test_df.RDS", package = "cleanepi")
#   )
#   keep <- "date.of.admission"
#   out <- clean_col_names(x, keep = keep)
#
#   expect_true(
#     out[["report"]][["modified_column_names"]][["new_name"]]
#     %in% colnames(out[["data"]])
#   )
#   expect_true(
#     keep %in% colnames(out[["data"]])
#   )
# })
