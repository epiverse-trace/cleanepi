test_that("clean_spelling_mistakes works as expected by default", {
  df <- data.frame(
    case_type = c("confirmed", "confermed", "probable", "susspected"),
    outcome = c("died", "recoverd", "did", "recovered")
  )
  clean_df <- clean_spelling_mistakes(
    data = df,
    target_columns = c("case_type", "outcome"),
    wordlist = c("confirmed", "probable", "suspected", "died", "recovered"),
    confirm = FALSE
  )
  expect_identical(
    clean_df$case_type,
    c("confirmed", "confirmed", "probable", "suspected")
  )
  expect_identical(
    clean_df$outcome,
    c("died", "recovered", "died", "recovered")
  )
})
