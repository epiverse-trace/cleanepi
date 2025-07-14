test_that("correct_misspelled_values works as expected by default", {
  df <- data.frame(
    case_type = c("confirmed", "confermed", "probable", "susspected"),
    outcome = c("died", "recoverd", "did", "recovered")
  )
  clean_df <- correct_misspelled_values(
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

test_that("correct_misspelled_values warns with multiple options", {
  df <- data.frame(
    case_type = c("confirmed", "confermed", "probable", "susspected"),
    outcome = c("died", "recoverd", "did", "recovered"),
    sex = c("male", "male", "mane", "female")
  )
  expect_warning(
    clean_df <- correct_misspelled_values(
      data = df,
      target_columns = c("case_type", "outcome", "sex"),
      wordlist = c(
        "confirmed", "probable", "suspected", "died", "recovered", "male", "make"
      ),
      confirm = FALSE
    )
  )
  expect_identical(
    clean_df$case_type,
    c("confirmed", "confirmed", "probable", "suspected")
  )
  expect_identical(
    clean_df$outcome,
    c("died", "recovered", "died", "recovered")
  )
  expect_identical(
    clean_df$sex,
    c("male", "male", "male", "female")
  )
})
