test_that("check_subject_ids works as expected", {
  expect_output(
    check_subject_ids(
      data=data.table::fread(system.file("extdata","test.txt", package = "cleanepi")),
      id.position=1,
      format="PS000P2",
      check=TRUE,
      prefix="PS",
      suffix="P2",
      range=c(0,100)
    ),
    ""
  )
})

test_that("check_subject_ids fails as expected", {
  expect_error(
    check_subject_ids(
      data=data.table::fread(system.file("extdata","test.txt", package = "cleanepi")),
      id.position=-1,
      format="PS000P2",
      check=TRUE,
      prefix="PS",
      suffix="P2",
      range=c(0,100)
    ),
    regexp = cat("Assertion on',id.position,'failed: negative column number not allowed.")
  )

  expect_error(
    check_subject_ids(
      data=NULL,
      id.position=1,
      format="PS000P2",
      check=TRUE,
      prefix="PS",
      suffix="P2",
      range=c(0,100)
    ),
    regexp = cat("Assertion on',data,'failed: input data frame must be provided.")
  )

  expect_error(
    check_subject_ids(
      data=data.table::fread(system.file("extdata","test.txt", package = "cleanepi")),
      id.position=1,
      format="PS000P2",
      check="TRUE",
      prefix="PS",
      suffix="P2",
      range=c(1,100)
    ),
    regexp = cat("Assertion on',check,'failed: Must be a logical.")
  )
})
