# We want to flag partial matching as part of our testing & continuous
# integration process because it makes code more brittle.
options( # nolint: undesirable_function_linter
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE
)
