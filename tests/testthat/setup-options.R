# We want to flag partial matching as part of our testing & continuous
# integration process because it makes code more brittle.
<<<<<<< HEAD
options( # nolint: undesirable_function_linter
=======
options(
>>>>>>> 65a523d (Fix partial matching warning (#22))
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE
)
