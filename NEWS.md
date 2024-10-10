# cleanepi 1.0.2.9000

## New minor features

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

* Fixed a bug in the index for empty rows returned by `print_report()` after `remove_constants()`. It was always returning the number of rows in the original data frame, instead of the index of empty rows (#173, @Bisaloo).

* Fixed a bug in the date guesser used by the `standardize_dates()` function. The followings have been added or corrected (#175, @Karim-Mane):
  * allow for quarter and partial dates
  * account for odd cases before parsing using {lubridate}

## Enhancements

* Rendering the report no more requires the use of {withr} package. Files generated during this process will be stored in the R temporary directory (#165, @Bisaloo).

* Simplification and improvement of code performance across several functions (#154, #156, #162, @Bisaloo).

* Make R version dependency explicit (#156, @Bisaloo).

* Replace base R pipe with {tidyverse} pipe (#155, @Karim-Mane)

* Stop automatic conversion of numeric values into Date due to unexpected results from date_guess() function (#151, @Karim-Mane).

* Allow for a vector of character for `na_strings` argument in `replace_missing_values()` (#180, Karim-Mane)

* Differentiate message sent when duplicates are found in subject ids column vs duplicates across several columns (#180, Karim-Mane)

* Allow for iterative constant data removal (#180, Karim-Mane)

* `scan_data()` now makes use of `date_guess()` to parse date from characters and `lubridate::as_date()` to parse dates from numeric. More importantly, the function now only operates on character columns only (#181, Karim-Mane).

* `remove_constants()` now works fine when the cut-off is different from 1. More importantly, the function no longer depends on {janitor} (#177, Karim-Mane).

* Replace `snakecase::to_snake_case()` with `janitor::clean_names()` and make sure variable names specified in `keep` and `rename` are preserved after applying `janitor::clean_names()` (#180, Karim-Mane).
