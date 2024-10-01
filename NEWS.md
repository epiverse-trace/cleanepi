# cleanepi 1.0.2.9000

## New minor features

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

* Fixed a bug in the index for empty rows returned by `print_report()` after `remove_constants()`. It was always returning the number of rows in the original data frame, instead of the index of empty rows (#173, @Bisaloo).

* Fixed a bug in the date guesser used by the `standardize_dates()` function. The followings have been added or corrected (#175, @Karim-Mane):
  * allow for quarter and partial dates
  * account for odd cases before parsing using {lubridate}
  * use `lubridate::origin` as origin when converting number into dates using `lubridate::as_date`.
  * Update the date guesser

## Enhancements

* Rendering the report no more requires the use of {withr} package. Files generated during this process will be stored in the R temporary directory (#165, @Bisaloo).

* Simplification and improvement of code performance across several functions (#154, #156, #162, @Bisaloo).

* Make R version dependency explicit (#156, @Bisaloo).

* Replace base R pipe with {tidyverse} pipe (#155, @Karim-Mane)
