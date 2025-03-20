# cleanepi (development version)

# cleanepi 1.1.1

## Bug fixes

* Added {systemfonts} as a dependency in Suggests as it is required to build the
vignette in **r-oldrel-macos-arm64** (#218, @Karim-Mane).

* Set a default value for the `orders` argument of the `standardize_dates()`
function using its initial value in version 1.0.2 (#224, @Karim-Mane).

# cleanepi 1.1.0

## Bug fixes

* Fixed a bug in the index for empty rows returned by `print_report()` after
`remove_constants()`. This function was incorrectly returning the number of rows
in the original data frame, instead of the index of empty rows (#173, @Bisaloo).

* Fixed a bug in the date guesser used by the `standardize_dates()` function.
The followings updates were made (#175, @Karim-Mane):
  * allow for quarter and partial dates, and
  * account for odd cases before parsing using the {lubridate} package.

## Enhancements

* Rendering the report no longer requires the use of the {withr} package. Files
generated during this process will be stored in the R temporary directory
(#165, @Bisaloo).

* Simplification and improvement of code performance across several functions
(#154, #156, #162, @Bisaloo).

* Make R version dependency explicit (#156, @Bisaloo).

* Replace base R pipe operator with the {tidyverse} package pipe operator
(#155, @Karim-Mane).

* Stop automatic conversion of numeric values into Date due to unexpected
results from `date_guess()` function (#151, @Karim-Mane).

* Allow for a vector of character for `na_strings` argument in
`replace_missing_values()` (#180, Karim-Mane).

* Improved messaging to differentiate between duplicates found in the subject
IDs column and duplicates across multiple columns (#180, Karim-Mane).

* Allow for iterative constant data removal (#180, Karim-Mane).

* The `remove_constants()` now works correctly when the `cutoff` is different
from 1. More importantly, the function no longer depends on the {janitor}
package (#177, Karim-Mane).

* The `scan_data()` now makes use of `date_guess()` to parse date from
characters and `lubridate::as_date()` to parse dates from numeric. This function
now operates only on character columns (#181, Karim-Mane).

* Replace `snakecase::to_snake_case()` with `janitor::clean_names()` and make
sure variable names specified in `keep` and `rename` are preserved after
applying `janitor::clean_names()` (#180, Karim-Mane).

* Use the {cli} package for formatting messages in the package
(#196, Karim-Mane).

* Translate messages in the package into French using the {potools}
package (#196, Karim-Mane).

* An additional column (**row_id**) with the row indices where the incorrect
date sequences are found is now added to the report made from the
`check_date_sequence()` function (#196, Karim-Mane).

* The `date_guess()` and related functions now returns a list of two elements:
the converted values and a boolean that informs about the presence of numeric
values that can also be of type Date (#181, Karim-Mane).

* Update the package vignette to account for the improvements made in the
different pull requests (#194, Karim-Mane).

## Deprecated and defunct

* Function documentations have been updated. The `clean_data()` no longer takes
the `params` argument. Instead, it takes many other arguments that are described
in the function documentation and the package vignettes (#184, Karim-Mane).

# cleanepi 1.0.1 to 1.0.2

*Minor version release of {cleanepi} include various improvements based on
feedback from the CRAN submission process.

# cleanepi 1.0.0 (development version)

* development of data cleaning modules.


