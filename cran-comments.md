## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## revdepcheck results

* There are currently no downstream dependencies for this package.

## Resubmission

This is a resubmission. In this version I have fixed the NOTE raised during the
previous submission. The changes include:

* Updating `inst/WORDLIST` with the missing words.
* Adding a '/' to the package Github link in the DESCRIPTION file.
* Removing "Type: Package" from the DESCRIPTION file.
* Updating package description from the DESCRIPTION file.
* Replacing `\dontrun `with `\donttest` in `print_report()` function.
* Replacing `base::cat()` with `message()` in `clean_data()` function.
* Removing the example in `date_guess()` function documentation.
* Replacing the base `R` pipe operator with the {tidyverse} pipe operator.
