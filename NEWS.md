# cleanepi 1.0.2.9000

## Bug fixes

* Fixed a bug in the index for empty rows returned by `print_report()` after `remove_constants()`. It was always returning the number of rows in the original data frame, instead of the index of empty rows (#173, @Bisaloo).

## New minor features

* Added a `NEWS.md` file to track changes to the package.
