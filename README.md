
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cleanepi: Clean and standardize epidemiological data <img src="man/figures/logo.svg" align="right" width="130"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/cleanepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/cleanepi?branch=main)
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-experimental.svg)](https://www.reconverse.org/lifecycle.html#experimental)

<!-- badges: end -->

**cleanepi** is an R package to clean, curate, and standardize
epidemiological data.

**cleanepi** performs several data cleaning tasks that an end-user would
anticipate to be performed on a cluttered dataset. These include
standard tasks such as: removing duplicated and empty rows and columns,
removing columns with constant values, replacing missing value strings
with `NA`, ensuring uniqueness of uniquely identified columns,
converting `character` columns to `Date` under certain conditions, etc.

**cleanepi** can also standardize column entries into specified formats,
calculates age (in years, months, weeks, or days) from a given `Date`
column and a reference date.

**cleanepi** expects input data in a form of data frame-like structure
(`data.frame`, `tibble`, etc) or a `linelist` object and returns a
processed data of the same type. It also returns an object of type
`list` that reports the outcomes from every cleaning task.

**cleanepi** is developed by the
[Epiverse-TRACE](https://data.org/initiatives/epiverse/) team at the
[Medical Research Council The Gambia unit at the London School of
Hygiene and Tropical
Medicine](https://www.lshtm.ac.uk/research/units/mrc-gambia).

## Installation

The latest development version of **cleanepi** can be installed from
[GitHub](https://epiverse-trace.github.io/cleanepi).

``` r
if (!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/cleanepi")
library(cleanepi)
```

## Quick start

The main function in **cleanepi** is `clean_data(),` which internally
makes call of almost all standard data cleaning functions, such as
removal of empty and duplicated rows and columns, replacement of missing
values, etc. However, each function can also be called independently to
perform a specific task. This mechanism is explained in details in the
**vignette**. Below is typical example of how to use the `clean_data()`
function.

``` r
# READING IN THE TEST DATASET
test_data       <- readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi"))
print(test_data)
#>     study_id event_name country_code country_name date.of.admission dateOfBirth
#> 1    PS001P2      day 0            2       Gambia        01/12/2020  06/01/1972
#> 2    PS002P2      day 0            2       Gambia        28/01/2021  02/20/1952
#> 3  PS004P2-1      day 0            2       Gambia        15/02/2021  06/15/1961
#> 4    PS003P2      day 0            2       Gambia        11/02/2021  11/11/1947
#> 5    P0005P2      day 0            2       Gambia        17/02/2021  09/26/2000
#> 6    PS006P2      day 0            2       Gambia        17/02/2021         -99
#> 7    PB500P2      day 0            2       Gambia        28/02/2021  11/03/1989
#> 8    PS008P2      day 0            2       Gambia        22/02/2021  10/05/1976
#> 9    PS010P2      day 0            2       Gambia        02/03/2021  09/23/1991
#> 10   PS011P2      day 0            2       Gambia        05/03/2021  02/08/1991
#>    date_first_pcr_positive_test sex
#> 1                  Dec 01, 2020   1
#> 2                  Jan 01, 2021   1
#> 3                  Feb 11, 2021 -99
#> 4                  Feb 01, 2021   1
#> 5                  Feb 16, 2021   2
#> 6                  May 02, 2021   2
#> 7                  Feb 19, 2021   1
#> 8                  Sep 20, 2021   2
#> 9                  Feb 26, 2021   1
#> 10                 Mar 03, 2021   2
```

``` r
# READING IN THE DATA DICTIONARY
test_dictionary <- readRDS(system.file("extdata", "test_dictionary.RDS",
                                       package = "cleanepi"))
print(test_dictionary)
#>   options values grp orders
#> 1       1   male sex      1
#> 2       2 female sex      2
```

``` r
# DEFINING THE CLEANING PARAMETERS
params <- list(
  remove_duplicates   = TRUE,
  target_columns      = NULL, 
  replace_missing     = TRUE,
  na_comes_as         = "-99",
  check_timeframe     = TRUE,
  timeframe           = as.Date(c("1973-05-29", "2023-05-29")),
  error_tolerance     = 0.5,
  subject_id_col_name = "study_id",
  subject_id_format   = "PS000P2",
  prefix              = "PS",
  suffix              = "P2",
  range               = c(1, 100),
  keep                = "date.of.admission",
  dictionary          = test_dictionary
)

# PERFORMING THE DATA CLEANING
res <- clean_data(
  data   = test_data,
  params = params
)
#> 
#> cleaning column names
#> replacing missing values with NA
#> removing empty rows and columns
#> removing constant columns
#> standardising date columns
#> checking for subject IDs uniqueness
#> removing duplicated rows
#> checking subject IDs format

cleaned_data    <- res$data
cleaning_report <- res$report

# VISUALISE THE CLEANED DATASET
print(cleaned_data)
#>    study_id date.of.admission date_of_birth date_first_pcr_positive_test    sex
#> 1   PS001P2        2020-12-01          <NA>                   2020-12-01   male
#> 2   PS002P2        2021-01-28          <NA>                   2021-01-01   male
#> 4   PS003P2        2021-02-11          <NA>                   2021-02-01   male
#> 6   PS006P2        2021-02-17          <NA>                   2021-05-02 female
#> 8   PS008P2        2021-02-22    1976-05-10                   2021-09-20 female
#> 9   PS010P2        2021-03-02    1991-09-23                   2021-02-26   male
#> 10  PS011P2        2021-03-05    1991-08-02                   2021-03-03 female
```

## Vignette

``` r
browseVignettes("cleanepi")
```

## Next steps

:white_check_mark: update and create test files  
:white_check_mark: build function to display the cleaning report

### Lifecycle

This package is currently an *experimental*, as defined by the [RECON
software lifecycle](https://www.reconverse.org/lifecycle.html). This
means that it is functional, but interfaces and functionalities may
change over time, testing and documentation may be lacking.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/%7B%7B%20gh_repo%20%7D%7D/pulls).

### Code of Conduct

Please note that the cleanepi project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citing this package

``` r
citation("cleanepi")
#> To cite package 'cleanepi' in publications use:
#> 
#>   Mané K (2023). _cleanepi: Data Cleaning Package_. R package version
#>   0.0.2.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {cleanepi: Data Cleaning Package},
#>     author = {Karim Mané},
#>     year = {2023},
#>     note = {R package version 0.0.2},
#>   }
```
