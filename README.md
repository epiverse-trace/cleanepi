
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cleanepi: Clean and standardize epidemiological data

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/cleanepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/cleanepi?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)

<!-- badges: end -->

**cleanepi** is an R package to clean, curate, and standardize
epidemiological data.

**cleanepi** performs several data cleaning tasks that an end-user would
anticipate to be performed on a cluttered dataset. These include
standard tasks such as: removing duplicated and empty rows and columns,
removing columns with constant values, replacing missing value strings
with `NA`, ensuring uniqueness of uniquely identified columns, and
converting `character` columns to `Date` under certain conditions.  
**cleanepi** can also standardize column entries into specified formats,
calculates age (in years, months, weeks, or days) from a given `Date`
column and reference date.  
**cleanepi** expects input data in a form of data frame-like structure
(`data.frame`, `tibble`, etc) or a `linelist` object and returns a
processed data of the same type. It also returns an object of type
`list` that reports the outcomes from every cleaning task.

**cleanepi** is developed by the
[Epiverse-TRACE](https://data.org/initiatives/epiverse/) team at the
Medical Research Council The Gambia unit at the London School of Hygiene
and Tropical Medicine (<MRCG@LSHTM>).

## Installation

The current development version of **cleanepi** can be installed from
[here](https://epiverse-trace.github.io/cleanepi/dev/).

``` r
# install.packages("remotes")
# remotes::install_github("epiverse-trace/cleanepi@develop", 
#                         build_vignettes=TRUE)
library(cleanepi)
```

## Quick start

The main function in **cleanepi** is `clean_data(),` which calls
functions for almost all standard data cleaning tasks, such as removal
of empty and duplicated rows and columns, replacement of missing values,
etc. However, each function can also be called independently to perform
a specific task. Below is typical example of how to use `clean_data()`
function.

``` r
# READING IN THE TEST DATASET
test_data <- readRDS(system.file("extdata", "test_df.RDS", 
                                 package = "cleanepi"))

# VISUALISE THE INPUT DATASET
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

# DEFINING THE CLEANING PARAMETERS
params <- list(
  remove_duplicates = TRUE,
  target_columns = NULL, 
  replace_missing = TRUE,
  na_comes_as = "-99",
  check_timeframe = TRUE,
  timeframe = as.Date(c("1973-05-29", "2023-05-29")),
  error_tolerance = 0.5,
  subject_id_col_name = "study_id",
  subject_id_format = "PS000P2",
  prefix = "PS",
  suffix = "P2",
  range = c(1, 100)
  )

# PERFORMING THE DATA CLEANING
res <- clean_data(
  data = test_data,
  params = params
)
#> 
#> cleaning column names
#> replacing missing values with NA
#> removing empty rows and columns
#> removing constant columns
#> removing duplicated rows
#> standardising date columns
#> checking subject IDs format

cleaned_data <- res$data
cleaning_report <- res$report

# VISUALISE THE CLEANED DATASET
print(cleaned_data)
#>    study_id date.of.admission dateOfBirth date_first_pcr_positive_test sex
#> 1   PS001P2        2020-12-01        <NA>                   2020-12-01   1
#> 2   PS002P2        2021-01-28        <NA>                   2021-01-01   1
#> 4   PS003P2        2021-02-11        <NA>                   2021-02-01   1
#> 6   PS006P2        2021-02-17        <NA>                   2021-05-02   2
#> 8   PS008P2        2021-02-22  1976-05-10                   2021-09-20   2
#> 9   PS010P2        2021-03-02  1991-09-23                   2021-02-26   1
#> 10  PS011P2        2021-03-05  1991-08-02                   2021-03-03   2
```

## Next steps

:white_check_mark: build function to display the cleaning report  
:white_check_mark: build function to perform dictionary based cleaning  
:white_check_mark: build function to quantify and handle missing data

### Lifecycle

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/%7B%7B%20gh_repo%20%7D%7D/pulls).

### Code of Conduct

Please note that the cleanepi project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
