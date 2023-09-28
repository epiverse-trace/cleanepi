
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cleanepi: Clean and standardize epidemiological data <img src="man/figures/logo.png" align="right" width="130"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/cleanepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/cleanepi?branch=main)
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-experimental.svg)](https://www.reconverse.org/lifecycle.html#experimental)

<!-- badges: end -->

**{cleanepi}** is an R package to clean, curate, and standardize
epidemiological data.

**{cleanepi}** performs several data cleaning tasks that an end-user
would anticipate to be performed on a cluttered dataset. These include
standard tasks such as: removing duplicated and empty rows and columns,
removing columns with constant values, replacing missing value strings
with `NA`, ensuring uniqueness of uniquely identified columns,
converting `character` columns to `Date` under certain conditions,
etc.  
**{cleanepi}** can also standardize column entries into specified
formats, calculates age (in years, months, weeks, or days) from a given
`Date` column and a reference date.  
**{cleanepi}** expects input data in a form of data frame-like structure
(`data.frame`, `tibble`, etc) or a `linelist` object and returns a
processed data of the same type. It also returns an object of type
`list` that reports the outcomes from every cleaning task.

**{cleanepi}** is developed by the
[Epiverse-TRACE](https://data.org/initiatives/epiverse/) team at the
Medical Research Council The Gambia unit at the London School of Hygiene
and Tropical Medicine (<MRCG@LSHTM>).

## Installation

The current development version of **cleanepi** can be installed from
[GitHub](https://github.com/epiverse-trace/cleanepi).

``` r
if (!require("pak")) install.packages("pak")
#> Loading required package: pak
pak::pak("epiverse-trace/readepi")
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> 
#> → Will install 31 packages.
#> → Will download 29 CRAN packages (30.80 MB), cached: 2 (0 B).
#> + askpass           1.2.0        ⬇ (24.54 kB)
#> + blob              1.2.4        ⬇ (46.33 kB)
#> + cellranger        1.1.0        ⬇ (101.94 kB)
#> + commonmark        1.9.0        ⬇ (357.18 kB)
#> + crosstalk         1.2.0        ⬇ (405.13 kB)
#> + data.table        1.14.8       ⬇ (2.51 MB)
#> + DBI               1.1.3        ⬇ (768.94 kB)
#> + DT                0.29         ⬇ (1.58 MB)
#> + fingertipsR       1.0.10.9002 👷🏾‍♀️🔧 (GitHub: caa9b7b)
#> + haven             2.5.3        ⬇ (1.12 MB)
#> + htmlwidgets       1.6.2        ⬇ (803.88 kB)
#> + httpuv            1.6.11       ⬇ (2.70 MB)
#> + httr              1.4.7        ⬇ (474.94 kB)
#> + later             1.3.1        ⬇ (607.27 kB)
#> + miniUI            0.1.1.1      ⬇ (34.72 kB)
#> + odbc              1.3.5        ⬇ (2.91 MB)
#> + openssl           2.1.1        ⬇ (2.89 MB)
#> + pool              1.0.1        ⬇ (188.23 kB)
#> + promises          1.2.1        ⬇ (1.82 MB)
#> + readepi           0.0.1       👷🏿‍♀️🔧 (GitHub: 6b4212c)
#> + readxl            1.4.3        ⬇ (1.56 MB)
#> + REDCapR           1.1.0        ⬇ (1.44 MB)
#> + rematch           2.0.0        ⬇ (16.47 kB)
#> + rio               1.0.1        ⬇ (591.36 kB)
#> + RMySQL            0.10.26      ⬇ (1.98 MB)
#> + shiny             1.7.5        ⬇ (4.35 MB)
#> + shinycssloaders   1.0.0        ⬇ (134.79 kB)
#> + sourcetools       0.1.7-1      ⬇ (137.44 kB)
#> + sys               3.4.2        ⬇ (51.52 kB)
#> + writexl           1.4.2        ⬇ (476.86 kB)
#> + xtable            1.8-4        ⬇ (701.99 kB)
#> ℹ Getting 29 pkgs (30.80 MB), 2 cached
#> ✔ Got askpass 1.2.0 (x86_64-apple-darwin20) (24.54 kB)
#> ✔ Got blob 1.2.4 (x86_64-apple-darwin20) (46.33 kB)
#> ✔ Got rematch 2.0.0 (x86_64-apple-darwin20) (16.47 kB)
#> ✔ Got cellranger 1.1.0 (x86_64-apple-darwin20) (101.94 kB)
#> ✔ Got miniUI 0.1.1.1 (x86_64-apple-darwin20) (34.72 kB)
#> ✔ Got sourcetools 0.1.7-1 (x86_64-apple-darwin20) (137.44 kB)
#> ✔ Got commonmark 1.9.0 (x86_64-apple-darwin20) (357.18 kB)
#> ✔ Got DBI 1.1.3 (x86_64-apple-darwin20) (768.94 kB)
#> ✔ Got htmlwidgets 1.6.2 (x86_64-apple-darwin20) (803.88 kB)
#> ✔ Got httr 1.4.7 (x86_64-apple-darwin20) (474.94 kB)
#> ✔ Got pool 1.0.1 (x86_64-apple-darwin20) (188.23 kB)
#> ✔ Got later 1.3.1 (x86_64-apple-darwin20) (607.27 kB)
#> ✔ Got REDCapR 1.1.0 (x86_64-apple-darwin20) (1.44 MB)
#> ✔ Got crosstalk 1.2.0 (x86_64-apple-darwin20) (405.13 kB)
#> ✔ Got writexl 1.4.2 (x86_64-apple-darwin20) (476.86 kB)
#> ✔ Got DT 0.29 (x86_64-apple-darwin20) (1.58 MB)
#> ✔ Got rio 1.0.1 (x86_64-apple-darwin20) (591.36 kB)
#> ✔ Got sys 3.4.2 (x86_64-apple-darwin20) (51.52 kB)
#> ✔ Got RMySQL 0.10.26 (x86_64-apple-darwin20) (1.98 MB)
#> ✔ Got fingertipsR 1.0.10.9002 (source) (142.79 kB)
#> ✔ Got haven 2.5.3 (x86_64-apple-darwin20) (1.12 MB)
#> ✔ Got promises 1.2.1 (x86_64-apple-darwin20) (1.82 MB)
#> ✔ Got xtable 1.8-4 (x86_64-apple-darwin20) (701.99 kB)
#> ✔ Got shinycssloaders 1.0.0 (x86_64-apple-darwin20) (134.79 kB)
#> ✔ Got openssl 2.1.1 (x86_64-apple-darwin20) (2.89 MB)
#> ✔ Got readxl 1.4.3 (x86_64-apple-darwin20) (1.56 MB)
#> ✔ Got httpuv 1.6.11 (x86_64-apple-darwin20) (2.70 MB)
#> ✔ Got odbc 1.3.5 (x86_64-apple-darwin20) (2.91 MB)
#> ✔ Got data.table 1.14.8 (x86_64-apple-darwin20) (2.51 MB)
#> ✔ Got shiny 1.7.5 (x86_64-apple-darwin20) (4.35 MB)
#> ✔ Got readepi 0.0.1 (source) (582.01 kB)
#> ✔ Installed DBI 1.1.3  (102ms)
#> ✔ Installed REDCapR 1.1.0  (148ms)
#> ✔ Installed RMySQL 0.10.26  (75ms)
#> ✔ Installed askpass 1.2.0  (56ms)
#> ✔ Installed blob 1.2.4  (91ms)
#> ✔ Installed cellranger 1.1.0  (57ms)
#> ✔ Installed data.table 1.14.8  (257ms)
#> ✔ Installed haven 2.5.3  (92ms)
#> ✔ Installed httr 1.4.7  (84ms)
#> ✔ Installed later 1.3.1  (76ms)
#> ✔ Installed odbc 1.3.5  (126ms)
#> ✔ Installed openssl 2.1.1  (89ms)
#> ✔ Installed pool 1.0.1  (59ms)
#> ✔ Installed readxl 1.4.3  (70ms)
#> ✔ Installed rematch 2.0.0  (37ms)
#> ✔ Installed rio 1.0.1  (60ms)
#> ✔ Installed sys 3.4.2  (69ms)
#> ✔ Installed writexl 1.4.2  (68ms)
#> ✔ Installed DT 0.29  (195ms)
#> ✔ Installed commonmark 1.9.0  (57ms)
#> ✔ Installed crosstalk 1.2.0  (40ms)
#> ✔ Installed htmlwidgets 1.6.2  (80ms)
#> ✔ Installed httpuv 1.6.11  (123ms)
#> ✔ Installed miniUI 0.1.1.1  (31ms)
#> ✔ Installed promises 1.2.1  (64ms)
#> ✔ Installed shiny 1.7.5  (174ms)
#> ✔ Installed shinycssloaders 1.0.0  (64ms)
#> ✔ Installed sourcetools 0.1.7-1  (58ms)
#> ✔ Installed xtable 1.8-4  (47ms)
#> ℹ Packaging fingertipsR 1.0.10.9002
#> ✔ Packaged fingertipsR 1.0.10.9002 (837ms)
#> ℹ Building fingertipsR 1.0.10.9002
#> ✔ Built fingertipsR 1.0.10.9002 (6.1s)
#> ✔ Installed fingertipsR 1.0.10.9002 (github::rOpenSci/fingertipsR@caa9b7b) (48ms)
#> ℹ Packaging readepi 0.0.1
#> ✔ Packaged readepi 0.0.1 (1s)
#> ℹ Building readepi 0.0.1
#> ✔ Built readepi 0.0.1 (3s)
#> ✔ Installed readepi 0.0.1 (github::epiverse-trace/readepi@6b4212c) (89ms)
#> ✔ 1 pkg + 90 deps: kept 59, added 31, dld 31 (NA B) [32.1s]
library(cleanepi)
```

## Quick start

The main function in **{cleanepi}** is `clean_data(),` which internally
makes call of almost all standard data cleaning functions, such as
removal of empty and duplicated rows and columns, replacement of missing
values, etc. However, each function can also be called independently to
perform a specific task. This mechanism is explained in details in the
**vignette**. Below is typical example of how to use the `clean_data()`
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
  range               = c(1, 100)
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
#>    study_id date_of_admission date_of_birth date_first_pcr_positive_test sex
#> 1   PS001P2        2020-12-01          <NA>                   2020-12-01   1
#> 2   PS002P2        2021-01-28          <NA>                   2021-01-01   1
#> 4   PS003P2        2021-02-11          <NA>                   2021-02-01   1
#> 6   PS006P2        2021-02-17          <NA>                   2021-05-02   2
#> 8   PS008P2        2021-02-22    1976-05-10                   2021-09-20   2
#> 9   PS010P2        2021-03-02    1991-09-23                   2021-02-26   1
#> 10  PS011P2        2021-03-05    1991-08-02                   2021-03-03   2
```

## Vignette

``` r
browseVignettes("cleanepi")
```

## Next steps

:white_check_mark: write test scripts  
:white_check_mark: build function to display the cleaning report  
:white_check_mark: build function to perform dictionary based cleaning  
:white_check_mark: improve package coverage

### Lifecycle

This package is currently *experimental*, as defined by the [RECON
software lifecycle](https://www.reconverse.org/lifecycle.html). This
means that it is functional, but interfaces and functionalities may
change over time.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/%7B%7B%20gh_repo%20%7D%7D/pulls).

### Code of Conduct

Please note that the cleanepi project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
