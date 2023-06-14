
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cleanepi

**cleanepi** provides functions to clean epidemiological data provided
in the form of a data frame or other related data type.

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/cleanepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/cleanepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/cleanepi?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)
<!-- badges: end -->

## Installation

You can install the development version of cleanepi from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
# remotes::install_github("epiverse-trace/cleanepi@develop", 
#                         build_vignettes=TRUE)
library(cleanepi)
```

## Manual

``` r
browseVignettes("cleanepi")
```

## Description

| function name           | description                                                                                                                                                                                           |
|:------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **clean_data**          | cleaning of column names, detecting and removing empty rows and columns as well as constant columns and duplicated rows. It also replaces missing values by NA and automatically convert date columns |
| **check_subject_ids**   | check whether the IDs comply with the expected format                                                                                                                                                 |
| **standardize_date**    | convert date column into *%Y-%m-%d*                                                                                                                                                                   |
| **calculate_age**       | calculate age from date column. Returned age can be in either *years*, or *months*, or *weeks*, or *days* or a combination of some of these                                                           |
| **check_date_sequence** | check whether the sequence of event dates is correct                                                                                                                                                  |
| **find_duplicates**     | detect and return duplicated rows from the input dataset                                                                                                                                              |
| **remove_duplicates**   | remove detected duplicated rows                                                                                                                                                                       |
| **WIP**                 | work in progress                                                                                                                                                                                      |

The main function in **cleanepi** is `clean_data()`. It makes call of
almost all the other exported functions, which can also be called
independently to perform a specific cleaning task.

Most functions **cleanepi** will return a list with 2 elements:

1.  the processed input data (could be modified or not).  
2.  a report object returned as list.

## GENERAL DATA CLEANING

The example below performs the following data cleaning operations on the
input dataset based on the following criteria:

1.  removal of any duplicated line across all columns
    (`remove_duplicates = TRUE`; `duplicates_from = NULL`)
2.  replacement of the missing character `-99` by `NA`
    (`replace_missing = TRUE`; `na_comes_as = "-99"`)
3.  conversion of any `character` column into `Date` if the 50% or more
    of its values turn out to be dates. Dates not within the specified
    time frame will be considered as outliers and replaced by `NA`
    (`check_timeframe = TRUE`;
    `timeframe = as.Date(c("1973-05-29", "2023-05-29"))`;
    `error_tolerance = 0.5`)
4.  Checking if the subject IDs comply with the expected format
    (`subject_id_col_name = "study_id"`;
    `subject_id_format = "PS000P2"`; `prefix = "PS"`; `suffix = "P2"`;
    `range = c(1, 100)`). This will detect and remove the rows where the
    format of the subject IDs are incorrect.
5.  The other cleaning operations, mentioned above, will be
    automatically applied to the input dataset.

``` r
# READING IN THE TEST DATASET
test_data <- data.table::fread(
  system.file("extdata", "test.txt", package = "cleanepi")
  )

# VISUALISE THE INPUT DATASET
print(test_data)
#>      study_id event_name country_code country_name date.of.admission
#>  1:   PS001P2      day 0            2       Gambia        01/12/2020
#>  2:   PS002P2      day 0            2       Gambia        28/01/2021
#>  3: PS004P2-1      day 0            2       Gambia        15/02/2021
#>  4:   PS003P2      day 0            2       Gambia        11/02/2021
#>  5:   P0005P2      day 0            2       Gambia        17/02/2021
#>  6:   PS006P2      day 0            2       Gambia        17/02/2021
#>  7:   PB500P2      day 0            2       Gambia        28/02/2021
#>  8:   PS008P2      day 0            2       Gambia        22/02/2021
#>  9:   PS010P2      day 0            2       Gambia        02/03/2021
#> 10:   PS011P2      day 0            2       Gambia        05/03/2021
#>     dateOfBirth date_first_pcr_positive_test sex
#>  1:  06/01/1972                 Dec 01, 2020   1
#>  2:  02/20/1952                 Jan 01, 2021   1
#>  3:  06/15/1961                 Feb 11, 2021 -99
#>  4:  11/11/1947                 Feb 01, 2021   1
#>  5:  09/26/2000                 Feb 16, 2021   2
#>  6:         -99                 May 02, 2021   2
#>  7:  11/03/1989                 Feb 19, 2021   1
#>  8:  10/05/1976                 Sep 20, 2021   2
#>  9:  09/23/1991                 Feb 26, 2021   1
#> 10:  02/08/1991                 Mar 03, 2021   2

# DEFINING THE CLEANING PARAMETERS
params <- list(
  remove_duplicates = TRUE,
  duplicates_from = NULL,
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
#> 1:  PS001P2        2020-12-01        <NA>                   2020-12-01   1
#> 2:  PS002P2        2021-01-28        <NA>                   2021-01-01   1
#> 3:  PS003P2        2021-02-11        <NA>                   2021-02-01   1
#> 4:  PS006P2        2021-02-17        <NA>                   2021-05-02   2
#> 5:  PS008P2        2021-02-22  1976-05-10                   2021-09-20   2
#> 6:  PS010P2        2021-03-02  1991-09-23                   2021-02-26   1
#> 7:  PS011P2        2021-03-05  1991-08-02                   2021-03-03   2
#>    row_id
#> 1:      1
#> 2:      2
#> 3:      4
#> 4:      6
#> 5:      8
#> 6:      9
#> 7:     10
```

Note that a function to visualize the report from the data cleaning will
be built soon.

## SPECIFIC DATA CLEANING TASKS

The examples in this section show how to use the individual data
cleaning functions.

### CHECKING IF THE SUBJECT IDs COMPLY WITH THE EXPECTED FORMAT

Below, we will inspect for subject IDs with incorrect format, report
them, but without removing them from the input data
(`remove = FALSE`).  
The `report` argument can take a report object from the `clean_data()`
function for example or another function in **cleanepi** that returns a
report along with the processed data.

``` r
# DETECTING INCORRECT SUBJECT IDs
dat <- check_subject_ids(
  data = test_data,
  id_column_name = "study_id",
  format = "PS000P2",
  prefix = "PS",
  suffix = "P2",
  range = c(1,100),
  remove = FALSE,
  verbose = TRUE,
  report = list()
)
#> 
#> Sample IDs with wrong prefix:
#> [1] "P0005P2" "PB500P2"
#> 
#> Sample IDs with wrong suffix:
#> [1] "PS004P2-1"
#> 
#> Sample IDs with wrong incorrect length:
#> [1] "PS004P2-1"
#> 
#> Sample IDs with wrong numbers:
#> [1] "PB500P2"

# VISUALISE THE REPORT
dat$report
#> $incorrect_subject_id
#>     study_id event_name country_code country_name date.of.admission dateOfBirth
#> 1:   P0005P2      day 0            2       Gambia        17/02/2021  09/26/2000
#> 2:   PB500P2      day 0            2       Gambia        28/02/2021  11/03/1989
#> 3: PS004P2-1      day 0            2       Gambia        15/02/2021  06/15/1961
#>    date_first_pcr_positive_test sex
#> 1:                 Feb 16, 2021   2
#> 2:                 Feb 19, 2021   1
#> 3:                 Feb 11, 2021 -99
```

### STANDARDIZING DATE COLUMN

This example shows how to convert a character column into date when it
contains date values. If known, users can specify the date format in the
target with the `format` argument. Otherwise, the function will
automatically infer the date format and perform the conversion
adequately.

``` r
dat <- standardize_date(
  data = test_data,
  date_column_name = "date_first_pcr_positive_test",
  format = NULL,
  timeframe = NULL,
  check_timeframe = FALSE,
  report = list(),
  error_tolerance = 0.5
)

print(dat$data)
#>      study_id event_name country_code country_name date.of.admission
#>  1:   PS001P2      day 0            2       Gambia        01/12/2020
#>  2:   PS002P2      day 0            2       Gambia        28/01/2021
#>  3: PS004P2-1      day 0            2       Gambia        15/02/2021
#>  4:   PS003P2      day 0            2       Gambia        11/02/2021
#>  5:   P0005P2      day 0            2       Gambia        17/02/2021
#>  6:   PS006P2      day 0            2       Gambia        17/02/2021
#>  7:   PB500P2      day 0            2       Gambia        28/02/2021
#>  8:   PS008P2      day 0            2       Gambia        22/02/2021
#>  9:   PS010P2      day 0            2       Gambia        02/03/2021
#> 10:   PS011P2      day 0            2       Gambia        05/03/2021
#>     dateOfBirth date_first_pcr_positive_test sex
#>  1:  06/01/1972                   2020-12-01   1
#>  2:  02/20/1952                   2021-01-01   1
#>  3:  06/15/1961                   2021-02-11 -99
#>  4:  11/11/1947                   2021-02-01   1
#>  5:  09/26/2000                   2021-02-16   2
#>  6:         -99                   2021-05-02   2
#>  7:  11/03/1989                   2021-02-19   1
#>  8:  10/05/1976                   2021-09-20   2
#>  9:  09/23/1991                   2021-02-26   1
#> 10:  02/08/1991                   2021-03-03   2
```

### CALCULATE AGE

Given a date column column and a reference date, we can use the function
in the example below to calculate individual ages in either years,
months, weeks, or days.  
Note the creation of new column(s) in the output data frame.

``` r
# CALCULATE INDIVIDUAL AGES IN MONTHS USING TODAY'S DATE AS REFERENCE
dat <- calculate_age(
  data = test_data,
  date_column_name = "dateOfBirth",
  end_date = Sys.Date(),
  age_in = "months"
)

print(dat)
#>      study_id event_name country_code country_name date.of.admission
#>  1:   PS001P2      day 0            2       Gambia        01/12/2020
#>  2:   PS002P2      day 0            2       Gambia        28/01/2021
#>  3: PS004P2-1      day 0            2       Gambia        15/02/2021
#>  4:   PS003P2      day 0            2       Gambia        11/02/2021
#>  5:   P0005P2      day 0            2       Gambia        17/02/2021
#>  6:   PS006P2      day 0            2       Gambia        17/02/2021
#>  7:   PB500P2      day 0            2       Gambia        28/02/2021
#>  8:   PS008P2      day 0            2       Gambia        22/02/2021
#>  9:   PS010P2      day 0            2       Gambia        02/03/2021
#> 10:   PS011P2      day 0            2       Gambia        05/03/2021
#>     dateOfBirth date_first_pcr_positive_test sex age_months remainder_days
#>  1:  1972-06-01                 Dec 01, 2020   1        612             12
#>  2:  1952-02-20                 Jan 01, 2021   1        855             22
#>  3:  1961-06-15                 Feb 11, 2021 -99        743             28
#>  4:  1947-11-11                 Feb 01, 2021   1        907              2
#>  5:  2000-09-26                 Feb 16, 2021   2        272             17
#>  6:        <NA>                 May 02, 2021   2         NA             NA
#>  7:  1989-11-03                 Feb 19, 2021   1        403              9
#>  8:  1976-10-05                 Sep 20, 2021   2        560              8
#>  9:  1991-09-23                 Feb 26, 2021   1        380             20
#> 10:  1991-02-08                 Mar 03, 2021   2        388              4
```

### CHECK DATE SEQUENCE

In this section, we are checking whether the sequence of dates is
respected in the specified columns.  
Set `remove_bad_seq = TRUE` if you wish to remove the detected rows with
incorrect date sequence.

``` r
good_date_sequence <- check_date_sequence(
  data = test_data,
  event_cols = c("date_first_pcr_positive_test", "date.of.admission"),
  remove_bad_seq = FALSE,
  report = list()
)
#> Warning in check_date_sequence(data = test_data, event_cols =
#> c("date_first_pcr_positive_test", : 2incorrect date sequences were detected and
#> removed

print(good_date_sequence$report)
#> $incorrect_date_sequence
#> $incorrect_date_sequence$date_sequence
#> date_first_pcr_positive_test < date.of.admission
#> 
#> $incorrect_date_sequence$bad_sequence
#>    study_id event_name country_code country_name date.of.admission dateOfBirth
#> 1:  PS006P2      day 0            2       Gambia        2021-02-17         -99
#> 2:  PS008P2      day 0            2       Gambia        2021-02-22  10/05/1976
#>    date_first_pcr_positive_test sex
#> 1:                   2021-05-02   2
#> 2:                   2021-09-20   2
```

## Next steps

- build function to display the cleaning report
- build function to perform dictionary based cleaning
- build function to quantify and handle missing data

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
