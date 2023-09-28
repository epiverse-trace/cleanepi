
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
#> → Will install 90 packages.
#> → All 90 packages (90.99 MB) are cached.
#> + askpass           1.2.0       
#> + backports         1.4.1       
#> + base64enc         0.1-3       
#> + bit               4.0.5       
#> + bit64             4.0.5       
#> + blob              1.2.4       
#> + bslib             0.5.1       
#> + cachem            1.0.8       
#> + cellranger        1.1.0       
#> + checkmate         2.2.0       
#> + cli               3.6.1       
#> + clipr             0.8.0       
#> + commonmark        1.9.0       
#> + crayon            1.5.2       
#> + crosstalk         1.2.0       
#> + curl              5.0.2       
#> + data.table        1.14.8      
#> + DBI               1.1.3       
#> + digest            0.6.33      
#> + dplyr             1.1.3       
#> + DT                0.29        
#> + ellipsis          0.3.2       
#> + evaluate          0.21        
#> + fansi             1.0.4       
#> + fastmap           1.1.1       
#> + fingertipsR       1.0.10.9002 👷🏼‍♂️🔧 (GitHub: caa9b7b)
#> + fontawesome       0.5.2       
#> + forcats           1.0.0       
#> + fs                1.6.3       
#> + generics          0.1.3       
#> + glue              1.6.2       
#> + haven             2.5.3       
#> + highr             0.10        
#> + hms               1.1.3       
#> + htmltools         0.5.6       
#> + htmlwidgets       1.6.2       
#> + httpuv            1.6.11      
#> + httr              1.4.7       
#> + jquerylib         0.1.4       
#> + jsonlite          1.8.7       
#> + knitr             1.44        
#> + later             1.3.1       
#> + lazyeval          0.2.2       
#> + lifecycle         1.0.3       
#> + magrittr          2.0.3       
#> + memoise           2.0.1       
#> + mime              0.12        
#> + miniUI            0.1.1.1     
#> + odbc              1.3.5       
#> + openssl           2.1.1       
#> + pillar            1.9.0       
#> + pkgconfig         2.0.3       
#> + pool              1.0.1       
#> + promises          1.2.1       
#> + purrr             1.0.2       
#> + R.methodsS3       1.8.2       
#> + R.oo              1.25.0      
#> + R.utils           2.12.2      
#> + R6                2.5.1       
#> + rappdirs          0.3.3       
#> + Rcpp              1.0.11      
#> + readepi           0.0.1       👷🏾‍♂️🔧 (GitHub: 6b4212c)
#> + readr             2.1.4       
#> + readxl            1.4.3       
#> + REDCapR           1.1.0       
#> + rematch           2.0.0       
#> + rio               1.0.1       
#> + rlang             1.1.1       
#> + rmarkdown         2.25        
#> + RMySQL            0.10.26     
#> + sass              0.4.7       
#> + shiny             1.7.5       
#> + shinycssloaders   1.0.0       
#> + sourcetools       0.1.7-1     
#> + stringi           1.7.12      
#> + stringr           1.5.0       
#> + sys               3.4.2       
#> + tibble            3.2.1       
#> + tidyr             1.3.0       
#> + tidyselect        1.2.0       
#> + tinytex           0.46        
#> + tzdb              0.4.0       
#> + utf8              1.2.3       
#> + vctrs             0.6.3       
#> + vroom             1.6.3       
#> + withr             2.5.1       
#> + writexl           1.4.2       
#> + xfun              0.40        
#> + xtable            1.8-4       
#> + yaml              2.3.7
#> ℹ No downloads are needed, 90 pkgs (90.99 MB) are cached
#> ✔ Installed readepi 0.0.1 (github::epiverse-trace/readepi@6b4212c) (49ms)
#> ✔ Installed DBI 1.1.3  (68ms)
#> ✔ Installed R.methodsS3 1.8.2  (87ms)
#> ✔ Installed R.oo 1.25.0  (98ms)
#> ✔ Installed R.utils 2.12.2  (108ms)
#> ✔ Installed R6 2.5.1  (119ms)
#> ✔ Installed REDCapR 1.1.0  (139ms)
#> ✔ Installed RMySQL 0.10.26  (171ms)
#> ✔ Installed askpass 1.2.0  (177ms)
#> ✔ Installed Rcpp 1.0.11  (201ms)
#> ✔ Installed backports 1.4.1  (97ms)
#> ✔ Installed bit64 4.0.5  (46ms)
#> ✔ Installed bit 4.0.5  (31ms)
#> ✔ Installed blob 1.2.4  (31ms)
#> ✔ Installed cellranger 1.1.0  (30ms)
#> ✔ Installed checkmate 2.2.0  (33ms)
#> ✔ Installed cli 3.6.1  (74ms)
#> ✔ Installed clipr 0.8.0  (72ms)
#> ✔ Installed crayon 1.5.2  (31ms)
#> ✔ Installed curl 5.0.2  (33ms)
#> ✔ Installed data.table 1.14.8  (45ms)
#> ✔ Installed dplyr 1.1.3  (45ms)
#> ✔ Installed fansi 1.0.4  (35ms)
#> ✔ Installed forcats 1.0.0  (32ms)
#> ✔ Installed generics 0.1.3  (33ms)
#> ✔ Installed glue 1.6.2  (56ms)
#> ✔ Installed haven 2.5.3  (55ms)
#> ✔ Installed hms 1.1.3  (34ms)
#> ✔ Installed httr 1.4.7  (33ms)
#> ✔ Installed jsonlite 1.8.7  (33ms)
#> ✔ Installed later 1.3.1  (33ms)
#> ✔ Installed lifecycle 1.0.3  (32ms)
#> ✔ Installed magrittr 2.0.3  (31ms)
#> ✔ Installed mime 0.12  (30ms)
#> ✔ Installed odbc 1.3.5  (65ms)
#> ✔ Installed openssl 2.1.1  (67ms)
#> ✔ Installed pillar 1.9.0  (34ms)
#> ✔ Installed pkgconfig 2.0.3  (32ms)
#> ✔ Installed pool 1.0.1  (31ms)
#> ✔ Installed purrr 1.0.2  (38ms)
#> ✔ Installed readr 2.1.4  (44ms)
#> ✔ Installed readxl 1.4.3  (35ms)
#> ✔ Installed rematch 2.0.0  (32ms)
#> ✔ Installed rio 1.0.1  (55ms)
#> ✔ Installed rlang 1.1.1  (58ms)
#> ✔ Installed stringr 1.5.0  (20ms)
#> ✔ Installed sys 3.4.2  (15ms)
#> ✔ Installed tibble 3.2.1  (27ms)
#> ✔ Installed stringi 1.7.12  (130ms)
#> ✔ Installed tidyr 1.3.0  (36ms)
#> ✔ Installed tidyselect 1.2.0  (34ms)
#> ✔ Installed tzdb 0.4.0  (33ms)
#> ✔ Installed utf8 1.2.3  (55ms)
#> ✔ Installed vctrs 0.6.3  (55ms)
#> ✔ Installed withr 2.5.1  (17ms)
#> ✔ Installed vroom 1.6.3  (64ms)
#> ✔ Installed writexl 1.4.2  (36ms)
#> ✔ Installed fingertipsR 1.0.10.9002 (github::rOpenSci/fingertipsR@caa9b7b) (31ms)
#> ✔ Installed base64enc 0.1-3  (13ms)
#> ✔ Installed DT 0.29  (85ms)
#> ✔ Installed cachem 1.0.8  (15ms)
#> ✔ Installed commonmark 1.9.0  (41ms)
#> ✔ Installed bslib 0.5.1  (144ms)
#> ✔ Installed crosstalk 1.2.0  (36ms)
#> ✔ Installed digest 0.6.33  (32ms)
#> ✔ Installed ellipsis 0.3.2  (30ms)
#> ✔ Installed evaluate 0.21  (31ms)
#> ✔ Installed fastmap 1.1.1  (36ms)
#> ✔ Installed fontawesome 0.5.2  (38ms)
#> ✔ Installed fs 1.6.3  (35ms)
#> ✔ Installed highr 0.10  (55ms)
#> ✔ Installed htmltools 0.5.6  (55ms)
#> ✔ Installed htmlwidgets 1.6.2  (35ms)
#> ✔ Installed httpuv 1.6.11  (40ms)
#> ✔ Installed jquerylib 0.1.4  (41ms)
#> ✔ Installed lazyeval 0.2.2  (16ms)
#> ✔ Installed knitr 1.44  (70ms)
#> ✔ Installed memoise 2.0.1  (40ms)
#> ✔ Installed miniUI 0.1.1.1  (30ms)
#> ✔ Installed promises 1.2.1  (55ms)
#> ✔ Installed rappdirs 0.3.3  (56ms)
#> ✔ Installed sass 0.4.7  (40ms)
#> ✔ Installed rmarkdown 2.25  (91ms)
#> ✔ Installed shinycssloaders 1.0.0  (19ms)
#> ✔ Installed shiny 1.7.5  (84ms)
#> ✔ Installed sourcetools 0.1.7-1  (47ms)
#> ✔ Installed tinytex 0.46  (31ms)
#> ✔ Installed xfun 0.40  (32ms)
#> ✔ Installed xtable 1.8-4  (56ms)
#> ✔ Installed yaml 2.3.7  (50ms)
#> ✔ 1 pkg + 90 deps: added 90 [9.5s]
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
