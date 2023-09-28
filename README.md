
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

The current development version of **{cleanepi}** can be installed from
[here](https://epiverse-trace.github.io/cleanepi).

``` r
if (!require("pak")) install.packages("pak")
#> Loading required package: pak
pak::pak("epiverse-trace/cleanepi")
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> 
#> → Will install 67 packages.
#> → Will update 1 package.
#> → All 68 packages (74.22 MB) are cached.
#> + arsenal              3.6.3    
#> + backports            1.4.1    
#> + bit                  4.0.5    
#> + bit64                4.0.5    
#> + checkmate            2.2.0    
#> + cleanepi     0.0.1 → 0.0.1    👷🏻‍♂️🔧 (GitHub: c4cf193)
#> + cli                  3.6.1    
#> + clipr                0.8.0    
#> + colorspace           2.1-0    
#> + crayon               1.5.2    
#> + distcrete            1.0.3    
#> + dplyr                1.1.3    
#> + epitrix              0.4.0    
#> + evaluate             0.21     
#> + fansi                1.0.4    
#> + farver               2.1.1    
#> + forcats              1.0.0    
#> + generics             0.1.3    
#> + ggplot2              3.4.3    
#> + glue                 1.6.2    
#> + gridExtra            2.3      
#> + gtable               0.3.4    
#> + highr                0.10     
#> + hms                  1.1.3    
#> + isoband              0.2.7    
#> + janitor              2.2.0    
#> + knitr                1.44     
#> + labeling             0.4.3    
#> + lifecycle            1.0.3    
#> + linelist             1.0.0    👷🏿🔧 (GitHub: 1afd6d3)
#> + lubridate            1.9.3    
#> + magrittr             2.0.3    
#> + munsell              0.5.0    
#> + naniar               1.0.0    
#> + norm                 1.0-11.1 
#> + numberize            0.0.1    👷🏼‍♀️🔧 (GitHub: 8f0650a)
#> + pillar               1.9.0    
#> + pkgconfig            2.0.3    
#> + plyr                 1.8.8    
#> + purrr                1.0.2    
#> + R.methodsS3          1.8.2    
#> + R.oo                 1.25.0   
#> + R.utils              2.12.2   
#> + R6                   2.5.1    
#> + RColorBrewer         1.1-3    
#> + Rcpp                 1.0.11   
#> + readr                2.1.4    
#> + rlang                1.1.1    
#> + scales               1.2.1    
#> + snakecase            0.11.1   
#> + sodium               1.3.0    
#> + stringi              1.7.12   
#> + stringr              1.5.0    
#> + tibble               3.2.1    
#> + tidyr                1.3.0    
#> + tidyselect           1.2.0    
#> + timechange           0.2.0    
#> + tzdb                 0.4.0    
#> + UpSetR               1.4.0    
#> + utf8                 1.2.3    
#> + vctrs                0.6.3    
#> + viridis              0.6.4    
#> + viridisLite          0.4.2    
#> + visdat               0.6.0    
#> + vroom                1.6.3    
#> + withr                2.5.1    
#> + xfun                 0.40     
#> + yaml                 2.3.7
#> ℹ No downloads are needed, 68 pkgs (74.22 MB) are cached
#> ✔ Got UpSetR 1.4.0 (aarch64-apple-darwin20) (4.30 MB)
#> ✔ Got janitor 2.2.0 (aarch64-apple-darwin20) (282.53 kB)
#> ✔ Got norm 1.0-11.1 (aarch64-apple-darwin20) (110.83 kB)
#> ✔ Got labeling 0.4.3 (aarch64-apple-darwin20) (61.31 kB)
#> ✔ Got clipr 0.8.0 (aarch64-apple-darwin20) (50.79 kB)
#> ✔ Got xfun 0.40 (aarch64-apple-darwin20) (436.18 kB)
#> ✔ Got yaml 2.3.7 (aarch64-apple-darwin20) (216.76 kB)
#> ✔ Got cli 3.6.1 (aarch64-apple-darwin20) (1.38 MB)
#> ✔ Got crayon 1.5.2 (aarch64-apple-darwin20) (161.59 kB)
#> ✔ Got purrr 1.0.2 (aarch64-apple-darwin20) (523.33 kB)
#> ✔ Got stringr 1.5.0 (aarch64-apple-darwin20) (308.75 kB)
#> ✔ Got epitrix 0.4.0 (aarch64-apple-darwin20) (196.22 kB)
#> ✔ Got gtable 0.3.4 (aarch64-apple-darwin20) (217.72 kB)
#> ✔ Got farver 2.1.1 (aarch64-apple-darwin20) (1.93 MB)
#> ✔ Got rlang 1.1.1 (aarch64-apple-darwin20) (1.88 MB)
#> ✔ Installed cleanepi 0.0.1 (github::epiverse-trace/cleanepi@c4cf193) (52ms)
#> ✔ Installed R.methodsS3 1.8.2  (65ms)
#> ✔ Installed R.oo 1.25.0  (82ms)
#> ✔ Installed R.utils 2.12.2  (91ms)
#> ✔ Installed R6 2.5.1  (101ms)
#> ✔ Installed RColorBrewer 1.1-3  (110ms)
#> ✔ Installed Rcpp 1.0.11  (126ms)
#> ✔ Installed UpSetR 1.4.0  (139ms)
#> ✔ Installed arsenal 3.6.3  (173ms)
#> ✔ Installed backports 1.4.1  (182ms)
#> ✔ Installed bit64 4.0.5  (97ms)
#> ✔ Installed bit 4.0.5  (32ms)
#> ✔ Installed checkmate 2.2.0  (32ms)
#> ✔ Installed cli 3.6.1  (32ms)
#> ✔ Installed clipr 0.8.0  (30ms)
#> ✔ Installed crayon 1.5.2  (14ms)
#> ✔ Installed colorspace 2.1-0  (57ms)
#> ✔ Installed distcrete 1.0.3  (52ms)
#> ✔ Installed dplyr 1.1.3  (31ms)
#> ✔ Installed epitrix 0.4.0  (31ms)
#> ✔ Installed evaluate 0.21  (30ms)
#> ✔ Installed fansi 1.0.4  (32ms)
#> ✔ Installed farver 2.1.1  (33ms)
#> ✔ Installed forcats 1.0.0  (35ms)
#> ✔ Installed generics 0.1.3  (33ms)
#> ✔ Installed ggplot2 3.4.3  (38ms)
#> ✔ Installed glue 1.6.2  (62ms)
#> ✔ Installed gridExtra 2.3  (32ms)
#> ✔ Installed gtable 0.3.4  (34ms)
#> ✔ Installed highr 0.10  (33ms)
#> ✔ Installed hms 1.1.3  (31ms)
#> ✔ Installed isoband 0.2.7  (33ms)
#> ✔ Installed janitor 2.2.0  (33ms)
#> ✔ Installed labeling 0.4.3  (12ms)
#> ✔ Installed knitr 1.44  (64ms)
#> ✔ Installed lifecycle 1.0.3  (63ms)
#> ✔ Installed lubridate 1.9.3  (35ms)
#> ✔ Installed magrittr 2.0.3  (36ms)
#> ✔ Installed munsell 0.5.0  (32ms)
#> ✔ Installed naniar 1.0.0  (38ms)
#> ✔ Installed norm 1.0-11.1  (37ms)
#> ✔ Installed pillar 1.9.0  (32ms)
#> ✔ Installed pkgconfig 2.0.3  (32ms)
#> ✔ Installed plyr 1.8.8  (39ms)
#> ✔ Installed purrr 1.0.2  (59ms)
#> ✔ Installed readr 2.1.4  (36ms)
#> ✔ Installed rlang 1.1.1  (36ms)
#> ✔ Installed scales 1.2.1  (35ms)
#> ✔ Installed snakecase 0.11.1  (33ms)
#> ✔ Installed sodium 1.3.0  (31ms)
#> ✔ Installed stringr 1.5.0  (19ms)
#> ✔ Installed tibble 3.2.1  (26ms)
#> ✔ Installed tidyr 1.3.0  (53ms)
#> ✔ Installed stringi 1.7.12  (170ms)
#> ✔ Installed tidyselect 1.2.0  (36ms)
#> ✔ Installed timechange 0.2.0  (34ms)
#> ✔ Installed tzdb 0.4.0  (34ms)
#> ✔ Installed utf8 1.2.3  (33ms)
#> ✔ Installed vctrs 0.6.3  (35ms)
#> ✔ Installed viridisLite 0.4.2  (34ms)
#> ✔ Installed viridis 0.6.4  (34ms)
#> ✔ Installed visdat 0.6.0  (58ms)
#> ✔ Installed withr 2.5.1  (17ms)
#> ✔ Installed vroom 1.6.3  (66ms)
#> ✔ Installed xfun 0.40  (34ms)
#> ✔ Installed yaml 2.3.7  (31ms)
#> ✔ Installed numberize 0.0.1 (github::bahadzie/numberize@8f0650a) (32ms)
#> ✔ Installed linelist 1.0.0 (github::epiverse-trace/linelist@1afd6d3) (26ms)
#> ✔ 1 pkg + 72 deps: kept 1, upd 1, added 67, dld 15 (12.06 MB) [12.4s]
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
