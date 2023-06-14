
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

<<<<<<< HEAD
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
=======
**cleanepi** performs several data cleaning tasks that an end-user would
anticipate to be performed on a cluttered dataset. These include
standard tasks such as removing duplicated and empty rows and row,
detecting columns of constant value, replacing missing values,
performing a sanity check on uniquely identified columns, and converting
dates in a string to date-format. **cleanepi** can also standardize
column entries into specified formats, calculate age (in years, months,
weeks, or days) for a given column and reference data, and perform
further dictionary-based cleaning operations. **cleanepi** expects input
data in form of dataframe-like structure (`data.frame`, `tibble`, etc)
or `linelist` object.
>>>>>>> c2704c2 (update readme)

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
#> → All 68 packages (75.16 MB) are cached.
#> + arsenal              3.6.3      
#> + backports            1.4.1      
#> + bit                  4.0.5      
#> + bit64                4.0.5      
#> + checkmate            2.3.0      
#> + cleanepi     0.0.2 → 0.0.2      👷🏾🔧 (GitHub: 1044209)
#> + cli                  3.6.1      
#> + clipr                0.8.0      
#> + colorspace           2.1-0      
#> + crayon               1.5.2      
#> + distcrete            1.0.3      
#> + dplyr                1.1.4      
#> + epitrix              0.4.0      
#> + evaluate             0.23       
#> + fansi                1.0.5      
#> + farver               2.1.1      
#> + forcats              1.0.0      
#> + generics             0.1.3      
#> + ggplot2              3.4.4      
#> + glue                 1.6.2      
#> + gridExtra            2.3        
#> + gtable               0.3.4      
#> + highr                0.10       
#> + hms                  1.1.3      
#> + isoband              0.2.7      
#> + janitor              2.2.0      
#> + knitr                1.45       
#> + labeling             0.4.3      
#> + lifecycle            1.0.4      
#> + linelist             1.0.0.9000 👷🔧 (GitHub: 9c65cc1)
#> + lubridate            1.9.3      
#> + magrittr             2.0.3      
#> + munsell              0.5.0      
#> + naniar               1.0.0      
#> + norm                 1.0-11.1   
#> + numberize            0.0.1      👷🏿‍♀️🔧 (GitHub: 8f0650a)
#> + pillar               1.9.0      
#> + pkgconfig            2.0.3      
#> + plyr                 1.8.9      
#> + purrr                1.0.2      
#> + R.methodsS3          1.8.2      
#> + R.oo                 1.25.0     
#> + R.utils              2.12.3     
#> + R6                   2.5.1      
#> + RColorBrewer         1.1-3      
#> + Rcpp                 1.0.11     
#> + readr                2.1.4      
#> + rlang                1.1.2      
#> + scales               1.3.0      
#> + snakecase            0.11.1     
#> + sodium               1.3.1      
#> + stringi              1.8.2      
#> + stringr              1.5.1      
#> + tibble               3.2.1      
#> + tidyr                1.3.0      
#> + tidyselect           1.2.0      
#> + timechange           0.2.0      
#> + tzdb                 0.4.0      
#> + UpSetR               1.4.0      
#> + utf8                 1.2.4      
#> + vctrs                0.6.4      
#> + viridis              0.6.4      
#> + viridisLite          0.4.2      
#> + visdat               0.6.0      
#> + vroom                1.6.4      
#> + withr                2.5.2      
#> + xfun                 0.41       
#> + yaml                 2.3.7
#> ℹ No downloads are needed, 68 pkgs (75.16 MB) are cached
#> ✔ Installed cleanepi 0.0.2 (github::epiverse-trace/cleanepi@1044209) (53ms)
#> ✔ Installed R.methodsS3 1.8.2  (65ms)
#> ✔ Installed R.oo 1.25.0  (106ms)
#> ✔ Installed R.utils 2.12.3  (117ms)
#> ✔ Installed R6 2.5.1  (127ms)
#> ✔ Installed RColorBrewer 1.1-3  (135ms)
#> ✔ Installed Rcpp 1.0.11  (151ms)
#> ✔ Installed UpSetR 1.4.0  (165ms)
#> ✔ Installed arsenal 3.6.3  (176ms)
#> ✔ Installed backports 1.4.1  (186ms)
#> ✔ Installed bit64 4.0.5  (76ms)
#> ✔ Installed bit 4.0.5  (67ms)
#> ✔ Installed checkmate 2.3.0  (67ms)
#> ✔ Installed cli 3.6.1  (33ms)
#> ✔ Installed clipr 0.8.0  (31ms)
#> ✔ Installed crayon 1.5.2  (13ms)
#> ✔ Installed colorspace 2.1-0  (57ms)
#> ✔ Installed distcrete 1.0.3  (30ms)
#> ✔ Installed dplyr 1.1.4  (31ms)
#> ✔ Installed epitrix 0.4.0  (31ms)
#> ✔ Installed evaluate 0.23  (30ms)
#> ✔ Installed fansi 1.0.5  (58ms)
#> ✔ Installed farver 2.1.1  (31ms)
#> ✔ Installed forcats 1.0.0  (31ms)
#> ✔ Installed generics 0.1.3  (33ms)
#> ✔ Installed ggplot2 3.4.4  (39ms)
#> ✔ Installed glue 1.6.2  (40ms)
#> ✔ Installed gridExtra 2.3  (32ms)
#> ✔ Installed gtable 0.3.4  (29ms)
#> ✔ Installed highr 0.10  (30ms)
#> ✔ Installed hms 1.1.3  (75ms)
#> ✔ Installed isoband 0.2.7  (48ms)
#> ✔ Installed janitor 2.2.0  (33ms)
#> ✔ Installed labeling 0.4.3  (13ms)
#> ✔ Installed knitr 1.45  (66ms)
#> ✔ Installed lifecycle 1.0.4  (44ms)
#> ✔ Installed lubridate 1.9.3  (34ms)
#> ✔ Installed magrittr 2.0.3  (34ms)
#> ✔ Installed munsell 0.5.0  (31ms)
#> ✔ Installed naniar 1.0.0  (63ms)
#> ✔ Installed norm 1.0-11.1  (67ms)
#> ✔ Installed pillar 1.9.0  (33ms)
#> ✔ Installed pkgconfig 2.0.3  (29ms)
#> ✔ Installed plyr 1.8.9  (28ms)
#> ✔ Installed purrr 1.0.2  (30ms)
#> ✔ Installed readr 2.1.4  (34ms)
#> ✔ Installed rlang 1.1.2  (34ms)
#> ✔ Installed scales 1.3.0  (31ms)
#> ✔ Installed snakecase 0.11.1  (55ms)
#> ✔ Installed sodium 1.3.1  (59ms)
#> ✔ Installed stringr 1.5.1  (19ms)
#> ✔ Installed tibble 3.2.1  (25ms)
#> ✔ Installed tidyr 1.3.0  (28ms)
#> ✔ Installed stringi 1.8.2  (144ms)
#> ✔ Installed tidyselect 1.2.0  (34ms)
#> ✔ Installed timechange 0.2.0  (30ms)
#> ✔ Installed tzdb 0.4.0  (32ms)
#> ✔ Installed utf8 1.2.4  (32ms)
#> ✔ Installed vctrs 0.6.4  (65ms)
#> ✔ Installed viridisLite 0.4.2  (33ms)
#> ✔ Installed viridis 0.6.4  (31ms)
#> ✔ Installed visdat 0.6.0  (31ms)
#> ✔ Installed withr 2.5.2  (16ms)
#> ✔ Installed vroom 1.6.4  (59ms)
#> ✔ Installed xfun 0.41  (32ms)
#> ✔ Installed yaml 2.3.7  (29ms)
#> ✔ Installed linelist 1.0.0.9000 (github::epiverse-trace/linelist@9c65cc1) (30ms)
#> ✔ Installed numberize 0.0.1 (github::bahadzie/numberize@8f0650a) (53ms)
#> ✔ 1 pkg + 72 deps: upd 1, added 67 [7.7s]
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
