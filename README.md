
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
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-stable.svg)](https://www.reconverse.org/lifecycle.html#stable)
[![DOI](https://zenodo.org/badge/607159823.svg)](https://zenodo.org/doi/10.5281/zenodo.11473984)

<!-- badges: end -->

**cleanepi** is an R package designed for cleaning, curating, and
standardizing epidemiological data. It streamlines various data cleaning
tasks that are typically expected when working with datasets in
epidemiology.

Key functionalities of **cleanepi** include:

1.  **Removing irregularities**: It removes duplicated and empty rows
    and columns, as well as columns with constant values.

2.  **Handling missing values**: It replaces missing values with the
    standard `NA` format, ensuring consistency and ease of analysis.

3.  **Ensuring data integrity**: It ensures the uniqueness of uniquely
    identified columns, thus maintaining data integrity and preventing
    duplicates.

4.  **Date conversion**: It offers functionality to convert character
    columns to Date format under specific conditions, enhancing data
    uniformity and facilitating temporal analysis. It also offers
    conversion of numeric values written in letters into numbers.

5.  **Standardizing entries**: It can standardize column entries into
    specified formats, promoting consistency across the dataset.

6.  **Time span calculation**: It calculates the time span between two
    elements of type `Date`, providing valuable demographic insights for
    epidemiological analysis.

**cleanepi** operates on data frames or similar structures like tibbles,
as well as linelist objects commonly used in epidemiological research.
It returns the processed data in the same format, ensuring seamless
integration into existing workflows. Additionally, it generates a
comprehensive report detailing the outcomes of each cleaning task.

**cleanepi** is developed by the
[Epiverse-TRACE](https://data.org/initiatives/epiverse/) team at the
[Medical Research Council The Gambia unit at the London School of
Hygiene and Tropical
Medicine](https://www.lshtm.ac.uk/research/units/mrc-gambia).

## Installation

**cleanepi** can be installed from CRAN using

``` r
install.packages("cleanepi")
```

The latest development version of **cleanepi** can be installed from
[GitHub](https://epiverse-trace.github.io/cleanepi/).

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
test_data <- readRDS(
  system.file("extdata", "test_df.RDS", package = "cleanepi")
)
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:100%; ">

<table class=" lightable-paper lightable-striped" style="font-size: 14px; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
study_id
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
event_name
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
country_code
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
country_name
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
date.of.admission
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
dateOfBirth
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
date_first_pcr_positive_test
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
sex
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
PS001P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
01/12/2020
</td>
<td style="text-align:left;">
06/01/1972
</td>
<td style="text-align:left;">
Dec 01, 2020
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
PS002P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
28/01/2021
</td>
<td style="text-align:left;">
02/20/1952
</td>
<td style="text-align:left;">
Jan 01, 2021
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
PS004P2-1
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
15/02/2021
</td>
<td style="text-align:left;">
06/15/1961
</td>
<td style="text-align:left;">
Feb 11, 2021
</td>
<td style="text-align:right;">
-99
</td>
</tr>
<tr>
<td style="text-align:left;">
PS003P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
11/02/2021
</td>
<td style="text-align:left;">
11/11/1947
</td>
<td style="text-align:left;">
Feb 01, 2021
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
P0005P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
17/02/2021
</td>
<td style="text-align:left;">
09/26/2000
</td>
<td style="text-align:left;">
Feb 16, 2021
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
PS006P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
17/02/2021
</td>
<td style="text-align:left;">
-99
</td>
<td style="text-align:left;">
May 02, 2021
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
PB500P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
28/02/2021
</td>
<td style="text-align:left;">
11/03/1989
</td>
<td style="text-align:left;">
Feb 19, 2021
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
PS008P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
22/02/2021
</td>
<td style="text-align:left;">
10/05/1976
</td>
<td style="text-align:left;">
Sep 20, 2021
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
PS010P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
02/03/2021
</td>
<td style="text-align:left;">
09/23/1991
</td>
<td style="text-align:left;">
Feb 26, 2021
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
PS011P2
</td>
<td style="text-align:left;">
day 0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gambia
</td>
<td style="text-align:left;">
05/03/2021
</td>
<td style="text-align:left;">
02/08/1991
</td>
<td style="text-align:left;">
Mar 03, 2021
</td>
<td style="text-align:right;">
2
</td>
</tr>
</tbody>
</table>

</div>

``` r
# READING IN THE DATA DICTIONARY
test_dictionary <- readRDS(
  system.file("extdata", "test_dictionary.RDS", package = "cleanepi")
)
```

<table class=" lightable-paper lightable-striped" style="font-size: 14px; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
options
</th>
<th style="text-align:left;">
values
</th>
<th style="text-align:left;">
grp
</th>
<th style="text-align:right;">
orders
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
sex
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
sex
</td>
<td style="text-align:right;">
2
</td>
</tr>
</tbody>
</table>

``` r
# SCAN THROUGH THE DATA
scan_res <- cleanepi::scan_data(test_data)
```

``` r
# DEFINING THE CLEANING PARAMETERS
replace_missing_values <- list(target_columns = NULL, na_strings = "-99")
remove_duplicates <- list(target_columns = NULL)
standardize_dates <- list(
  target_columns = NULL,
  error_tolerance = 0.4,
  format = NULL,
  timeframe = as.Date(c("1973-05-29", "2023-05-29")),
  orders = list(
    world_named_months = c("Ybd", "dby"),
    world_digit_months = c("dmy", "Ymd"),
    US_formats = c("Omdy", "YOmd")
  )
)
standardize_subject_ids <- list(
  target_columns = "study_id",
  prefix = "PS",
  suffix = "P2",
  range = c(1, 100),
  nchar = 7
)
remove_constants <- list(cutoff = 1)
standardize_column_names <- list(
  keep = "date.of.admission",
  rename = c(DOB = "dateOfBirth")
)
to_numeric <- list(target_columns = "sex", lang = "en")
```

``` r
# PERFORMING THE DATA CLEANING
cleaned_data <- clean_data(
  data = test_data,
  standardize_column_names = standardize_column_names,
  remove_constants = remove_constants,
  replace_missing_values = replace_missing_values,
  remove_duplicates = remove_duplicates,
  standardize_dates = standardize_dates,
  standardize_subject_ids = standardize_subject_ids,
  to_numeric = to_numeric,
  dictionary = test_dictionary,
  check_date_sequence = NULL
)
#> ℹ Cleaning column names
#> ℹ Replacing missing values with NA
#> ℹ Removing constant columns and empty rows
#> ℹ Removing duplicated rows
#> ℹ No duplicates were found.
#> ℹ Standardizing Date columns
#> ! Detected 8 values that comply with multiple formats and no values that are
#>   outside of the specified time frame.
#> ℹ Enter `print_report(data = dat, "date_standardization")` to access them,
#>   where "dat" is the object used to store the output from this operation.
#> ℹ Checking subject IDs format
#> 
#> ! Detected 0 missing, 0 duplicated, and 3 incorrect subject IDs.
#> ℹ Enter `print_report(data = dat, "incorrect_subject_id")` to access them,
#>   where "dat" is the object used to store the output from this operation.
#> ℹ You can use the `correct_subject_ids()` function to correct them.
#> ℹ Converting the following  column into numeric: sex
#> 
#> ℹ Performing dictionary-based cleaning
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:100%; ">

<table class=" lightable-paper lightable-striped" style="font-size: 14px; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
study_id
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
date.of.admission
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
DOB
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
date_first_pcr_positive_test
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
sex
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
PS001P2
</td>
<td style="text-align:left;">
2020-12-01
</td>
<td style="text-align:left;">
06/01/1972
</td>
<td style="text-align:left;">
2020-12-01
</td>
<td style="text-align:left;">
male
</td>
</tr>
<tr>
<td style="text-align:left;">
PS002P2
</td>
<td style="text-align:left;">
2021-01-28
</td>
<td style="text-align:left;">
02/20/1952
</td>
<td style="text-align:left;">
2021-01-01
</td>
<td style="text-align:left;">
male
</td>
</tr>
<tr>
<td style="text-align:left;">
PS004P2-1
</td>
<td style="text-align:left;">
2021-02-15
</td>
<td style="text-align:left;">
06/15/1961
</td>
<td style="text-align:left;">
2021-02-11
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
PS003P2
</td>
<td style="text-align:left;">
2021-02-11
</td>
<td style="text-align:left;">
11/11/1947
</td>
<td style="text-align:left;">
2021-02-01
</td>
<td style="text-align:left;">
male
</td>
</tr>
<tr>
<td style="text-align:left;">
P0005P2
</td>
<td style="text-align:left;">
2021-02-17
</td>
<td style="text-align:left;">
09/26/2000
</td>
<td style="text-align:left;">
2021-02-16
</td>
<td style="text-align:left;">
female
</td>
</tr>
<tr>
<td style="text-align:left;">
PS006P2
</td>
<td style="text-align:left;">
2021-02-17
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
2021-05-02
</td>
<td style="text-align:left;">
female
</td>
</tr>
<tr>
<td style="text-align:left;">
PB500P2
</td>
<td style="text-align:left;">
2021-02-28
</td>
<td style="text-align:left;">
11/03/1989
</td>
<td style="text-align:left;">
2021-02-19
</td>
<td style="text-align:left;">
male
</td>
</tr>
<tr>
<td style="text-align:left;">
PS008P2
</td>
<td style="text-align:left;">
2021-02-22
</td>
<td style="text-align:left;">
10/05/1976
</td>
<td style="text-align:left;">
2021-09-20
</td>
<td style="text-align:left;">
female
</td>
</tr>
<tr>
<td style="text-align:left;">
PS010P2
</td>
<td style="text-align:left;">
2021-03-02
</td>
<td style="text-align:left;">
09/23/1991
</td>
<td style="text-align:left;">
2021-02-26
</td>
<td style="text-align:left;">
male
</td>
</tr>
<tr>
<td style="text-align:left;">
PS011P2
</td>
<td style="text-align:left;">
2021-03-05
</td>
<td style="text-align:left;">
02/08/1991
</td>
<td style="text-align:left;">
2021-03-03
</td>
<td style="text-align:left;">
female
</td>
</tr>
</tbody>
</table>

</div>

``` r
# ADD THE DATA SCANNING RESULT TO THE REPORT
cleaned_data <- cleanepi::add_to_report(
  x = cleaned_data,
  key = "scanning_result",
  value = scan_res
)
```

``` r
# DISPLAY THE DATA CLEANING REPORT
print_report(cleaned_data, print = TRUE)
```

## Vignette

``` r
browseVignettes("cleanepi")
```

### Lifecycle

This package is currently an *experimental*, as defined by the [RECON
software lifecycle](https://www.reconverse.org/lifecycle.html). This
means that it is functional, but interfaces and functionalities may
change over time, testing and documentation may be lacking.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/cleanepi/pulls).

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
#>   Mané K, Degoot A, Ahadzie B, Mohammed N, Bah B (2025). _cleanepi:
#>   Clean and Standardize Epidemiological Data_.
#>   doi:10.5281/zenodo.11473985
#>   <https://doi.org/10.5281/zenodo.11473985>,
#>   <https://epiverse-trace.github.io/cleanepi/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {cleanepi: Clean and Standardize Epidemiological Data},
#>     author = {Karim Mané and Abdoelnaser Degoot and Bankolé Ahadzie and Nuredin Mohammed and Bubacarr Bah},
#>     year = {2025},
#>     doi = {10.5281/zenodo.11473985},
#>     url = {https://epiverse-trace.github.io/cleanepi/},
#>   }
```
