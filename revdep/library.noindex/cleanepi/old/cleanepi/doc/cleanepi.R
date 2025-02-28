## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk[["set"]](collapse = TRUE, comment = "#>", eval = FALSE,
                           fig.width = 7L, fig.height = 7L,
                           fig.align = "center")
row_id <- group_id <- NULL

## ----setup, eval=TRUE---------------------------------------------------------
library("cleanepi")

## ----eval=TRUE, comment=""----------------------------------------------------
# IMPORTING THE TEST DATASET
test_data <- readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi"))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
test_data |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----eval=TRUE, comment=""----------------------------------------------------
# SCAN THE DATA
scan_result <- scan_data(test_data)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
scan_result |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = FALSE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----eval=TRUE, comment=""----------------------------------------------------
# PARAMETERS FOR REPLACING MISSING VALUES WITH NA
rm_na <- list(target_columns = NULL, na_strings = "-99")

# PARAMETERS FOR COLUMN NAMES STANDARDIZATION
standardize_col_names <- list(keep = NULL, rename = NULL)

# PARAMETERS FOR DUBLICATES DETECTION AND REMOVAL
rm_dup <- list(target_columns   = NULL)

# PARAMETERS FOR STANDARDING DATES
stdn_date <- list(target_columns  = NULL,
                  error_tolerance = 0.4,
                  format          = NULL,
                  timeframe       = as.Date(c("1973-05-29", "2023-05-29")),
                  orders          = list(world_named_months = c("Ybd", "dby"),
                                         world_digit_months = c("dmy", "Ymd"),
                                         US_formats       = c("Omdy", "YOmd")),
                  modern_excel    = TRUE)

# PARAMETERS FOR STANDARDING SUBJECT IDs
stdn_ids <- list(target_columns = "study_id",
                 prefix         = "PS",
                 suffix         = "P2",
                 range          = c(1, 100),
                 nchar          = 7)

# PARAMETERS FOR CONSTANT COLUMNS, EMPTY ROWS AND COLUMNS REMOVAL
remove_cte <- list(cutoff = 1)

# LAOD THE DATA DICTIONARY
test_dictionary <- readRDS(system.file("extdata", "test_dictionary.RDS",
                                       package = "cleanepi"))

# DEFINE THE LIST OF PARAMETERS
params <- list(
  standardize_column_names = standardize_col_names,
  remove_constants         = remove_cte,
  replace_missing_values   = rm_na,
  remove_duplicates        = rm_dup,
  standardize_dates        = stdn_date,
  standardize_subject_ids  = stdn_ids,
  dictionary               = test_dictionary
)

## ----eval=TRUE, comment=""----------------------------------------------------
# CLEAN THE INPUT DATA FRAME
cleaned_data <- clean_data(
  data   = test_data,
  params = params
)

## ----eval=TRUE----------------------------------------------------------------
# ACCESS THE DATA CLEANING REPORT
report <- attr(cleaned_data, "report")

# SUMMARIZE THE REPORT OBJECT
summary(report)

## ----eval=FALSE---------------------------------------------------------------
#  print_report(report)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
# IMPORT THE INPUT DATA
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

# INTRODUCE AN EMPTY COLUMN
data$empty_column <- NA

# remove the constant columns, empty rows and columns
dat <- remove_constants(data   = data,
                        cutoff = 1)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
dat |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----eval=TRUE, comment="col_name_cleaning"-----------------------------------
# IMPORT AND PRINT THE INITAL COLUMN NAMES
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
print(colnames(data))

# KEEP 'date.of.admission' AS IS
cleaned_data <- standardize_column_names(
  data = data,
  keep = "date.of.admission"
)
print(colnames(data))

# KEEP 'date.of.admission' AS IS, BUT RENAME 'dateOfBirth' AND 'sex' TO
# 'DOB' AND 'gender' RESPECTIVELY
cleaned_data <- standardize_column_names(
  data   = data,
  keep   = "date.of.admission",
  rename = c(DOB = "dateOfBirth", gender = "sex")
)
print(colnames(data))

## ----eval=TRUE, comment="default_missing_values"------------------------------
# VISUALIZE THE PREDEFINED VECTOR OF MISSING CHARACTERS
print(cleanepi::common_na_strings)

## ----eval=TRUE, comment="replace_missing"-------------------------------------
# REPLACE ALL OCCURENCES OF "-99" WITH NA IN THE "sex" COLUMN
cleaned_data <- replace_missing_values(
  data           = readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi")),
  target_columns = "sex",
  na_strings     = "-99"
)

# REPLACE ALL OCCURENCES OF "-99" WITH NA FROM ALL COLUMNS
cleaned_data <- replace_missing_values(
  data           = readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi")),
  target_columns = NULL,
  na_strings     = "-99"
)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
cleaned_data |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## -----------------------------------------------------------------------------
#  orders <- list(
#    world_named_months = c("Ybd", "dby"),
#    world_digit_months = c("dmy", "Ymd"),
#    US_formats         = c("Omdy", "YOmd")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  # GIVE PRIORITY TO AMERICAN-STYLE DATES
#  us_ord <- orders[c(1L, 3L, 2L)]
#  
#  # ADD A FORMAT WITH HOURS TO THE EXISTING orders
#  # THIS WILL ALLOW FOR THE CONVERSION OF VALUES SUCH AS "2014_04_05_23:15:43"
#  # WHEN THEY APPEAR IN THE TARGET COLUMNS.
#  orders$ymdhms <- c("Ymdhms", "Ymdhm")

## ----eval=TRUE, comment="date_standardisation"--------------------------------
# STANDARDIZE VALUES IN THE 'date_first_pcr_positive_test' COLUMN
test_data <- readRDS(system.file("extdata", "test_df.RDS",
                                 package = "cleanepi"))

head(test_data$date_first_pcr_positive_test)

res <- standardize_dates(
  data            = test_data,
  target_columns  = "date_first_pcr_positive_test",
  format          = NULL,
  timeframe       = NULL,
  error_tolerance = 0.4,
  orders          = list(world_named_months = c("Ybd", "dby"),
                         world_digit_months = c("dmy", "Ymd"),
                         US_formats         = c("Omdy", "YOmd")),
  modern_excel    = TRUE
)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
res |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
# STANDARDIZE VALUES IN ALL COLUMNS
res <- standardize_dates(
  data            = test_data,
  target_columns  = NULL,
  format          = NULL,
  timeframe       = NULL,
  error_tolerance = 0.4,
  orders          = list(world_named_months = c("Ybd", "dby"),
                         world_digit_months = c("dmy", "Ymd"),
                         US_formats         = c("Omdy", "YOmd")),
  modern_excel    = TRUE
)

# GET THE REPORT
report <- attr(res, "report")

# DISPLAY DATE VALUES THAT COMPLY WITH MULTIPLE FORMATS
report$multi_format_dates |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----eval=TRUE, comment="subject_ids_standardisation"-------------------------
# DETECT AND REMOVE INCORRECT SUBJECT IDs
res <- check_subject_ids(
  data           = readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi")),
  target_columns = "study_id",
  prefix         = "PS",
  suffix         = "P2",
  range          = c(1L, 100L),
  nchar          = 7L
)

# EXTRACT REPORT
report <- attr(res, "report")

# SUMMARIZE THE REPORT OBJECT
summary(report)

## ----eval=TRUE, comment="correct ids"-----------------------------------------
# IMPORT THE INPUT DATA
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

# GENERATE THE CORRECTION TABLE
correction_table <- data.frame(
  from = c("P0005P2", "PB500P2", "PS004P2-1"),
  to   = c("PB005P2", "PB050P2", "PS004P2"),
  stringsAsFactors = FALSE
)

# PERFORM THE CORRECTION
dat <- correct_subject_ids(
  data             = data,
  target_columns   = "study_id",
  correction_table = correction_table
)

## ----eval=TRUE, comment="check_date_order"------------------------------------
# DETECT ROWS WITH INCORRECT DATE SEQUENCE
res <- check_date_sequence(
  data           = readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi")),
  target_columns = c("date_first_pcr_positive_test", "date.of.admission")
)

# EXTRACT THE REPORT
report <- attr(res, "report")

# SUMMARIZE THE REPORT OBJECT
summary(report)

## ----eval=TRUE, comment="check_date_order"------------------------------------
# CONVERT THE 'age' COLUMN IN THE TEST LINELIST DATA
dat <- readRDS(system.file("extdata", "messy_data.RDS", package = "cleanepi"))
head(dat$age, 10L)
dat <- convert_to_numeric(dat, target_columns = "age", lang = "en")
head(dat$age, 10L)

## ----eval=TRUE, comment=""----------------------------------------------------
# IMPORT A `linelist` DATA
data <- readRDS(system.file("extdata", "test_linelist.RDS",
                            package = "cleanepi"))

# SHOW THE TAGGED VARIABLES
linelist::tags(data)

# FIND DUPLICATES ACROSS ALL COLUMNS EXCEPT THE SUBJECT IDs COLUMN
all_columns    <- names(data)
target_columns <- all_columns[all_columns != "id"]
dups           <- find_duplicates(data           = data,
                                  target_columns = target_columns)

# FIND DUPLICATES ACROSS TAGGED VARIABLES
dups <- find_duplicates(
  data           = data,
  target_columns = "linelist_tags"
)

## ----eval=TRUE----------------------------------------------------------------
# VISUALIZE THE DUPLICATES
report     <- attr(dups, "report")
duplicates <- report$duplicated_rows
duplicates |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = FALSE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----eval=TRUE----------------------------------------------------------------
# REMOVE DUPLICATE ACROSS TAGGED COLUMNS ONLY.
res <- remove_duplicates(
  data           = readRDS(system.file("extdata", "test_linelist.RDS",
                                       package = "cleanepi")),
  target_columns = "linelist_tags"
)

## ----eval=TRUE, comment=""----------------------------------------------------
# ACCESS THE REPORT
report <- attr(res, "report")

# SUMMARIZE THE REPORT OBJECT
summary(report)

## ----eval=TRUE, comment="find_and_remove_dups"--------------------------------
# DETECT DUPLICATES FROM TAGGED COLUMNS
dups <- find_duplicates(
  data           = readRDS(system.file("extdata", "test_linelist.RDS",
                                       package = "cleanepi")),
  target_columns = "linelist_tags"
)

# EXTRACT THE DUPLICATES
report     <- attr(dups, "report")
duplicates <- report$duplicated_rows

# REMOVE FIRST OCCURRENCE OF DUPLICATED ROWS
dups_index_to_remove <- duplicates[["row_id"]][seq(1L, nrow(dups), 2L)]
dups_index_to_remove <- dups_index_to_remove[!is.na(dups_index_to_remove)]
no_dups <- data[-dups_index_to_remove, ]

## ----eval=TRUE, echo=FALSE----------------------------------------------------
test_dictionary |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE)

## ----eval=TRUE----------------------------------------------------------------
# READING IN THE DATA
data <- readRDS(system.file("extdata", "test_df.RDS",
                            package = "cleanepi"))

# ADD THE EXTRA OPTION TO THE DICTIONARY
test_dictionary <- add_to_dictionary(test_dictionary,
                                      option = "-99",
                                      value  = "unknow",
                                      grp    = "sex",
                                      order  = NULL)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
test_dictionary |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE)

## ----eval=TRUE, comment=""----------------------------------------------------
# PERFORM THE DICTIONARY-BASED SUBSTITUTION
cleaned_df <- clean_using_dictionary(
  data       = data,
  dictionary = test_dictionary
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
cleaned_df |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----eval=TRUE----------------------------------------------------------------
# IMPORT DATA, REPLACE MISSING VALUES WITH 'NA' & STANDARDIZE DATES
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")) |>
  replace_missing_values(target_columns = "dateOfBirth",
                         na_strings     = "-99") |>
  standardize_dates(target_columns  = "dateOfBirth",
                    error_tolerance = 0.0)

# CALCULATE INDIVIDUAL AGE IN YEARS FROM THE 'dateOfBirth' COLUMN AND SEND THE
# REMAINDER IN MONTHS
age <- timespan(
  data                = data,
  target_column       = "dateOfBirth",
  end_date            = Sys.Date(),
  span_unit           = "years",
  span_column_name    = "age_in_years",
  span_remainder_unit = "months"
)

# CALCULATE THE TIME SPAN IN YEARS BETWEEN INDIVIDUALS 'dateOfBirth' AND THE DAY
# THEY TESTED POSITIVE
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")) |>
  replace_missing_values(target_columns = "dateOfBirth",
                         na_strings     = "-99") |>
  standardize_dates(
    target_columns  = c("dateOfBirth", "date_first_pcr_positive_test"),
    error_tolerance = 0.0
  ) |>
  timespan(
    target_column       = "dateOfBirth",
    end_date            = "date_first_pcr_positive_test",
    span_unit           = "years",
    span_column_name    = "elapsed_time",
    span_remainder_unit = NULL
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
# DISPLAY THE OUTPUT OBJECT
data |>
  kableExtra::kbl() |>
  kableExtra::kable_paper("striped", font_size = 14, full_width = TRUE) |>
  kableExtra::scroll_box(height = "200px", width = "100%",
                         box_css = "border: 1px solid #ddd; padding: 5px; ",
                         extra_css = NULL,
                         fixed_thead = TRUE)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  print_report(
#    data             = data,
#    report_title     = "{cleanepi} data cleaning report",
#    output_directory = ".",
#    output_filename  = "cleaning_report",
#    format           = "html",
#    print            = TRUE
#  )

