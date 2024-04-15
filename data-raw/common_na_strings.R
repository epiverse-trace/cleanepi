## below is the code used to prepare `common_na_strings` dataset
## Note that this a combination of the `naniar::common_na_strings` and other
## strings that are frequently encountered. This will be updated as time goes
## based on user's suggestions.

common_na_strings <- c(naniar::common_na_strings,
                       "not available", "Not Available", "NOt available",
                       "not avail", "Not Avail", "nan", "NAN", "not a number",
                       "Not A Number")

usethis::use_data(common_na_strings, overwrite = TRUE)
