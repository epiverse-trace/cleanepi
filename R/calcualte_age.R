#' function to calculate age from date of birth
#' @param data a data frame with the date column to standardise
#' @param date.column.name the name of the date column of interest. default: 'Date', or 'DATE', or 'date'
#' @param end.date the end date. default: today's date
#' @param format the format of the date values in the date column
#' @param age.in a character string to specify whether to calculate the age in 'years', or 'months', or 'days', or 'weeks'. default is: 'years'
#' @importFrom lubridate %--%
#' @returns a data frame with the bellow 1 or 2 extra columns compared to the input data frame
#' \enumerate{
#'   \item "age_years", or "age_months", or "age_weeks", or "age_days", depending on the value of the 'age.in' parameter
#'   \item "remainder_days": a column with the number of remaining days after the age is converted in weeks or months
#'   }
#' @export
#' @examples
calculate_age = function(data, date.column.name=NULL, end.date=Sys.Date(),
                         format=NULL, age.in="years"){
  # check input data format
  date.column.name = check_input_data(data, date.column.name)

  # standardise the input data if required
  if(class(data[[date.column.name]]) != "Date"){
    data = standardize_date(data, date.column.name=date.column.name, format=format)
  }

  # calculate age
  if(!(age.in %in% c('years','months','weeks','days'))){
    stop("Incorrect value for 'age.in' parameter.\nPlease specify whether the age should be either in 'year', or 'month', or in 'weeks', or in 'days'.")
  }
  end.date = as.Date(end.date)
  res = switch(age.in,
               'years' = data %>% dplyr::mutate(age_years = round((data[[date.column.name]] %--% end.date) %/% lubridate::years(1))),
               'months' = data %>%
                 dplyr::mutate(tmp_age = lubridate::as.period(end.date-data[[date.column.name]])) %>%
                 dplyr::mutate(age_months = tmp_age %/% months(1), remainder_days= (tmp_age %% months(1)) %/% lubridate::days(1)) %>%
                 dplyr::select(-tmp_age),
               'days' = data %>%
                 dplyr::mutate(tmp_age = lubridate::as.period(end.date-data[[date.column.name]])) %>%
                 dplyr::mutate(age_days = tmp_age %/% lubridate::days(1)) %>%
                 dplyr::select(-tmp_age),
               'weeks' = data %>%
                 dplyr::mutate(tmp_age = lubridate::as.period(end.date-data[[date.column.name]])) %>%
                 dplyr::mutate(age_weeks = tmp_age %/% lubridate::weeks(1),
                               remainder_days= (tmp_age %% lubridate::weeks(1)) %/% lubridate::days(1)) %>%
                 dplyr::select(-tmp_age)
  )
  # dplyr::mutate(age_weeks = abs(difftime(end.date, data[[date.column.name]], units = "weeks"))))
  if(age.in %in% c("months","weeks")){
    if(all(res$remainder_days == 0)){
      res = res %>% dplyr::select(-remainder_days)
    }
  }
  res
}
