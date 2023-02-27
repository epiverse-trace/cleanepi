

#' function to clean the subject ID column in a data frame
#' @param data the data frame of interest
#' @param id.position the column position of the variable that unique identifies the subjects. This should only be specified when the column with the subject IDs is not the first column. default is 1.
#' @param format the correct format for the subject IDs
#' @param check a boolean that specifies whether to clean the subject IDs or not
#' @param prefix the prefix used in the subject IDs
#' @param suffix the prefix used in the subject IDs
#' @param range a vector with the range of numbers in the sample IDs
#' @returns the input data frame with the corrected subject IDs
#' @examples
#' @export
clean_subject_ids = function(data=NULL, id.position=1, format=NULL,
                             check=TRUE, prefix=NULL,suffix=NULL,range=NULL){
  if (is.null(data)) {
    stop("Must specify data frame from which subject IDs will be cleaned!")
  }
  if(check){
    if(is.null(format)){
      stop("Please provide a template format for the sample IDs!")
    }
    subject.id.col.name = names(data)[id.position]

    # check prefix of subject IDs
    if(!is.null(prefix)){
      R.utils::cat("\nChecking prefix of subject IDs")
      prefix.check = as.logical(as.character(lapply(data[[subject.id.col.name]],checkPrefix,prefix)))
      idx=which(!(prefix.check))
      if(length(idx)>0){
        failed.prefix = data[[subject.id.col.name]][idx]
      }
      message("Detected sample IDs with wrong prefix\n")
      R.utils::cat(paste(failed.prefix,collapse = ", "))
    }

    # check suffix of subject IDs
    if(!is.null(suffix)){
      R.utils::cat("\nChecking suffix of subject IDs")
      suffix.check = as.logical(as.character(lapply(data[[subject.id.col.name]],checkSuffix,suffix)))
      idx=which(!(suffix.check))
      if(length(idx)>0){
        failed.suffix = data[[subject.id.col.name]][idx]
      }
      message("Detected sample IDs with wrong suffix\n")
      R.utils::cat(paste(failed.suffix,collapse = ", "))
    }

    # check subject IDs that do not match the provided format
    length.check = as.logical(as.character(lapply(data[[subject.id.col.name]],checkIDlength,format)))
    idx=which(!(length.check))
    if(length(idx)>0){
      failed.length = data[[subject.id.col.name]][idx]
    }
    message("Detected sample IDs with wrong incorrect length\n")
    R.utils::cat(paste(failed.length,collapse = ", "))

    # check the numbers in the sample IDs
    if(!is.null(range) & length(range)==2){
      numbers.in = as.numeric(unlist(lapply(data[[subject.id.col.name]], readr::parse_number)))
      idx = which(numbers.in>=min(range) & numbers.in<=max(range))
      if(length(idx)>0){
        failed.range = data[[subject.id.col.name]][idx]
      }
      message("Detected sample IDs with wrong numbers\n")
      R.utils::cat(paste(failed.range,collapse = ", "))
    }

  }
  data
}

#' function to standardize date variable
#' @param data a data frame with the date column to standardise
#' @param date.column.name the name of the date column of interest. default: 'Date', or 'DATE', or 'date'
#' @param format the format of the date values in the date column
#' @returns a data frame where the specified date column is converted into the format 'yyyy-mm-dd'
#' @examples
#' @export
standardize_date = function(data, date.column.name=NULL, format=NULL){
  # check input data format
  date.column.name = check_input_data(data, date.column.name)

  # standardize it
  sep = unique(as.character(unlist(lapply(data[[date.column.name]],detect_date_separator))))
  if(is.null(format)){
    data[[date.column.name]] = as.character(data[[date.column.name]])
      format = NULL
      part1 = as.character(unlist(lapply(data[[date.column.name]], get_part1, sep[1])))
      part2 = as.character(unlist(lapply(data[[date.column.name]], get_part2, sep[1])))
      part3 = as.character(unlist(lapply(data[[date.column.name]], get_part3, sep[1])))
      if(!all(is.na(part1)) & !all(is.na(part2)) & !all(is.na(part3))){
        f1 = detect_date_format(part1)
        f2 = detect_date_format(part2)
        f3 = detect_date_format(part3)
        format = paste0(format,f1,sep[1],f2,sep[1],f3)
      }else if(!all(is.na(part1)) & !all(is.na(part2)) & all(is.na(part3))){
        f1 = detect_date_format(part1)
        f2 = detect_date_format(part2)
        format = paste0(format,f1,sep[1],f2)
      }else if(!all(is.na(part1)) & all(is.na(part2)) & all(is.na(part3))){
        f1 = detect_date_format(part1)
        format = paste0(format,f1)
      }else{
        stop("Unrecognised date format.\nPlease specify the date format using the 'format' argument.")
      }
  }
  data[[date.column.name]] = as.Date(data[[date.column.name]], format = format)
  data
}


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


#' function to clean epidemiological data
<<<<<<< HEAD
#' @export
=======
#'
>>>>>>> 7cd62df02662e2cd55da7f796ed13301699ba3ff
cleanepi = function(data, clean.col.names=FALSE,
                    standardise.date=FALSE, date.columns=NULL, format=NULL,
                    remove.duplicates=FALSE, duplicates.from=NULL,
                    remove.empty=FALSE, remove.constant=FALSE,
                    replace.missing=FALSE, na.comes.as=NULL, replace.na.at=NULL,replace.na.in.type=NULL){
  # clean the column names based on janitor package
  if(clean.col.names){
    cat("\nCleaning the column names")
    data = data %>% janitor::clean_names()
  }

  # standardize date columns
  # if(standardise.date){
  #   cat("\nStandardizing date columns")
  #   if(length(date.columns)>0){
  #     for(date.col in date.columns){
  #       data = standardize_date(data, date.column.name = date.col, format = format)
  #     }
  #   }
  # }

  # remove duplicated records
  if(remove.duplicates){
    cat("\nRemoving duplicated records")
    if(!is.null(duplicates.from)){
      data = data %>% dplyr::distinct(dplyr::pick(duplicates.from), .keep_all = TRUE)
      # dup.rows = data %>% janitor::get_dupes(dplyr::all_of(duplicates.from))
    }else{
      data = data %>% dplyr::distinct()
    }
  }

  # remove empty records and columns
  if(remove.empty){
    cat("\nRemoving empty records and columns.")
    data = data %>%
      janitor::remove_empty(c("rows", "cols"))
  }

  # remove constant columns
  if(remove.constant){
    cat("\nRemoving constant columns i.e. columns with same values across all records.")
    data = data %>% janitor::remove_constant()
  }

  # replace missing values with NA
  if(replace.missing){
    cat("\nReplacing missing values with 'NA'.")
    common.na.strings = c(naniar::common_na_strings, "not available","Not Available","NOt available",
                          "not avail","Not Avail","nan","NAN","not a number","Not A Number")
    if(!is.null(replace.na.in.type)){
      if(replace.na.in.type %in% c('numeric','character','factor')){
        if(!is.null(na.comes.as)){
          data = switch(replace.na.in.type,
                       'numeric' = data %>% naniar::replace_with_na_if(.predicate = is.numeric,
                                                               condition = ~.x %in% (na.comes.as)),
                       'character' = data %>% naniar::replace_with_na_if(.predicate = is.character,
                                                                       condition = ~.x %in% (na.comes.as)),
                       'factor' = data %>% naniar::replace_with_na_if(.predicate = is.factor,
                                                                         condition = ~.x %in% (na.comes.as))
          )
        }else{
          data = switch(replace.na.in.type,
                        'numeric' = data %>% naniar::replace_with_na_if(.predicate = is.numeric,
                                                                        condition = ~.x %in% c(common.na.strings, naniar::common_na_numbers)),
                        'character' = data %>% naniar::replace_with_na_if(.predicate = is.character,
                                                                          condition = ~.x %in% c(common.na.strings, naniar::common_na_numbers)),
                        'factor' = data %>% naniar::replace_with_na_if(.predicate = is.factor,
                                                                       condition = ~.x %in% c(common.na.strings, naniar::common_na_numbers))
          )
        }
      }else{
        stop("Incorrect value for 'replace.na.in.type' argument.\n Value should be one of: 'numeric','character','factor'")
      }
    }
    if(is.null(replace.na.at)){
      if(!is.null(na.comes.as)){
        data = data %>%
          naniar::replace_with_na_all(condition = ~.x %in% (na.comes.as))
      }else{
        data = data %>%
          naniar::replace_with_na_all(condition = ~.x %in% c(common.na.strings, naniar::common_na_numbers))
      }
    }else{
      if(is.character(replace.na.at)){
        replace.na.at = as.character(unlist(strsplit(replace.na.at,",")))
      }
      if(!is.null(na.comes.as)){
        data = data %>%
          naniar::replace_with_na_at(.vars=replace.na.at,
                                     condition = ~.x %in% na.comes.as)
      }else{
        data = data %>%
          naniar::replace_with_na_at(.vars=replace.na.at,
                                     condition = ~.x %in% c(common.na.strings,
                                                            naniar::common_na_numbers))
      }
    }

  }



  data
}












