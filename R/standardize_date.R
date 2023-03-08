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
