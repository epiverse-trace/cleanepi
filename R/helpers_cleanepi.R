
# function to check the length of sample IDs
checkIDlength = function(x, ref){
  res=TRUE
  if(nchar(ref)<nchar(x)){
    res=FALSE
  }
  res
}

#' function to check the prefix of the subject IDs
checkPrefix = function(x, prefix){
  res=TRUE
  prefix.found.at = stringr::str_locate(x, prefix)
  if(all(is.na(prefix.found.at[1,])) | (prefix.found.at[1,1]!=1 & prefix.found.at[1,2]!=nchar(prefix))){
    res=FALSE
  }
  res
}

#' function to check the suffix of the subject IDs
checkSuffix = function(x, suffix){
  res=TRUE
  suffix.found.at = as.matrix(stringr::str_locate_all(x, suffix)[[1]])
  if(all(is.na(suffix.found.at[nrow(suffix.found.at),])) | (suffix.found.at[nrow(suffix.found.at),1]!=(nchar(x)-(nchar(suffix)-1)) & suffix.found.at[nrow(suffix.found.at),2]!=nchar(x))){
    res=FALSE
  }
  res
}

#' function to check the input data
check_input_data = function(data, date.column.name){
  # check the input data
  if (!any((class(data) %in% c("data.frame","data.table","tibble")))) {
    stop("Incorrect input data type")
  }

  # check the column name
  if(is.null(date.column.name)){
    idx = which(names(data) %in% c('Date', 'DATE', 'date'))
    if(length(idx)==0){
      stop("Could not find column named as ", paste(c('Date', 'DATE', 'date'), collapse = " "),"\nPlease specify the date column name.")
    }
    date.column.name = names(data)[idx]
  }

  # check whether the provided column name belong to the data
  if(!(date.column.name %in% names(data))){
    stop("Could not find column named as ",date.column.name)
  }
  date.column.name
}


#' function to detect the special character that is the separator in the date values
detect_date_separator = function(x){
  special.characters = c('-','/',',',' ')  # look for other common sep in date values
  sep=NULL
  for(spec.char in special.characters){
    if(stringr::str_detect(x,spec.char)){
      sep = c(sep,spec.char)
    }
  }

  if(is.null(sep)){  #| length(sep)>1
    stop("date format not recognised. Please specify the date format using the 'format' argument.")
  }
  sep
}

#' functions to split up the date value
get_part1 = function(x, sep){unlist(strsplit(x,sep))[1]}
get_part2 = function(x, sep){unlist(strsplit(x,sep))[2]}
get_part3 = function(x, sep){unlist(strsplit(x,sep))[3]}

#' detect whether a string contains only numbers
numbers_only = function(x) !grepl("\\D", x)

#' extract letters from a string
only_letters = function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }

#' get sum from number
get_sum = function(x){
  if(nchar(x)==2) x = sum(as.numeric(substr(x,1,1)),as.numeric(substr(x,2,2)))
  x
}

#' function to get simple format
detect_simple_format = function(x){
  f1 = NULL
  if(all(nchar(x)==4)){
    f1='%Y' # year with century i.e 4 digits year
  }else if(any(nchar(x)==4) & any(nchar(x)==2)){
    stop("Detected different lengths in first digits of date column.\nPlease use same number of digits or specify the date format with the 'format' argument.")
  }else if(all(nchar(x)==2)){
    tmp = as.numeric(x)
    if(all(tmp<=12)){
      f1='%m'
    }else if(all(tmp>=1) & all(tmp<=31)){
      f1='%d'
    }else{
      f1='%y'
    }
  }
  f1
}

#' function to detect whether it's day or month
detect_day_or_month = function(x){
  f1=NULL
  full.days = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  abreviated.days = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  if(all(x %in% month.name) |
     all(x %in% toupper(month.name)) |
     all(x %in% tolower(month.name))){
    f1 = "%B"
  }else if(all(x %in% month.abb) |
           all(x %in% toupper(month.abb)) |
           all(x %in% tolower(month.abb))){
    f1 = "%b"
  }else if(all(x %in% full.days) |
           all(x %in% toupper(full.days)) |
           all(x %in% tolower(full.days))){
    f1 = "%A"
  }else if(all(x %in% abreviated.days) |
           all(x %in% toupper(abreviated.days)) |
           all(x %in% tolower(abreviated.days))){
    f1 = "%a"
  }
  f1
}

#' function to detect complex format
detect_complex_format = function(x){
  tmp.sep = unique(as.character(lapply(x, detect_date_separator)))
  if(!is.na(tmp.sep) & length(tmp.sep)==1){
    p1 = as.character(unlist(lapply(x, get_part1, tmp.sep)))
    p2 = as.character(unlist(lapply(x, get_part2, tmp.sep)))
    f1 = detect_simple_format(p1)
    if(is.null(f1)){
      f1 = detect_day_or_month(p1)
    }
    f2 = detect_simple_format(p2)
    if(is.null(f2)){
      f2 = detect_day_or_month(p2)
    }
  }else{
    stop("Unrecognised date format.\nPlease specify the date format using the 'format' argument.")
  }
  if(is.null(f1) & is.null(f2)){
    stop("Unrecognised date format.\nPlease specify the date format using the 'format' argument.")
  }else if(!is.null(f1) & !is.null(f2)){
    format = paste0(f1,tmp.sep,f2)
  }else if(!is.null(f1) & is.null(f2)){
    format=f1
  }else if(!is.null(f2) & is.null(f1)){
    format=f2
  }

  format
}

#' function to detect the date format with only 1 separator
detect_date_format = function(x){
  # check the format in x
  if(all(numbers_only(x))){
    f1 = detect_simple_format(x)
  }else{
    f1 = detect_complex_format(x)
  }
  f1
}





