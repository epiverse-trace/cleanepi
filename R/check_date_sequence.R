#' Check the sequence of event dates
#'
#' @description This function should be used to check whether date sequence in the columns specified by the user is correct or not.
#' @param data the input data frame
#' @param event.cols a vector or a comma-separated list of the names of the event columns.
#' Users should specify at least 2 column names. These column names should be provided
#' with the expected order. For example: event.cols = c("DS","DH","DD") where
#' DS=date of the symptoms onset, DH=date of hospitalization, DD=date of death
#' @param remove.bad.seq a Boolean to specify if rows with incorrect order should be filtered out or not. default is TRUE
#' @param
#'
#' @returns a data frame where rows with incorrect date sequence have been removed if `remove.bad.seq=TRUE`, the input data frame if not
#' @export
#'
#' @examples
#'
check_date_sequence <- function(data, event.cols, remove.bad.seq=TRUE){
  # check if input is character string
  if(is.character(event.cols)){
    event.cols = as.character(unlist(strsplit(event.cols,",")))
  }

  # check if all columns are part of the data frame
  if(any(!(event.cols %in% names(data)))){
    idx = which(!(event.cols %in% names(data)))
    event.cols = event.cols[-idx]
    warning("\nRemoving unrecognised column name: ",event.cols[idx])
    if(length(event.cols)<2){
      stop("\nAt least 2 event dates are required!")
    }
  }

  # checking the date sequence
  data = data %>% dplyr::select(dplyr::all_of(event.cols))
  order.date = apply(data, 1, is_order)
  bad.order <- which(!order.date)
  if(any(!order.date)){
    if(remove.bad.seq){
      warning("\nDetected ",length(bad.order)," incorrect date sequence at line: ",paste(bad.order, collapse = ", "))
      data = data[-bad.order,]
    }else{
      message("\nIncorrect date sequence order found at line:", paste(bad.order, collapse = ", "))
    }
  }
  data
}


