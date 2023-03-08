#' Clean data
#' @description this function is the first function to be applied on a data frame with "dirty data".
#' It will clean up the column names, detect and remove duplicates, remove empty records
#' and columns, remove constant columns, replace missing value by NA.
#'
#' @param data the input data frame
#' @param clean.col.names whether to cleanup the column name or not. default is FALSE
#' @param remove.duplicates whether to detect duplicated records or not. default is FALSE
#' @param duplicates.from a vector of columns names to use when looking for duplicates. Only used when `remove.duplicates=TRUE`
#' @param remove.empty whether to remove the records and column or not. default is FALSE
#' @param remove.constant whether to remove constant columns or not. default is FALSE
#' @param replace.missing whether to replace the missing value characters with NA or not. default is FALSE
#' @param na.comes.as the characters that represent the missing data in the data frame. only used when `replace.missing=TRUE`
#' @param replace.na.at a vector of columns where the characters that represent the missing data should be replaced by NA. only used when `replace.missing=TRUE`
#' @param replace.na.in.type a string that specify on which column type to replace the characters that represent the missing data. If missing characters should be replaced by NA in character columns, the value is `character`
#'
#' @return a data frame with the records and columns that have passed the above checks.
#' @export
#'
#' @examples
cleanepi = function(data, clean.col.names=FALSE,
                    remove.duplicates=FALSE, duplicates.from=NULL,
                    remove.empty=FALSE, remove.constant=FALSE,
                    replace.missing=FALSE, na.comes.as=NULL, replace.na.at=NULL,replace.na.in.type=NULL){
  # clean the column names based on janitor package
  if(clean.col.names){
    cat("\nCleaning the column names")
    data = data %>% janitor::clean_names()
  }

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
