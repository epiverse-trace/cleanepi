#' Check whether the subject IDs complies with user-specified format
#' @param data the data frame of interest
#' @param id.position the column position of the variable that unique identifies the subjects. This should only be specified when the column with the subject IDs is not the first column. default is 1.
#' @param format a template of the correct format for the subject IDs
#' @param check a boolean that specifies whether to clean the subject IDs or not
#' @param prefix the prefix used in the subject IDs
#' @param suffix the prefix used in the subject IDs
#' @param range a vector with the range of numbers in the sample IDs
#' @returns The will display messages about incorrect subject IDs, nothing if all IDs are correct.
#' @examples
#' check_subject_ids(data=data.table::fread(system.file("extdata","test.txt", package = "cleanepi")),
#'                   id.position=1,
#'                   format="PS000P2",
#'                   check=TRUE,
#'                   prefix="PS",
#'                   suffix="P2",
#'                   range=c(1,100)
#'                   )
#' @export
check_subject_ids = function(data=NULL, id.position=1, format=NULL,
                             check=TRUE, prefix=NULL,suffix=NULL,range=NULL){
  checkmate::assert_data_frame(data, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_number(id.position, lower = 1, null.ok = FALSE)
  checkmate::assert_character(format, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(prefix, len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(suffix, len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_vector(range,
                           any.missing = FALSE, min.len = 2,
                           null.ok = TRUE, unique = TRUE,max.len = 2
  )
  checkmate::assert_logical(check, any.missing = FALSE, len = 1, null.ok = TRUE)
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
      # R.utils::cat("\nChecking prefix of subject IDs")
      prefix.check = as.logical(as.character(lapply(data[[subject.id.col.name]],checkPrefix,prefix)))
      idx=which(!(prefix.check))
      if(length(idx)>0){
        failed.prefix = data[[subject.id.col.name]][idx]
      }
      message("\nSample IDs with wrong prefix: ",paste(failed.prefix,collapse = ", "))
    }

    # check suffix of subject IDs
    if(!is.null(suffix)){
      # R.utils::cat("\nChecking suffix of subject IDs")
      suffix.check = as.logical(as.character(lapply(data[[subject.id.col.name]],checkSuffix,suffix)))
      idx=which(!(suffix.check))
      if(length(idx)>0){
        failed.suffix = data[[subject.id.col.name]][idx]
      }
      message("\nSample IDs with wrong suffix: ", paste(failed.suffix,collapse = ", "))
    }

    # check subject IDs that do not match the provided format
    length.check = as.logical(as.character(lapply(data[[subject.id.col.name]],checkIDlength,format)))
    idx=which(!(length.check))
    if(length(idx)>0){
      failed.length = data[[subject.id.col.name]][idx]
    }
    message("\nSample IDs with wrong incorrect length: ",paste(failed.length,collapse = ", "))

    # check the numbers in the sample IDs
    if(!is.null(range) & length(range)==2){
      numbers.in = as.numeric(unlist(lapply(data[[subject.id.col.name]], readr::parse_number)))
      idx = which(!(numbers.in>=min(range) & numbers.in<=max(range)))
      if(length(idx)>0){
        failed.range = data[[subject.id.col.name]][idx]
      }
      message("\nSample IDs with wrong numbers: ", paste(failed.range,collapse = ", "))
    }
  }
}
