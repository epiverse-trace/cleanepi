
report_cleaning <- function(original, modified,
                            state = "current", report) {
  if (is.null(report)) {
    report <- list()
  }
  report <- switch(state,
    "empty_remove" = report_remove_empty(report, state, original, modified),
    "remove_constant" = report_remove_constant(state, original, modified,
                                                report),
    "remove_dupliates" = report_remove_dups(report, state, original, modified),
    "standardize_date" = report_dates(report, state, original, modified)
  )
  report
}

report_remove_empty <- function(report, state, original, modified) {
  report[[state]] <- list()
  report[[state]]$columns <- report[[state]]$rows <- NULL

  idx <- which(!(names(original) %in% names(modified)))
  if (length(idx) > 0) {
    report[[state]]$columns <- names(original)[idx]
  }

  if (nrow(summary(arsenal::comparedf(original, modified))$obs.table) > 0) {
    report[[state]]$rows <-
      summary(arsenal::comparedf(original, modified))$obs.table$observation
  }

  if (is.null(report[[state]]$columns) && is.null(report[[state]]$rows)) {
    report[[state]] <- NULL
  }
  report
}

report_remove_constant <- function(state, original, modified, report) {
  report[[state]] <- list()
  report[[state]]$constant_columns <- NULL
  idx <- which(!(names(original) %in% names(modified)))
  if (length(idx) > 0) {
    report[[state]]$constant_columns <- names(original)[idx]
  }

  if (is.null(report[[state]]$constant_columns)) {
    report[[state]] <- NULL
  }
  report
}

report_remove_dups <- function(report, state, original, modified) {
  report[[state]] <- list()
  report[[state]]$duplicates <- NULL

  if (nrow(summary(arsenal::comparedf(original, modified))$obs.table) > 0) {
    report[[state]]$duplicates <-
      summary(arsenal::comparedf(original, modified))$obs.table$observation
  }

  if (is.null(report[[state]]$duplicates)) {
    report[[state]] <- NULL
  }

  report
}

report_dates <- function(report, state, original, modified) {
  if (!(state %in% names(report))) {
    report[[state]] <- list()
    report[[state]]$standardized_date <- NULL
  }

  if (nrow(summary(arsenal::comparedf(original, modified))$vars.nc.table) > 0) {
    report[[state]]$standardized_date <-
      unique(summary(arsenal::comparedf(original,
                                        modified))$vars.nc.table$var.x)
  }

  if (is.null(report[[state]]$standardized_date)) {
    report[[state]] <- NULL
  }
  report
}
