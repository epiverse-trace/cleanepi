
report_cleaning <- function(original, modified,
                            state = "current", report = NULL) {
  if (is.null(report)) {
    report <- list()
  }
  report <- switch(state,
    "remove_empty" = report_remove_empty(report, state, original, modified),
    "remove_constant" = report_remove_constant(state, original, modified,
                                               report),
    "remove_dupliates" = report_remove_dups(report, state, original, modified),
    "standardize_date" = report_dates(report, state, original, modified)
  )
  report
}

report_remove_empty <- function(report, state, original, modified) {
  cols <- rows <- NULL
  idx <- which(!(names(original) %in% names(modified)))
  if (length(idx) > 0L) {
    cols <- names(original)[idx]
  }

  if (nrow(summary(arsenal::comparedf(original, modified))[["obs.table"]]) > 0L) { # nolint: line_length_linter
    rows <-
      summary(arsenal::comparedf(original, modified))[["obs.table"]][["observation"]] # nolint: line_length_linter
  }

  if (!is.null(cols)) {
    report[[state]] <- list()
    report[[state]][["columns"]] <- cols
  }
  if (!is.null(rows)) {
    if (state %in% names(report)) {
      report[[state]][["rows"]] <- rows
    } else {
      report[[state]] <- list()
      report[[state]][["rows"]] <- rows
    }
  }

  report
}

report_remove_constant <- function(state, original, modified, report) {
  report[[state]] <- list()
  report[[state]][["constant_columns"]] <- NULL
  idx <- which(!(names(original) %in% names(modified)))
  if (length(idx) > 0L) {
    report[[state]][["constant_columns"]] <- names(original)[idx]
  }

  if (is.null(report[[state]][["constant_columns"]])) {
    report[[state]] <- NULL
  }
  report
}

report_remove_dups <- function(report, state, original, modified) {
  report[[state]] <- list()
  report[[state]][["duplicates"]] <- NULL

  if (nrow(summary(arsenal::comparedf(original, modified))[["obs.table"]]) > 0L) { # nolint: line_length_linter
    report[[state]][["duplicates"]] <-
      summary(arsenal::comparedf(original, modified))[["obs.table"]][["observation"]] # nolint: line_length_linter
  }

  if (is.null(report[[state]][["duplicates"]])) {
    report[[state]] <- NULL
  }

  report
}

report_dates <- function(report, state, original, modified) {
  if (!(state %in% names(report))) {
    report[[state]] <- list()
    report[[state]][["standardized_date"]] <- NULL
  }

  if (nrow(summary(arsenal::comparedf(original, modified))[["vars.nc.table"]]) > 0L) { # nolint: line_length_linter
    report[[state]][["standardized_date"]] <-
      unique(summary(arsenal::comparedf(original,
                                        modified))[["vars.nc.table"]][["var.x"]]) # nolint: line_length_linter
  }

  if (is.null(report[[state]][["standardized_date"]])) {
    report[[state]] <- NULL
  }
  report
}
