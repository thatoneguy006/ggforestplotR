forest_row_types <- function() {
  c("estimate", "reference", "header", "summary", "spacer")
}

forest_geometry_row_types <- function() {
  c("estimate", "summary")
}

normalize_forest_row_type <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("`row_type` must contain character or factor values.", call. = FALSE)
  }
  values <- tolower(trimws(as.character(x)))
  bad <- unique(values[is.na(values) | !values %in% forest_row_types()])
  if (length(bad) > 0L) {
    quoted <- ifelse(is.na(bad), "NA", sprintf('"%s"', bad))
    stop(sprintf(
      "Unsupported `row_type` value(s): %s. Allowed values are: %s.",
      paste(quoted, collapse = ", "),
      paste(sprintf('"%s"', forest_row_types()), collapse = ", ")
    ), call. = FALSE)
  }
  values
}
