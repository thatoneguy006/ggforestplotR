normalize_table_formatters <- function(formatters, data = NULL) {
  if (is.null(formatters)) {
    return(list())
  }
  if (!is.list(formatters) || is.null(names(formatters)) ||
      anyNA(names(formatters)) || any(!nzchar(names(formatters)))) {
    stop("`formatters` must be a named list of functions with non-empty names.", call. = FALSE)
  }
  keys <- names(formatters)
  keys[keys == "p.value"] <- "p"
  if (anyDuplicated(keys)) {
    stop("`formatters` names must be unique after alias normalization.", call. = FALSE)
  }
  source_names <- if (inherits(data, "forest_data")) {
    names(forest_source_columns(data))
  } else if (!is.null(data)) {
    names(data)
  } else {
    character()
  }
  allowed <- unique(c(
    "estimate", "ci", "conf.low", "conf.high", "p", "n", "events", "group",
    setdiff(source_names, c("term", "label"))
  ))
  unknown <- setdiff(keys, allowed)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown formatter target(s): %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  if (!all(vapply(formatters, is.function, logical(1)))) {
    stop("Every `formatters` entry must be a function.", call. = FALSE)
  }
  names(formatters) <- keys
  formatters
}

resolve_table_formatter <- function(formatters, key) {
  formatter <- formatters[[key]]
  if (is.null(formatter) && key %in% c("conf.low", "conf.high")) {
    formatter <- formatters[["ci"]]
  }
  formatter
}

apply_table_formatter <- function(values, formatter = NULL, fallback = as.character,
                                  key = "value") {
  result <- tryCatch(
    if (is.null(formatter)) fallback(values) else formatter(values),
    error = function(e) {
      stop(sprintf("Formatter for `%s` failed: %s", key, conditionMessage(e)),
           call. = FALSE)
    }
  )
  if (!is.atomic(result)) {
    stop(sprintf("Formatter for `%s` must return an atomic vector.", key), call. = FALSE)
  }
  if (length(result) != length(values)) {
    stop(sprintf("Formatter for `%s` must return one value per input value.", key), call. = FALSE)
  }
  result <- as.character(result)
  result[is.na(result)] <- ""
  result
}
