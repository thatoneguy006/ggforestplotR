resolve_column <- function(data, column, arg, required = TRUE) {
  if (is.null(column)) {
    if (required) {
      stop(sprintf("`%s` must be supplied.", arg), call. = FALSE)
    }

    return(NULL)
  }

  if (is.character(column) && length(column) == 1L) {
    resolved <- column
  } else {
    expr <- substitute(column)

    if (!is.symbol(expr)) {
      stop(sprintf("`%s` must be a single column name.", arg), call. = FALSE)
    }

    resolved <- deparse(expr)
  }

  if (!resolved %in% names(data)) {
    stop(sprintf("Column `%s` was not found in `data`.", resolved), call. = FALSE)
  }

  resolved
}

validate_forest_data <- function(data, exponentiate = FALSE) {
  required <- c("term", "estimate", "conf.low", "conf.high", "label", "group", "p.value")
  missing <- setdiff(required, names(data))

  if (length(missing) > 0) {
    stop(
      sprintf("Missing required forest data columns: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  numeric_cols <- c("estimate", "conf.low", "conf.high")

  for (col in numeric_cols) {
    if (anyNA(data[[col]])) {
      stop(sprintf("Column `%s` cannot contain missing values.", col), call. = FALSE)
    }
  }

  if (any(data$conf.low > data$conf.high)) {
    stop("`conf.low` cannot be greater than `conf.high`.", call. = FALSE)
  }

  if (isTRUE(exponentiate) && any(data$conf.low <= 0 | data$conf.high <= 0 | data$estimate <= 0)) {
    stop(
      "Exponentiated forest plots require strictly positive `estimate`, `conf.low`, and `conf.high` values.",
      call. = FALSE
    )
  }

  invisible(data)
}
