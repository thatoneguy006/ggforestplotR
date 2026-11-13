normalize_table_formatters <- function(formatters, data = NULL,
                                       error_call = rlang::caller_env()) {
  if (is.null(formatters)) {
    return(list())
  }
  if (!is.list(formatters) || is.null(names(formatters)) ||
    anyNA(names(formatters)) || any(!nzchar(names(formatters)))) {
    cli::cli_abort(
      "{.arg formatters} must be a named list of functions with non-empty names.",
      class = c("ggforestplotR_invalid_formatter", "ggforestplotR_error"),
      call = error_call
    )
  }
  keys <- names(formatters)
  keys[keys == "p.value"] <- "p"
  if (anyDuplicated(keys)) {
    cli::cli_abort(
      "{.arg formatters} names must be unique after alias normalization.",
      class = c("ggforestplotR_invalid_formatter", "ggforestplotR_error"),
      call = error_call
    )
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
    cli::cli_abort(
      "Unknown formatter target(s): {.val {unknown}}.",
      class = c("ggforestplotR_invalid_formatter", "ggforestplotR_error"),
      call = error_call
    )
  }
  if (!all(vapply(formatters, is.function, logical(1)))) {
    cli::cli_abort(
      "Every {.arg formatters} entry must be a function.",
      class = c("ggforestplotR_invalid_formatter", "ggforestplotR_error"),
      call = error_call
    )
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
                                  key = "value", error_call = rlang::caller_env()) {
  result <- tryCatch(
    if (is.null(formatter)) fallback(values) else formatter(values),
    error = function(cnd) {
      cli::cli_abort(
        "Formatter for {.arg {key}} failed.",
        parent = cnd,
        class = c("ggforestplotR_formatter_error", "ggforestplotR_error"),
        call = error_call
      )
    }
  )
  if (!is.atomic(result)) {
    cli::cli_abort(
      "Formatter for {.arg {key}} must return an atomic vector.",
      class = c("ggforestplotR_invalid_formatter_result", "ggforestplotR_error"),
      call = error_call
    )
  }
  if (length(result) != length(values)) {
    cli::cli_abort(
      c(
        "Formatter for {.arg {key}} must return one value per input value.",
        "x" = "Expected {length(values)} values, but received {length(result)}."
      ),
      class = c("ggforestplotR_invalid_formatter_result", "ggforestplotR_error"),
      call = error_call
    )
  }
  result <- as.character(result)
  result[is.na(result)] <- ""
  result
}

default_table_value_formatter <- function(digits) {
  function(values) {
    if (!is.numeric(values)) {
      return(as.character(values))
    }
    out <- rep(NA_character_, length(values))
    present <- !is.na(values)
    whole <- present & is.finite(values) &
      abs(values - round(values)) < sqrt(.Machine$double.eps)
    out[whole] <- formatC(round(values[whole]), format = "f", digits = 0)
    other <- present & !whole
    out[other] <- formatC(values[other], format = "f", digits = digits)
    out
  }
}

# --- Formatting helpers ------------------------------------------------------

#' Shared logic for collapsing formatted values with optional group labels.
#'
#' All three public formatters (p-values, estimates, n-values) produce a
#' character vector of per-row formatted strings, then need identical
#' deduplication / group-prefixing / newline-collapsing.  This function
#' handles that single concern.
#' @keywords internal
#' @noRd
collapse_grouped_values <- function(formatted,
                                    group = NULL,
                                    force_group_labels = FALSE,
                                    align_groups = FALSE) {
  group <- as.character(group)

  if (isTRUE(align_groups)) {
    formatted[is.na(formatted)] <- ""
    return(paste(formatted, collapse = "\n"))
  }

  keep <- !is.na(formatted) & nzchar(formatted)

  if (!any(keep)) {
    return("")
  }

  non_empty <- formatted[keep]

  if (all(is.na(group) | !nzchar(group))) {
    if (length(unique(non_empty)) == 1L) {
      return(non_empty[1L])
    }

    return(paste(non_empty, collapse = "\n"))
  }

  group_labels <- ifelse(
    is.na(group) | !nzchar(group),
    paste0("Series ", seq_along(formatted)),
    group
  )

  if (!isTRUE(force_group_labels) && length(unique(non_empty)) == 1L) {
    return(non_empty[1L])
  }

  paste(paste0(group_labels[keep], ": ", formatted[keep]), collapse = "\n")
}

format_forest_p_values <- function(values, group = NULL, digits = 2, p_digits = digits,
                                   force_group_labels = FALSE, align_groups = FALSE,
                                   formatter = NULL) {
  p_digits <- resolve_table_digits(digits = digits, p_digits = p_digits)$p_digits
  values <- as.numeric(values)
  eps <- 10^(-p_digits)
  fallback <- function(values) ifelse(
    is.na(values),
    "",
    ifelse(values < eps, paste0("<", sprintf(paste0("%.", p_digits, "f"), eps)),
      sprintf(paste0("%.", p_digits, "f"), values)
    )
  )
  formatted <- apply_table_formatter(values, formatter, fallback, key = "p")
  collapse_grouped_values(
    formatted,
    group,
    force_group_labels = force_group_labels,
    align_groups = align_groups
  )
}

format_forest_estimates <- function(estimate, conf.low, conf.high,
                                    group = NULL, digits = 2,
                                    estimate_digits = digits,
                                    interval_digits = digits,
                                    estimate_fmt = NULL,
                                    force_group_labels = FALSE,
                                    align_groups = FALSE,
                                    estimate_formatter = NULL,
                                    conf_low_formatter = NULL,
                                    conf_high_formatter = NULL) {
  digits <- resolve_table_digits(
    digits = digits,
    estimate_digits = estimate_digits,
    interval_digits = interval_digits
  )
  if (is.null(estimate_fmt)) {
    estimate_fmt <- "{estimate} ({conf.low}, {conf.high})"
  }
  if (!is.character(estimate_fmt) || length(estimate_fmt) != 1L || is.na(estimate_fmt)) {
    abort_forest(
      "`estimate_fmt` must be a single character string.",
      class = "ggforestplotR_invalid_table"
    )
  }

  estimate_text <- apply_table_formatter(estimate, estimate_formatter,
    function(x) sprintf(paste0("%.", digits$estimate_digits, "f"), x), "estimate")
  conf_low_text <- apply_table_formatter(conf.low, conf_low_formatter,
    function(x) sprintf(paste0("%.", digits$interval_digits, "f"), x), "conf.low")
  conf_high_text <- apply_table_formatter(conf.high, conf_high_formatter,
    function(x) sprintf(paste0("%.", digits$interval_digits, "f"), x), "conf.high")

  formatted <- purrr::pmap_chr(
    list(conf_low_text, conf_high_text, estimate_text),
    function(lower, upper, estimate) {
      format_interval_template(estimate_fmt, lower, upper, estimate)
    }
  )
  collapse_grouped_values(
    formatted,
    group,
    force_group_labels = force_group_labels,
    align_groups = align_groups
  )
}

format_forest_intervals <- function(conf.low, conf.high,
                                    group = NULL, digits = 2,
                                    interval_digits = digits,
                                    ci_fmt = NULL,
                                    force_group_labels = FALSE,
                                    align_groups = FALSE,
                                    conf_low_formatter = NULL,
                                    conf_high_formatter = NULL) {
  digits <- resolve_table_digits(
    digits = digits,
    interval_digits = interval_digits
  )
  if (is.null(ci_fmt)) {
    ci_fmt <- "({conf.low}, {conf.high})"
  }
  if (!is.character(ci_fmt) || length(ci_fmt) != 1L || is.na(ci_fmt)) {
    abort_forest(
      "`ci_fmt` must be a single character string.",
      class = "ggforestplotR_invalid_table"
    )
  }

  conf_low_text <- apply_table_formatter(conf.low, conf_low_formatter,
    function(x) sprintf(paste0("%.", digits$interval_digits, "f"), x), "conf.low")
  conf_high_text <- apply_table_formatter(conf.high, conf_high_formatter,
    function(x) sprintf(paste0("%.", digits$interval_digits, "f"), x), "conf.high")

  formatted <- purrr::map2_chr(
    conf_low_text,
    conf_high_text,
    function(lower, upper) format_interval_template(ci_fmt, lower, upper)
  )
  collapse_grouped_values(
    formatted,
    group,
    force_group_labels = force_group_labels,
    align_groups = align_groups
  )
}

format_interval_template <- function(template, lower, upper, estimate = NULL) {
  text <- gsub("{conf.low, conf.high}", paste0(lower, ", ", upper), template, fixed = TRUE)
  if (!is.null(estimate)) {
    text <- gsub("{estimate}", estimate, text, fixed = TRUE)
  }
  text <- gsub("{conf.low}", lower, text, fixed = TRUE)
  gsub("{conf.high}", upper, text, fixed = TRUE)
}

format_forest_table_values <- function(values,
                                       group = NULL,
                                       force_group_labels = FALSE,
                                       align_groups = FALSE,
                                       formatter = NULL,
                                       key = "value",
                                       fallback = as.character) {
  formatted <- apply_table_formatter(values, formatter, fallback, key)
  collapse_grouped_values(
    formatted,
    group,
    force_group_labels = force_group_labels,
    align_groups = align_groups
  )
}

# Digit settings

normalize_digits <- function(value, arg) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0 || value != floor(value)) {
    abort_forest(
      sprintf("`%s` must be a single non-negative whole number.", arg),
      class = "ggforestplotR_invalid_table"
    )
  }

  as.integer(value)
}

resolve_table_digits <- function(digits = NULL,
                                 estimate_digits = NULL,
                                 interval_digits = NULL,
                                 p_digits = NULL) {
  digits <- normalize_digits(if (is.null(digits)) 2 else digits, "digits")
  estimate_digits <- normalize_digits(estimate_digits, "estimate_digits")
  interval_digits <- normalize_digits(interval_digits, "interval_digits")
  p_digits <- normalize_digits(p_digits, "p_digits")

  list(
    estimate_digits = if (is.null(estimate_digits)) digits else estimate_digits,
    interval_digits = if (is.null(interval_digits)) digits else interval_digits,
    p_digits = if (is.null(p_digits)) max(3L, digits) else p_digits
  )
}
