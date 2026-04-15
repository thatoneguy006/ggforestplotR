#' Standardize coefficient data for forest plots
#'
#' Standardizes a coefficient table into the internal forest-plot data
#' structure used throughout `ggforestplotR`.
#'
#' @param data A data frame containing coefficient estimates and intervals.
#' @param term Column name holding the model term identifier.
#' @param estimate Column name holding the point estimate.
#' @param conf.low Column name holding the lower confidence bound.
#' @param conf.high Column name holding the upper confidence bound.
#' @param label Optional column name used for the displayed row label.
#' @param group Optional column name used for color-grouping multiple
#'   estimates per row.
#' @param grouping Optional column name used to split rows into grouped plot
#'   sections.
#' @param separate_groups Optional column name used to identify labeled
#'   variable blocks that can be outlined with separator lines.
#' @param n Optional column name holding sample sizes or other N labels for
#'   table helpers.
#' @param events Optional column name holding event counts or event labels for
#'   table helpers.
#' @param p.value Optional column name holding p-values.
#' @param exponentiate Logical; if `TRUE`, require positive values for
#'   estimates and intervals.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#'
#' @return A standardized data frame ready for [ggforestplot()] and the table
#'   composition helpers.
#' @export
#'
#' @examples
#' raw <- data.frame(
#'   variable = c("Age", "BMI", "Treatment"),
#'   beta = c(0.10, -0.08, 0.34),
#'   lower = c(0.02, -0.16, 0.12),
#'   upper = c(0.18, 0.00, 0.56)
#' )
#'
#' as_forest_data(
#'   data = raw,
#'   term = "variable",
#'   estimate = "beta",
#'   conf.low = "lower",
#'   conf.high = "upper"
#' )
as_forest_data <- function(data,
                           term,
                           estimate,
                           conf.low,
                           conf.high,
                           label = term,
                           group = NULL,
                           grouping = NULL,
                           separate_groups = NULL,
                           n = NULL,
                           events = NULL,
                           p.value = NULL,
                           exponentiate = FALSE,
                           sort_terms = c("none", "descending", "ascending")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  sort_terms <- match.arg(sort_terms)

  cols <- list(
    term = resolve_column(data, term, "term"),
    estimate = resolve_column(data, estimate, "estimate"),
    conf.low = resolve_column(data, conf.low, "conf.low"),
    conf.high = resolve_column(data, conf.high, "conf.high"),
    label = resolve_column(data, label, "label", required = FALSE),
    group = resolve_column(data, group, "group", required = FALSE),
    grouping = resolve_column(data, grouping, "grouping", required = FALSE),
    separate_groups = resolve_column(data, separate_groups, "separate_groups", required = FALSE),
    n = resolve_column(data, n, "n", required = FALSE),
    events = resolve_column(data, events, "events", required = FALSE),
    p.value = resolve_column(data, p.value, "p.value", required = FALSE)
  )

  out <- data.frame(
    term = as.character(data[[cols$term]]),
    estimate = as.numeric(data[[cols$estimate]]),
    conf.low = as.numeric(data[[cols$conf.low]]),
    conf.high = as.numeric(data[[cols$conf.high]]),
    stringsAsFactors = FALSE
  )

  out$label <- if (is.null(cols$label)) {
    out$term
  } else {
    as.character(data[[cols$label]])
  }

  out$group <- if (is.null(cols$group)) {
    NA_character_
  } else {
    as.character(data[[cols$group]])
  }

  out$grouping <- if (is.null(cols$grouping)) {
    NA_character_
  } else {
    as.character(data[[cols$grouping]])
  }

  out$separate_groups <- if (is.null(cols$separate_groups)) {
    NA_character_
  } else {
    as.character(data[[cols$separate_groups]])
  }

  out$n <- if (is.null(cols$n)) {
    NA_character_
  } else {
    as.character(data[[cols$n]])
  }

  out$events <- if (is.null(cols$events)) {
    NA_character_
  } else {
    as.character(data[[cols$events]])
  }

  out$p.value <- if (is.null(cols$p.value)) {
    NA_real_
  } else {
    as.numeric(data[[cols$p.value]])
  }

  validate_forest_data(out, exponentiate = exponentiate)

  if (sort_terms == "descending") {
    if (any(!is.na(out$grouping))) {
      out <- out[order(out$grouping, out$estimate, decreasing = c(FALSE, TRUE)), , drop = FALSE]
    } else {
      out <- out[order(out$estimate, decreasing = TRUE), , drop = FALSE]
    }
  } else if (sort_terms == "ascending") {
    if (any(!is.na(out$grouping))) {
      out <- out[order(out$grouping, out$estimate, decreasing = c(FALSE, FALSE)), , drop = FALSE]
    } else {
      out <- out[order(out$estimate, decreasing = FALSE), , drop = FALSE]
    }
  }

  rownames(out) <- NULL

  out
}
