#' Standardize coefficient data for forest plots
#'
#' Standardizes a coefficient table into the internal forest-plot data
#' structure used throughout `ggforestplotR`.
#'
#' @param data A data frame or data-frame subclass containing coefficient
#'   estimates and intervals. Tibbles and `data.table` objects are supported.
#' @param term Column name holding the model term identifier.
#' @param estimate Column name holding the point estimate.
#' @param conf.low Column name holding the lower confidence bound.
#' @param conf.high Column name holding the upper confidence bound.
#' @param label Optional column name used for the displayed row label.
#' @param term_labels Optional named vector used to relabel displayed terms.
#'   Names should match values in the term column and values are the labels to
#'   display.
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
#' @param exponentiate Compatibility argument. `TRUE` is equivalent to
#'   `estimate_scale = "ratio"`; `FALSE` is equivalent to
#'   `estimate_scale = "identity"` when `estimate_scale` is not supplied.
#' @param estimate_scale Semantic scale of the stored estimates. One of
#'   `"identity"`, `"log"`, `"ratio"`, `"probability"`, or
#'   `"risk_difference"`.
#' @param axis_transform Transformation used for the plotting axis. Defaults
#'   to `"log10"` for ratios and `"identity"` otherwise.
#' @param effect_label Short label for the effect measure, such as `"Beta"`,
#'   `"OR"`, `"HR"`, `"RR"`, or `"RD"`.
#' @param conf.level Confidence level represented by the interval columns, or
#'   `NA` when it is unknown.
#' @param reference_value Numeric null/reference value, or `NULL` when the
#'   effect measure has no universal reference value.
#' @param source_model Optional character vector identifying the source model
#'   class. The complete fitted model is not retained.
#' @param source_package Optional package name identifying the model source.
#' @param conf.int Logical; model methods require `TRUE` because forest data
#'   include confidence-interval columns.
#' @param intercept Logical; for model methods, whether to retain the
#'   intercept term.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#' @param ... Arguments passed to an `as_forest_data()` method.
#'
#' @return A `forest_data` data-frame subclass ready for [ggforestplot()] and
#'   the table composition helpers. Original data-frame columns are retained
#'   for table helpers so they can be displayed with
#'   `add_forest_table(columns = ...)`.
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
as_forest_data <- function(data, ...) {
  UseMethod("as_forest_data")
}

#' @rdname as_forest_data
#' @export
as_forest_data.forest_data <- function(data,
                                       term_labels = NULL,
                                       sort_terms = c("none", "descending", "ascending"),
                                       exponentiate = NULL,
                                       ...) {
  if (!is.null(exponentiate)) {
    stop(
      "`exponentiate` is already defined by the `forest_data` metadata.",
      call. = FALSE
    )
  }

  sort_terms <- match.arg(sort_terms)
  metadata <- forest_metadata(data)
  validate_forest_metadata(data, metadata)

  out <- data
  if (!"label" %in% names(out)) {
    out$label <- out$term
  }
  out$label <- apply_term_labels(out$term, out$label, term_labels)

  out <- sort_forest_data(out, sort_terms = sort_terms)
  rownames(out) <- NULL
  set_forest_metadata(out, forest_metadata(out))
}

#' @rdname as_forest_data
#' @export
as_forest_data.data.frame <- function(data,
                                      term,
                                      estimate,
                                      conf.low,
                                      conf.high,
                                      label = term,
                                      term_labels = NULL,
                                      group = NULL,
                                      grouping = NULL,
                                      separate_groups = NULL,
                                      n = NULL,
                                      events = NULL,
                                      p.value = NULL,
                                      exponentiate = NULL,
                                      estimate_scale = NULL,
                                      axis_transform = NULL,
                                      effect_label = NULL,
                                      conf.level = 0.95,
                                      reference_value = NULL,
                                      source_model = NULL,
                                      source_package = NULL,
                                      sort_terms = c("none", "descending", "ascending"),
                                      ...) {
  if (!inherits(data, "data.frame")) {
    stop(
      "`data` must be a data frame or data-frame subclass, such as a tibble or `data.table`.",
      call. = FALSE
    )
  }
  if (anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
    stop("`data` must have unique, non-empty column names.", call. = FALSE)
  }

  source_column_names <- names(data)
  reference_value_missing <- missing(reference_value)
  sort_terms <- match.arg(sort_terms)

  if (!is.null(exponentiate) &&
      (!is.logical(exponentiate) || length(exponentiate) != 1L || is.na(exponentiate))) {
    stop("`exponentiate` must be `NULL`, `TRUE`, or `FALSE`.", call. = FALSE)
  }

  if (is.null(estimate_scale)) {
    estimate_scale <- if (isTRUE(exponentiate)) "ratio" else "identity"
  } else {
    estimate_scale <- match.arg(estimate_scale, forest_estimate_scales())

    if (!is.null(exponentiate) &&
        !identical(isTRUE(exponentiate), identical(estimate_scale, "ratio"))) {
      stop(
        "`exponentiate` and `estimate_scale` describe incompatible scales.",
        call. = FALSE
      )
    }
  }

  if (is.null(axis_transform)) {
    axis_transform <- default_axis_transform(estimate_scale)
  }
  if (is.null(effect_label)) {
    effect_label <- default_effect_label(estimate_scale)
  }
  if (reference_value_missing) {
    reference_value <- default_reference_value(estimate_scale)
  }

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
  column_mapping <- unlist(cols, use.names = TRUE)

  grouping_levels <- if (!is.null(cols$grouping) && is.factor(data[[cols$grouping]])) {
    levels(data[[cols$grouping]])
  } else {
    NULL
  }

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
  out$label <- apply_term_labels(out$term, out$label, term_labels)

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

  canonical_columns <- names(out)
  extra_cols <- setdiff(source_column_names, canonical_columns)

  for (extra in extra_cols) {
    out[[extra]] <- data[[extra]]
  }

  source_storage <- stats::setNames(source_column_names, source_column_names)
  conflicting_columns <- intersect(source_column_names, canonical_columns)

  for (source_name in conflicting_columns) {
    canonical_uses_source <- source_name %in% names(column_mapping) &&
      identical(unname(column_mapping[[source_name]]), source_name)

    if (!canonical_uses_source) {
      stored_name <- paste0("..source..", source_name)
      while (stored_name %in% names(out)) {
        stored_name <- paste0(stored_name, ".")
      }
      out[[stored_name]] <- data[[source_name]]
      source_storage[[source_name]] <- stored_name
    }
  }

  attr(out, "grouping_levels") <- grouping_levels

  validate_forest_data(out, exponentiate = identical(estimate_scale, "ratio"))

  out <- sort_forest_data(out, sort_terms = sort_terms)

  rownames(out) <- NULL
  metadata <- new_forest_metadata(
    estimate_scale = estimate_scale,
    axis_transform = axis_transform,
    effect_label = effect_label,
    conf_level = conf.level,
    reference_value = reference_value,
    source_model = source_model,
    source_package = source_package,
    source_columns = source_storage,
    column_mapping = column_mapping,
    grouping_levels = grouping_levels
  )

  new_forest_data(out, metadata)
}
