# ─── Column resolution ────────────────────────────────────────────────────────

resolve_column <- function(data, column, arg, required = TRUE) {
  if (is.null(column)) {
    if (required) {
      stop(sprintf("`%s` must be supplied.", arg), call. = FALSE)
    }
    return(NULL)
  }

  if (!is.character(column) || length(column) != 1L) {
    stop(sprintf("`%s` must be a single column name (string).", arg), call. = FALSE)
  }

  if (!column %in% names(data)) {
    stop(sprintf("Column `%s` was not found in `data`.", column), call. = FALSE)
  }

  column
}

# ─── Data validation ──────────────────────────────────────────────────────────

validate_forest_data <- function(data, exponentiate = FALSE) {
  required <- c("term", "estimate", "conf.low", "conf.high")
  missing <- setdiff(required, names(data))

  if (length(missing) > 0L) {
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

    if (isTRUE(exponentiate) && any(data[[col]] <= 0)) {
      stop(
        "Exponentiated forest plots require strictly positive `estimate`, `conf.low`, and `conf.high` values.",
        call. = FALSE
      )
    }
  }

  if (any(data$conf.low > data$conf.high)) {
    stop("`conf.low` cannot be greater than `conf.high`.", call. = FALSE)
  }

  invisible(data)
}

# ─── Column normalisation ────────────────────────────────────────────────────

normalize_table_columns <- function(columns, data = NULL) {
  if (is.null(columns)) {
    return(NULL)
  }

  if (is.numeric(columns)) {
    source_names <- if (!is.null(data)) names(attr(data, "source_columns")) else NULL
    available <- if (length(source_names) > 0L) source_names else names(data)

    if (is.null(available)) {
      stop("Numeric table columns require named data.", call. = FALSE)
    }

    idx <- as.integer(columns)

    if (anyNA(idx) || any(columns != idx) || any(idx < 1L | idx > length(available))) {
      stop(
        sprintf("Numeric table columns must be between 1 and %s.", length(available)),
        call. = FALSE
      )
    }

    return(normalize_table_columns(available[idx], data = data))
  }

  if (!is.character(columns)) {
    stop("Table columns must be specified by name or position.", call. = FALSE)
  }

  aliases <- c(
    term = "term", terms = "term", label = "term", labels = "term",
    subgroup = "term", subgroups = "term",
    n = "n", samplesize = "n", sample_size = "n",
    events = "events", event = "events", cases = "events", count = "events",
    estimate = "estimate", estimates = "estimate",
    effect = "estimate", effects = "estimate",
    ci = "ci", cis = "ci", interval = "ci", intervals = "ci",
    confidenceinterval = "ci", confidenceintervals = "ci",
    "conf.low" = "ci", conflow = "ci",
    "conf.high" = "ci", confhigh = "ci",
    p = "p", pvalue = "p", p.value = "p", p_value = "p", pvalues = "p"
  )

  if (!is.null(data)) {
    exact <- columns %in% names(data)
  } else {
    exact <- rep(FALSE, length(columns))
  }

  normalized <- gsub("\\s+", "", tolower(columns))
  resolved <- unname(aliases[normalized])
  interval_alias <- normalized %in% c("conf.low", "conflow", "conf.high", "confhigh")
  p_value_alias <- normalized %in% "p.value"
  resolved[exact & !interval_alias & !p_value_alias] <- columns[exact & !interval_alias & !p_value_alias]

  if (anyNA(resolved)) {
    bad <- unique(columns[is.na(resolved)])
    stop(
      sprintf("Unsupported table columns: %s", paste(bad, collapse = ", ")),
      call. = FALSE
    )
  }

  unique(resolved)
}

normalize_column_labels <- function(column_labels, data = NULL) {
  if (is.null(column_labels)) {
    return(NULL)
  }

  if (!is.atomic(column_labels) || is.null(names(column_labels)) || any(!nzchar(names(column_labels)))) {
    stop("`column_labels` must be a named vector.", call. = FALSE)
  }

  label_keys <- vapply(
    names(column_labels),
    function(label_name) normalize_table_columns(label_name, data = data),
    character(1)
  )
  out <- stats::setNames(as.character(column_labels), label_keys)
  out[!duplicated(names(out), fromLast = TRUE)]
}

has_table_values <- function(data, column) {
  if (!column %in% names(data)) {
    return(FALSE)
  }

  values <- data[[column]]
  if (is.numeric(values)) {
    any(!is.na(values))
  } else {
    any(!is.na(values) & nzchar(as.character(values)))
  }
}

default_forest_table_columns <- function(data) {
  c(
    "term",
    if (has_table_values(data, "n")) "n",
    if (has_table_values(data, "events")) "events",
    "estimate"
  )
}

default_split_left_columns <- function(data) {
  c(
    "term",
    if (has_table_values(data, "n")) "n",
    if (has_table_values(data, "events")) "events"
  )
}

default_split_right_columns <- function(data) {
  "estimate"
}

normalize_digits <- function(value, arg) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0 || value != floor(value)) {
    stop(sprintf("`%s` must be a single non-negative whole number.", arg), call. = FALSE)
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

warn_deprecated_argument <- function(arg, replacement) {
  rlang::warn(
    sprintf("`%s` is deprecated; use %s instead.", arg, replacement),
    class = "ggforestplotR_deprecated_argument"
  )
}

apply_term_labels <- function(term, label, term_labels = NULL) {
  if (is.null(term_labels)) {
    return(label)
  }

  if (!is.atomic(term_labels) || is.null(names(term_labels)) || any(!nzchar(names(term_labels)))) {
    stop("`term_labels` must be a named vector.", call. = FALSE)
  }

  term_labels <- stats::setNames(as.character(term_labels), names(term_labels))
  matched <- match(term, names(term_labels))
  replace <- !is.na(matched)
  label[replace] <- unname(term_labels[matched[replace]])
  label
}

sort_forest_data <- function(data, sort_terms = c("none", "descending", "ascending")) {
  sort_terms <- match.arg(sort_terms)

  if (sort_terms == "none") {
    return(data)
  }

  decreasing <- sort_terms == "descending"
  has_grouping <- any(!is.na(data$grouping) & nzchar(data$grouping))

  if (!isTRUE(has_grouping)) {
    return(data[order(data$estimate, decreasing = decreasing), , drop = FALSE])
  }

  group_key <- ifelse(
    is.na(data$grouping) | !nzchar(data$grouping),
    "(Ungrouped)",
    data$grouping
  )
  grouping_levels <- attr(data, "grouping_levels")
  group_levels <- if (is.null(grouping_levels)) {
    unique(group_key)
  } else {
    c(grouping_levels[grouping_levels %in% group_key], setdiff(unique(group_key), grouping_levels))
  }
  row_order <- unlist(lapply(group_levels, function(level) {
    idx <- which(group_key == level)
    idx[order(data$estimate[idx], decreasing = decreasing)]
  }), use.names = FALSE)

  data[row_order, , drop = FALSE]
}

format_conf_level_label <- function(conf.level = 0.95) {
  pct <- conf.level * 100
  if (isTRUE(all.equal(pct, round(pct)))) {
    paste0(as.integer(round(pct)), "%")
  } else {
    paste0(format(pct, trim = TRUE, scientific = FALSE), "%")
  }
}

infer_model_estimate_info <- function(model,
                                      exponentiate = NULL,
                                      conf.level = 0.95) {
  auto_exponentiate <- FALSE
  estimate_label <- "Estimate"
  model_family <- if (inherits(model, "glm") && !is.null(model$family)) {
    model$family
  } else {
    tryCatch(stats::family(model), error = function(e) NULL)
  }

  if (inherits(model, "coxph")) {
    auto_exponentiate <- TRUE
    estimate_label <- "HR"
  } else if (!is.null(model_family)) {
    family <- model_family$family
    link <- model_family$link

    if (identical(family, "binomial") && identical(link, "logit")) {
      auto_exponentiate <- TRUE
      estimate_label <- "OR"
    } else if (link == "log") {
      auto_exponentiate <- TRUE
      estimate_label <- if (family %in% c("poisson", "quasipoisson")) "IRR" else "RR"
    }
  }

  resolved_exponentiate <- if (is.null(exponentiate)) auto_exponentiate else isTRUE(exponentiate)

  if (isTRUE(resolved_exponentiate)) {
    if (identical(estimate_label, "Estimate")) {
      estimate_label <- "Ratio"
    }
  } else {
    estimate_label <- "Estimate"
  }

  list(
    exponentiate = resolved_exponentiate,
    estimate_label = estimate_label,
    axis_label = sprintf("%s (%s CI)", estimate_label, format_conf_level_label(conf.level))
  )
}

# ─── Formatting helpers ──────────────────────────────────────────────────────

#' Shared logic for collapsing formatted values with optional group labels.
#'
#' All three public formatters (p-values, estimates, n-values) produce a
#' character vector of per-row formatted strings, then need identical
#' deduplication / group-prefixing / newline-collapsing.  This function
#' handles that single concern.
#' @keywords internal
#' @noRd
collapse_grouped_values <- function(formatted, group = NULL, force_group_labels = FALSE) {
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
                                   force_group_labels = FALSE) {
  p_digits <- resolve_table_digits(digits = digits, p_digits = p_digits)$p_digits
  values <- as.numeric(values)
  eps <- 10^(-p_digits)
  formatted <- ifelse(
    is.na(values),
    "",
    ifelse(values < eps, paste0("<", sprintf(paste0("%.", p_digits, "f"), eps)),
      sprintf(paste0("%.", p_digits, "f"), values)
    )
  )
  collapse_grouped_values(formatted, group, force_group_labels = force_group_labels)
}

format_forest_estimates <- function(estimate, conf.low, conf.high,
                                    group = NULL, digits = 2,
                                    estimate_digits = digits,
                                    interval_digits = digits,
                                    estimate_fmt = NULL,
                                    force_group_labels = FALSE) {
  digits <- resolve_table_digits(
    digits = digits,
    estimate_digits = estimate_digits,
    interval_digits = interval_digits
  )
  if (is.null(estimate_fmt)) {
    estimate_fmt <- "{estimate} ({conf.low}, {conf.high})"
  }
  if (!is.character(estimate_fmt) || length(estimate_fmt) != 1L || is.na(estimate_fmt)) {
    stop("`estimate_fmt` must be a single character string.", call. = FALSE)
  }

  estimate_text <- sprintf(paste0("%.", digits$estimate_digits, "f"), estimate)
  conf_low_text <- sprintf(paste0("%.", digits$interval_digits, "f"), conf.low)
  conf_high_text <- sprintf(paste0("%.", digits$interval_digits, "f"), conf.high)

  formatted <- vapply(
    seq_along(estimate_text),
    function(i) {
      value <- estimate_fmt
      value <- gsub(
        "{conf.low, conf.high}",
        paste0(conf_low_text[[i]], ", ", conf_high_text[[i]]),
        value,
        fixed = TRUE
      )
      value <- gsub("{estimate}", estimate_text[[i]], value, fixed = TRUE)
      value <- gsub("{conf.low}", conf_low_text[[i]], value, fixed = TRUE)
      value <- gsub("{conf.high}", conf_high_text[[i]], value, fixed = TRUE)
      value
    },
    character(1)
  )
  collapse_grouped_values(formatted, group, force_group_labels = force_group_labels)
}

format_forest_intervals <- function(conf.low, conf.high,
                                    group = NULL, digits = 2,
                                    interval_digits = digits,
                                    ci_fmt = NULL,
                                    force_group_labels = FALSE) {
  digits <- resolve_table_digits(
    digits = digits,
    interval_digits = interval_digits
  )
  if (is.null(ci_fmt)) {
    ci_fmt <- "({conf.low}, {conf.high})"
  }
  if (!is.character(ci_fmt) || length(ci_fmt) != 1L || is.na(ci_fmt)) {
    stop("`ci_fmt` must be a single character string.", call. = FALSE)
  }

  conf_low_text <- sprintf(paste0("%.", digits$interval_digits, "f"), conf.low)
  conf_high_text <- sprintf(paste0("%.", digits$interval_digits, "f"), conf.high)

  formatted <- vapply(
    seq_along(conf_low_text),
    function(i) {
      value <- ci_fmt
      value <- gsub(
        "{conf.low, conf.high}",
        paste0(conf_low_text[[i]], ", ", conf_high_text[[i]]),
        value,
        fixed = TRUE
      )
      value <- gsub("{conf.low}", conf_low_text[[i]], value, fixed = TRUE)
      value <- gsub("{conf.high}", conf_high_text[[i]], value, fixed = TRUE)
      value
    },
    character(1)
  )
  collapse_grouped_values(formatted, group, force_group_labels = force_group_labels)
}

format_forest_table_values <- function(values, group = NULL, force_group_labels = FALSE) {
  formatted <- as.character(values)
  formatted[is.na(formatted)] <- ""
  collapse_grouped_values(formatted, group, force_group_labels = force_group_labels)
}

# ─── Plot data construction (decomposed into single-purpose passes) ──────────

observed_grouping_panels <- function(data, has_groupings) {
  if (!isTRUE(has_groupings)) {
    return("__all__")
  }

  panels <- data$grouping_panel

  if (is.factor(panels)) {
    observed <- unique(as.character(panels))
    return(levels(panels)[levels(panels) %in% observed])
  }

  unique(panels)
}

resolve_grouping_panel_levels <- function(grouping, grouping_levels = NULL) {
  panel_values <- ifelse(
    is.na(grouping) | !nzchar(grouping),
    "(Ungrouped)",
    grouping
  )
  observed <- unique(panel_values)

  if (is.null(grouping_levels)) {
    return(observed)
  }

  c(grouping_levels[grouping_levels %in% observed], setdiff(observed, grouping_levels))
}

assign_grouping_panels <- function(data, has_groupings) {
  if (has_groupings) {
    panels <- ifelse(
      is.na(data$grouping) | !nzchar(data$grouping),
      "(Ungrouped)",
      data$grouping
    )

    grouping_levels <- attr(data, "grouping_levels")
    if (is.null(grouping_levels)) {
      panels
    } else {
      factor(panels, levels = resolve_grouping_panel_levels(panels, grouping_levels))
    }
  } else {
    rep(NA_character_, nrow(data))
  }
}

#' Within each panel, if a separate_groups value appears more than once the
#' labels are ambiguous.  Prefix them with "group: label" so the axis is
#' readable.
#' @keywords internal
#' @noRd
prefix_ambiguous_labels <- function(data, has_groupings) {
  panel_values <- observed_grouping_panels(data, has_groupings)

  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    sep_vals <- data$separate_groups[idx]
    counts <- table(sep_vals[!is.na(sep_vals) & nzchar(sep_vals)])
    prefix_groups <- names(counts[counts > 1L])

    if (length(prefix_groups) > 0L) {
      prefix_idx <- idx[!is.na(sep_vals) & sep_vals %in% prefix_groups]
      data$label[prefix_idx] <- paste0(data$separate_groups[prefix_idx], ": ", data$label[prefix_idx])
    }
  }

  data
}

#' Identify labels that need panel-qualified row keys.
#' @keywords internal
#' @noRd
labels_requiring_panel_keys <- function(data, has_groupings) {
  if (!isTRUE(has_groupings)) {
    return(character())
  }

  panel_labels <- unique(data[c("grouping_panel", "label")])
  label_counts <- table(panel_labels$label)
  names(label_counts[label_counts > 1L])
}

make_panel_row_keys <- function(panel, labels, panel_key_labels) {
  labels <- as.character(labels)

  if (length(panel_key_labels) == 0L) {
    return(labels)
  }

  ifelse(labels %in% panel_key_labels, paste(panel, labels, sep = "___"), labels)
}

#' Assign a row_key per label within each panel and set factor levels in display
#' order. Facet names are encoded only when a visible label is reused across
#' panels and needs a unique internal key.
#' @keywords internal
#' @noRd
assign_row_keys <- function(data, has_groupings) {
  panel_values <- observed_grouping_panels(data, has_groupings)
  panel_key_labels <- labels_requiring_panel_keys(data, has_groupings)
  data$row_key <- NA_character_
  all_levels <- character()

  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    panel_labels <- unique(data$label[idx])

    keys <- make_panel_row_keys(pv, panel_labels, panel_key_labels)
    row_map <- stats::setNames(keys, panel_labels)

    data$row_key[idx] <- unname(row_map[data$label[idx]])
    all_levels <- c(all_levels, keys)
  }

  data$row_key <- factor(data$row_key, levels = all_levels)
  data
}

#' Build axis label lookup: row_key -> display label.
#' @keywords internal
#' @noRd
build_axis_labels <- function(data, has_groupings) {
  panel_values <- observed_grouping_panels(data, has_groupings)
  panel_key_labels <- labels_requiring_panel_keys(data, has_groupings)
  labels <- character()

  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    panel_labels <- unique(data$label[idx])
    keys <- make_panel_row_keys(pv, panel_labels, panel_key_labels)
    labels <- c(labels, stats::setNames(panel_labels, keys))
  }

  labels
}

#' Build a data frame of alternating stripe rectangles for each panel.
#' @keywords internal
#' @noRd
build_stripe_rectangles <- function(data, has_groupings) {
  panel_values <- observed_grouping_panels(data, has_groupings)
  parts <- vector("list", length(panel_values))

  for (i in seq_along(panel_values)) {
    pv <- panel_values[[i]]
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    n_rows <- length(unique(data$row_key[idx]))

    parts[[i]] <- data.frame(
      grouping_panel = if (has_groupings) pv else NA_character_,
      stripe_id = seq_len(n_rows),
      xmin = -Inf,
      xmax = Inf,
      ymin = seq_len(n_rows) - 0.5,
      ymax = seq_len(n_rows) + 0.5,
      stringsAsFactors = FALSE
    )
  }

  stripe_data <- do.call(rbind, parts)
  if (isTRUE(has_groupings) && is.factor(data$grouping_panel)) {
    stripe_data$grouping_panel <- factor(
      stripe_data$grouping_panel,
      levels = levels(data$grouping_panel)
    )
  }
  stripe_data$fill_key <- ifelse(stripe_data$stripe_id %% 2 == 1, "stripe", "base")
  stripe_data
}

#' Detect runs of identical separate_groups values within each panel and
#' return a data frame of horizontal separator positions.
#' @keywords internal
#' @noRd
build_separate_lines <- function(data, has_groupings) {
  panel_values <- observed_grouping_panels(data, has_groupings)
  parts <- vector("list", length(panel_values))

  for (i in seq_along(panel_values)) {
    pv <- panel_values[[i]]
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))

    row_keys <- levels(data$row_key)[levels(data$row_key) %in% data$row_key[idx]]

    # Map each row_key to its separate_groups value
    sep_groups <- vapply(row_keys, function(rk) {
      row_idx <- idx[as.character(data$row_key[idx]) == rk]
      vals <- unique(data$separate_groups[row_idx])
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0L) NA_character_ else vals[1L]
    }, character(1))

    separator_rows <- list()
    run_start <- 1L

    while (run_start <= length(sep_groups)) {
      current <- sep_groups[run_start]
      run_end <- run_start

      if (!is.na(current) && nzchar(current)) {
        while (run_end < length(sep_groups) &&
               !is.na(sep_groups[run_end + 1L]) &&
               sep_groups[run_end + 1L] == current) {
          run_end <- run_end + 1L
        }

        separator_rows[[length(separator_rows) + 1L]] <- data.frame(
          grouping_panel = if (has_groupings) pv else NA_character_,
          separate_groups = unname(current),
          yintercept = c(run_start - 0.5, run_end + 0.5),
          stringsAsFactors = FALSE
        )
      }

      run_start <- run_end + 1L
    }

    parts[[i]] <- if (length(separator_rows) > 0L) do.call(rbind, separator_rows) else NULL
  }

  separator_data <- do.call(rbind, parts)

  if (is.null(separator_data)) {
    data.frame(
      grouping_panel = character(),
      yintercept = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    separator_data <- unique(separator_data[c("grouping_panel", "yintercept")])
    if (isTRUE(has_groupings) && is.factor(data$grouping_panel)) {
      separator_data$grouping_panel <- factor(
        separator_data$grouping_panel,
        levels = levels(data$grouping_panel)
      )
    }
    separator_data
  }
}

#' Main entry point.  Orchestrates the four passes defined above.
#' @keywords internal
#' @noRd
build_forest_plot_data <- function(data) {
  has_groupings <- any(!is.na(data$grouping) & nzchar(data$grouping))
  plot_data <- data
  attr(plot_data, "source_columns") <- attr(data, "source_columns")
  plot_data$grouping_panel <- assign_grouping_panels(plot_data, has_groupings)

  plot_data <- prefix_ambiguous_labels(plot_data, has_groupings)
  attr(plot_data, "source_columns") <- attr(data, "source_columns")
  plot_data <- assign_row_keys(plot_data, has_groupings)
  attr(plot_data, "source_columns") <- attr(data, "source_columns")

  stripe_data <- build_stripe_rectangles(plot_data, has_groupings)
  separator_data <- build_separate_lines(plot_data, has_groupings)
  axis_labels <- build_axis_labels(plot_data, has_groupings)

  list(
    plot_data = plot_data,
    stripe_data = stripe_data,
    separator_data = separator_data,
    axis_labels = axis_labels,
    has_groupings = has_groupings && length(unique(plot_data$grouping_panel)) > 1
  )
}

# ─── Table data construction ─────────────────────────────────────────────────

extract_trained_y_limits <- function(plot) {
  built <- tryCatch(
    ggplot2::ggplot_build(plot),
    error = function(e) NULL
  )

  if (is.null(built) || is.null(built$layout) || is.null(built$layout$panel_params)) {
    return(NULL)
  }

  limits <- unlist(lapply(built$layout$panel_params, function(panel) {
    y_scale <- panel$y

    if (!is.null(y_scale) && is.function(y_scale$get_limits)) {
      return(y_scale$get_limits())
    }

    NULL
  }), use.names = FALSE)

  limits <- as.character(limits)
  limits <- limits[!is.na(limits) & nzchar(limits)]

  if (length(limits) == 0L) {
    return(NULL)
  }

  unique(limits)
}

align_forest_state_to_plot_y_scale <- function(state, plot) {
  row_levels <- levels(state$forest_data$row_key)
  y_limits <- extract_trained_y_limits(plot)

  if (is.null(row_levels) || is.null(y_limits)) {
    return(state)
  }

  matched_limits <- y_limits[y_limits %in% row_levels]

  if (length(matched_limits) == 0L || identical(matched_limits, row_levels)) {
    return(state)
  }

  keep_rows <- as.character(state$forest_data$row_key) %in% matched_limits
  aligned_data <- state$forest_data[keep_rows, , drop = FALSE]
  aligned_data$row_key <- factor(as.character(aligned_data$row_key), levels = matched_limits)

  source_columns <- attr(state$forest_data, "source_columns")
  if (!is.null(source_columns)) {
    source_columns <- source_columns[keep_rows, , drop = FALSE]
    attr(aligned_data, "source_columns") <- source_columns
  }

  aligned_state <- state
  aligned_state$forest_data <- aligned_data
  aligned_state$stripe_data <- build_stripe_rectangles(aligned_data, state$has_groupings)
  aligned_state
}

build_forest_table_data <- function(data,
                                    term_header = "Term",
                                    n_header = "N",
                                    events_header = "Events",
                                    estimate_label = "Estimate",
                                    p_header = "P-value",
                                    digits = 2,
                                    estimate_digits = NULL,
                                    interval_digits = NULL,
                                    p_digits = NULL,
                                    estimate_fmt = NULL,
                                    ci_fmt = NULL,
                                    column_labels = NULL,
                                    columns = NULL) {
  digits <- resolve_table_digits(
    digits = digits,
    estimate_digits = estimate_digits,
    interval_digits = interval_digits,
    p_digits = p_digits
  )
  source_columns <- attr(data, "source_columns")
  if (is.null(source_columns)) {
    source_columns <- data
  }
  force_group_labels <- any(!is.na(data$group) & nzchar(data$group))
  row_levels <- levels(data$row_key)
  row_parts <- vector("list", length(row_levels))

  for (i in seq_along(row_levels)) {
    row_key <- row_levels[[i]]
    idx <- which(as.character(data$row_key) == row_key)

    if (length(idx) == 0L) next

    rd <- data[idx, , drop = FALSE]
    row_parts[[i]] <- data.frame(
      row_key = row_key,
      grouping_panel = rd$grouping_panel[1L],
      term_text = rd$label[1L],
      n_text = format_forest_table_values(
        rd$n,
        rd$group,
        force_group_labels = force_group_labels
      ),
      events_text = format_forest_table_values(
        rd$events,
        rd$group,
        force_group_labels = force_group_labels
      ),
      estimate_text = format_forest_estimates(
        rd$estimate,
        rd$conf.low,
        rd$conf.high,
        rd$group,
        estimate_digits = digits$estimate_digits,
        interval_digits = digits$interval_digits,
        estimate_fmt = estimate_fmt,
        force_group_labels = force_group_labels
      ),
      estimate_value_text = format_forest_estimates(
        rd$estimate,
        rd$conf.low,
        rd$conf.high,
        rd$group,
        estimate_digits = digits$estimate_digits,
        interval_digits = digits$interval_digits,
        estimate_fmt = if (is.null(estimate_fmt)) "{estimate}" else estimate_fmt,
        force_group_labels = force_group_labels
      ),
      ci_text = format_forest_intervals(
        rd$conf.low,
        rd$conf.high,
        rd$group,
        interval_digits = digits$interval_digits,
        ci_fmt = ci_fmt,
        force_group_labels = force_group_labels
      ),
      p_text = format_forest_p_values(
        rd$p.value,
        rd$group,
        p_digits = digits$p_digits,
        force_group_labels = force_group_labels
      ),
      stringsAsFactors = FALSE
    )

    extra_columns <- setdiff(unique(c(names(data), names(source_columns))), names(row_parts[[i]]))
    extra_columns <- setdiff(
      extra_columns,
      c("row_key", "grouping_panel")
    )

    for (extra in extra_columns) {
      values <- if (extra %in% names(source_columns)) {
        source_columns[[extra]][idx]
      } else {
        rd[[extra]]
      }
      row_parts[[i]][[extra]] <- format_forest_table_values(
        values,
        rd$group,
        force_group_labels = force_group_labels
      )
    }
  }

  table_rows <- do.call(rbind, row_parts)
  table_rows$row_key <- factor(table_rows$row_key, levels = row_levels)
  attr(table_rows, "source_columns") <- source_columns

  # Determine which columns to show
  if (is.null(columns)) {
    column_keys <- default_forest_table_columns(data)
  } else {
    column_keys <- normalize_table_columns(columns, data = table_rows)
  }

  if (length(column_keys) == 0L) {
    stop("Select at least one table column to display.", call. = FALSE)
  }

  column_field_lookup <- c(
    term = "term_text",
    n = "n_text",
    events = "events_text",
    estimate = if ("ci" %in% column_keys) "estimate_value_text" else "estimate_text",
    ci = "ci_text",
    p = "p_text"
  )
  header_lookup <- c(
    term = term_header,
    n = n_header,
    events = events_header,
    estimate = if ("ci" %in% column_keys) estimate_label else sprintf("%s (95%% CI)", estimate_label),
    ci = "95% CI",
    p = p_header
  )
  extra_column_keys <- setdiff(column_keys, names(column_field_lookup))
  extra_field_lookup <- stats::setNames(extra_column_keys, extra_column_keys)
  column_field_lookup <- c(column_field_lookup, extra_field_lookup)
  header_lookup <- c(header_lookup, stats::setNames(extra_column_keys, extra_column_keys))

  resolved_column_labels <- normalize_column_labels(column_labels, data = table_rows)
  if (!is.null(resolved_column_labels)) {
    header_lookup[names(resolved_column_labels)] <- unname(resolved_column_labels)
  }

  # Build long-format table.  column_position is left as NA — the layout

  # functions (layout_split_table_spec / layout_center_table_spec) are
  # responsible for setting real positions.
  long_parts <- vector("list", length(column_keys))

  for (i in seq_along(column_keys)) {
    key <- column_keys[[i]]
    long_parts[[i]] <- data.frame(
      row_key = table_rows$row_key,
      grouping_panel = table_rows$grouping_panel,
      column_key = key,
      column_position = NA_real_,
      text = table_rows[[column_field_lookup[[key]]]],
      stringsAsFactors = FALSE
    )
  }

  table_data <- do.call(rbind, long_parts)
  table_data$row_key <- factor(table_data$row_key, levels = row_levels)

  list(
    table_data = table_data,
    positions = rep(NA_real_, length(column_keys)),
    header_positions = rep(NA_real_, length(column_keys)),
    headers = unname(header_lookup[column_keys]),
    column_keys = column_keys
  )
}

# ─── Grid line data ──────────────────────────────────────────────────────────

build_table_line_data <- function(stripe_data, has_groupings = FALSE) {
  if (isTRUE(has_groupings)) {
    line_parts <- lapply(
      split(stripe_data, stripe_data$grouping_panel, drop = TRUE),
      function(panel_data) {
        boundary_values <- unique(c(
          min(panel_data$ymin),
          panel_data$ymin[panel_data$stripe_id > 1L],
          max(panel_data$ymax)
        ))
        data.frame(
          grouping_panel = panel_data$grouping_panel[1L],
          yintercept = boundary_values,
          stringsAsFactors = FALSE
        )
      }
    )
    do.call(rbind, line_parts)
  } else {
    data.frame(
      yintercept = unique(c(
        min(stripe_data$ymin),
        stripe_data$ymin[stripe_data$stripe_id > 1L],
        max(stripe_data$ymax)
      )),
      stringsAsFactors = FALSE
    )
  }
}

# ─── Text measurement ────────────────────────────────────────────────────────

#' Ground-truth width measurement via grid graphics.  Handles multi-line
#' strings by splitting on newlines and returning the widest line.
#' @keywords internal
#' @noRd
measure_max_grob_width <- function(text,
                                   fontsize_pt,
                                   fontface = "plain",
                                   fontfamily = "") {
  text <- as.character(text)
  text[is.na(text)] <- ""

  if (length(text) == 0L) return(0)

  widths <- vapply(text, function(value) {
    lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
    if (length(lines) == 0L) return(0)

    max(vapply(lines, function(line) {
      if (!nzchar(line)) return(0)
      grid::convertWidth(
        grid::grobWidth(
          grid::textGrob(
            line,
            gp = grid::gpar(
              fontsize = fontsize_pt,
              fontface = fontface,
              fontfamily = fontfamily
            )
          )
        ),
        "inches",
        valueOnly = TRUE
      )
    }, numeric(1)), 0)
  }, numeric(1))

  max(widths, 0)
}

#' Measure the displayed text width (in inches) for each column, taking the
#' max of the header and all cell values.
#' @keywords internal
#' @noRd
measure_table_text_widths <- function(table_spec,
                                      text_size = 3.2,
                                      header_text_size = 11,
                                      header_fontface = "bold",
                                      header_family = "") {
  text_size_pt <- text_size * (72.27 / 25.4)

  stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
    key <- table_spec$column_keys[[i]]
    values <- table_spec$table_data$text[table_spec$table_data$column_key == key]
    max(
      measure_max_grob_width(
        table_spec$headers[[i]],
        fontsize_pt = header_text_size,
        fontface = header_fontface,
        fontfamily = header_family
      ),
      measure_max_grob_width(values, fontsize_pt = text_size_pt, fontface = "plain")
    )
  }, numeric(1)), table_spec$column_keys)
}

# ─── Column width estimation ─────────────────────────────────────────────────

#' Default per-column base padding.  Uses a known lookup for the four standard
#' keys and a sensible fallback for anything else.
#' @keywords internal
#' @noRd
column_base_padding <- function(column_key) {
  known <- c(term = 0.16, n = 0.10, events = 0.12, estimate = 0.18, ci = 0.14, p = 0.12)
  pad <- unname(known[column_key])
  if (is.na(pad)) 0.14 else pad
}

#' Total column width = measured text width + base padding + alignment padding.
#' @keywords internal
#' @noRd
estimate_split_column_widths <- function(table_spec,
                                         text_size = 3.2,
                                         header_text_size = 11,
                                         header_fontface = "bold",
                                         header_family = "",
                                         alignment = c("left", "center", "right")) {
  alignment <- match.arg(alignment)
  text_widths <- measure_table_text_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )
  alignment_padding <- switch(alignment, left = 0.06, right = 0.06, center = 0.05)

  stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
    key <- table_spec$column_keys[[i]]
    unname(text_widths[[key]]) + column_base_padding(key) + alignment_padding
  }, numeric(1)), table_spec$column_keys)
}

# ─── Layout engines ──────────────────────────────────────────────────────────

#' Lay out a table spec for a split forest plot.
#'
#' Computes column positions and a content width.  Does NOT try to account for
#' which side of the plot the table will sit on — that's handled at assembly
#' time by equalising the two table widths.
#' @keywords internal
#' @noRd
layout_split_table_spec <- function(table_spec,
                                    text_size = 3.2,
                                    header_text_size = 11,
                                    header_fontface = "bold",
                                    header_family = "",
                                    alignment = c("left", "right")) {
  alignment <- match.arg(alignment)
  column_widths <- estimate_split_column_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    alignment = alignment
  )
  text_widths <- measure_table_text_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )
  gap <- 0.2

  positions <- if (alignment == "left") {
    starts <- c(0, utils::head(cumsum(column_widths + gap), -1))
    starts + 0.02
  } else {
    cumsum(column_widths + c(rep(gap, length(column_widths) - 1L), 0))
  }

  table_spec$table_data$column_position <- unname(
    positions[match(table_spec$table_data$column_key, table_spec$column_keys)]
  )
  table_spec$alignment               <- alignment
  table_spec$positions               <- unname(positions)
  table_spec$header_positions        <- unname(positions)
  table_spec$estimated_column_widths <- unname(column_widths)
  table_spec$displayed_column_widths <- unname(text_widths)
  table_spec$content_width <- sum(column_widths) +
    gap * max(length(column_widths) - 1L, 0)
  table_spec
}

layout_center_table_spec <- function(table_spec,
                                     text_size = 3.2,
                                     header_text_size = 11,
                                     header_fontface = "bold",
                                     header_family = "") {
  column_widths <- estimate_split_column_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    alignment = "center"
  )
  text_widths <- measure_table_text_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )
  gap <- 0.55
  left_edges <- cumsum(c(0, utils::head(column_widths + gap, -1)))
  positions <- left_edges + column_widths / 2

  table_spec$table_data$column_position <- unname(
    positions[match(table_spec$table_data$column_key, table_spec$column_keys)]
  )
  table_spec$positions               <- unname(positions)
  table_spec$header_positions        <- unname(positions)
  table_spec$estimated_column_widths <- unname(column_widths)
  table_spec$displayed_column_widths <- unname(text_widths)
  table_spec$content_width <- sum(column_widths) +
    gap * max(length(column_widths) - 1L, 0)
  table_spec
}

compute_table_x_limits <- function(table_spec, pad = 0.03) {
  widths <- if (!is.null(table_spec$displayed_column_widths)) {
    table_spec$displayed_column_widths
  } else {
    table_spec$estimated_column_widths
  }
  positions <- table_spec$positions
  alignment <- if (!is.null(table_spec$alignment)) table_spec$alignment else "center"

  if (alignment == "left") {
    xmin <- min(positions) - pad
    xmax <- max(positions + widths) + pad
  } else if (alignment == "right") {
    xmin <- min(positions - widths) - pad
    xmax <- max(positions) + pad
  } else {
    xmin <- min(positions - widths / 2) - pad
    xmax <- max(positions + widths / 2) + pad
  }

  c(xmin, xmax)
}

split_table_width_multiplier <- function(n_columns) {
  n_columns <- as.integer(n_columns[[1]])

  if (is.na(n_columns) || n_columns <= 0L) {
    return(0)
  }

  if (n_columns == 1L) {
    return(0.5)
  }

  1 + (n_columns - 2L) / 3
}

# ─── Plot limits ─────────────────────────────────────────────────────────────

default_plot_background_limits <- function(forest_data,
                                           exponentiate = FALSE,
                                           include_zero = TRUE,
                                           ref_line = NULL) {
  xmin <- min(forest_data$conf.low, na.rm = TRUE)
  xmax <- max(forest_data$conf.high, na.rm = TRUE)

  if (isTRUE(include_zero)) {
    null_value <- if (is.null(ref_line)) {
      if (isTRUE(exponentiate)) 1 else 0
    } else {
      ref_line
    }
    xmin <- min(xmin, null_value)
    xmax <- max(xmax, null_value)
  }

  if (isTRUE(exponentiate)) {
    pad_mult <- 1.08
    c(xmin / pad_mult, xmax * pad_mult)
  } else {
    span <- xmax - xmin
    pad <- if (is.finite(span) && span > 0) span * 0.08 else max(abs(xmax), 1) * 0.08
    c(xmin - pad, xmax + pad)
  }
}

# ─── ggplot2 table panel ─────────────────────────────────────────────────────

validate_ci_limits <- function(ci_limits = NULL, exponentiate = FALSE) {
  if (is.null(ci_limits)) {
    return(NULL)
  }

  if (!is.numeric(ci_limits) || length(ci_limits) != 2L ||
      anyNA(ci_limits) || any(!is.finite(ci_limits))) {
    stop("`ci_limits` must be `NULL` or a numeric vector of length 2.", call. = FALSE)
  }

  ci_limits <- sort(ci_limits)

  if (ci_limits[[1]] == ci_limits[[2]]) {
    stop("`ci_limits` must contain two distinct values.", call. = FALSE)
  }

  if (isTRUE(exponentiate) && any(ci_limits <= 0)) {
    stop("`ci_limits` must be positive for exponentiated plots.", call. = FALSE)
  }

  ci_limits
}

build_ci_plot_data <- function(data, ci_limits = NULL, exponentiate = FALSE) {
  data$ci_low <- data$conf.low
  data$ci_high <- data$conf.high
  data$ci_estimate <- data$estimate
  data$ci_truncated_left <- FALSE
  data$ci_truncated_right <- FALSE
  data$ci_arrow_left_start <- NA_real_
  data$ci_arrow_right_start <- NA_real_

  if (is.null(ci_limits)) {
    return(data)
  }

  lower <- ci_limits[[1]]
  upper <- ci_limits[[2]]
  data$ci_truncated_left <- data$conf.low < lower
  data$ci_truncated_right <- data$conf.high > upper
  data$ci_low <- pmax(data$conf.low, lower)
  data$ci_high <- pmin(data$conf.high, upper)
  data$ci_estimate <- pmin(pmax(data$estimate, lower), upper)

  if (isTRUE(exponentiate)) {
    arrow_ratio <- exp(log(upper / lower) * 0.025)
    data$ci_arrow_left_start[data$ci_truncated_left] <- lower * arrow_ratio
    data$ci_arrow_right_start[data$ci_truncated_right] <- upper / arrow_ratio
  } else {
    arrow_offset <- (upper - lower) * 0.025
    data$ci_arrow_left_start[data$ci_truncated_left] <- lower + arrow_offset
    data$ci_arrow_right_start[data$ci_truncated_right] <- upper - arrow_offset
  }

  data
}

#' Build a ggplot2 table panel for one side of a split forest plot.
#'
#' Uses symmetric expansion and uniform margins.  The "equal spacing"
#' guarantee comes from `combine_split_forest_plot()` giving both table
#' panels the same patchwork width — not from per-side padding hacks here.
#' @keywords internal
#' @noRd
build_forest_table_plot <- function(table_spec,
                                    stripe_data,
                                    has_groupings = FALSE,
                                    grouping_strip_position = c("left", "right"),
                                    table_position = c("left", "right"),
                                    striped_rows = FALSE,
                                    stripe_fill = "grey95",
                                    stripe_colour = NA,
                                    stripe_alpha = 1,
                                    text_size = 3.2,
                                    grid_lines = FALSE,
                                    grid_line_colour = "black",
                                    grid_line_size = 0.3,
                                    grid_line_linetype = 1,
                                    x_expand = ggplot2::expansion(mult = 0.05),
                                    plot_margin = ggplot2::margin(5.5, 4, 5.5, 4),
                                    text_hjust = 0.5,
                                    header_hjust = 0.5,
                                    header_text_size = 11,
                                    header_fontface = "bold",
                                    header_family = NULL) {
  grouping_strip_position <- match.arg(grouping_strip_position)
  table_position <- match.arg(table_position)

  if (all(is.na(table_spec$positions))) {
    table_spec <- layout_center_table_spec(table_spec, text_size = text_size)
  }

  p <- ggplot2::ggplot(
    table_spec$table_data,
    ggplot2::aes(x = .data$column_position, y = .data$row_key, label = .data$text)
  )

  x_limits <- compute_table_x_limits(table_spec)

  if (isTRUE(striped_rows)) {
    p <- p + ggplot2::geom_rect(
      data = stripe_data[stripe_data$fill_key == "stripe", , drop = FALSE],
      mapping = ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = stripe_fill,
      colour = stripe_colour,
      alpha = stripe_alpha
    )
  }

  if (isTRUE(grid_lines)) {
    line_data <- build_table_line_data(stripe_data, has_groupings = has_groupings)

    if (nrow(line_data) > 0L) {
      p <- p + ggplot2::geom_hline(
        data = line_data,
        mapping = ggplot2::aes(yintercept = .data$yintercept),
        inherit.aes = FALSE,
        colour = grid_line_colour,
        linewidth = grid_line_size,
        linetype = grid_line_linetype
      )
    }
  }

  p <- p +
    ggplot2::geom_text(hjust = text_hjust, size = text_size, lineheight = 0.95) +
    ggplot2::scale_x_continuous(
      breaks = table_spec$header_positions,
      labels = table_spec$headers,
      position = "top",
      expand = x_expand,
      limits = x_limits
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_y_discrete(labels = function(x) rep("", length(x)), drop = TRUE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.title            = ggplot2::element_blank(),
      axis.text.y           = ggplot2::element_blank(),
      axis.text.x.bottom    = ggplot2::element_blank(),
      axis.text.x.top       = ggplot2::element_text(
        face = header_fontface,
        family = header_family,
        size = header_text_size,
        colour = "black",
        hjust = header_hjust,
        margin = ggplot2::margin(b = 0)
      ),
      axis.ticks            = ggplot2::element_blank(),
      panel.grid.major.y    = ggplot2::element_blank(),
      panel.grid.major.x    = ggplot2::element_blank(),
      panel.grid.minor      = ggplot2::element_blank(),
      strip.background      = ggplot2::element_blank(),
      strip.text.y          = ggplot2::element_blank(),
      strip.text.y.left     = ggplot2::element_blank(),
      strip.text.y.right    = ggplot2::element_blank(),
      strip.placement       = "outside",
      plot.margin           = plot_margin
    )

  if (isTRUE(has_groupings)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(grouping_panel),
      ncol = 1, scales = "free_y",
      strip.position = grouping_strip_position
    )
  }

  p
}

# ─── Final assembly ──────────────────────────────────────────────────────────

#' Combine a forest plot with one table on either side.
#'
#' The key idea: both table panels are given the same patchwork width (the max
#' of the two measured content widths).  This guarantees the forest plot is
#' centred and both sides have equal spacing, with no per-side padding logic.
#'
#' @param plot The forest plot (ggplot2 object).
#' @param left_table Left-side table plot, or NULL if none.
#' @param right_table Right-side table plot, or NULL if none.
#' @param left_spec Left table_spec (needs `content_width`), or NULL.
#' @param right_spec Right table_spec (needs `content_width`), or NULL.
#' @param plot_width Width ratio for the forest plot panel.
#' @keywords internal
#' @noRd
combine_split_forest_plot <- function(plot,
                                      left_table = NULL,
                                      right_table = NULL,
                                      left_spec = NULL,
                                      right_spec = NULL,
                                      plot_width = 2.5) {
  left_w  <- if (!is.null(left_spec))  left_spec$content_width  else 0
  right_w <- if (!is.null(right_spec)) right_spec$content_width else 0

  panels <- list()
  widths <- numeric()

  if (!is.null(left_table)) {
    panels <- c(panels, list(left_table))
    widths <- c(widths, left_w)
  }

  panels <- c(panels, list(plot))
  widths <- c(widths, plot_width)

  if (!is.null(right_table)) {
    panels <- c(panels, list(right_table))
    widths <- c(widths, right_w)
  }

  patchwork::wrap_plots(panels, nrow = 1, widths = widths)
}

#' Convenience wrapper for single-table layouts (table on one side only).
#' @keywords internal
#' @noRd
combine_forest_plot_and_table <- function(plot, table_plot,
                                          table_position = c("left", "right"),
                                          table_width = 2.2,
                                          plot_width = 2.4) {
  table_position <- match.arg(table_position)
  widths <- c(table_width, plot_width)

  if (table_position == "left") {
    patchwork::wrap_plots(table_plot, plot, nrow = 1, widths = widths)
  } else {
    patchwork::wrap_plots(plot, table_plot, nrow = 1, widths = rev(widths))
  }
}
