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

forest_display_reserved_columns <- function() {
  c(
    "row_key", "grouping_panel", "row_type", "display_label",
    ".forest_source_row", ".display_identity"
  )
}

forest_p_method <- function(data) {
  method <- if (inherits(data, "forest_data")) {
    forest_metadata(data)$p_method
  } else {
    attr(data, "p_method", exact = TRUE)
  }

  if (is.null(method)) "overall" else method
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
        "Ratio estimates and logarithmic axes require strictly positive `estimate`, `conf.low`, and `conf.high` values.",
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
    source_names <- if (!is.null(data) && inherits(data, "forest_data")) {
      names(forest_source_columns(data))
    } else if (!is.null(data)) {
      source_columns <- attr(data, "source_columns")
      if (is.character(source_columns)) source_columns else names(source_columns)
    } else {
      NULL
    }
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
    group = "group", groups = "group", model = "group", models = "group",
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
    column_mapping <- if (inherits(data, "forest_data")) {
      forest_column_mapping(data)
    } else {
      attr(data, "column_mapping", exact = TRUE)
    }
    if (!is.character(column_mapping)) {
      column_mapping <- NULL
    }
    source_names <- if (inherits(data, "forest_data")) {
      names(forest_source_columns(data))
    } else {
      source_columns <- attr(data, "source_columns", exact = TRUE)
      if (is.character(source_columns) && is.null(names(source_columns))) {
        source_columns
      } else {
        names(source_columns)
      }
    }
    exact <- columns %in% c(names(data), source_names)
  } else {
    exact <- rep(FALSE, length(columns))
    source_names <- character()
    column_mapping <- NULL
  }

  normalized <- gsub("\\s+", "", tolower(columns))
  resolved <- unname(aliases[normalized])
  interval_alias <- normalized %in% c("conf.low", "conflow", "conf.high", "confhigh")
  p_value_alias <- normalized %in% "p.value"
  mapped_p_value <- rep(FALSE, length(columns))
  if (!is.null(column_mapping) && "p.value" %in% names(column_mapping)) {
    source_p_value <- unname(column_mapping[["p.value"]])
    if (length(source_p_value) == 1L && !is.na(source_p_value) &&
        nzchar(source_p_value)) {
      mapped_p_value <- columns == source_p_value
    }
  }
  subgroup_alias <- normalized %in% c("subgroup", "subgroups")
  has_subgroup_values <- !is.null(data) && has_table_values(data, "subgroup")
  has_source_subgroup <- columns %in% source_names
  preserve_subgroup_alias <- subgroup_alias &
    !has_subgroup_values & !has_source_subgroup
  exact_override <- exact & !interval_alias & !p_value_alias & !mapped_p_value &
    !preserve_subgroup_alias
  resolved[exact_override] <- columns[exact_override]
  resolved[mapped_p_value] <- "p"

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

map_source_group_column_label <- function(column_labels,
                                          column_mapping,
                                          column_keys) {
  if (is.null(column_labels) || is.null(names(column_labels)) ||
      !"group" %in% column_keys || !is.character(column_mapping) ||
      !"group" %in% names(column_mapping)) {
    return(column_labels)
  }

  source_group <- unname(column_mapping[["group"]])

  if (length(source_group) != 1L || is.na(source_group) || !nzchar(source_group)) {
    return(column_labels)
  }

  source_matches <- names(column_labels) == source_group
  names(column_labels)[source_matches] <- "group"
  column_labels
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
    if (has_table_values(data, "group")) "group",
    if (has_table_values(data, "n")) "n",
    if (has_table_values(data, "events")) "events",
    "estimate"
  )
}

default_split_left_columns <- function(data) {
  c(
    "term",
    if (has_table_values(data, "group")) "group",
    if (has_table_values(data, "n")) "n",
    if (has_table_values(data, "events")) "events"
  )
}

default_split_right_columns <- function(data) {
  "estimate"
}

default_group_table_header <- function(data) {
  if (inherits(data, "ggforestplot_bound_models")) {
    return("Model")
  }

  if (inherits(data, "forest_data")) {
    column_mapping <- forest_column_mapping(data)

    if (is.character(column_mapping) && "group" %in% names(column_mapping)) {
      source_name <- unname(column_mapping[["group"]])

      if (length(source_name) == 1L && !is.na(source_name) && nzchar(source_name)) {
        return(source_name)
      }
    }
  }

  "Group"
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

warn_deprecated_table_headers <- function(supplied) {
  replacements <- c(
    term_header = "`column_labels = c(term = \"...\")`",
    n_header = "`column_labels = c(n = \"...\")`",
    events_header = "`column_labels = c(events = \"...\")`",
    p_header = "`column_labels = c(p = \"...\")`"
  )

  for (arg in names(supplied)[supplied]) {
    warn_deprecated_argument(arg, replacements[[arg]])
  }

  invisible(NULL)
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

  has_subgroups <- "subgroup" %in% names(data) &&
    any(!is.na(data$subgroup) & nzchar(data$subgroup))

  if (isTRUE(has_subgroups)) {
    stop(
      "`sort_terms` must be \"none\" when `subgroup` is used so source row order is preserved.",
      call. = FALSE
    )
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
  grouping_levels <- if (inherits(data, "forest_data")) {
    forest_metadata(data)$grouping_levels
  } else {
    attr(data, "grouping_levels")
  }
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

validate_subgroup_blocks <- function(data) {
  if (!"subgroup" %in% names(data)) {
    return(invisible(data))
  }

  subgroup <- as.character(data$subgroup)
  present <- !is.na(subgroup) & nzchar(subgroup)

  if (!any(present)) {
    return(invisible(data))
  }

  grouping <- if ("grouping" %in% names(data)) {
    as.character(data$grouping)
  } else {
    rep(NA_character_, nrow(data))
  }
  panel_key <- ifelse(
    is.na(grouping) | !nzchar(grouping),
    "(Ungrouped)",
    grouping
  )
  repeated <- character()

  for (panel in unique(panel_key)) {
    idx <- which(panel_key == panel)
    panel_subgroup <- subgroup[idx]
    panel_present <- !is.na(panel_subgroup) & nzchar(panel_subgroup)
    run_key <- ifelse(
      panel_present,
      paste0("subgroup:", nchar(panel_subgroup), ":", panel_subgroup),
      "standalone"
    )
    runs <- rle(run_key)
    run_starts <- c(1L, utils::head(cumsum(runs$lengths), -1L) + 1L)
    run_subgroups <- panel_subgroup[run_starts]
    run_subgroups <- run_subgroups[
      !is.na(run_subgroups) & nzchar(run_subgroups)
    ]
    duplicated_subgroups <- unique(run_subgroups[duplicated(run_subgroups)])

    if (length(duplicated_subgroups) > 0L) {
      panel_labels <- if (identical(panel, "(Ungrouped)")) {
        duplicated_subgroups
      } else {
        paste0(duplicated_subgroups, " [facet: ", panel, "]")
      }
      repeated <- c(repeated, panel_labels)
    }
  }

  if (length(repeated) > 0L) {
    stop(
      paste0(
        "Each `subgroup` must form one contiguous block within a facet. ",
        "Repeated noncontiguous subgroup block(s): ",
        paste(repeated, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(data)
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
  canonical_label <- "Beta"
  estimate_scale <- "identity"
  model_family <- if (inherits(model, "glm") && !is.null(model$family)) {
    model$family
  } else {
    tryCatch(stats::family(model), error = function(e) NULL)
  }

  if (inherits(model, "coxph")) {
    auto_exponentiate <- TRUE
    canonical_label <- "HR"
  } else if (!is.null(model_family)) {
    family <- model_family$family
    link <- model_family$link

    if (identical(family, "binomial") && identical(link, "logit")) {
      auto_exponentiate <- TRUE
      canonical_label <- "OR"
    } else if (identical(family, "binomial") && identical(link, "log")) {
      auto_exponentiate <- TRUE
      canonical_label <- "RR"
    } else if (identical(family, "binomial") && identical(link, "identity")) {
      estimate_scale <- "risk_difference"
      canonical_label <- "RD"
    } else if (identical(link, "log")) {
      auto_exponentiate <- TRUE
      canonical_label <- if (family %in% c("poisson", "quasipoisson")) "IRR" else "Ratio"
    }
  }

  resolved_exponentiate <- if (is.null(exponentiate)) auto_exponentiate else isTRUE(exponentiate)

  if (isTRUE(resolved_exponentiate)) {
    estimate_scale <- "ratio"
    estimate_label <- if (isTRUE(auto_exponentiate)) canonical_label else "Ratio"
  } else if (isTRUE(auto_exponentiate)) {
    estimate_scale <- "log"
    estimate_label <- sprintf("log(%s)", canonical_label)
  } else {
    estimate_label <- canonical_label
  }

  list(
    exponentiate = resolved_exponentiate,
    estimate_scale = estimate_scale,
    axis_transform = default_axis_transform(estimate_scale),
    estimate_label = estimate_label,
    effect_label = estimate_label,
    reference_value = default_reference_value(estimate_scale),
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
                                   force_group_labels = FALSE, align_groups = FALSE) {
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
                                    align_groups = FALSE) {
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
                                    align_groups = FALSE) {
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
  collapse_grouped_values(
    formatted,
    group,
    force_group_labels = force_group_labels,
    align_groups = align_groups
  )
}

format_forest_table_values <- function(values,
                                       group = NULL,
                                       force_group_labels = FALSE,
                                       align_groups = FALSE) {
  formatted <- as.character(values)
  formatted[is.na(formatted)] <- ""
  collapse_grouped_values(
    formatted,
    group,
    force_group_labels = force_group_labels,
    align_groups = align_groups
  )
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

    grouping_levels <- if (inherits(data, "forest_data")) {
      forest_metadata(data)$grouping_levels
    } else {
      attr(data, "grouping_levels")
    }
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
  if (".display_identity" %in% names(data)) {
    return(assign_hierarchical_row_keys(data, has_groupings))
  }

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

assign_hierarchical_row_keys <- function(data, has_groupings) {
  panel_values <- observed_grouping_panels(data, has_groupings)
  panel_key_labels <- labels_requiring_panel_keys(data, has_groupings)
  data$row_key <- NA_character_
  all_levels <- character()

  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    panel_identities <- unique(data$.display_identity[idx])
    first_rows <- match(panel_identities, data$.display_identity[idx])
    panel_labels <- data$label[idx][first_rows]
    keys <- make_panel_row_keys(pv, panel_labels, panel_key_labels)
    keys <- make.unique(keys, sep = "___")
    if (length(all_levels) > 0L) {
      keys <- utils::tail(
        make.unique(c(all_levels, keys), sep = "___"),
        length(keys)
      )
    }
    row_map <- stats::setNames(keys, panel_identities)

    data$row_key[idx] <- unname(row_map[data$.display_identity[idx]])
    all_levels <- c(all_levels, keys)
  }

  factor_levels <- if (
    "row_type" %in% names(data) &&
      any(data$row_type == "subgroup_header")
  ) {
    rev(all_levels)
  } else {
    all_levels
  }
  data$row_key <- factor(data$row_key, levels = factor_levels)
  data
}

#' Build axis label lookup: row_key -> display label.
#' @keywords internal
#' @noRd
build_axis_labels <- function(data, has_groupings) {
  if ("display_label" %in% names(data)) {
    row_levels <- levels(data$row_key)
    first_rows <- match(row_levels, as.character(data$row_key))
    return(stats::setNames(data$display_label[first_rows], row_levels))
  }

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

encode_display_identity <- function(row_type, subgroup, label) {
  subgroup <- ifelse(
    is.na(subgroup) | !nzchar(subgroup),
    "",
    subgroup
  )
  paste0(
    nchar(row_type), ":", row_type, "|",
    nchar(subgroup), ":", subgroup, "|",
    nchar(label), ":", label
  )
}

blank_display_row <- function(row) {
  row[] <- lapply(row, function(column) {
    if (is.list(column)) {
      column[] <- list(NA)
    } else {
      column[] <- NA
    }
    column
  })
  row
}

as_forest_display_frame <- function(data) {
  metadata <- if (inherits(data, "forest_data")) forest_metadata(data) else NULL
  out <- if (inherits(data, "forest_data")) {
    strip_forest_data_class(data)
  } else {
    as.data.frame(data, stringsAsFactors = FALSE)
  }
  class(out) <- "data.frame"

  if (!is.null(metadata)) {
    attr(out, "source_columns") <- metadata$source_columns
    attr(out, "column_mapping") <- metadata$column_mapping
    attr(out, "grouping_levels") <- metadata$grouping_levels
    attr(out, "conf.level") <- metadata$conf_level
    attr(out, "p_method") <- forest_p_method(data)
  }

  out
}

expand_subgroup_display_rows <- function(data, has_groupings) {
  display_data <- as_forest_display_frame(data)
  p_method <- forest_p_method(display_data)
  display_data$.forest_source_row <- seq_len(nrow(display_data))
  display_data$row_type <- "estimate"
  display_data$display_label <- display_data$label

  has_subgroups <- "subgroup" %in% names(display_data) &&
    any(!is.na(display_data$subgroup) & nzchar(display_data$subgroup))

  if (!isTRUE(has_subgroups)) {
    return(display_data)
  }

  child <- !is.na(display_data$subgroup) & nzchar(display_data$subgroup)
  display_data$display_label[child] <- paste0("   ", display_data$label[child])
  panel_values <- observed_grouping_panels(display_data, has_groupings)
  parts <- list()

  for (pv in panel_values) {
    idx <- if (has_groupings) {
      which(display_data$grouping_panel == pv)
    } else {
      seq_len(nrow(display_data))
    }
    previous_subgroup <- NA_character_

    for (position in seq_along(idx)) {
      row_index <- idx[[position]]
      subgroup <- display_data$subgroup[[row_index]]
      is_subgroup <- !is.na(subgroup) && nzchar(subgroup)
      starts_subgroup <- is_subgroup &&
        (is.na(previous_subgroup) || !identical(subgroup, previous_subgroup))

      if (starts_subgroup) {
        header <- blank_display_row(display_data[row_index, , drop = FALSE])
        header$term <- subgroup
        header$label <- subgroup
        header$subgroup <- subgroup
        header$grouping_panel <- display_data$grouping_panel[row_index]
        header$row_type <- "subgroup_header"
        header$display_label <- subgroup

        block_end <- position
        while (block_end < length(idx)) {
          next_subgroup <- display_data$subgroup[[idx[[block_end + 1L]]]]
          if (is.na(next_subgroup) || !nzchar(next_subgroup) ||
              !identical(next_subgroup, subgroup)) {
            break
          }
          block_end <- block_end + 1L
        }
        block_rows <- idx[seq.int(position, block_end)]
        if (identical(p_method, "overall")) {
          block_p_values <- display_data$p.value[block_rows]
          block_p_values <- block_p_values[!is.na(block_p_values)]
          if (length(block_p_values) > 0L) {
            header$p.value <- block_p_values[[1L]]
          }
          display_data$p.value[block_rows] <- NA_real_
        }
        block_separators <- display_data$separate_groups[block_rows]
        separator_present <- !is.na(block_separators) &
          nzchar(block_separators)
        common_separator <- unique(block_separators[separator_present])
        if (all(separator_present) && length(common_separator) == 1L) {
          header$separate_groups <- common_separator
        }

        parts[[length(parts) + 1L]] <- header
      }

      parts[[length(parts) + 1L]] <- display_data[row_index, , drop = FALSE]
      previous_subgroup <- if (is_subgroup) subgroup else NA_character_
    }
  }

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  for (attribute in c(
    "source_columns", "column_mapping", "grouping_levels", "conf.level",
    "p_method"
  )) {
    attr(out, attribute) <- attr(display_data, attribute, exact = TRUE)
  }
  out
}

subgroup_header_rows <- function(data, header_row) {
  subgroup <- as.character(header_row$subgroup[[1L]])
  if (is.na(subgroup) || !nzchar(subgroup)) {
    subgroup <- as.character(header_row$term[[1L]])
  }

  same_subgroup <- !is.na(data$subgroup) & data$subgroup == subgroup
  header_panel <- as.character(header_row$grouping_panel[[1L]])
  data_panel <- as.character(data$grouping_panel)
  same_panel <- if (is.na(header_panel) || !nzchar(header_panel)) {
    is.na(data_panel) | !nzchar(data_panel)
  } else {
    !is.na(data_panel) & data_panel == header_panel
  }
  data[same_subgroup & same_panel, , drop = FALSE]
}

format_subgroup_header_p_values <- function(data,
                                            header_row,
                                            p_digits,
                                            align_groups) {
  block <- subgroup_header_rows(data, header_row)

  if (nrow(block) == 0L) {
    return("")
  }

  block_values <- block$p.value[!is.na(block$p.value)]
  if (length(block_values) > 0L &&
      length(unique(block_values)) == 1L) {
    return(format_forest_p_values(
      block_values[[1L]],
      p_digits = p_digits
    ))
  }

  group_values <- as.character(block$group)
  has_groups <- any(!is.na(group_values) & nzchar(group_values))
  if (!isTRUE(has_groups)) {
    values <- block$p.value[!is.na(block$p.value)]
    value <- if (length(values) == 0L) NA_real_ else values[[1L]]
    return(format_forest_p_values(value, p_digits = p_digits))
  }

  group_key <- ifelse(
    is.na(group_values) | !nzchar(group_values),
    "(Ungrouped)",
    group_values
  )
  group_levels <- unique(group_key)
  values <- vapply(group_levels, function(group) {
    candidates <- block$p.value[group_key == group & !is.na(block$p.value)]
    if (length(candidates) == 0L) NA_real_ else candidates[[1L]]
  }, numeric(1))

  format_forest_p_values(
    values,
    group = group_levels,
    p_digits = p_digits,
    align_groups = align_groups
  )
}

subgroup_header_group_values <- function(data, header_row) {
  block <- subgroup_header_rows(data, header_row)
  groups <- as.character(block$group)

  unique(groups)
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

#' Main entry point. Orchestrates display-row expansion and layout passes.
#' @keywords internal
#' @noRd
build_forest_plot_data <- function(data) {
  if (!"subgroup" %in% names(data)) {
    data$subgroup <- NA_character_
  }
  validate_subgroup_blocks(data)

  has_groupings <- any(!is.na(data$grouping) & nzchar(data$grouping))
  forest_data <- data
  forest_data$grouping_panel <- assign_grouping_panels(forest_data, has_groupings)

  forest_data <- prefix_ambiguous_labels(forest_data, has_groupings)
  plot_data <- expand_subgroup_display_rows(forest_data, has_groupings)
  plot_data$.display_identity <- encode_display_identity(
    plot_data$row_type,
    ifelse(plot_data$row_type == "estimate", plot_data$subgroup, NA_character_),
    plot_data$label
  )
  plot_data <- assign_row_keys(plot_data, has_groupings)

  estimate_rows <- plot_data$row_type == "estimate"
  source_rows <- plot_data$.forest_source_row[estimate_rows]
  source_keys <- as.character(plot_data$row_key[estimate_rows])
  forest_data$row_key <- factor(
    source_keys[match(seq_len(nrow(forest_data)), source_rows)],
    levels = levels(plot_data$row_key)
  )

  stripe_data <- build_stripe_rectangles(plot_data, has_groupings)
  separator_data <- build_separate_lines(plot_data, has_groupings)
  axis_labels <- build_axis_labels(plot_data, has_groupings)
  plot_data$.forest_source_row <- NULL
  plot_data$.display_identity <- NULL

  list(
    forest_data = forest_data,
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
  y_limits <- extract_trained_y_limits(plot)
  align_forest_state_to_row_levels(state, y_limits)
}

align_forest_state_to_row_levels <- function(state, requested_levels) {
  base_display_data <- if (is.null(state$full_display_data)) {
    if (is.null(state$display_data)) state$forest_data else state$display_data
  } else {
    state$full_display_data
  }
  base_forest_data <- if (is.null(state$full_forest_data)) {
    state$forest_data
  } else {
    state$full_forest_data
  }
  display_data <- base_display_data
  row_levels <- levels(display_data$row_key)

  if (is.null(row_levels) || is.null(requested_levels)) {
    return(state)
  }

  requested_levels <- as.character(requested_levels)
  matched_limits <- unique(requested_levels[requested_levels %in% row_levels])

  if (length(matched_limits) == 0L) {
    return(state)
  }

  keep_display_rows <- as.character(display_data$row_key) %in% matched_limits
  aligned_display_data <- display_data[keep_display_rows, , drop = FALSE]
  aligned_display_data$row_key <- factor(
    as.character(aligned_display_data$row_key),
    levels = matched_limits
  )

  keep_estimate_rows <- as.character(base_forest_data$row_key) %in% matched_limits
  aligned_forest_data <- base_forest_data[keep_estimate_rows, , drop = FALSE]
  aligned_forest_data$row_key <- factor(
    as.character(aligned_forest_data$row_key),
    levels = matched_limits
  )
  if (isTRUE(state$has_groupings)) {
    panel_levels <- observed_grouping_panels(base_display_data, TRUE)
    aligned_display_data$grouping_panel <- factor(
      as.character(aligned_display_data$grouping_panel),
      levels = panel_levels
    )
    aligned_forest_data$grouping_panel <- factor(
      as.character(aligned_forest_data$grouping_panel),
      levels = panel_levels
    )
  }

  aligned_state <- state
  aligned_state$forest_data <- aligned_forest_data
  aligned_state$display_data <- aligned_display_data
  aligned_state$stripe_data <- build_stripe_rectangles(
    aligned_display_data,
    state$has_groupings
  )
  if (!is.null(state$separator_data)) {
    aligned_state$separator_data <- build_separate_lines(
      aligned_display_data,
      state$has_groupings
    )
  }
  aligned_state
}

align_forest_row_layers_to_state <- function(plot, state) {
  stripe_index <- state$stripe_layer_index
  if (!is.null(stripe_index) && stripe_index <= length(plot$layers)) {
    old_stripes <- plot$layers[[stripe_index]]$data
    new_stripes <- state$stripe_data[
      state$stripe_data$fill_key == "stripe",
      ,
      drop = FALSE
    ]

    if (nrow(old_stripes) > 0L && nrow(new_stripes) > 0L) {
      new_stripes$xmin <- old_stripes$xmin[[1L]]
      new_stripes$xmax <- old_stripes$xmax[[1L]]
    }
    plot$layers[[stripe_index]]$data <- new_stripes
  }

  separator_index <- state$separator_layer_index
  if (!is.null(separator_index) && separator_index <= length(plot$layers) &&
      !is.null(state$separator_data)) {
    plot$layers[[separator_index]]$data <- state$separator_data
  }

  plot$ggforestplotR_state <- state
  plot
}

build_forest_table_data <- function(data,
                                    term_header = "Term",
                                    n_header = "N",
                                    events_header = "Events",
                                    estimate_label = "Estimate",
                                    conf.level = NULL,
                                    p_header = "P-value",
                                    digits = 2,
                                    estimate_digits = NULL,
                                    interval_digits = NULL,
                                    p_digits = NULL,
                                    estimate_fmt = NULL,
                                    ci_fmt = NULL,
                                    column_labels = NULL,
                                    columns = NULL,
                                    display_data = data) {
  digits <- resolve_table_digits(
    digits = digits,
    estimate_digits = estimate_digits,
    interval_digits = interval_digits,
    p_digits = p_digits
  )
  if (is.null(conf.level) && inherits(data, "forest_data")) {
    conf.level <- forest_metadata(data)$conf_level
  }
  confidence_label <- if (is.null(conf.level) || is.na(conf.level)) {
    "CI"
  } else {
    sprintf("%s CI", format_conf_level_label(conf.level))
  }
  source_storage <- if (inherits(data, "forest_data")) {
    forest_source_columns(data)
  } else {
    source_columns <- attr(data, "source_columns")
    if (is.character(source_columns) && !is.null(names(source_columns))) {
      source_columns
    } else {
      source_names <- if (is.character(source_columns)) source_columns else names(source_columns)
      stats::setNames(source_names, source_names)
    }
  }
  source_column_names <- names(source_storage)
  column_mapping <- if (inherits(data, "forest_data")) {
    forest_column_mapping(data)
  } else {
    attr(data, "column_mapping")
  }
  if (is.list(column_mapping)) {
    column_mapping <- NULL
  }
  has_groups <- has_table_values(data, "group")
  align_groups <- has_groups
  group_levels <- if (is.factor(data$group)) levels(data$group) else NULL
  p_method <- forest_p_method(data)
  group_header <- default_group_table_header(data)
  row_levels <- levels(display_data$row_key)
  row_parts <- vector("list", length(row_levels))
  row_group_values <- vector("list", length(row_levels))
  row_types <- stats::setNames(rep("estimate", length(row_levels)), row_levels)
  mapped_row_label_columns <- unique(unname(column_mapping[c("term", "label")]))
  mapped_row_label_columns <- mapped_row_label_columns[!is.na(mapped_row_label_columns)]
  row_label_data_columns <- unique(c(
    "term", "label", mapped_row_label_columns
  ))
  has_subgroup_headers <- "row_type" %in% names(display_data) &&
    any(display_data$row_type == "subgroup_header")
  visible_data_columns <- names(display_data)[
    !startsWith(names(display_data), "..source..") &
      !names(display_data) %in% forest_display_reserved_columns()
  ]
  extra_columns <- unique(c(visible_data_columns, source_column_names))
  extra_columns <- setdiff(extra_columns, "group")
  structural_fields <- c(
    "row_key", "grouping_panel", "term_text", "group_text", "n_text",
    "events_text", "estimate_text", "estimate_value_text", "ci_text",
    "p_text"
  )
  extra_storage_lookup <- stats::setNames(extra_columns, extra_columns)
  colliding_extra_columns <- intersect(extra_columns, structural_fields)
  used_storage_names <- unique(c(extra_columns, structural_fields))

  for (i in seq_along(colliding_extra_columns)) {
    extra <- colliding_extra_columns[[i]]
    stored_name <- paste0("..forest_table_source..", i)
    while (stored_name %in% used_storage_names) {
      stored_name <- paste0(stored_name, ".")
    }
    extra_storage_lookup[[extra]] <- stored_name
    used_storage_names <- c(used_storage_names, stored_name)
  }

  for (i in seq_along(row_levels)) {
    row_key <- row_levels[[i]]
    idx <- which(as.character(display_data$row_key) == row_key)

    if (length(idx) == 0L) next

    rd <- display_data[idx, , drop = FALSE]
    row_type <- if ("row_type" %in% names(rd)) {
      as.character(rd$row_type[[1L]])
    } else {
      "estimate"
    }
    row_types[[row_key]] <- row_type
    is_header <- identical(row_type, "subgroup_header")
    is_child <- !is_header &&
      any(!is.na(rd$subgroup) & nzchar(rd$subgroup))
    term_text <- if ("display_label" %in% names(rd)) {
      rd$display_label[[1L]]
    } else {
      rd$label[[1L]]
    }
    group_values <- if (!has_groups && "group" %in% names(source_storage)) {
      rd[[source_storage[["group"]]]]
    } else {
      rd$group
    }
    row_group_values[[i]] <- if (is_header && identical(p_method, "overall")) {
      subgroup_header_group_values(data, rd)
    } else {
      as.character(rd$group)
    }
    row_parts[[i]] <- data.frame(
      row_key = row_key,
      grouping_panel = rd$grouping_panel[1L],
      term_text = term_text,
      group_text = if (is_header) "" else format_forest_table_values(
        group_values, rd$group, align_groups = align_groups
      ),
      n_text = if (is_header) "" else format_forest_table_values(
        rd$n, rd$group, align_groups = align_groups
      ),
      events_text = if (is_header) "" else format_forest_table_values(
        rd$events, rd$group, align_groups = align_groups
      ),
      estimate_text = if (is_header) "" else format_forest_estimates(
        rd$estimate, rd$conf.low, rd$conf.high, rd$group,
        estimate_digits = digits$estimate_digits,
        interval_digits = digits$interval_digits,
        estimate_fmt = estimate_fmt,
        align_groups = align_groups
      ),
      estimate_value_text = if (is_header) "" else format_forest_estimates(
        rd$estimate, rd$conf.low, rd$conf.high, rd$group,
        estimate_digits = digits$estimate_digits,
        interval_digits = digits$interval_digits,
        estimate_fmt = if (is.null(estimate_fmt)) "{estimate}" else estimate_fmt,
        align_groups = align_groups
      ),
      ci_text = if (is_header) "" else format_forest_intervals(
        rd$conf.low, rd$conf.high, rd$group,
        interval_digits = digits$interval_digits,
        ci_fmt = ci_fmt,
        align_groups = align_groups
      ),
      p_text = if (is_header && identical(p_method, "overall")) {
        format_subgroup_header_p_values(
          data,
          rd,
          p_digits = digits$p_digits,
          align_groups = align_groups
        )
      } else if (is_header ||
                 (is_child && identical(p_method, "overall"))) {
        ""
      } else {
        format_forest_p_values(
          rd$p.value, rd$group,
          p_digits = digits$p_digits,
          align_groups = align_groups
        )
      },
      stringsAsFactors = FALSE
    )

    for (extra in extra_columns) {
      storage_field <- extra_storage_lookup[[extra]]
      if (isTRUE(has_subgroup_headers) && extra %in% row_label_data_columns) {
        if (is_header) {
          row_parts[[i]][[storage_field]] <- term_text
          next
        }

        label_values <- if (extra %in% names(source_storage)) {
          rd[[source_storage[[extra]]]]
        } else {
          rd[[extra]]
        }
        label_text <- format_forest_table_values(
          label_values,
          rd$group,
          align_groups = FALSE
        )
        if (is_child) {
          label_text <- paste0(
            "   ",
            gsub("\n", "\n   ", label_text, fixed = TRUE)
          )
        }
        row_parts[[i]][[storage_field]] <- label_text
        next
      }

      if (is_header) {
        row_parts[[i]][[storage_field]] <- ""
        next
      }

      values <- if (extra %in% names(source_storage)) {
        rd[[source_storage[[extra]]]]
      } else {
        rd[[extra]]
      }
      row_parts[[i]][[storage_field]] <- format_forest_table_values(
        values,
        rd$group,
        align_groups = align_groups
      )
    }
  }

  table_rows <- do.call(rbind, row_parts)
  table_rows$row_key <- factor(table_rows$row_key, levels = row_levels)
  attr(table_rows, "source_columns") <- source_column_names
  attr(table_rows, "column_mapping") <- column_mapping

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
    group = "group_text",
    n = "n_text",
    events = "events_text",
    estimate = if ("ci" %in% column_keys) "estimate_value_text" else "estimate_text",
    ci = "ci_text",
    p = "p_text"
  )
  header_lookup <- c(
    term = term_header,
    group = group_header,
    n = n_header,
    events = events_header,
    estimate = if ("ci" %in% column_keys) estimate_label else sprintf("%s (%s)", estimate_label, confidence_label),
    ci = confidence_label,
    p = p_header
  )
  extra_column_keys <- setdiff(column_keys, names(column_field_lookup))
  extra_field_lookup <- stats::setNames(
    unname(extra_storage_lookup[extra_column_keys]),
    extra_column_keys
  )
  column_field_lookup <- c(column_field_lookup, extra_field_lookup)
  header_lookup <- c(header_lookup, stats::setNames(extra_column_keys, extra_column_keys))

  mapped_row_label_columns <- intersect(mapped_row_label_columns, column_keys)
  header_lookup[mapped_row_label_columns] <- term_header

  column_labels <- map_source_group_column_label(
    column_labels,
    column_mapping = column_mapping,
    column_keys = column_keys
  )
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
    long_part <- data.frame(
      row_key = table_rows$row_key,
      grouping_panel = table_rows$grouping_panel,
      row_type = unname(row_types[as.character(table_rows$row_key)]),
      column_key = key,
      column_position = NA_real_,
      text = table_rows[[column_field_lookup[[key]]]],
      align_groups = isTRUE(has_groups) &&
        !key %in% c("term", mapped_row_label_columns),
      stringsAsFactors = FALSE
    )
    long_part$group_values <- I(row_group_values)
    long_parts[[i]] <- long_part
  }

  table_data <- do.call(rbind, long_parts)
  table_data$row_key <- factor(table_data$row_key, levels = row_levels)

  list(
    table_data = table_data,
    positions = rep(NA_real_, length(column_keys)),
    header_positions = rep(NA_real_, length(column_keys)),
    headers = unname(header_lookup[column_keys]),
    column_keys = column_keys,
    group_levels = group_levels
  )
}

expand_grouped_table_text <- function(table_data, group_levels = NULL) {
  parts <- lapply(seq_len(nrow(table_data)), function(i) {
    row <- table_data[i, , drop = FALSE]
    groups <- row$group_values[[1L]]
    lines <- strsplit(row$text[[1L]], "\n", fixed = TRUE)[[1L]]

    can_dodge <- isTRUE(row$align_groups[[1L]]) &&
      length(groups) > 1L &&
      length(lines) == length(groups)

    if (!can_dodge) {
      row$text_group <- NA_character_
      row$dodge_text <- FALSE
      return(row)
    }

    row <- row[rep(1L, length(groups)), , drop = FALSE]
    row$text <- lines
    row$text_group <- groups
    row$dodge_text <- TRUE
    row
  })

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  if (!is.null(group_levels)) {
    out$text_group <- factor(out$text_group, levels = group_levels)
  }
  out
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

left_align_center_table_columns <- function(table_spec,
                                            columns,
                                            inset = 0.02) {
  columns <- intersect(columns, table_spec$column_keys)
  if (length(columns) == 0L) {
    return(table_spec)
  }

  table_spec$table_data$text_hjust <- 0.5
  for (column in columns) {
    column_index <- match(column, table_spec$column_keys)
    left_position <- table_spec$positions[[column_index]] -
      table_spec$estimated_column_widths[[column_index]] / 2 + inset
    rows <- table_spec$table_data$column_key == column
    table_spec$table_data$column_position[rows] <- left_position
    table_spec$table_data$text_hjust[rows] <- 0
  }

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
    xmin <- min(positions)
    xmax <- max(positions + widths)
  } else if (alignment == "right") {
    xmin <- min(positions - widths)
    xmax <- max(positions)
  } else {
    xmin <- min(positions - widths / 2)
    xmax <- max(positions + widths / 2)
  }

  cell_fields <- c("column_key", "column_position", "text_hjust")
  if (all(cell_fields %in% names(table_spec$table_data))) {
    column_index <- match(
      table_spec$table_data$column_key,
      table_spec$column_keys
    )
    cell_widths <- widths[column_index]
    cell_positions <- table_spec$table_data$column_position
    cell_hjust <- table_spec$table_data$text_hjust
    valid_cells <- is.finite(cell_widths) & is.finite(cell_positions) &
      is.finite(cell_hjust)

    if (any(valid_cells)) {
      cell_left <- cell_positions - cell_hjust * cell_widths
      cell_right <- cell_positions + (1 - cell_hjust) * cell_widths
      xmin <- min(xmin, cell_left[valid_cells])
      xmax <- max(xmax, cell_right[valid_cells])
    }
  }

  c(xmin - pad, xmax + pad)
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

validate_composition_width <- function(value, arg) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value <= 0) {
    stop(sprintf("`%s` must be a single positive number.", arg), call. = FALSE)
  }

  invisible(value)
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
                                    facet_strip_position = c("left", "right"),
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
                                    header_family = NULL,
                                    dodge_width = 0.6) {
  facet_strip_position <- match.arg(facet_strip_position)
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

  text_data <- expand_grouped_table_text(
    table_spec$table_data,
    group_levels = table_spec$group_levels
  )
  centered_text <- text_data[!text_data$dodge_text, , drop = FALSE]
  grouped_text <- text_data[text_data$dodge_text, , drop = FALSE]
  text_layers <- list()

  if (nrow(centered_text) > 0L) {
    text_layers[[length(text_layers) + 1L]] <- if (
      "text_hjust" %in% names(centered_text)
    ) {
      ggplot2::geom_text(
        data = centered_text,
        mapping = ggplot2::aes(hjust = .data$text_hjust),
        size = text_size,
        lineheight = 0.95
      )
    } else {
      ggplot2::geom_text(
        data = centered_text,
        hjust = text_hjust,
        size = text_size,
        lineheight = 0.95
      )
    }
  }

  if (nrow(grouped_text) > 0L) {
    text_layers[[length(text_layers) + 1L]] <- if (
      "text_hjust" %in% names(grouped_text)
    ) {
      ggplot2::geom_text(
        data = grouped_text,
        mapping = ggplot2::aes(
          group = .data$text_group,
          hjust = .data$text_hjust
        ),
        position = ggplot2::position_dodge(width = dodge_width),
        size = text_size,
        lineheight = 0.95
      )
    } else {
      ggplot2::geom_text(
        data = grouped_text,
        mapping = ggplot2::aes(group = .data$text_group),
        position = ggplot2::position_dodge(width = dodge_width),
        hjust = text_hjust,
        size = text_size,
        lineheight = 0.95
      )
    }
  }

  p <- p +
    text_layers +
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
      strip.position = facet_strip_position,
      drop = FALSE
    )
  }

  p
}

# ─── Final assembly ──────────────────────────────────────────────────────────

resolved_plot_legend_position <- function(plot) {
  position <- plot$theme$legend.position

  if (is.null(position)) {
    position <- ggplot2::theme_get()$legend.position
  }

  if (!is.character(position) || length(position) != 1L || is.na(position)) {
    return(NULL)
  }

  position
}

wrap_forest_composition <- function(panels, widths, plot) {
  legend_position <- resolved_plot_legend_position(plot)
  collect_outer_legend <- legend_position %in% c("top", "bottom")

  out <- patchwork::wrap_plots(
    panels,
    nrow = 1,
    widths = widths,
    guides = if (collect_outer_legend) "collect" else "keep"
  )

  if (collect_outer_legend) {
    out <- out & ggplot2::theme(legend.position = legend_position)
  }

  out
}

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

  wrap_forest_composition(panels, widths = widths, plot = plot)
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
    panels <- list(table_plot, plot)
  } else {
    panels <- list(plot, table_plot)
    widths <- rev(widths)
  }

  wrap_forest_composition(panels, widths = widths, plot = plot)
}
