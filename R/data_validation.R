# --- Column resolution --------------------------------------------------------

resolve_column <- function(data, column, arg, required = TRUE) {
  if (is.null(column)) {
    if (required) {
      abort_forest(
        sprintf("`%s` must be supplied.", arg),
        class = "ggforestplotR_invalid_data"
      )
    }
    return(NULL)
  }

  if (!is.character(column) || length(column) != 1L) {
    abort_forest(
      sprintf("`%s` must be a single column name (string).", arg),
      class = "ggforestplotR_invalid_data"
    )
  }

  if (!column %in% names(data)) {
    abort_forest(
      sprintf("Column `%s` was not found in `data`.", column),
      class = "ggforestplotR_invalid_data"
    )
  }

  column
}

forest_display_reserved_columns <- function() {
  c(
    "row_key", "grouping_panel", "row_type", "display_label",
    ".forest_source_row", ".display_identity",
    ".forest_generated_subgroup_header"
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

# --- Data validation ----------------------------------------------------------

validate_forest_data <- function(data, exponentiate = FALSE) {
  required <- c("term", "estimate", "conf.low", "conf.high")
  missing <- setdiff(required, names(data))

  if (length(missing) > 0L) {
    abort_forest(
      sprintf("Missing required forest data columns: %s", paste(missing, collapse = ", ")),
      class = "ggforestplotR_invalid_data"
    )
  }

  row_type <- if ("row_type" %in% names(data)) {
    normalize_forest_row_type(data$row_type)
  } else {
    rep("estimate", nrow(data))
  }
  geometry <- row_type %in% forest_geometry_row_types()
  numeric_cols <- c("estimate", "conf.low", "conf.high")

  if (!any(geometry)) {
    abort_forest(
      "Forest data must contain at least one `estimate` or `summary` row.",
      class = "ggforestplotR_invalid_data"
    )
  }
  for (col in numeric_cols) {
    if (!is.numeric(data[[col]]) ||
      any(!is.finite(data[[col]][geometry]))) {
      abort_forest(
        sprintf("Column `%s` must contain finite numeric values for estimate and summary rows.", col),
        class = "ggforestplotR_invalid_data"
      )
    }
    if (any(!is.na(data[[col]][!geometry]))) {
      abort_forest(
        "Reference, header, and spacer rows cannot contain estimate or confidence-interval values.",
        class = "ggforestplotR_invalid_data"
      )
    }
    if (isTRUE(exponentiate) && any(data[[col]][geometry] <= 0)) {
      abort_forest(
        paste0(
          "Ratio estimates and logarithmic axes require strictly positive ",
          "`estimate`, `conf.low`, and `conf.high` values. For data frames, ",
          "`exponentiate = TRUE` declares values that are already ratios; ",
          "exponentiate log-scale estimates before plotting."
        ),
        class = "ggforestplotR_invalid_data"
      )
    }
  }

  if (any(data$conf.low[geometry] > data$conf.high[geometry])) {
    abort_forest(
      "`conf.low` cannot be greater than `conf.high`.",
      class = "ggforestplotR_invalid_data"
    )
  }

  labels <- if ("label" %in% names(data)) data$label else data$term
  for (values in list(data$term, labels)) {
    if (any(!is.na(row_type) & row_type != "spacer" &
      (is.na(values) | !nzchar(trimws(as.character(values)))))) {
      abort_forest(
        "Non-spacer rows require non-empty term and label values.",
        class = "ggforestplotR_invalid_data"
      )
    }
  }

  invisible(data)
}

# Row types

forest_row_types <- function() {
  c("estimate", "reference", "header", "summary", "spacer")
}

forest_geometry_row_types <- function() {
  c("estimate", "summary")
}

normalize_forest_row_type <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    abort_forest(
      "`row_type` must contain character or factor values.",
      class = "ggforestplotR_invalid_data"
    )
  }
  values <- tolower(trimws(as.character(x)))
  bad <- unique(values[is.na(values) | !values %in% forest_row_types()])
  if (length(bad) > 0L) {
    quoted <- ifelse(is.na(bad), "NA", sprintf('"%s"', bad))
    abort_forest(
      sprintf(
        "Unsupported `row_type` value(s): %s. Allowed values are: %s.",
        paste(quoted, collapse = ", "),
        paste(sprintf('"%s"', forest_row_types()), collapse = ", ")
      ),
      class = "ggforestplotR_invalid_data"
    )
  }
  values
}

# Row labels, ordering, and subgroup validation

apply_term_labels <- function(term, label, term_labels = NULL) {
  if (is.null(term_labels)) {
    return(label)
  }

  if (!is.atomic(term_labels) || is.null(names(term_labels)) || any(!nzchar(names(term_labels)))) {
    abort_forest(
      "`term_labels` must be a named vector.",
      class = "ggforestplotR_invalid_data"
    )
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

  has_structured_rows <- "row_type" %in% names(data) &&
    any(data$row_type != "estimate")

  if (isTRUE(has_structured_rows)) {
    abort_forest(
      "`sort_terms` must be \"none\" when structured `row_type` values are used so row order is preserved.",
      class = "ggforestplotR_invalid_data"
    )
  }

  if (isTRUE(has_subgroups)) {
    abort_forest(
      "`sort_terms` must be \"none\" when `subgroup` is used so source row order is preserved.",
      class = "ggforestplotR_invalid_data"
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
    abort_forest(
      paste0(
        "Each `subgroup` must form one contiguous block within a facet. ",
        "Repeated noncontiguous subgroup block(s): ",
        paste(repeated, collapse = ", "),
        "."
      ),
      class = "ggforestplotR_invalid_data"
    )
  }

  invisible(data)
}
