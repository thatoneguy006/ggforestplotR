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
                                    formatters = NULL,
                                    reference_text = "Reference",
                                    column_labels = NULL,
                                    columns = NULL,
                                    display_data = data) {
  digits <- resolve_table_digits(
    digits = digits,
    estimate_digits = estimate_digits,
    interval_digits = interval_digits,
    p_digits = p_digits
  )
  if (!is.character(reference_text) || length(reference_text) != 1L ||
    is.na(reference_text)) {
    abort_forest(
      "`reference_text` must be a single non-missing character string.",
      class = "ggforestplotR_invalid_table"
    )
  }
  formatters <- normalize_table_formatters(formatters, data)
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
  has_subgroup_headers <- ".forest_generated_subgroup_header" %in%
    names(display_data) && any(display_data$.forest_generated_subgroup_header)
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
    row_types[[i]] <- row_type
    is_header <- identical(row_type, "header")
    is_generated_header <- is_header &&
      ".forest_generated_subgroup_header" %in% names(rd) &&
      isTRUE(rd$.forest_generated_subgroup_header[[1L]])
    is_reference <- identical(row_type, "reference")
    is_spacer <- identical(row_type, "spacer")
    is_geometry <- row_type %in% forest_geometry_row_types()
    is_child <- !is_header && !is_spacer &&
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
    row_group_values[[i]] <- if (is_generated_header && identical(p_method, "overall")) {
      subgroup_header_group_values(data, rd)
    } else {
      as.character(rd$group)
    }
    row_parts[[i]] <- data.frame(
      row_key = row_key,
      grouping_panel = rd$grouping_panel[1L],
      term_text = term_text,
      group_text = if (is_header || is_spacer) "" else format_forest_table_values(
        group_values, rd$group, align_groups = align_groups,
        formatter = formatters[["group"]], key = "group"
      ),
      n_text = if (is_header || is_spacer) "" else format_forest_table_values(
        rd$n, rd$group, align_groups = align_groups,
        formatter = formatters[["n"]], key = "n"
      ),
      events_text = if (is_header || is_spacer) "" else format_forest_table_values(
        rd$events, rd$group, align_groups = align_groups,
        formatter = formatters[["events"]], key = "events"
      ),
      estimate_text = if (is_reference) collapse_grouped_values(
        rep(reference_text, nrow(rd)), rd$group, align_groups = align_groups
      ) else if (!is_geometry) "" else format_forest_estimates(
        rd$estimate, rd$conf.low, rd$conf.high, rd$group,
        estimate_digits = digits$estimate_digits,
        interval_digits = digits$interval_digits,
        estimate_fmt = estimate_fmt,
        align_groups = align_groups,
        estimate_formatter = formatters[["estimate"]],
        conf_low_formatter = resolve_table_formatter(formatters, "conf.low"),
        conf_high_formatter = resolve_table_formatter(formatters, "conf.high")
      ),
      estimate_value_text = if (is_reference) collapse_grouped_values(
        rep(reference_text, nrow(rd)), rd$group, align_groups = align_groups
      ) else if (!is_geometry) "" else format_forest_estimates(
        rd$estimate, rd$conf.low, rd$conf.high, rd$group,
        estimate_digits = digits$estimate_digits,
        interval_digits = digits$interval_digits,
        estimate_fmt = if (is.null(estimate_fmt)) "{estimate}" else estimate_fmt,
        align_groups = align_groups,
        estimate_formatter = formatters[["estimate"]],
        conf_low_formatter = resolve_table_formatter(formatters, "conf.low"),
        conf_high_formatter = resolve_table_formatter(formatters, "conf.high")
      ),
      ci_text = if (!is_geometry) "" else format_forest_intervals(
        rd$conf.low, rd$conf.high, rd$group,
        interval_digits = digits$interval_digits,
        ci_fmt = ci_fmt,
        align_groups = align_groups,
        conf_low_formatter = resolve_table_formatter(formatters, "conf.low"),
        conf_high_formatter = resolve_table_formatter(formatters, "conf.high")
      ),
      p_text = if (is_generated_header && identical(p_method, "overall")) {
        format_subgroup_header_p_values(
          data,
          rd,
          p_digits = digits$p_digits,
          align_groups = align_groups,
          formatter = formatters[["p"]]
        )
      } else if (is_reference || is_spacer || is_generated_header ||
        (is_child && identical(p_method, "overall"))) {
        ""
      } else {
        format_forest_p_values(
          rd$p.value, rd$group,
          p_digits = digits$p_digits,
          align_groups = align_groups,
          formatter = formatters[["p"]]
        )
      },
      stringsAsFactors = FALSE
    )

    for (extra in extra_columns) {
      storage_field <- extra_storage_lookup[[extra]]
      if (is_spacer) {
        row_parts[[i]][[storage_field]] <- ""
        next
      }
      if (isTRUE(has_subgroup_headers) && extra %in% row_label_data_columns) {
        if (is_generated_header) {
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
          align_groups = FALSE,
          formatter = formatters[[extra]], key = extra,
          fallback = default_table_value_formatter(digits$estimate_digits)
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

      if (is_generated_header) {
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
        align_groups = align_groups,
        formatter = formatters[[extra]], key = extra,
        fallback = default_table_value_formatter(digits$estimate_digits)
      )
    }
  }

  table_rows <- purrr::list_rbind(row_parts)
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
    abort_forest(
      "Select at least one table column to display.",
      class = "ggforestplotR_invalid_table"
    )
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

  # Build cells in column order. The layout pass sets their x positions.
  row_type <- unname(row_types[match(as.character(table_rows$row_key), row_levels)])
  table_data <- column_keys |>
    purrr::map(function(key) {
      data.frame(
        row_key = table_rows$row_key,
        grouping_panel = table_rows$grouping_panel,
        row_type = row_type,
        column_key = key,
        column_position = NA_real_,
        text = table_rows[[column_field_lookup[[key]]]],
        align_groups = isTRUE(has_groups) &&
          !key %in% c("term", mapped_row_label_columns),
        group_values = I(row_group_values),
        stringsAsFactors = FALSE
      )
    }) |>
    purrr::list_rbind()
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
  parts <- purrr::map(seq_len(nrow(table_data)), function(i) {
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

  prototype <- table_data[0, , drop = FALSE]
  prototype$text_group <- character()
  prototype$dodge_text <- logical()
  out <- purrr::list_rbind(parts, ptype = prototype)
  if (!is.null(group_levels)) {
    out$text_group <- factor(out$text_group, levels = group_levels)
  }
  out
}

# --- Column normalisation ----------------------------------------------------

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
      abort_forest(
        "Numeric table columns require named data.",
        class = "ggforestplotR_invalid_table"
      )
    }

    idx <- as.integer(columns)

    if (anyNA(idx) || any(columns != idx) || any(idx < 1L | idx > length(available))) {
      abort_forest(
        sprintf("Numeric table columns must be between 1 and %s.", length(available)),
        class = "ggforestplotR_invalid_table"
      )
    }

    return(normalize_table_columns(available[idx], data = data))
  }

  if (!is.character(columns)) {
    abort_forest(
      "Table columns must be specified by name or position.",
      class = "ggforestplotR_invalid_table"
    )
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
    abort_forest(
      sprintf("Unsupported table columns: %s", paste(bad, collapse = ", ")),
      class = "ggforestplotR_invalid_table"
    )
  }

  unique(resolved)
}

normalize_column_labels <- function(column_labels, data = NULL) {
  if (is.null(column_labels)) {
    return(NULL)
  }

  if (!is.atomic(column_labels) || is.null(names(column_labels)) || any(!nzchar(names(column_labels)))) {
    abort_forest(
      "`column_labels` must be a named vector.",
      class = "ggforestplotR_invalid_table"
    )
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
