# --- Plot data construction (decomposed into single-purpose passes) ----------

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

  # Discrete y scales draw the first level at the bottom; reverse so rows read
  # top-to-bottom in data order, matching the side tables.
  data$row_key <- factor(data$row_key, levels = rev(all_levels))
  data
}

#' Build axis label lookup: row_key -> display label.
#' @keywords internal
#' @noRd
build_axis_labels <- function(data, has_groupings) {
  stopifnot("display_label" %in% names(data))
  row_levels <- levels(data$row_key)
  first_rows <- match(row_levels, as.character(data$row_key))
  stats::setNames(data$display_label[first_rows], row_levels)
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
  if (!"row_type" %in% names(display_data)) {
    display_data$row_type <- "estimate"
  }
  display_data$.forest_generated_subgroup_header <- FALSE
  display_data$display_label <- display_data$label
  display_data$display_label[display_data$row_type == "spacer"] <- ""

  has_subgroups <- "subgroup" %in% names(display_data) &&
    any(!is.na(display_data$subgroup) & nzchar(display_data$subgroup))

  if (!isTRUE(has_subgroups)) {
    return(display_data)
  }

  child <- !is.na(display_data$subgroup) & nzchar(display_data$subgroup)
  display_data$display_label[child] <- paste0("   ", display_data$label[child])
  panel_values <- observed_grouping_panels(display_data, has_groupings)
  parts <- panel_values |>
    purrr::map(function(panel) {
      rows <- if (has_groupings) {
        which(display_data$grouping_panel == panel)
      } else {
        seq_len(nrow(display_data))
      }
      panel_data <- display_data[rows, , drop = FALSE]
      subgroup <- panel_data$subgroup
      subgroup[is.na(subgroup)] <- ""
      runs <- rle(subgroup)$lengths
      run <- rep(seq_along(runs), runs)
      split(panel_data, run) |>
        purrr::map(add_subgroup_header, p_method = p_method) |>
        purrr::list_flatten()
    }) |>
    purrr::list_flatten()
  # Display data includes user-supplied columns. Preserve base binding's
  # handling of matrix columns and their attributes.
  out <- do.call(rbind, unname(parts))
  rownames(out) <- NULL
  for (attribute in c(
    "source_columns", "column_mapping", "grouping_levels", "conf.level",
    "p_method"
  )) {
    attr(out, attribute) <- attr(display_data, attribute, exact = TRUE)
  }
  out
}

add_subgroup_header <- function(block, p_method) {
  subgroup <- block$subgroup[[1L]]
  if (is.na(subgroup) || !nzchar(subgroup)) {
    return(list(block))
  }

  header <- blank_display_row(block[1L, , drop = FALSE])
  header$term <- subgroup
  header$label <- subgroup
  header$subgroup <- subgroup
  header$grouping_panel <- block$grouping_panel[1L]
  header$row_type <- "header"
  header$.forest_generated_subgroup_header <- TRUE
  header$display_label <- subgroup

  if (p_method == "overall") {
    values <- block$p.value[!is.na(block$p.value)]
    if (length(values) > 0L) {
      header$p.value <- values[[1L]]
    }
    block$p.value[] <- NA_real_
  }

  separators <- unique(block$separate_groups)
  if (length(separators) == 1L && !is.na(separators) && nzchar(separators)) {
    header$separate_groups <- separators
  }

  list(header, block)
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
                                            align_groups,
                                            formatter = NULL) {
  block <- subgroup_header_rows(data, header_row)

  if (nrow(block) == 0L) {
    return("")
  }

  block_values <- block$p.value[!is.na(block$p.value)]
  if (length(block_values) > 0L &&
    length(unique(block_values)) == 1L) {
    return(format_forest_p_values(
      block_values[[1L]],
      p_digits = p_digits, formatter = formatter
    ))
  }

  group_values <- as.character(block$group)
  has_groups <- any(!is.na(group_values) & nzchar(group_values))
  if (!isTRUE(has_groups)) {
    values <- block$p.value[!is.na(block$p.value)]
    value <- if (length(values) == 0L) NA_real_ else values[[1L]]
    return(format_forest_p_values(value, p_digits = p_digits, formatter = formatter))
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
    align_groups = align_groups,
    formatter = formatter
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

  stripe_data <- purrr::list_rbind(parts)
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

    runs <- rle(sep_groups)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    present <- which(!is.na(runs$values) & nzchar(runs$values))

    if (length(present) > 0L) {
      parts[[i]] <- data.frame(
        grouping_panel = if (has_groupings) pv else NA_character_,
        yintercept = as.vector(rbind(starts[present] - 0.5, ends[present] + 0.5)),
        stringsAsFactors = FALSE
      )
    }
  }

  separator_data <- purrr::list_rbind(parts)

  if (nrow(separator_data) == 0L) {
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
    ifelse(plot_data$.forest_generated_subgroup_header |
      plot_data$row_type %in% c("estimate", "reference", "summary"),
    plot_data$subgroup, NA_character_),
    plot_data$label
  )
  unique_source_rows <- plot_data$row_type == "spacer" |
    (plot_data$row_type == "header" &
      !plot_data$.forest_generated_subgroup_header)
  plot_data$.display_identity[unique_source_rows] <- paste0(
    plot_data$.display_identity[unique_source_rows],
    "|source:", plot_data$.forest_source_row[unique_source_rows]
  )
  plot_data <- assign_hierarchical_row_keys(plot_data, has_groupings)

  original_rows <- !is.na(plot_data$.forest_source_row)
  source_rows <- plot_data$.forest_source_row[original_rows]
  source_keys <- as.character(plot_data$row_key[original_rows])
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
