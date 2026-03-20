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
  required <- c(
    "term", "estimate", "conf.low", "conf.high",
    "label", "group", "grouping", "separator_group", "n", "p.value"
  )
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

normalize_table_columns <- function(columns) {
  default_order <- c("term", "n", "estimate", "p")
  
  if (is.null(columns)) {
    return(NULL)
  }
  
  if (is.numeric(columns)) {
    idx <- as.integer(columns)
    
    if (anyNA(idx) || any(idx < 1L | idx > length(default_order))) {
      stop("Numeric table columns must be between 1 and 4.", call. = FALSE)
    }
    
    return(unique(default_order[idx]))
  }
  
  if (!is.character(columns)) {
    stop("Table columns must be specified by name or position.", call. = FALSE)
  }
  
  aliases <- c(
    term = "term", terms = "term", label = "term", labels = "term",
    subgroup = "term", subgroups = "term",
    n = "n", samplesize = "n", sample_size = "n",
    estimate = "estimate", estimates = "estimate",
    effect = "estimate", effects = "estimate",
    p = "p", pvalue = "p", p.value = "p", p_value = "p", pvalues = "p"
  )
  
  normalized <- gsub("\\s+", "", tolower(columns))
  resolved <- unname(aliases[normalized])
  
  if (anyNA(resolved)) {
    bad <- unique(columns[is.na(resolved)])
    stop(
      sprintf("Unsupported table columns: %s", paste(bad, collapse = ", ")),
      call. = FALSE
    )
  }
  
  unique(resolved)
}

# ─── Formatting helpers ──────────────────────────────────────────────────────

#' Shared logic for collapsing formatted values with optional group labels.
#'
#' All three public formatters (p-values, estimates, n-values) produce a
#' character vector of per-row formatted strings, then need identical
#' deduplication / group-prefixing / newline-collapsing.  This function
#' handles that single concern.
collapse_grouped_values <- function(formatted, group = NULL) {
  keep <- !is.na(formatted) & nzchar(formatted)
  
  if (!any(keep)) {
    return("")
  }
  
  non_empty <- formatted[keep]
  
  if (length(unique(non_empty)) == 1L) {
    return(non_empty[1L])
  }
  
  if (all(is.na(group) | !nzchar(group))) {
    return(paste(non_empty, collapse = "\n"))
  }
  
  group_labels <- ifelse(
    is.na(group) | !nzchar(group),
    paste0("Series ", seq_along(formatted)),
    group
  )
  paste(paste0(group_labels[keep], ": ", formatted[keep]), collapse = "\n")
}

format_forest_p_values <- function(values, group = NULL, digits = 2) {
  d <- max(3L, digits)
  values <- as.numeric(values)
  formatted <- ifelse(is.na(values), "", format.pval(values, digits = d, eps = 10^(-d)))
  collapse_grouped_values(formatted, group)
}

format_forest_estimates <- function(estimate, conf.low, conf.high,
                                    group = NULL, digits = 2) {
  fmt <- paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)")
  formatted <- sprintf(fmt, estimate, conf.low, conf.high)
  collapse_grouped_values(formatted, group)
}

format_forest_table_values <- function(values, group = NULL) {
  formatted <- as.character(values)
  formatted[is.na(formatted)] <- ""
  collapse_grouped_values(formatted, group)
}

# ─── Plot data construction (decomposed into single-purpose passes) ──────────

assign_grouping_panels <- function(data, has_groupings) {
  if (has_groupings) {
    ifelse(
      is.na(data$grouping) | !nzchar(data$grouping),
      "(Ungrouped)",
      data$grouping
    )
  } else {
    rep(NA_character_, nrow(data))
  }
}

#' Within each panel, if a separator_group value appears more than once the
#' labels are ambiguous.  Prefix them with "group: label" so the axis is
#' readable.
prefix_ambiguous_labels <- function(data, has_groupings) {
  panel_values <- if (has_groupings) unique(data$grouping_panel) else "__all__"
  
  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    sep_vals <- data$separator_group[idx]
    counts <- table(sep_vals[!is.na(sep_vals) & nzchar(sep_vals)])
    prefix_groups <- names(counts[counts > 1L])
    
    if (length(prefix_groups) > 0L) {
      prefix_idx <- idx[!is.na(sep_vals) & sep_vals %in% prefix_groups]
      data$label[prefix_idx] <- paste0(data$separator_group[prefix_idx], ": ", data$label[prefix_idx])
    }
  }
  
  data
}

#' Assign a unique row_key per label within each panel and set factor levels
#' in display order.
assign_row_keys <- function(data, has_groupings) {
  panel_values <- if (has_groupings) unique(data$grouping_panel) else "__all__"
  data$row_key <- NA_character_
  all_levels <- character()
  
  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    panel_labels <- unique(data$label[idx])
    
    keys <- if (has_groupings) paste(pv, panel_labels, sep = "___") else panel_labels
    row_map <- stats::setNames(keys, panel_labels)
    
    data$row_key[idx] <- unname(row_map[data$label[idx]])
    all_levels <- c(all_levels, keys)
  }
  
  data$row_key <- factor(data$row_key, levels = all_levels)
  data
}

#' Build axis label lookup: row_key -> display label.
build_axis_labels <- function(data, has_groupings) {
  panel_values <- if (has_groupings) unique(data$grouping_panel) else "__all__"
  labels <- character()
  
  for (pv in panel_values) {
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    panel_labels <- unique(data$label[idx])
    keys <- if (has_groupings) paste(pv, panel_labels, sep = "___") else panel_labels
    labels <- c(labels, stats::setNames(panel_labels, keys))
  }
  
  labels
}

#' Build a data frame of alternating stripe rectangles for each panel.
build_stripe_rectangles <- function(data, has_groupings) {
  panel_values <- if (has_groupings) unique(data$grouping_panel) else "__all__"
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
  stripe_data$fill_key <- ifelse(stripe_data$stripe_id %% 2 == 1, "stripe", "base")
  stripe_data
}

#' Detect runs of identical separator_group values within each panel and
#' return a data frame of horizontal separator positions.
build_separator_lines <- function(data, has_groupings) {
  panel_values <- if (has_groupings) unique(data$grouping_panel) else "__all__"
  parts <- vector("list", length(panel_values))
  
  for (i in seq_along(panel_values)) {
    pv <- panel_values[[i]]
    idx <- if (has_groupings) which(data$grouping_panel == pv) else seq_len(nrow(data))
    
    row_keys <- levels(data$row_key)[levels(data$row_key) %in% data$row_key[idx]]
    
    # Map each row_key to its separator_group value
    sep_groups <- vapply(row_keys, function(rk) {
      row_idx <- idx[as.character(data$row_key[idx]) == rk]
      vals <- unique(data$separator_group[row_idx])
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
          separator_group = unname(current),
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
    unique(separator_data[c("grouping_panel", "yintercept")])
  }
}

#' Main entry point.  Orchestrates the four passes defined above.
build_forest_plot_data <- function(data) {
  has_groupings <- any(!is.na(data$grouping) & nzchar(data$grouping))
  plot_data <- data
  plot_data$grouping_panel <- assign_grouping_panels(plot_data, has_groupings)
  
  plot_data <- prefix_ambiguous_labels(plot_data, has_groupings)
  plot_data <- assign_row_keys(plot_data, has_groupings)
  
  stripe_data <- build_stripe_rectangles(plot_data, has_groupings)
  separator_data <- build_separator_lines(plot_data, has_groupings)
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

build_forest_table_data <- function(data,
                                    show_terms = TRUE,
                                    show_n = FALSE,
                                    show_estimate = TRUE,
                                    show_p = FALSE,
                                    term_header = "Term",
                                    n_header = "N",
                                    estimate_label = "Estimate",
                                    p_header = "P-value",
                                    digits = 2,
                                    columns = NULL) {
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
      n_text = format_forest_table_values(rd$n, rd$group),
      estimate_text = format_forest_estimates(
        rd$estimate, rd$conf.low, rd$conf.high, rd$group, digits = digits
      ),
      p_text = format_forest_p_values(rd$p.value, rd$group, digits = digits),
      stringsAsFactors = FALSE
    )
  }
  
  table_rows <- do.call(rbind, row_parts)
  table_rows$row_key <- factor(table_rows$row_key, levels = row_levels)
  
  # Determine which columns to show
  if (is.null(columns)) {
    column_keys <- character()
    if (isTRUE(show_terms))    column_keys <- c(column_keys, "term")
    if (isTRUE(show_n))        column_keys <- c(column_keys, "n")
    if (isTRUE(show_estimate)) column_keys <- c(column_keys, "estimate")
    if (isTRUE(show_p))        column_keys <- c(column_keys, "p")
  } else {
    column_keys <- normalize_table_columns(columns)
  }
  
  if (length(column_keys) == 0L) {
    stop("Select at least one table column to display.", call. = FALSE)
  }
  
  column_field_lookup <- c(
    term = "term_text",
    n = "n_text",
    estimate = "estimate_text",
    p = "p_text"
  )
  header_lookup <- c(
    term = term_header,
    n = n_header,
    estimate = sprintf("%s (95%% CI)", estimate_label),
    p = p_header
  )
  
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
measure_max_grob_width <- function(text, fontsize_pt, fontface = "plain") {
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
          grid::textGrob(line, gp = grid::gpar(fontsize = fontsize_pt, fontface = fontface))
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
measure_table_text_widths <- function(table_spec, text_size = 3.2) {
  text_size_pt <- text_size * (72.27 / 25.4)
  header_size_pt <- 11
  
  stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
    key <- table_spec$column_keys[[i]]
    values <- table_spec$table_data$text[table_spec$table_data$column_key == key]
    max(
      measure_max_grob_width(table_spec$headers[[i]], fontsize_pt = header_size_pt, fontface = "bold"),
      measure_max_grob_width(values, fontsize_pt = text_size_pt, fontface = "plain")
    )
  }, numeric(1)), table_spec$column_keys)
}

# ─── Column width estimation ─────────────────────────────────────────────────

#' Default per-column base padding.  Uses a known lookup for the four standard
#' keys and a sensible fallback for anything else.
column_base_padding <- function(column_key) {
  known <- c(term = 0.16, n = 0.10, estimate = 0.18, p = 0.12)
  pad <- known[[column_key]]
  if (is.null(pad)) 0.14 else unname(pad)
}

#' Total column width = measured text width + base padding + alignment padding.
estimate_split_column_widths <- function(table_spec,
                                         text_size = 3.2,
                                         alignment = c("left", "center", "right")) {
  alignment <- match.arg(alignment)
  text_widths <- measure_table_text_widths(table_spec, text_size = text_size)
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
layout_split_table_spec <- function(table_spec,
                                    text_size = 3.2,
                                    alignment = c("left", "right")) {
  alignment <- match.arg(alignment)
  column_widths <- estimate_split_column_widths(table_spec, text_size = text_size, alignment = alignment)
  text_widths <- measure_table_text_widths(table_spec, text_size = text_size)
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

layout_center_table_spec <- function(table_spec, text_size = 3.2) {
  column_widths <- estimate_split_column_widths(table_spec, text_size = text_size, alignment = "center")
  text_widths <- measure_table_text_widths(table_spec, text_size = text_size)
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

# ─── Plot limits ─────────────────────────────────────────────────────────────

default_plot_background_limits <- function(forest_data,
                                           exponentiate = FALSE,
                                           include_zero = TRUE) {
  xmin <- min(forest_data$conf.low, na.rm = TRUE)
  xmax <- max(forest_data$conf.high, na.rm = TRUE)
  
  if (isTRUE(include_zero)) {
    null_value <- if (isTRUE(exponentiate)) 1 else 0
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

default_split_plot_limits <- function(forest_data,
                                      exponentiate = FALSE,
                                      include_zero = TRUE) {
  null_value <- if (isTRUE(exponentiate)) 1 else 0
  xmin <- min(forest_data$conf.low, na.rm = TRUE)
  xmax <- max(forest_data$conf.high, na.rm = TRUE)
  
  if (isTRUE(include_zero)) {
    xmin <- min(xmin, null_value)
    xmax <- max(xmax, null_value)
  }
  
  if (isTRUE(exponentiate)) {
    max_dist <- max(abs(log10(xmin / null_value)), abs(log10(xmax / null_value)))
    c(null_value / (10^max_dist), null_value * (10^max_dist))
  } else {
    max_dist <- max(abs(xmin - null_value), abs(xmax - null_value))
    c(null_value - max_dist, null_value + max_dist)
  }
}

# ─── ggplot2 table panel ─────────────────────────────────────────────────────

#' Build a ggplot2 table panel for one side of a split forest plot.
#'
#' Uses symmetric expansion and uniform margins.  The "equal spacing"
#' guarantee comes from `combine_split_forest_plot()` giving both table
#' panels the same patchwork width — not from per-side padding hacks here.
build_forest_table_plot <- function(table_spec,
                                    stripe_data,
                                    has_groupings = FALSE,
                                    grouping_strip_position = c("left", "right"),
                                    table_position = c("left", "right"),
                                    striped_rows = FALSE,
                                    stripe_fill = "grey95",
                                    stripe_colour = NA,
                                    text_size = 3.2,
                                    grid_lines = FALSE,
                                    grid_line_colour = "black",
                                    grid_line_size = 0.3,
                                    grid_line_linetype = 1,
                                    x_expand = ggplot2::expansion(mult = 0.05),
                                    x_limits = NULL,
                                    plot_margin = ggplot2::margin(5.5, 4, 5.5, 4),
                                    text_hjust = 0.5,
                                    header_hjust = 0.5) {
  grouping_strip_position <- match.arg(grouping_strip_position)
  table_position <- match.arg(table_position)
  
  p <- ggplot2::ggplot(
    table_spec$table_data,
    ggplot2::aes(x = .data$column_position, y = .data$row_key, label = .data$text)
  )
  
  if (isTRUE(striped_rows)) {
    p <- p + ggplot2::geom_rect(
      data = stripe_data[stripe_data$fill_key == "stripe", , drop = FALSE],
      mapping = ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = stripe_fill,
      colour = stripe_colour
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
        face = "bold", colour = "black", hjust = header_hjust,
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
combine_split_forest_plot <- function(plot,
                                      left_table = NULL,
                                      right_table = NULL,
                                      left_spec = NULL,
                                      right_spec = NULL,
                                      plot_width = 2.5) {
  left_w  <- if (!is.null(left_spec))  left_spec$content_width  else 0
  right_w <- if (!is.null(right_spec)) right_spec$content_width else 0
  
  # Equal table widths = centred plot
  table_width <- max(left_w, right_w, 1)
  
  panels <- list()
  widths <- numeric()
  
  if (!is.null(left_table)) {
    panels <- c(panels, list(left_table))
    widths <- c(widths, table_width)
  }
  
  panels <- c(panels, list(plot))
  widths <- c(widths, plot_width)
  
  if (!is.null(right_table)) {
    panels <- c(panels, list(right_table))
    widths <- c(widths, table_width)
  }
  
  patchwork::wrap_plots(panels, nrow = 1, widths = widths)
}

#' Convenience wrapper for single-table layouts (table on one side only).
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
