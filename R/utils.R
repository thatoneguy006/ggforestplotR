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
  required <- c("term", "estimate", "conf.low", "conf.high", "label", "group", "grouping", "separator_group", "n", "p.value")
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
    term = "term",
    terms = "term",
    label = "term",
    labels = "term",
    subgroup = "term",
    subgroups = "term",
    n = "n",
    samplesize = "n",
    sample_size = "n",
    estimate = "estimate",
    estimates = "estimate",
    effect = "estimate",
    effects = "estimate",
    p = "p",
    pvalue = "p",
    p.value = "p",
    p_value = "p",
    pvalues = "p"
  )

  normalized <- tolower(columns)
  normalized <- gsub("\\s+", "", normalized)
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

build_forest_plot_data <- function(data) {
  has_groupings <- any(!is.na(data$grouping) & nzchar(data$grouping))
  plot_data <- data
  plot_data$grouping_panel <- if (has_groupings) {
    ifelse(is.na(plot_data$grouping) | !nzchar(plot_data$grouping), "(Ungrouped)", plot_data$grouping)
  } else {
    NA_character_
  }
  plot_data$row_key <- NA_character_

  panel_values <- if (has_groupings) unique(plot_data$grouping_panel) else "__all__"
  row_levels <- character()
  axis_labels <- character()
  stripe_parts <- vector("list", length(panel_values))
  separator_parts <- vector("list", length(panel_values))

  for (i in seq_along(panel_values)) {
    panel_value <- panel_values[[i]]
    panel_idx <- if (has_groupings) which(plot_data$grouping_panel == panel_value) else seq_len(nrow(plot_data))
    panel_separator_values <- plot_data$separator_group[panel_idx]
    separator_counts <- table(panel_separator_values[!is.na(panel_separator_values) & nzchar(panel_separator_values)])
    prefix_groups <- names(separator_counts[separator_counts > 1L])

    if (length(prefix_groups) > 0L) {
      prefix_idx <- panel_idx[!is.na(panel_separator_values) & panel_separator_values %in% prefix_groups]
      plot_data$label[prefix_idx] <- paste0(plot_data$separator_group[prefix_idx], ": ", plot_data$label[prefix_idx])
    }

    panel_labels <- unique(plot_data$label[panel_idx])
    panel_row_keys <- if (has_groupings) {
      paste(panel_value, panel_labels, sep = "___")
    } else {
      panel_labels
    }
    panel_levels <- panel_row_keys
    row_map <- stats::setNames(panel_row_keys, panel_labels)

    plot_data$row_key[panel_idx] <- unname(row_map[plot_data$label[panel_idx]])
    row_levels <- c(row_levels, panel_levels)
    axis_labels <- c(axis_labels, stats::setNames(panel_labels, panel_row_keys))

    stripe_parts[[i]] <- data.frame(
      grouping_panel = if (has_groupings) panel_value else NA_character_,
      stripe_id = seq_along(panel_levels),
      xmin = -Inf,
      xmax = Inf,
      ymin = seq_along(panel_levels) - 0.5,
      ymax = seq_along(panel_levels) + 0.5,
      stringsAsFactors = FALSE
    )

    panel_separator_groups <- vapply(panel_row_keys, function(row_key) {
      row_idx <- panel_idx[as.character(plot_data$row_key[panel_idx]) == row_key]
      group_values <- unique(plot_data$separator_group[row_idx])
      group_values <- group_values[!is.na(group_values) & nzchar(group_values)]

      if (length(group_values) == 0L) {
        return(NA_character_)
      }

      group_values[1]
    }, character(1))

    separator_rows <- list()
    run_start <- 1L

    while (run_start <= length(panel_separator_groups)) {
      current_group <- panel_separator_groups[run_start]
      run_end <- run_start

      if (!is.na(current_group) && nzchar(current_group)) {
        while (run_end < length(panel_separator_groups) && !is.na(panel_separator_groups[run_end + 1L]) && panel_separator_groups[run_end + 1L] == current_group) {
          run_end <- run_end + 1L
        }

        separator_rows[[length(separator_rows) + 1L]] <- data.frame(
          grouping_panel = if (has_groupings) panel_value else NA_character_,
          separator_group = unname(current_group),
          yintercept = c(run_start - 0.5, run_end + 0.5),
          stringsAsFactors = FALSE
        )
      }

      run_start <- run_end + 1L
    }

    separator_parts[[i]] <- if (length(separator_rows) > 0L) {
      do.call(rbind, separator_rows)
    } else {
      NULL
    }
  }

  plot_data$row_key <- factor(plot_data$row_key, levels = row_levels)
  stripe_data <- do.call(rbind, stripe_parts)
  stripe_data$fill_key <- ifelse(stripe_data$stripe_id %% 2 == 1, "stripe", "base")
  separator_data <- do.call(rbind, separator_parts)

  if (is.null(separator_data)) {
    separator_data <- data.frame(
      grouping_panel = character(),
      separator_group = character(),
      yintercept = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    separator_data <- unique(separator_data[c("grouping_panel", "yintercept")])
  }

  list(
    plot_data = plot_data,
    stripe_data = stripe_data,
    separator_data = separator_data,
    axis_labels = axis_labels,
    has_groupings = has_groupings && length(unique(plot_data$grouping_panel)) > 1
  )
}

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

    if (length(idx) == 0) {
      next
    }

    row_data <- data[idx, , drop = FALSE]
    row_parts[[i]] <- data.frame(
      row_key = row_key,
      grouping_panel = row_data$grouping_panel[1],
      term_text = row_data$label[1],
      n_text = format_forest_table_values(row_data$n, row_data$group),
      estimate_text = format_forest_estimates(
        row_data$estimate,
        row_data$conf.low,
        row_data$conf.high,
        row_data$group,
        digits = digits
      ),
      p_text = format_forest_p_values(
        row_data$p.value,
        row_data$group,
        digits = digits
      ),
      stringsAsFactors = FALSE
    )
  }

  table_rows <- do.call(rbind, row_parts)
  table_rows$row_key <- factor(table_rows$row_key, levels = row_levels)

  if (is.null(columns)) {
    column_keys <- character()

    if (isTRUE(show_terms)) {
      column_keys <- c(column_keys, "term")
    }

    if (isTRUE(show_n)) {
      column_keys <- c(column_keys, "n")
    }

    if (isTRUE(show_estimate)) {
      column_keys <- c(column_keys, "estimate")
    }

    if (isTRUE(show_p)) {
      column_keys <- c(column_keys, "p")
    }
  } else {
    column_keys <- normalize_table_columns(columns)
  }

  if (length(column_keys) == 0) {
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

  positions <- seq(1, by = 1.6, length.out = length(column_keys))
  long_parts <- vector("list", length(column_keys))

  for (i in seq_along(column_keys)) {
    column_key <- column_keys[[i]]
    field_name <- unname(column_field_lookup[[column_key]])

    long_parts[[i]] <- data.frame(
      row_key = table_rows$row_key,
      grouping_panel = table_rows$grouping_panel,
      column_key = column_key,
      column_position = positions[i],
      text = table_rows[[field_name]],
      stringsAsFactors = FALSE
    )
  }

  table_data <- do.call(rbind, long_parts)
  table_data$row_key <- factor(table_data$row_key, levels = row_levels)

  list(
    table_data = table_data,
    positions = positions,
    header_positions = positions,
    headers = unname(header_lookup[column_keys]),
    column_keys = column_keys
  )
}

format_forest_p_values <- function(values, group = NULL, digits = 2) {
  values <- as.numeric(values)

  if (all(is.na(values))) {
    return("")
  }

  formatted <- ifelse(
    is.na(values),
    "",
    format.pval(values, digits = max(3L, digits), eps = 10^(-max(3L, digits)))
  )

  non_empty <- formatted[nzchar(formatted)]

  if (length(non_empty) == 0L) {
    return("")
  }

  if (length(unique(non_empty)) == 1L) {
    return(non_empty[1])
  }

  if (all(is.na(group) | !nzchar(group))) {
    return(paste(non_empty, collapse = "\n"))
  }

  group_labels <- ifelse(is.na(group) | !nzchar(group), paste0("Series ", seq_along(values)), group)
  keep <- nzchar(formatted)
  paste(paste0(group_labels[keep], ": ", formatted[keep]), collapse = "\n")
}

format_forest_estimates <- function(estimate, conf.low, conf.high, group = NULL, digits = 2) {
  values <- sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    estimate,
    conf.low,
    conf.high
  )

  if (all(is.na(group) | !nzchar(group))) {
    return(paste(values, collapse = "\n"))
  }

  group_labels <- ifelse(is.na(group) | !nzchar(group), paste0("Series ", seq_along(values)), group)
  paste(paste0(group_labels, ": ", values), collapse = "\n")
}

format_forest_table_values <- function(values, group = NULL) {
  values <- as.character(values)
  values[is.na(values)] <- ""

  if (!any(nzchar(values))) {
    return("")
  }

  non_empty <- values[nzchar(values)]

  if (length(unique(non_empty)) == 1L) {
    return(non_empty[1])
  }

  if (all(is.na(group) | !nzchar(group))) {
    return(paste(non_empty, collapse = "\n"))
  }

  group_labels <- ifelse(is.na(group) | !nzchar(group), paste0("Series ", seq_along(values)), group)
  keep <- nzchar(values)
  paste(paste0(group_labels[keep], ": ", values[keep]), collapse = "\n")
}

build_table_line_data <- function(stripe_data, has_groupings = FALSE) {
  if (isTRUE(has_groupings)) {
    line_parts <- lapply(split(stripe_data, stripe_data$grouping_panel, drop = TRUE), function(panel_data) {
      boundary_values <- unique(c(
        min(panel_data$ymin),
        panel_data$ymin[panel_data$stripe_id > 1],
        max(panel_data$ymax)
      ))

      data.frame(
        grouping_panel = panel_data$grouping_panel[1],
        yintercept = boundary_values,
        stringsAsFactors = FALSE
      )
    })

    line_data <- do.call(rbind, line_parts)
  } else {
    line_data <- data.frame(
      yintercept = unique(c(
        min(stripe_data$ymin),
        stripe_data$ymin[stripe_data$stripe_id > 1],
        max(stripe_data$ymax)
      )),
      stringsAsFactors = FALSE
    )
  }

  line_data
}

string_display_width <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  if (length(x) == 0L) {
    return(0)
  }

  widths <- vapply(strsplit(x, "\n", fixed = TRUE), function(parts) {
    if (length(parts) == 0L) {
      return(0)
    }

    max(nchar(parts, type = "width"), 0)
  }, numeric(1))

  max(widths, 0)
}

measure_table_text_widths <- function(table_spec, text_size = 3.2) {
  char_scale <- c(
    term = 0.055,
    n = 0.03,
    estimate = 0.05,
    p = 0.045
  )

  stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
    column_key <- table_spec$column_keys[[i]]
    column_values <- table_spec$table_data$text[table_spec$table_data$column_key == column_key]
    max_chars <- string_display_width(c(table_spec$headers[[i]], column_values))

    max(max_chars, 1) * unname(char_scale[[column_key]]) * (text_size / 3.2)
  }, numeric(1)), table_spec$column_keys)
}

estimate_split_column_widths <- function(table_spec, text_size = 3.2, alignment = c("left", "center", "right")) {
  alignment <- match.arg(alignment)
  char_scale <- c(
    term = 0.055,
    n = 0.03,
    estimate = 0.05,
    p = 0.045
  )
  base_padding <- c(
    term = 0.45,
    n = 0.25,
    estimate = 0.55,
    p = 0.35
  )
  alignment_padding <- switch(
    alignment,
    left = 0.25,
    right = 0.25,
    center = 0.18
  )

  stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
    column_key <- table_spec$column_keys[[i]]
    column_values <- table_spec$table_data$text[table_spec$table_data$column_key == column_key]
    max_chars <- string_display_width(c(table_spec$headers[[i]], column_values))

    unname(base_padding[[column_key]]) + alignment_padding +
      max(max_chars, 1) * unname(char_scale[[column_key]]) * (text_size / 3.2)
  }, numeric(1)), table_spec$column_keys)
}

layout_split_table_spec <- function(table_spec, text_size = 3.2, alignment = c("left", "right")) {
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

  table_spec$table_data$column_position <- unname(positions[match(table_spec$table_data$column_key, table_spec$column_keys)])
  table_spec$positions <- unname(positions)
  table_spec$header_positions <- unname(positions)
  table_spec$estimated_column_widths <- unname(column_widths)
  table_spec$displayed_column_widths <- unname(text_widths)
  table_spec$estimated_width <- sum(column_widths) + gap * max(length(column_widths) - 1L, 0) + 0.15
  table_spec
}

default_split_table_width <- function(table_spec, text_size = 3.2, alignment = c("left", "center", "right")) {
  if (!is.null(table_spec$estimated_width)) {
    return(table_spec$estimated_width)
  }

  alignment <- match.arg(alignment)
  column_widths <- estimate_split_column_widths(table_spec, text_size = text_size, alignment = alignment)
  sum(column_widths) + 0.2 * max(length(column_widths) - 1L, 0) + 0.15
}

default_split_plot_width <- function(left_width, right_width) {
  max(2.1, min(3.0, 0.5 * mean(c(left_width, right_width))))
}

layout_center_table_spec <- function(table_spec, text_size = 3.2) {
  column_widths <- estimate_split_column_widths(table_spec, text_size = text_size, alignment = "center")
  gap <- 0.55
  left_edges <- cumsum(c(0, utils::head(column_widths + gap, -1)))
  positions <- left_edges + column_widths / 2

  table_spec$table_data$column_position <- unname(positions[match(table_spec$table_data$column_key, table_spec$column_keys)])
  table_spec$positions <- unname(positions)
  table_spec$header_positions <- unname(positions)
  table_spec$estimated_column_widths <- unname(column_widths)
  table_spec$estimated_width <- sum(column_widths) + gap * max(length(column_widths) - 1L, 0) + 0.35
  table_spec
}

default_center_table_width <- function(table_spec, text_size = 3.2) {
  if (!is.null(table_spec$estimated_width)) {
    return(table_spec$estimated_width)
  }

  column_widths <- estimate_split_column_widths(table_spec, text_size = text_size, alignment = "center")
  sum(column_widths) + 0.55 * max(length(column_widths) - 1L, 0) + 0.35
}

default_center_table_limits <- function(table_spec, pad = 0.12) {
  widths <- table_spec$estimated_column_widths
  positions <- table_spec$positions
  c(min(positions - widths / 2) - pad, max(positions + widths / 2) + pad)
}

default_split_table_limits <- function(table_spec,
                                       alignment = c("left", "right"),
                                       inner_pad = 0.03,
                                       outer_pad = 0.08) {
  alignment <- match.arg(alignment)
  widths <- if (!is.null(table_spec$displayed_column_widths)) {
    table_spec$displayed_column_widths
  } else {
    table_spec$estimated_column_widths
  }
  positions <- table_spec$positions

  if (alignment == "left") {
    xmin <- min(positions) - outer_pad
    xmax <- max(positions + widths) + inner_pad
  } else {
    xmin <- min(positions - widths) - inner_pad
    xmax <- max(positions) + outer_pad
  }

  c(xmin, xmax)
}

default_plot_background_limits <- function(forest_data, exponentiate = FALSE, include_zero = TRUE) {
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

default_split_plot_limits <- function(forest_data, exponentiate = FALSE, include_zero = TRUE) {
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
                                    x_expand = c(0.55, 1.7),
                                    x_limits = NULL,
                                    plot_margin = NULL,
                                    text_hjust = 0.5,
                                    header_hjust = 0.5) {
  grouping_strip_position <- match.arg(grouping_strip_position)
  table_position <- match.arg(table_position)

  p <- ggplot2::ggplot(
    table_spec$table_data,
    ggplot2::aes(
      x = .data$column_position,
      y = .data$row_key,
      label = .data$text
    )
  )

  if (isTRUE(striped_rows)) {
    p <- p + ggplot2::geom_rect(
      data = stripe_data[stripe_data$fill_key == "stripe", , drop = FALSE],
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = stripe_fill,
      colour = stripe_colour
    )
  }

  if (isTRUE(grid_lines)) {
    line_data <- build_table_line_data(
      stripe_data = stripe_data,
      has_groupings = has_groupings
    )

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
    ggplot2::geom_text(
      hjust = text_hjust,
      size = text_size,
      lineheight = 0.95
    ) +
    ggplot2::scale_x_continuous(
      breaks = table_spec$header_positions,
      labels = table_spec$headers,
      position = "top",
      expand = ggplot2::expansion(add = x_expand),
      limits = x_limits
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) rep("", length(x)),
      drop = TRUE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(
        face = "bold",
        colour = "black",
        hjust = header_hjust,
        margin = ggplot2::margin(b = 0)
      ),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_blank(),
      strip.placement = "outside",
      plot.margin = if (is.null(plot_margin)) {
        if (table_position == "left") {
          ggplot2::margin(5.5, 4, 5.5, 5.5)
        } else {
          ggplot2::margin(5.5, 5.5, 5.5, 4)
        }
      } else {
        plot_margin
      }
    )

  if (isTRUE(has_groupings)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(grouping_panel),
      ncol = 1,
      scales = "free_y",
      strip.position = grouping_strip_position
    )
  }

  p
}

combine_forest_plot_and_table <- function(plot, table_plot, table_position = c("left", "right"), table_width = 2.2, plot_width = 2.4) {
  table_position <- match.arg(table_position)
  widths <- c(table_width, plot_width)

  if (table_position == "left") {
    patchwork::wrap_plots(table_plot, plot, nrow = 1, widths = widths)
  } else {
    patchwork::wrap_plots(plot, table_plot, nrow = 1, widths = rev(widths))
  }
}
