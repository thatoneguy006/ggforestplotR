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
  required <- c("term", "estimate", "conf.low", "conf.high", "label", "group", "grouping", "n", "p.value")
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

  for (i in seq_along(panel_values)) {
    panel_value <- panel_values[[i]]
    panel_idx <- if (has_groupings) which(plot_data$grouping_panel == panel_value) else seq_len(nrow(plot_data))
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
  }

  plot_data$row_key <- factor(plot_data$row_key, levels = row_levels)
  stripe_data <- do.call(rbind, stripe_parts)
  stripe_data$fill_key <- ifelse(stripe_data$stripe_id %% 2 == 1, "stripe", "base")

  list(
    plot_data = plot_data,
    stripe_data = stripe_data,
    axis_labels = axis_labels,
    has_groupings = has_groupings && length(unique(plot_data$grouping_panel)) > 1
  )
}

build_forest_table_data <- function(data,
                                    show_terms = TRUE,
                                    show_n = FALSE,
                                    show_estimate = TRUE,
                                    term_header = "Term",
                                    n_header = "N",
                                    estimate_label = "Estimate",
                                    digits = 2) {
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
      stringsAsFactors = FALSE
    )
  }

  table_rows <- do.call(rbind, row_parts)
  table_rows$row_key <- factor(table_rows$row_key, levels = row_levels)

  column_keys <- character()
  headers <- character()

  if (isTRUE(show_terms)) {
    column_keys <- c(column_keys, "term_text")
    headers <- c(headers, term_header)
  }

  if (isTRUE(show_n)) {
    column_keys <- c(column_keys, "n_text")
    headers <- c(headers, n_header)
  }

  if (isTRUE(show_estimate)) {
    column_keys <- c(column_keys, "estimate_text")
    headers <- c(headers, sprintf("%s (95%% CI)", estimate_label))
  }

  if (length(column_keys) == 0) {
    stop("Select at least one table column to display.", call. = FALSE)
  }

  positions <- seq(1, by = 1.6, length.out = length(column_keys))
  long_parts <- vector("list", length(column_keys))

  for (i in seq_along(column_keys)) {
    long_parts[[i]] <- data.frame(
      row_key = table_rows$row_key,
      grouping_panel = table_rows$grouping_panel,
      column_key = column_keys[i],
      column_position = positions[i],
      text = table_rows[[column_keys[i]]],
      stringsAsFactors = FALSE
    )
  }

  table_data <- do.call(rbind, long_parts)
  table_data$row_key <- factor(table_data$row_key, levels = row_levels)

  list(
    table_data = table_data,
    positions = positions,
    headers = headers
  )
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

build_forest_table_plot <- function(table_spec,
                                    stripe_data,
                                    has_groupings = FALSE,
                                    table_position = c("left", "right"),
                                    striped_rows = FALSE,
                                    stripe_fill = "grey95",
                                    stripe_colour = NA,
                                    text_size = 3.2) {
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

  p <- p +
    ggplot2::geom_text(
      data = table_spec$table_data[table_spec$table_data$column_key == "term_text", , drop = FALSE],
      hjust = 0,
      size = text_size,
      lineheight = 0.95
    ) +
    ggplot2::geom_text(
      data = table_spec$table_data[table_spec$table_data$column_key != "term_text", , drop = FALSE],
      hjust = 0.5,
      size = text_size,
      lineheight = 0.95
    ) +
    ggplot2::scale_x_continuous(
      breaks = table_spec$positions,
      labels = table_spec$headers,
      position = "top",
      expand = ggplot2::expansion(add = c(0.15, 1.3))
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
      axis.text.x.top = ggplot2::element_text(face = "bold", colour = "black"),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_blank(),
      strip.placement = "outside",
      plot.margin = if (table_position == "left") {
        ggplot2::margin(5.5, 4, 5.5, 5.5)
      } else {
        ggplot2::margin(5.5, 5.5, 5.5, 4)
      }
    )

  if (isTRUE(has_groupings)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(grouping_panel),
      ncol = 1,
      scales = "free_y",
      strip.position = "left"
    )
  }

  p
}

combine_forest_plot_and_table <- function(plot, table_plot, table_position = c("left", "right")) {
  table_position <- match.arg(table_position)
  widths <- c(1.8, 2.4)

  if (table_position == "left") {
    patchwork::wrap_plots(table_plot, plot, nrow = 1, widths = widths)
  } else {
    patchwork::wrap_plots(plot, table_plot, nrow = 1, widths = rev(widths))
  }
}