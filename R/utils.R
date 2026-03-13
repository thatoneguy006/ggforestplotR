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
  plot_data$plot_label <- NA_character_
  plot_data$grouping_panel <- if (has_groupings) {
    ifelse(is.na(plot_data$grouping) | !nzchar(plot_data$grouping), "(Ungrouped)", plot_data$grouping)
  } else {
    NA_character_
  }

  panel_values <- if (has_groupings) unique(plot_data$grouping_panel) else "__all__"
  plot_label_levels <- character()
  axis_labels <- character()
  stripe_parts <- vector("list", length(panel_values))

  for (i in seq_along(panel_values)) {
    panel_value <- panel_values[[i]]
    panel_idx <- if (has_groupings) which(plot_data$grouping_panel == panel_value) else seq_len(nrow(plot_data))
    panel_labels <- unique(plot_data$label[panel_idx])
    panel_plot_labels <- if (has_groupings) {
      paste(panel_value, panel_labels, sep = "___")
    } else {
      panel_labels
    }

    plot_data$plot_label[panel_idx] <- panel_plot_labels[match(plot_data$label[panel_idx], panel_labels)]
    plot_label_levels <- c(plot_label_levels, panel_plot_labels)
    axis_labels <- c(axis_labels, stats::setNames(panel_labels, panel_plot_labels))

    stripe_parts[[i]] <- data.frame(
      plot_label = panel_plot_labels,
      grouping_panel = if (has_groupings) panel_value else NA_character_,
      stripe_id = seq_along(panel_plot_labels),
      xmin = rev(seq_along(panel_plot_labels)) - 0.5,
      xmax = rev(seq_along(panel_plot_labels)) + 0.5,
      ymin = -Inf,
      ymax = Inf,
      stringsAsFactors = FALSE
    )
  }

  plot_data$plot_label <- factor(plot_data$plot_label, levels = rev(plot_label_levels))

  stripe_data <- do.call(rbind, stripe_parts)
  stripe_data$plot_label <- factor(stripe_data$plot_label, levels = rev(plot_label_levels))
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
  plot_levels <- levels(data$plot_label)
  row_parts <- vector("list", length(plot_levels))

  for (i in seq_along(plot_levels)) {
    plot_label <- plot_levels[[i]]
    idx <- which(as.character(data$plot_label) == plot_label)

    if (length(idx) == 0) {
      next
    }

    row_data <- data[idx, , drop = FALSE]
    row_parts[[i]] <- data.frame(
      plot_label = plot_label,
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
  table_rows$plot_label <- factor(table_rows$plot_label, levels = plot_levels)

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
      plot_label = table_rows$plot_label,
      grouping_panel = table_rows$grouping_panel,
      column_position = positions[i],
      text = table_rows[[column_keys[i]]],
      stringsAsFactors = FALSE
    )
  }

  table_data <- do.call(rbind, long_parts)
  table_data$plot_label <- factor(table_data$plot_label, levels = plot_levels)

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
  text_hjust <- if (table_position == "left") 0 else 0

  p <- ggplot2::ggplot(
    table_spec$table_data,
    ggplot2::aes(
      x = .data$plot_label,
      y = .data$column_position,
      label = .data$text
    )
  )

  if (isTRUE(striped_rows)) {
    p <- p + ggplot2::geom_rect(
      data = stripe_data[stripe_data$fill_key == "stripe", , drop = FALSE],
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = stripe_fill,
      colour = stripe_colour
    )
  }

  p <- p +
    ggplot2::geom_text(
      hjust = text_hjust,
      size = text_size,
      lineheight = 0.95
    ) +
    ggplot2::scale_x_discrete(labels = rep("", length(levels(table_spec$table_data$plot_label)))) +
    ggplot2::scale_y_continuous(
      breaks = table_spec$positions,
      labels = table_spec$headers,
      position = "right",
      expand = ggplot2::expansion(add = c(0.15, 0.9))
    ) +
    ggplot2::coord_flip(clip = "off") +
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
        ggplot2::margin(5.5, 8, 5.5, 5.5)
      } else {
        ggplot2::margin(5.5, 5.5, 5.5, 8)
      }
    )

  if (isTRUE(has_groupings)) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(grouping_panel),
      scales = "free_x",
      space = "free",
      switch = "y"
    )
  }

  p
}

combine_forest_plot_and_table <- function(plot, table_plot, table_position = c("left", "right")) {
  table_position <- match.arg(table_position)
  plot_grob <- ggplot2::ggplotGrob(plot)
  table_grob <- ggplot2::ggplotGrob(table_plot)

  if (table_position == "left") {
    cbind(table_grob, plot_grob, size = "first")
  } else {
    cbind(plot_grob, table_grob, size = "first")
  }
}