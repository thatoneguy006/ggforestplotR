ggforestplot <- function(data,
                         term = "term",
                         estimate = "estimate",
                         conf.low = "conf.low",
                         conf.high = "conf.high",
                         label = term,
                         group = NULL,
                         grouping = NULL,
                         n = NULL,
                         p.value = NULL,
                         exponentiate = FALSE,
                         sort_terms = c("none", "descending", "ascending"),
                         point_size = 2.3,
                         line_size = 0.5,
                         dodge_width = 0.6,
                         striped_rows = FALSE,
                         stripe_fill = "grey95",
                         stripe_colour = NA,
                         table_position = c("none", "left", "right"),
                         table_show_terms = TRUE,
                         table_show_n = !is.null(n),
                         table_show_estimate = TRUE,
                         table_term_header = "Term",
                         table_n_header = "N",
                         table_estimate_label = "Estimate",
                         table_digits = 2,
                         table_text_size = 3.2,
                         zero_line = TRUE,
                         zero_line_linetype = 2,
                         zero_line_colour = "grey60",
                         xlab = NULL,
                         ylab = NULL,
                         title = NULL) {
  sort_terms <- match.arg(sort_terms)
  table_position <- match.arg(table_position)

  forest_data <- if (is.data.frame(data)) {
    as_forest_data(
      data = data,
      term = term,
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      label = label,
      group = group,
      grouping = grouping,
      n = n,
      p.value = p.value,
      exponentiate = exponentiate,
      sort_terms = sort_terms
    )
  } else {
    tidy_forest_model(
      model = data,
      exponentiate = exponentiate,
      sort_terms = sort_terms
    )
  }

  if (isTRUE(table_show_n) && all(is.na(forest_data$n) | !nzchar(forest_data$n))) {
    stop(
      "`table_show_n = TRUE` requires an `n` column when `data` is a data frame.",
      call. = FALSE
    )
  }

  display_data <- build_forest_plot_data(forest_data)
  forest_data <- display_data$plot_data
  stripe_data <- display_data$stripe_data

  has_groups <- any(!is.na(forest_data$group))
  dodge <- ggplot2::position_dodge(width = dodge_width)
  mapping <- if (has_groups) {
    ggplot2::aes(
      x = .data$plot_label,
      y = .data$estimate,
      ymin = .data$conf.low,
      ymax = .data$conf.high,
      colour = .data$group
    )
  } else {
    ggplot2::aes(
      x = .data$plot_label,
      y = .data$estimate,
      ymin = .data$conf.low,
      ymax = .data$conf.high
    )
  }

  p <- ggplot2::ggplot(forest_data, mapping)

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
    ggplot2::geom_errorbar(
      width = 0,
      linewidth = line_size,
      position = dodge
    ) +
    ggplot2::geom_point(
      size = point_size,
      position = dodge
    ) +
    ggplot2::scale_x_discrete(labels = display_data$axis_labels) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.placement = "outside"
    )

  null_value <- if (isTRUE(exponentiate)) 1 else 0

  if (isTRUE(zero_line)) {
    p <- p + ggplot2::geom_hline(
      yintercept = null_value,
      linetype = zero_line_linetype,
      colour = zero_line_colour
    )
  }

  if (isTRUE(exponentiate)) {
    p <- p + ggplot2::scale_y_log10()
  }

  if (isTRUE(display_data$has_groupings)) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(grouping_panel),
      scales = "free_x",
      space = "free",
      switch = "y"
    )
  }

  if (is.null(xlab)) {
    xlab <- if (isTRUE(exponentiate)) "Estimate (log scale)" else "Estimate"
  }

  if (is.null(ylab)) {
    ylab <- NULL
  }

  if (table_position != "none" && isTRUE(table_show_terms)) {
    p <- p + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }

  p <- p + ggplot2::labs(
    x = ylab,
    y = xlab,
    title = title,
    colour = if (has_groups) "Group" else NULL
  )

  if (table_position == "none") {
    return(p)
  }

  table_spec <- build_forest_table_data(
    forest_data,
    show_terms = table_show_terms,
    show_n = table_show_n,
    show_estimate = table_show_estimate,
    term_header = table_term_header,
    n_header = table_n_header,
    estimate_label = table_estimate_label,
    digits = table_digits
  )

  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = stripe_data,
    has_groupings = display_data$has_groupings,
    table_position = table_position,
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    text_size = table_text_size
  )

  combine_forest_plot_and_table(
    plot = p,
    table_plot = table_plot,
    table_position = table_position
  )
}