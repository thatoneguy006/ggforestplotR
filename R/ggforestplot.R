.compose_forest_table <- function(plot,
                                  position = c("left", "right"),
                                  show_terms = TRUE,
                                  show_n = NULL,
                                  show_estimate = TRUE,
                                  show_p = FALSE,
                                  term_header = "Term",
                                  n_header = "N",
                                  estimate_label = "Estimate",
                                  p_header = "P-value",
                                  digits = NULL,
                                  text_size = NULL,
                                  striped_rows = NULL,
                                  stripe_fill = NULL,
                                  stripe_colour = NULL,
                                  grid_lines = FALSE,
                                  grid_line_colour = "black",
                                  grid_line_size = 0.3,
                                  grid_line_linetype = 1) {
  position <- match.arg(position)

  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object created by `ggforestplot()`.", call. = FALSE)
  }

  state <- plot$ggforestplotR_state

  if (is.null(state)) {
    stop("`plot` must be created by `ggforestplot()` before calling `add_forest_table()`.", call. = FALSE)
  }

  if (is.null(show_n)) {
    show_n <- any(!is.na(state$forest_data$n) & nzchar(state$forest_data$n))
  }

  if (is.null(digits)) {
    digits <- 2
  }

  if (is.null(text_size)) {
    text_size <- 3.2
  }

  if (is.null(striped_rows)) {
    striped_rows <- isTRUE(state$defaults$striped_rows)
  }

  if (is.null(stripe_fill)) {
    stripe_fill <- state$defaults$stripe_fill
  }

  if (is.null(stripe_colour)) {
    stripe_colour <- state$defaults$stripe_colour
  }

  if (isTRUE(show_n) && all(is.na(state$forest_data$n) | !nzchar(state$forest_data$n))) {
    stop("`show_n = TRUE` requires an `n` column in the underlying forest data.", call. = FALSE)
  }

  if (isTRUE(show_p) && all(is.na(state$forest_data$p.value))) {
    stop("`show_p = TRUE` requires a `p.value` column in the underlying forest data.", call. = FALSE)
  }

  plot_out <- plot

  if (isTRUE(show_terms)) {
    plot_out <- plot_out + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }

  table_spec <- build_forest_table_data(
    state$forest_data,
    show_terms = show_terms,
    show_n = show_n,
    show_estimate = show_estimate,
    show_p = show_p,
    term_header = term_header,
    n_header = n_header,
    estimate_label = estimate_label,
    p_header = p_header,
    digits = digits
  )

  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = position,
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    text_size = text_size,
    grid_lines = grid_lines,
    grid_line_colour = grid_line_colour,
    grid_line_size = grid_line_size,
    grid_line_linetype = grid_line_linetype
  )

  combine_forest_plot_and_table(
    plot = plot_out,
    table_plot = table_plot,
    table_position = position
  )
}

ggforestplot <- function(data,
                         term = "term",
                         estimate = "estimate",
                         conf.low = "conf.low",
                         conf.high = "conf.high",
                         label = term,
                         group = NULL,
                         grouping = NULL,
                         grouping_strip_position = c("left", "right"),
                         separator_group = NULL,
                         n = NULL,
                         p.value = NULL,
                         exponentiate = FALSE,
                         sort_terms = c("none", "descending", "ascending"),
                         point_size = 2.3,
                         point_shape = 19,
                         line_size = 0.5,
                         staple_width = 0.2,
                         dodge_width = 0.6,
                         separator_lines = FALSE,
                         separator_line_linetype = 2,
                         separator_line_colour = "black",
                         separator_line_size = 0.4,
                         striped_rows = FALSE,
                         stripe_fill = "grey95",
                         stripe_colour = NA,
                         zero_line = TRUE,
                         zero_line_linetype = 2,
                         zero_line_colour = "grey60") {
  sort_terms <- match.arg(sort_terms)
  grouping_strip_position <- match.arg(grouping_strip_position)

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
      separator_group = separator_group,
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

  display_data <- build_forest_plot_data(forest_data)
  forest_data <- display_data$plot_data
  stripe_data <- display_data$stripe_data
  separator_data <- display_data$separator_data

  has_groups <- any(!is.na(forest_data$group))
  dodge <- ggplot2::position_dodge(width = dodge_width)
  mapping <- if (has_groups) {
    ggplot2::aes(
      x = .data$estimate,
      y = .data$row_key,
      xmin = .data$conf.low,
      xmax = .data$conf.high,
      colour = .data$group
    )
  } else {
    ggplot2::aes(
      x = .data$estimate,
      y = .data$row_key,
      xmin = .data$conf.low,
      xmax = .data$conf.high
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

  if (isTRUE(separator_lines) && nrow(separator_data) > 0L) {
    p <- p + ggplot2::geom_hline(
      data = separator_data,
      mapping = ggplot2::aes(yintercept = .data$yintercept),
      inherit.aes = FALSE,
      linetype = separator_line_linetype,
      colour = separator_line_colour,
      linewidth = separator_line_size
    )
  }

  p <- p +
    ggplot2::geom_errorbar(
      width = staple_width,
      linewidth = line_size,
      position = dodge,
      orientation = "y"
    ) +
    ggplot2::geom_point(
      size = point_size,
      shape = point_shape,
      position = dodge
    ) +
    ggplot2::scale_y_discrete(
      labels = display_data$axis_labels,
      drop = TRUE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.placement = "outside"
    )

  null_value <- if (isTRUE(exponentiate)) 1 else 0

  if (isTRUE(zero_line)) {
    p <- p + ggplot2::geom_vline(
      xintercept = null_value,
      linetype = zero_line_linetype,
      colour = zero_line_colour
    )
  }

  if (isTRUE(exponentiate)) {
    p <- p + ggplot2::scale_x_log10()
  }

  if (isTRUE(display_data$has_groupings)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(grouping_panel),
      ncol = 1,
      scales = "free_y",
      strip.position = grouping_strip_position
    )
  }

  p <- p + ggplot2::labs(
    x = if (isTRUE(exponentiate)) "Estimate (log scale)" else "Estimate",
    y = NULL,
    colour = if (has_groups) "Group" else NULL
  )

  p$ggforestplotR_state <- list(
    forest_data = forest_data,
    stripe_data = stripe_data,
    has_groupings = display_data$has_groupings,
    grouping_strip_position = grouping_strip_position,
    defaults = list(
      striped_rows = striped_rows,
      stripe_fill = stripe_fill,
      stripe_colour = stripe_colour
    )
  )

  p
}

add_forest_table <- function(plot = NULL,
                             position = c("left", "right"),
                             show_terms = TRUE,
                             show_n = NULL,
                             show_estimate = TRUE,
                             show_p = FALSE,
                             term_header = "Term",
                             n_header = "N",
                             estimate_label = "Estimate",
                             p_header = "P-value",
                             digits = NULL,
                             text_size = NULL,
                             striped_rows = NULL,
                             stripe_fill = NULL,
                             stripe_colour = NULL,
                             grid_lines = FALSE,
                             grid_line_colour = "black",
                             grid_line_size = 0.3,
                             grid_line_linetype = 1) {
  position <- match.arg(position)

  if (is.null(plot)) {
    return(structure(
      list(
        position = position,
        show_terms = show_terms,
        show_n = show_n,
        show_estimate = show_estimate,
        show_p = show_p,
        term_header = term_header,
        n_header = n_header,
        estimate_label = estimate_label,
        p_header = p_header,
        digits = digits,
        text_size = text_size,
        striped_rows = striped_rows,
        stripe_fill = stripe_fill,
        stripe_colour = stripe_colour,
        grid_lines = grid_lines,
        grid_line_colour = grid_line_colour,
        grid_line_size = grid_line_size,
        grid_line_linetype = grid_line_linetype
      ),
      class = "ggforestplot_table_adder"
    ))
  }

  .compose_forest_table(
    plot = plot,
    position = position,
    show_terms = show_terms,
    show_n = show_n,
    show_estimate = show_estimate,
    show_p = show_p,
    term_header = term_header,
    n_header = n_header,
    estimate_label = estimate_label,
    p_header = p_header,
    digits = digits,
    text_size = text_size,
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    grid_lines = grid_lines,
    grid_line_colour = grid_line_colour,
    grid_line_size = grid_line_size,
    grid_line_linetype = grid_line_linetype
  )
}

ggplot_add.ggforestplot_table_adder <- function(object, plot, ...) {
  do.call(
    .compose_forest_table,
    c(list(plot = plot), object)
  )
}
