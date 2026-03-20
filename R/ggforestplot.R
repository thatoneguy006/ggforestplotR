.compose_forest_table <- function(plot,
                                  position = c("left", "right"),
                                  show_terms = TRUE,
                                  show_n = NULL,
                                  show_estimate = TRUE,
                                  show_p = FALSE,
                                  columns = NULL,
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
    digits = digits,
    columns = columns
  )
  table_spec <- layout_center_table_spec(table_spec, text_size = text_size)
  table_width <- max(2.4, table_spec$content_width + 0.15)

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
    grid_line_linetype = grid_line_linetype,
    x_expand = ggplot2::expansion(mult = 0.08),
    text_hjust = 0.5,
    header_hjust = 0.5
  )

  combine_forest_plot_and_table(
    plot = plot_out,
    table_plot = table_plot,
    table_position = position,
    table_width = table_width
  )
}

.compose_split_table <- function(plot,
                                 show_terms = TRUE,
                                 show_n = NULL,
                                 show_estimate = TRUE,
                                 show_p = FALSE,
                                 left_columns = NULL,
                                 right_columns = NULL,
                                 term_header = "Term",
                                 n_header = "N",
                                 estimate_label = "Estimate",
                                 p_header = "P-value",
                                 digits = NULL,
                                 text_size = NULL,
                                 striped_rows = NULL,
                                 stripe_fill = NULL,
                                 stripe_colour = NULL,
                                 left_width = NULL,
                                 plot_width = NULL,
                                 right_width = NULL) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object created by `ggforestplot()`.", call. = FALSE)
  }

  state <- plot$ggforestplotR_state

  if (is.null(state)) {
    stop("`plot` must be created by `ggforestplot()` before calling `add_split_table()`.", call. = FALSE)
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

  default_left <- c(if (isTRUE(show_terms)) "term", if (isTRUE(show_n)) "n")
  default_right <- c(if (isTRUE(show_estimate)) "estimate", if (isTRUE(show_p)) "p")
  resolved_left <- if (is.null(left_columns)) default_left else normalize_table_columns(left_columns)
  resolved_right <- if (is.null(right_columns)) default_right else normalize_table_columns(right_columns)

  if (length(resolved_left) == 0L) {
    stop(
      "`add_split_table()` requires at least one left-side column. Supply `left_columns` or enable `show_terms`/`show_n`.",
      call. = FALSE
    )
  }

  if (length(resolved_right) == 0L) {
    stop(
      "`add_split_table()` requires at least one right-side column. Supply `right_columns` or enable `show_estimate`/`show_p`.",
      call. = FALSE
    )
  }

  overlap <- intersect(resolved_left, resolved_right)

  if (length(overlap) > 0L) {
    stop(
      sprintf("Split table columns cannot appear on both sides: %s", paste(overlap, collapse = ", ")),
      call. = FALSE
    )
  }

  if ("n" %in% c(resolved_left, resolved_right) && all(is.na(state$forest_data$n) | !nzchar(state$forest_data$n))) {
    stop("An `n` column is required when split table columns include `n`.", call. = FALSE)
  }

  if ("p" %in% c(resolved_left, resolved_right) && all(is.na(state$forest_data$p.value))) {
    stop("A `p.value` column is required when split table columns include `p`.", call. = FALSE)
  }

  left_spec <- build_forest_table_data(
    state$forest_data,
    show_terms = FALSE,
    show_n = FALSE,
    show_estimate = FALSE,
    show_p = FALSE,
    term_header = term_header,
    n_header = n_header,
    estimate_label = estimate_label,
    p_header = p_header,
    digits = digits,
    columns = resolved_left
  )

  right_spec <- build_forest_table_data(
    state$forest_data,
    show_terms = FALSE,
    show_n = FALSE,
    show_estimate = FALSE,
    show_p = FALSE,
    term_header = term_header,
    n_header = n_header,
    estimate_label = estimate_label,
    p_header = p_header,
    digits = digits,
    columns = resolved_right
  )

  left_spec <- layout_split_table_spec(left_spec, text_size = text_size, alignment = "left")
  right_spec <- layout_split_table_spec(right_spec, text_size = text_size, alignment = "right")

  if (is.null(plot_width)) {
    plot_width <- 2.5
  }

  if (is.null(left_width)) {
    left_width <- plot_width * split_table_width_multiplier(length(left_spec$column_keys))
  }

  if (is.null(right_width)) {
    right_width <- plot_width * split_table_width_multiplier(length(right_spec$column_keys))
  }

  left_plot <- build_forest_table_plot(
    table_spec = left_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = "left",
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    text_size = text_size,
    grid_lines = FALSE,
    plot_margin = ggplot2::margin(5.5, 0, 5.5, 5.5),
    text_hjust = 0,
    header_hjust = 0
  )

  right_plot <- build_forest_table_plot(
    table_spec = right_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = "right",
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    text_size = text_size,
    grid_lines = FALSE,
    plot_margin = ggplot2::margin(5.5, 5.5, 5.5, 0),
    text_hjust = 1,
    header_hjust = 1
  )

  plot_theme_args <- list(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

  if (is.null(plot$theme$axis.line.x)) {
    plot_theme_args$axis.line.x <- ggplot2::element_line(colour = "black")
  }

  if (is.null(plot$theme$plot.margin)) {
    plot_theme_args$plot.margin <- ggplot2::margin(5.5, 0, 5.5, 0)
  }

  plot_out <- plot + do.call(ggplot2::theme, plot_theme_args)

  left_spec$content_width <- left_width
  right_spec$content_width <- right_width

  combine_split_forest_plot(
    plot = plot_out,
    left_table = left_plot,
    right_table = right_plot,
    left_spec = left_spec,
    right_spec = right_spec,
    plot_width = plot_width
  )
}

#' Draw a ggplot2 forest plot
#'
#' Builds a forest plot from standardized coefficient data or directly from a
#' fitted model.
#'
#' @param data Either a tidy coefficient data frame or a model object
#'   supported by [broom::tidy()].
#' @param term Column name holding the model term identifiers.
#' @param estimate Column name holding the point estimates.
#' @param conf.low Column name holding the lower confidence bounds.
#' @param conf.high Column name holding the upper confidence bounds. 
#' @param label Optional column name used for the displayed row labels. 
#' @param group Optional column name used for color-grouping estimates.
#' @param grouping Optional column name used to split rows into grouped plot
#'   sections.
#' @param grouping_strip_position Positioning for grouped section strips.
#' @param separate_groups Optional column name used to identify labeled
#'   variable blocks that can be outlined with grid lines.
#' @param n Optional column name holding sample sizes or other N labels for
#'   table helpers.
#' @param p.value Optional column name holding p-values. 
#' @param exponentiate Logical; if `TRUE`, transform the estimates and draw the
#'  axis on the log scale with the null line at 1.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#' @param point_size Point size for coefficient markers.
#' @param point_shape Shape used for coefficient markers.
#' @param line_size Line width for confidence intervals.
#' @param staple_width Width of the terminal staples on confidence interval
#'   lines.
#' @param dodge_width Horizontal dodging used for grouped estimates.
#' @param separate_lines Logical; if `TRUE`, draw grid lines
#'   around each labeled block identified by `separate_groups`.
#' @param separator_line_linetype Line type used for separator lines.
#' @param separator_line_colour Colour used for separator lines.
#' @param separator_line_size Line width used for separator lines.
#' @param striped_rows Logical; if `TRUE`, shade alternating rows.
#' @param stripe_fill Fill color used for shaded rows.
#' @param stripe_colour Border color for shaded rows.
#' @param zero_line Logical; if `TRUE`, draw a null reference line.
#' @param zero_line_linetype Line type for the null reference line.
#' @param zero_line_colour Color for the null reference line.
#'
#' @return A `ggplot` object. Use standard `ggplot2` functions such as
#'   [ggplot2::labs()] for plot labels, and add composition helpers after
#'   styling the main plot.
#' @export
#'
#' @examples
#' coefs <- data.frame(
#'   term = c("Age", "BMI", "Treatment"),
#'   estimate = c(0.10, -0.08, 0.34),
#'   conf.low = c(0.02, -0.16, 0.12),
#'   conf.high = c(0.18, 0.00, 0.56)
#' )
#'
#' ggforestplot(coefs)
#'
#' ggforestplot(coefs, striped_rows = TRUE, point_shape = 17) +
#'   ggplot2::labs(title = "Basic forest plot")
ggforestplot <- function(data,
                         term = "term",
                         estimate = "estimate",
                         conf.low = "conf.low",
                         conf.high = "conf.high",
                         label = term,
                         group = NULL,
                         grouping = NULL,
                         grouping_strip_position = c("left", "right"),
                         separate_groups = NULL,
                         n = NULL,
                         p.value = NULL,
                         exponentiate = FALSE,
                         sort_terms = c("none", "descending", "ascending"),
                         point_size = 2.3,
                         point_shape = 19,
                         line_size = 0.5,
                         staple_width = 0.2,
                         dodge_width = 0.6,
                         separate_lines = FALSE,
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
      separate_groups = separate_groups,
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
  plot_stripe_data <- stripe_data
  plot_x_limits <- NULL

  if (isTRUE(exponentiate)) {
    plot_x_limits <- default_plot_background_limits(
      forest_data,
      exponentiate = exponentiate,
      include_zero = zero_line
    )

    plot_stripe_data$xmin <- plot_x_limits[1]
    plot_stripe_data$xmax <- plot_x_limits[2]
  }

  has_groups <- any(!is.na(forest_data$group) & nzchar(forest_data$group))
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
      data = plot_stripe_data[plot_stripe_data$fill_key == "stripe", , drop = FALSE],
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

  if (isTRUE(separate_lines) && nrow(separator_data) > 0L) {
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
    p <- p + ggplot2::scale_x_log10(
      limits = plot_x_limits,
      expand = ggplot2::expansion(mult = 0)
    )
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
      stripe_colour = stripe_colour,
      exponentiate = exponentiate,
      zero_line = zero_line
    )
  )

  p
}

#' Add a summary table to a forest plot
#'
#' Compose a summary table onto a forest plot.
#'
#' @param plot A plot created by [ggforestplot()]. Leave as `NULL` to use
#'   `+ add_forest_table(...)` syntax.
#' @param position Whether to place the table on the left or right of the
#'   forest plot.
#' @param show_terms Whether to show the term column in the table.
#' @param show_n Whether to show the `N` column. Defaults to `TRUE` when the
#'   underlying plot data includes an `n` column.
#' @param show_estimate Whether to show the formatted estimate and confidence
#'   interval column.
#' @param show_p Whether to display the p-value column.
#' @param columns Optional explicit columns to display in the side table, in
#'   the order they should appear. Accepts names such as `"n"` and `"term"`,
#'   or positions `1:4` corresponding to `term`, `n`, `estimate`, and `p`.
#'   When supplied, this overrides the default `show_*` column selection.
#' @param term_header Header text for the term column.
#' @param n_header Header text for the `N` column.
#' @param estimate_label Header label for the estimate column.
#' @param p_header Header text for the p-value column.
#' @param digits Number of digits used when formatting estimates and p-values.
#'   Defaults to `2`.
#' @param text_size Text size for table contents. Defaults to `3.2`.
#' @param striped_rows Whether to draw alternating row stripes behind the
#'   table. Defaults to the stripe setting used in [ggforestplot()].
#' @param stripe_fill Fill colour used for striped rows. Defaults to the
#'   stripe fill used in [ggforestplot()].
#' @param stripe_colour Outline colour for striped rows. Defaults to the
#'   stripe outline used in [ggforestplot()].
#' @param grid_lines Whether to draw black horizontal grid lines in the table.
#' @param grid_line_colour Colour used for the table grid lines.
#' @param grid_line_size Line width used for the table grid lines.
#' @param grid_line_linetype Line type used for the table grid lines.
#'
#' @return A patchwork-composed plot containing the forest plot and side
#'   table, or a ggplot add-on object when `plot = NULL`.
#' @export
#'
#' @examples
#' coefs <- data.frame(
#'   term = c("Age", "BMI", "Treatment"),
#'   estimate = c(0.3, -0.2, 0.4),
#'   conf.low = c(0.1, -0.4, 0.2),
#'   conf.high = c(0.5, 0.0, 0.6),
#'   sample_size = c(120, 115, 98),
#'   p_value = c(0.012, 0.031, 0.004)
#' )
#'
#' p <- ggforestplot(coefs, n = "sample_size", p.value = "p_value")
#' add_forest_table(
#'   p,
#'   position = "left",
#'   show_n = TRUE,
#'   show_p = TRUE,
#'   estimate_label = "Beta"
#' )
#'
#' ggforestplot(coefs, n = "sample_size", p.value = "p_value") +
#'   add_forest_table(
#'     position = "right",
#'     show_n = TRUE,
#'     show_p = TRUE,
#'     estimate_label = "Beta"
#'   )
add_forest_table <- function(plot = NULL,
                             position = c("left", "right"),
                             show_terms = TRUE,
                             show_n = NULL,
                             show_estimate = TRUE,
                             show_p = FALSE,
                             columns = NULL,
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
        columns = columns,
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
    columns = columns,
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

#' Add split tables around a forest plot
#'
#' Compose split table blocks around a forest plot so that summary data appear
#' on both sides of the plotting panel.
#'
#' @param plot A plot created by [ggforestplot()]. Leave as `NULL` to use
#'   `+ add_split_table(...)` syntax.
#' @param show_terms Whether to include the term column in the default
#'   left-side selection when `left_columns` is not supplied.
#' @param show_n Whether to include the `N` column in the default left-side
#'   selection when `left_columns` is not supplied. Defaults to `TRUE` when
#'   the underlying plot data include an `n` column.
#' @param show_estimate Whether to include the formatted estimate and
#'   confidence interval column in the default right-side selection when
#'   `right_columns` is not supplied.
#' @param show_p Whether to include the p-value column in the default
#'   right-side selection when `right_columns` is not supplied.
#' @param left_columns Optional explicit columns to place on the left side of
#'   the forest plot. Accepts names such as `"term"` and `"n"`, or positions
#'   `1:4` corresponding to `term`, `n`, `estimate`, and `p`.
#' @param right_columns Optional explicit columns to place on the right side
#'   of the forest plot. Accepts names such as `"estimate"` and `"p"`, or
#'   positions `1:4` corresponding to `term`, `n`, `estimate`, and `p`.
#' @param term_header Header text for the term column.
#' @param n_header Header text for the `N` column.
#' @param estimate_label Header label for the estimate column.
#' @param p_header Header text for the p-value column.
#' @param digits Number of digits used when formatting estimates and p-values.
#'   Defaults to `2`.
#' @param text_size Text size for table contents. Defaults to `3.2`.
#' @param striped_rows Whether to draw alternating row stripes behind the
#'   split table layout. Defaults to the stripe setting used in
#'   [ggforestplot()].
#' @param stripe_fill Fill colour used for striped rows. Defaults to the
#'   stripe fill used in [ggforestplot()].
#' @param stripe_colour Outline colour for striped rows. Defaults to the
#'   stripe outline used in [ggforestplot()].
#' @param left_width Optional width allocated to the left table block. By
#'   default this is estimated from the displayed left-side text so long
#'   labels get more room.
#' @param plot_width Optional width allocated to the forest plot panel. By
#'   default this is derived from the left and right table widths.
#' @param right_width Optional width allocated to the right table block. By
#'   default this is estimated from the displayed right-side text.
#'
#' @return A patchwork-composed plot containing a left table, the forest plot,
#'   and a right table, or a ggplot add-on object when `plot = NULL`.
#' @export
#'
#' @examples
#' coefs <- data.frame(
#'   term = c("Age", "BMI", "Treatment"),
#'   estimate = c(0.3, -0.2, 0.4),
#'   conf.low = c(0.1, -0.4, 0.2),
#'   conf.high = c(0.5, 0.0, 0.6),
#'   sample_size = c(120, 115, 98),
#'   p_value = c(0.012, 0.031, 0.004)
#' )
#'
#' p <- ggforestplot(coefs, n = "sample_size", p.value = "p_value")
#' add_split_table(
#'   p,
#'   left_columns = c("term", "n"),
#'   right_columns = c("estimate", "p"),
#'   estimate_label = "HR"
#' )
#'
#' ggforestplot(coefs, n = "sample_size", p.value = "p_value") +
#'   add_split_table(
#'     left_columns = c(1, 2),
#'     right_columns = c(3, 4),
#'     estimate_label = "HR"
#'   )
add_split_table <- function(plot = NULL,
                            show_terms = TRUE,
                            show_n = NULL,
                            show_estimate = TRUE,
                            show_p = FALSE,
                            left_columns = NULL,
                            right_columns = NULL,
                            term_header = "Term",
                            n_header = "N",
                            estimate_label = "Estimate",
                            p_header = "P-value",
                            digits = NULL,
                            text_size = NULL,
                            striped_rows = NULL,
                            stripe_fill = NULL,
                            stripe_colour = NULL,
                            left_width = NULL,
                            plot_width = NULL,
                            right_width = NULL) {
  if (is.null(plot)) {
    return(structure(
      list(
        show_terms = show_terms,
        show_n = show_n,
        show_estimate = show_estimate,
        show_p = show_p,
        left_columns = left_columns,
        right_columns = right_columns,
        term_header = term_header,
        n_header = n_header,
        estimate_label = estimate_label,
        p_header = p_header,
        digits = digits,
        text_size = text_size,
        striped_rows = striped_rows,
        stripe_fill = stripe_fill,
        stripe_colour = stripe_colour,
        left_width = left_width,
        plot_width = plot_width,
        right_width = right_width
      ),
      class = "ggforestplot_split_table_adder"
    ))
  }

  .compose_split_table(
    plot = plot,
    show_terms = show_terms,
    show_n = show_n,
    show_estimate = show_estimate,
    show_p = show_p,
    left_columns = left_columns,
    right_columns = right_columns,
    term_header = term_header,
    n_header = n_header,
    estimate_label = estimate_label,
    p_header = p_header,
    digits = digits,
    text_size = text_size,
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    left_width = left_width,
    plot_width = plot_width,
    right_width = right_width
  )
}

#' @export
#' @keywords internal
ggplot_add.ggforestplot_table_adder <- function(object, plot, ...) {
  do.call(
    .compose_forest_table,
    c(list(plot = plot), object)
  )
}

#' @export
#' @keywords internal
ggplot_add.ggforestplot_split_table_adder <- function(object, plot, ...) {
  do.call(
    .compose_split_table,
    c(list(plot = plot), object)
  )
}





