.compose_split_table <- function(plot,
                                 left_columns = NULL,
                                 right_columns = NULL,
                                 term_header = "Term",
                                 n_header = "N",
                                 events_header = "Events",
                                 estimate_label = NULL,
                                 p_header = "P-value",
                                 column_labels = NULL,
                                 digits = NULL,
                                 estimate_digits = NULL,
                                 interval_digits = NULL,
                                 p_digits = NULL,
                                 estimate_fmt = NULL,
                                 ci_fmt = NULL,
                                 formatters = NULL,
                                 reference_text = "Reference",
                                 striped_rows = NULL,
                                 stripe_fill = NULL,
                                 stripe_colour = NULL,
                                 stripe_alpha = NULL,
                                 left_width = NULL,
                                 plot_width = NULL,
                                 right_width = NULL,
                                 text_style = NULL) {
  if (!inherits(plot, "ggplot")) {
    abort_forest(
      "`plot` must be a ggplot object created by `ggforestplot()`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  state <- plot$ggforestplotR_state

  if (is.null(state)) {
    abort_forest(
      "`plot` must be created by `ggforestplot()` before calling `add_split_table()`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  source_forest_data <- if (is.null(state$full_forest_data)) {
    state$forest_data
  } else {
    state$full_forest_data
  }
  state <- align_forest_state_to_plot_y_scale(state, plot)
  plot <- align_forest_row_layers_to_state(plot, state)
  display_data <- if (is.null(state$display_data)) {
    state$forest_data
  } else {
    state$display_data
  }

  if (is.null(digits)) {
    digits <- 2
  }

  digits <- resolve_table_digits(
    digits = digits,
    estimate_digits = estimate_digits,
    interval_digits = interval_digits,
    p_digits = p_digits
  )

  if (is.null(estimate_label)) {
    estimate_label <- state$defaults$estimate_label
  }

  if (is.null(estimate_label)) {
    estimate_label <- "Estimate"
  }

  text_style <- resolve_forest_text_style(
    inherited = state$table_text_style, overrides = text_style
  )
  text_size <- text_style$text_size
  header_text_size <- text_style$header_text_size
  header_fontface <- text_style$header_fontface
  header_family <- text_style$header_family

  if (is.null(striped_rows)) {
    striped_rows <- isTRUE(state$defaults$striped_rows)
  }

  if (is.null(stripe_fill)) {
    stripe_fill <- state$defaults$stripe_fill
  }

  if (is.null(stripe_colour)) {
    stripe_colour <- state$defaults$stripe_colour
  }

  if (is.null(stripe_alpha)) {
    stripe_alpha <- state$defaults$stripe_alpha
  }

  default_left <- default_split_left_columns(source_forest_data)
  default_right <- default_split_right_columns(source_forest_data)
  resolved_left <- if (is.null(left_columns)) default_left else normalize_table_columns(left_columns, data = source_forest_data)
  resolved_right <- if (is.null(right_columns)) default_right else normalize_table_columns(right_columns, data = source_forest_data)

  if (length(resolved_left) == 0L) {
    abort_forest(
      "`add_split_table()` requires at least one left-side column. Supply `left_columns`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  if (length(resolved_right) == 0L) {
    abort_forest(
      "`add_split_table()` requires at least one right-side column. Supply `right_columns`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  overlap <- intersect(resolved_left, resolved_right)

  if (length(overlap) > 0L) {
    abort_forest(
      sprintf("Split table columns cannot appear on both sides: %s", paste(overlap, collapse = ", ")),
      class = "ggforestplotR_invalid_table"
    )
  }

  if ("n" %in% c(resolved_left, resolved_right) && all(is.na(source_forest_data$n) | !nzchar(source_forest_data$n))) {
    abort_forest(
      "An `n` column is required when split table columns include `n`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  if ("events" %in% c(resolved_left, resolved_right) &&
    all(is.na(source_forest_data$events) | !nzchar(source_forest_data$events))) {
    abort_forest(
      "An `events` column is required when split table columns include `events`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  if ("p" %in% c(resolved_left, resolved_right) && all(is.na(source_forest_data$p.value))) {
    abort_forest(
      "A `p.value` column is required when split table columns include `p`.",
      class = "ggforestplotR_invalid_table"
    )
  }

  left_spec <- build_forest_table_data(
    source_forest_data,
    term_header = term_header,
    n_header = n_header,
    events_header = events_header,
    estimate_label = estimate_label,
    p_header = p_header,
    column_labels = column_labels,
    estimate_digits = digits$estimate_digits,
    interval_digits = digits$interval_digits,
    p_digits = digits$p_digits,
    estimate_fmt = estimate_fmt,
    ci_fmt = ci_fmt,
    formatters = formatters,
    reference_text = reference_text,
    columns = resolved_left,
    display_data = display_data
  )

  right_spec <- build_forest_table_data(
    source_forest_data,
    term_header = term_header,
    n_header = n_header,
    events_header = events_header,
    estimate_label = estimate_label,
    p_header = p_header,
    column_labels = column_labels,
    estimate_digits = digits$estimate_digits,
    interval_digits = digits$interval_digits,
    p_digits = digits$p_digits,
    estimate_fmt = estimate_fmt,
    ci_fmt = ci_fmt,
    formatters = formatters,
    reference_text = reference_text,
    columns = resolved_right,
    display_data = display_data
  )

  left_spec$text_style <- text_style
  left_spec <- layout_split_table_spec(
    left_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = if (is.null(header_family)) "" else header_family,
    alignment = "left"
  )
  right_spec$text_style <- text_style
  right_spec <- layout_split_table_spec(
    right_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = if (is.null(header_family)) "" else header_family,
    alignment = "right"
  )

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
    facet_strip_position = state$facet_strip_position,
    table_position = "left",
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    stripe_alpha = stripe_alpha,
    text_size = text_size,
    grid_lines = FALSE,
    plot_margin = ggplot2::margin(5.5, 0, 5.5, 5.5),
    text_hjust = 0,
    header_hjust = 0,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    dodge_width = if (is.null(state$defaults$dodge_width)) {
      0.6
    } else {
      state$defaults$dodge_width
    }
  )

  right_plot <- build_forest_table_plot(
    table_spec = right_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    facet_strip_position = state$facet_strip_position,
    table_position = "right",
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    stripe_alpha = stripe_alpha,
    text_size = text_size,
    grid_lines = FALSE,
    plot_margin = ggplot2::margin(5.5, 5.5, 5.5, 0),
    text_hjust = 1,
    header_hjust = 1,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    dodge_width = if (is.null(state$defaults$dodge_width)) {
      0.6
    } else {
      state$defaults$dodge_width
    }
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

  plot_out <- plot + rlang::exec(ggplot2::theme, !!!plot_theme_args)

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

#' Add split tables around a forest plot
#'
#' Compose split table blocks around a forest plot so that summary data appear
#' on both sides of the plotting panel.
#'
#' @param plot A plot created by [ggforestplot()]. Leave as `NULL` to use
#'   `+ add_split_table(...)` syntax.
#' @param left_columns Optional explicit columns to place on the left side of
#'   the forest plot. Accepts built-in names such as `"term"`, `"group"`, `"n"`,
#'   `"events"`, `"estimate"`, `"ci"`, and `"p"`, arbitrary original
#'   dataframe columns, or numeric positions in the supplied data. `"conf.low"`
#'   and `"conf.high"` are accepted as aliases for `"ci"`. The default grouped
#'   layout includes `"group"` on the left. Its header defaults to the source
#'   column supplied through `group =` for data frames and to `"Model"` for
#'   [bind_forest_models()] output.
#' @param right_columns Optional explicit columns to place on the right side
#'   of the forest plot. Accepts built-in names such as `"group"`, `"estimate"`,
#'   `"ci"`, and `"p"`, arbitrary original dataframe columns, or numeric positions in
#'   the supplied data. `"conf.low"` and `"conf.high"` are accepted as aliases
#'   for `"ci"`.
#' @param term_header Deprecated. Use the corresponding entry in
#'   `column_labels`, such as `column_labels = c(term = "Variable")`.
#' @param n_header Deprecated. Use
#'   `column_labels = c(n = "N")` instead.
#' @param events_header Deprecated. Use
#'   `column_labels = c(events = "Events")` instead.
#' @param estimate_label Header label for the estimate column. Defaults to the
#'   model-derived label when available.
#' @param p_header Deprecated. Use
#'   `column_labels = c(p = "P-value")` instead.
#' @param column_labels Optional named vector used to relabel table column
#'   headers. Names should match values supplied to `left_columns` or
#'   `right_columns` after column resolution, such as `"term"`, `"group"`, `"estimate"`,
#'   `"ci"`, `"p"`, or an arbitrary original dataframe column. For grouped
#'   data frames, either `"group"` or the source column supplied through
#'   `group =` can relabel the group column. Use this argument instead of the
#'   deprecated dedicated header arguments.
#' @param digits Deprecated. Number of digits used when formatting estimates
#'   and p-values. Defaults to `2`. Use `estimate_digits`, `interval_digits`,
#'   and `p_digits` for separate control.
#' @param estimate_digits Number of digits used for point estimates.
#' @param interval_digits Number of digits used for confidence interval bounds.
#' @param p_digits Number of digits used for p-values.
#' @param estimate_fmt Format string for the estimate column. Use
#'   `{estimate}`, `{conf.low}`, and `{conf.high}` as placeholders. The
#'   shorthand `{conf.low, conf.high}` is also supported. Defaults to
#'   `"{estimate} ({conf.low}, {conf.high})"`, or `"{estimate}"` when table
#'   columns include `"ci"`.
#' @param ci_fmt Format string for the confidence interval column when
#'   table columns include `"ci"`. Use `{conf.low}` and `{conf.high}` as
#'   placeholders. The shorthand `{conf.low, conf.high}` is also supported.
#'   Defaults to `"({conf.low}, {conf.high})"`.
#' @param formatters Named list of vectorized functions for scalar table
#'   values. Keys include `estimate`, `ci`, `conf.low`, `conf.high`, `p`
#'   (or `p.value`), `n`, `events`, `group`, and preserved source columns.
#'   Functions take precedence over digit settings; `estimate_fmt` and
#'   `ci_fmt` compose the resulting strings.
#'   Numeric source columns without a formatter are rounded to `estimate_digits`;
#'   whole numbers are shown without decimals.
#' @param reference_text Text shown in estimate cells for reference rows.
#' @param text_size Text size for table contents. Defaults to `3.2`.
#' @param header_text_size Header text size for table column labels. Defaults
#'   to `11`.
#' @param header_fontface Font face used for table column labels. Defaults to
#'   `"bold"`.
#' @param header_family Optional font family used for table column labels.
#' @param striped_rows Whether to draw alternating row stripes behind the
#'   split table layout. Defaults to the stripe setting used in
#'   [ggforestplot()].
#' @param stripe_fill Fill colour used for striped rows. Defaults to the
#'   stripe fill used in [ggforestplot()].
#' @param stripe_colour Outline colour for striped rows. Defaults to the
#'   stripe outline used in [ggforestplot()].
#' @param stripe_alpha Transparency for striped rows. Defaults to the stripe
#'   alpha used in [ggforestplot()].
#' @param left_width Optional width allocated to the left table block. By
#'   default this is derived from the number of displayed left-side columns
#'   relative to `plot_width`.
#' @param plot_width Optional width allocated to the forest plot panel.
#'   Defaults to `2.5`.
#' @param right_width Optional width allocated to the right table block. By
#'   default this is derived from the number of displayed right-side columns
#'   relative to `plot_width`.
#'
#' @param text_fontface,text_family,text_colour Body font face, family, and colour.
#'   Font faces accept "plain", "bold", "italic", "bold.italic", or codes 1:4.
#' @param header_colour Column-header text colour.
#' @param reference_text_size,reference_fontface,reference_family,reference_colour
#'   Text styling for all cells of reference rows, including their labels.
#'   Size is in millimeters. NULL inherits the corresponding body setting.
#' @param row_header_text_size,row_header_fontface,row_header_family,row_header_colour
#'   Text styling for section and generated subgroup header rows, distinct from
#'   column headers. Size is in millimeters. NULL inherits body styling.
#' @details Text settings inherit from [forest_theme()] when applied before the
#'   table. Explicit non-NULL table arguments take precedence. Without a forest
#'   theme, body text defaults to 3.2 mm, plain, default device font, and black;
#'   column headers default to 11 points and bold. Body and column-header size
#'   units differ for compatibility with earlier versions.
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
#'     left_columns = c(1, 5),
#'     right_columns = c(2, 6),
#'     estimate_label = "HR"
#'   )
add_split_table <- function(plot = NULL,
                            left_columns = NULL,
                            right_columns = NULL,
                            term_header = "Term",
                            n_header = "N",
                            events_header = "Events",
                            estimate_label = NULL,
                            p_header = "P-value",
                            column_labels = NULL,
                            digits = NULL,
                            estimate_digits = NULL,
                            interval_digits = NULL,
                            p_digits = NULL,
                            estimate_fmt = NULL,
                            ci_fmt = NULL,
                            formatters = NULL,
                            reference_text = "Reference",
                            text_size = NULL,
                            header_text_size = NULL,
                            header_fontface = NULL,
                            header_family = NULL,
                            striped_rows = NULL,
                            stripe_fill = NULL,
                            stripe_colour = NULL,
                            stripe_alpha = NULL,
                            left_width = NULL,
                            plot_width = NULL,
                            right_width = NULL,
                            text_fontface = NULL,
                            text_family = NULL,
                            text_colour = NULL,
                            header_colour = NULL,
                            reference_text_size = NULL,
                            reference_fontface = NULL,
                            reference_family = NULL,
                            reference_colour = NULL,
                            row_header_text_size = NULL,
                            row_header_fontface = NULL,
                            row_header_family = NULL,
                            row_header_colour = NULL) {
  text_style <- forest_text_settings(
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    text_fontface = text_fontface,
    text_family = text_family,
    text_colour = text_colour,
    header_colour = header_colour,
    reference_text_size = reference_text_size,
    reference_fontface = reference_fontface,
    reference_family = reference_family,
    reference_colour = reference_colour,
    row_header_text_size = row_header_text_size,
    row_header_fontface = row_header_fontface,
    row_header_family = row_header_family,
    row_header_colour = row_header_colour
  )
  warn_deprecated_table_headers(c(
    term_header = !missing(term_header),
    n_header = !missing(n_header),
    events_header = !missing(events_header),
    p_header = !missing(p_header)
  ))

  if (!missing(digits)) {
    warn_deprecated_argument("digits", "`estimate_digits`, `interval_digits`, and `p_digits`")
  }

  adder <- structure(
    list(
      left_columns = left_columns,
      right_columns = right_columns,
      term_header = term_header,
      n_header = n_header,
      events_header = events_header,
      estimate_label = estimate_label,
      p_header = p_header,
      column_labels = column_labels,
      digits = digits,
      estimate_digits = estimate_digits,
      interval_digits = interval_digits,
      p_digits = p_digits,
      estimate_fmt = estimate_fmt,
      ci_fmt = ci_fmt,
      formatters = formatters,
      reference_text = reference_text,
      text_style = text_style,
      striped_rows = striped_rows,
      stripe_fill = stripe_fill,
      stripe_colour = stripe_colour,
      stripe_alpha = stripe_alpha,
      left_width = left_width,
      plot_width = plot_width,
      right_width = right_width
    ),
    class = "ggforestplot_split_table_adder"
  )

  if (is.null(plot)) {
    return(adder)
  }
  ggplot_add.ggforestplot_split_table_adder(adder, plot)
}

#' @export
#' @keywords internal
ggplot_add.ggforestplot_split_table_adder <- function(object, plot, ...) {
  rlang::exec(.compose_split_table, plot = plot, !!!object)
}
