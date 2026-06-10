.compose_forest_table <- function(plot,
                                  position = c("left", "right"),
                                  columns = NULL,
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
                                  text_size = NULL,
                                  header_text_size = NULL,
                                  header_fontface = "bold",
                                  header_family = NULL,
                                  striped_rows = NULL,
                                  stripe_fill = NULL,
                                  stripe_colour = NULL,
                                  stripe_alpha = NULL,
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

  state <- align_forest_state_to_plot_y_scale(state, plot)

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

  if (is.null(text_size)) {
    text_size <- 3.2
  }

  if (is.null(header_text_size)) {
    header_text_size <- 11
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

  if (is.null(stripe_alpha)) {
    stripe_alpha <- state$defaults$stripe_alpha
  }

  table_columns <- if (is.null(columns)) {
    default_forest_table_columns(state$forest_data)
  } else {
    normalize_table_columns(columns, data = state$forest_data)
  }

  if ("n" %in% table_columns && all(is.na(state$forest_data$n) | !nzchar(state$forest_data$n))) {
    stop("`columns = \"n\"` requires an `n` column in the underlying forest data.", call. = FALSE)
  }

  if ("events" %in% table_columns && all(is.na(state$forest_data$events) | !nzchar(state$forest_data$events))) {
    stop("`columns = \"events\"` requires an `events` column in the underlying forest data.", call. = FALSE)
  }

  if ("p" %in% table_columns && all(is.na(state$forest_data$p.value))) {
    stop("`columns = \"p\"` requires a `p.value` column in the underlying forest data.", call. = FALSE)
  }

  plot_out <- plot

  if ("term" %in% table_columns) {
    plot_out <- plot_out + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }

  table_spec <- build_forest_table_data(
    state$forest_data,
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
    columns = table_columns
  )
  table_spec <- layout_center_table_spec(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = if (is.null(header_family)) "" else header_family
  )
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
    stripe_alpha = stripe_alpha,
    text_size = text_size,
    grid_lines = grid_lines,
    grid_line_colour = grid_line_colour,
    grid_line_size = grid_line_size,
    grid_line_linetype = grid_line_linetype,
    x_expand = ggplot2::expansion(mult = 0.08),
    text_hjust = 0.5,
    header_hjust = 0.5,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )

  combine_forest_plot_and_table(
    plot = plot_out,
    table_plot = table_plot,
    table_position = position,
    table_width = table_width
  )
}

#' Add a summary table to a forest plot
#'
#' Compose a summary table onto a forest plot.
#'
#' @param plot A plot created by [ggforestplot()]. Leave as `NULL` to use
#'   `+ add_forest_table(...)` syntax.
#' @param position Whether to place the table on the left or right of the
#'   forest plot.
#' @param columns Optional explicit columns to display in the side table, in
#'   the order they should appear. Accepts built-in names such as `"term"`,
#'   `"n"`, `"events"`, `"estimate"`, `"ci"`, and `"p"`, arbitrary original
#'   dataframe columns, or numeric positions in the supplied data. `"conf.low"`
#'   and `"conf.high"` are accepted as aliases for `"ci"`.
#' @param term_header Header text for the term column.
#' @param n_header Header text for the `N` column.
#' @param events_header Header text for the `Events` column.
#' @param estimate_label Header label for the estimate column. Defaults to the
#'   model-derived label when available.
#' @param p_header Header text for the p-value column.
#' @param column_labels Optional named vector used to relabel table column
#'   headers. Names should match values supplied to `columns` after column
#'   resolution, such as `"term"`, `"estimate"`, `"ci"`, `"p"`, or an arbitrary
#'   original dataframe column.
#' @param digits Deprecated. Number of digits used when formatting estimates
#'   and p-values. Defaults to `2`. Use `estimate_digits`, `interval_digits`,
#'   and `p_digits` for separate control.
#' @param estimate_digits Number of digits used for point estimates.
#' @param interval_digits Number of digits used for confidence interval bounds.
#' @param p_digits Number of digits used for p-values.
#' @param estimate_fmt Format string for the estimate column. Use
#'   `{estimate}`, `{conf.low}`, and `{conf.high}` as placeholders. The
#'   shorthand `{conf.low, conf.high}` is also supported. Defaults to
#'   `"{estimate} ({conf.low}, {conf.high})"`, or `"{estimate}"` when
#'   `columns` includes `"ci"`.
#' @param ci_fmt Format string for the confidence interval column when
#'   `columns` includes `"ci"`. Use `{conf.low}` and `{conf.high}` as
#'   placeholders. The shorthand `{conf.low, conf.high}` is also supported.
#'   Defaults to `"({conf.low}, {conf.high})"`.
#' @param text_size Text size for table contents. Defaults to `3.2`.
#' @param header_text_size Header text size for table column labels. Defaults
#'   to `11`.
#' @param header_fontface Font face used for table column labels. Defaults to
#'   `"bold"`.
#' @param header_family Optional font family used for table column labels.
#' @param striped_rows Whether to draw alternating row stripes behind the
#'   table. Defaults to the stripe setting used in [ggforestplot()].
#' @param stripe_fill Fill colour used for striped rows. Defaults to the
#'   stripe fill used in [ggforestplot()].
#' @param stripe_colour Outline colour for striped rows. Defaults to the
#'   stripe outline used in [ggforestplot()].
#' @param stripe_alpha Transparency for striped rows. Defaults to the stripe
#'   alpha used in [ggforestplot()].
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
#'   columns = c("term", "n", "estimate", "p"),
#'   estimate_label = "Beta"
#' )
#'
#' ggforestplot(coefs, n = "sample_size", p.value = "p_value") +
#'   add_forest_table(
#'     position = "right",
#'     columns = c("term", "n", "estimate", "p"),
#'     estimate_label = "Beta"
#'   )
add_forest_table <- function(plot = NULL,
                             position = c("left", "right"),
                             columns = NULL,
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
                             text_size = NULL,
                             header_text_size = NULL,
                             header_fontface = "bold",
                             header_family = NULL,
                             striped_rows = NULL,
                             stripe_fill = NULL,
                             stripe_colour = NULL,
                             stripe_alpha = NULL,
                             grid_lines = FALSE,
                             grid_line_colour = "black",
                             grid_line_size = 0.3,
                             grid_line_linetype = 1) {
  if (!missing(digits)) {
    warn_deprecated_argument("digits", "`estimate_digits`, `interval_digits`, and `p_digits`")
  }

  position <- match.arg(position)

  if (is.null(plot)) {
    return(structure(
      list(
        position = position,
        columns = columns,
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
        text_size = text_size,
        header_text_size = header_text_size,
        header_fontface = header_fontface,
        header_family = header_family,
        striped_rows = striped_rows,
        stripe_fill = stripe_fill,
        stripe_colour = stripe_colour,
        stripe_alpha = stripe_alpha,
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
    columns = columns,
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
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    striped_rows = striped_rows,
    stripe_fill = stripe_fill,
    stripe_colour = stripe_colour,
    stripe_alpha = stripe_alpha,
    grid_lines = grid_lines,
    grid_line_colour = grid_line_colour,
    grid_line_size = grid_line_size,
    grid_line_linetype = grid_line_linetype
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
