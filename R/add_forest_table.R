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

#' @export
#' @keywords internal
ggplot_add.ggforestplot_table_adder <- function(object, plot, ...) {
  do.call(
    .compose_forest_table,
    c(list(plot = plot), object)
  )
}
