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
#'   default this is derived from the number of displayed left-side columns
#'   relative to `plot_width`.
#' @param plot_width Optional width allocated to the forest plot panel.
#'   Defaults to `2.5`.
#' @param right_width Optional width allocated to the right table block. By
#'   default this is derived from the number of displayed right-side columns
#'   relative to `plot_width`.
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
ggplot_add.ggforestplot_split_table_adder <- function(object, plot, ...) {
  do.call(
    .compose_split_table,
    c(list(plot = plot), object)
  )
}
