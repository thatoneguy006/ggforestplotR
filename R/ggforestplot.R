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
#' @param term_labels Optional named vector used to relabel displayed terms.
#'   Names should match values in the term column and values are the labels to
#'   display.
#' @param group Optional column name used for color-grouping estimates.
#' @param grouping Optional column name used to split rows into grouped plot
#'   sections.
#' @param grouping_strip_position Positioning for grouped section strips.
#' @param separate_groups Optional column name used to identify labeled
#'   variable blocks that can be outlined with grid lines.
#' @param n Optional column name holding sample sizes or other N labels for
#'   table helpers.
#' @param events Optional column name holding event counts or event labels for
#'   table helpers.
#' @param p.value Optional column name holding p-values.
#' @param exponentiate Logical; if `TRUE`, transform the estimates and draw the
#'   axis on the log scale with the reference line at 1. For model objects,
#'   `NULL` uses the canonical scale when it can be inferred, such as hazard
#'   ratios for Cox models.
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
#' @param zero_line Logical; if `TRUE`, draw a null reference line. Superseded
#'   by `ref_line`.
#' @param zero_line_linetype Line type for the null reference line. Superseded
#'   by `ref_line_linetype`.
#' @param zero_line_colour Color for the null reference line. Superseded by
#'   `ref_line_colour`.
#' @param ref_line Logical; if `TRUE`, draw a reference line. Defaults to
#'   `zero_line` for backward compatibility.
#' @param ref_line_value Numeric x-value where the reference line is drawn.
#'   Defaults to `0` for additive effects and `1` for exponentiated effects.
#' @param ref_line_label Optional label drawn alongside the reference line.
#' @param ref_line_linetype Line type for the reference line.
#' @param ref_line_colour Color for the reference line.
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
#' ggforestplot(coefs, striped_rows = TRUE, point_shape = 17)
ggforestplot <- function(data,
                         term = "term",
                         estimate = "estimate",
                         conf.low = "conf.low",
                         conf.high = "conf.high",
                         label = term,
                         term_labels = NULL,
                         group = NULL,
                         grouping = NULL,
                         grouping_strip_position = c("left", "right"),
                         separate_groups = NULL,
                         n = NULL,
                         events = NULL,
                         p.value = NULL,
                         exponentiate = NULL,
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
                         zero_line_colour = "grey60",
                         ref_line = NULL,
                         ref_line_value = NULL,
                         ref_line_label = NULL,
                         ref_line_linetype = NULL,
                         ref_line_colour = NULL) {
  sort_terms <- match.arg(sort_terms)
  grouping_strip_position <- match.arg(grouping_strip_position)
  draw_ref_line <- if (is.null(ref_line)) isTRUE(zero_line) else isTRUE(ref_line)
  ref_line_linetype <- if (is.null(ref_line_linetype)) zero_line_linetype else ref_line_linetype
  ref_line_colour <- if (is.null(ref_line_colour)) zero_line_colour else ref_line_colour

  forest_data <- if (is.data.frame(data)) {
    as_forest_data(
      data = data,
      term = term,
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      label = label,
      term_labels = term_labels,
      group = group,
      grouping = grouping,
      separate_groups = separate_groups,
      n = n,
      events = events,
      p.value = p.value,
      exponentiate = isTRUE(exponentiate),
      sort_terms = sort_terms
    )
  } else {
    tidy_forest_model(
      model = data,
      exponentiate = exponentiate,
      term_labels = term_labels,
      sort_terms = sort_terms
    )
  }
  plot_exponentiate <- isTRUE(attr(forest_data, "exponentiate"))
  estimate_label <- attr(forest_data, "estimate_label")
  axis_label <- attr(forest_data, "axis_label")

  if (is.null(estimate_label)) {
    estimate_label <- "Estimate"
  }

  if (is.null(axis_label)) {
    axis_label <- if (isTRUE(plot_exponentiate)) "Estimate (log scale)" else "Estimate"
  }

  if (is.null(ref_line_value)) {
    ref_line_value <- if (isTRUE(plot_exponentiate)) 1 else 0
  }

  if (!is.numeric(ref_line_value) || length(ref_line_value) != 1L || is.na(ref_line_value)) {
    stop("`ref_line_value` must be a single numeric value.", call. = FALSE)
  }

  if (isTRUE(plot_exponentiate) && ref_line_value <= 0) {
    stop("`ref_line_value` must be positive for exponentiated plots.", call. = FALSE)
  }

  display_data <- build_forest_plot_data(forest_data)
  forest_data <- display_data$plot_data
  stripe_data <- display_data$stripe_data
  separator_data <- display_data$separator_data
  plot_stripe_data <- stripe_data
  plot_x_limits <- NULL

  if (isTRUE(plot_exponentiate)) {
    plot_x_limits <- default_plot_background_limits(
      forest_data,
      exponentiate = plot_exponentiate,
      include_zero = draw_ref_line,
      ref_line_value = ref_line_value
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

  if (isTRUE(draw_ref_line)) {
    p <- p + ggplot2::geom_vline(
      xintercept = ref_line_value,
      linetype = ref_line_linetype,
      colour = ref_line_colour
    )

    if (!is.null(ref_line_label)) {
      p <- p + ggplot2::annotate(
        "text",
        x = ref_line_value,
        y = Inf,
        label = ref_line_label,
        angle = 90,
        hjust = 1.1,
        vjust = -0.4,
        colour = ref_line_colour
      )
    }
  }

  if (isTRUE(plot_exponentiate)) {
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
    x = axis_label,
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
      exponentiate = plot_exponentiate,
      estimate_label = estimate_label,
      axis_label = axis_label,
      ref_line = draw_ref_line,
      ref_line_value = ref_line_value,
      ref_line_label = ref_line_label,
      zero_line = draw_ref_line
    )
  )

  p
}
