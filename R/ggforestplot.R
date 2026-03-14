ggforestplot <- function(data,
                         term = "term",
                         estimate = "estimate",
                         conf.low = "conf.low",
                         conf.high = "conf.high",
                         label = term,
                         group = NULL,
                         p.value = NULL,
                         exponentiate = FALSE,
                         sort_terms = c("none", "descending", "ascending"),
                         point_size = 2.3,
                         line_size = 0.5,
                         dodge_width = 0.6,
                         zero_line = TRUE,
                         zero_line_linetype = 2,
                         zero_line_colour = "grey60",
                         xlab = NULL,
                         ylab = NULL,
                         title = NULL) {
  sort_terms <- match.arg(sort_terms)

  forest_data <- if (is.data.frame(data)) {
    as_forest_data(
      data = data,
      term = term,
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      label = label,
      group = group,
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

  has_groups <- any(!is.na(forest_data$group))
  mapping <- if (has_groups) {
    ggplot2::aes(
      x = .data$label,
      y = .data$estimate,
      ymin = .data$conf.low,
      ymax = .data$conf.high,
      colour = .data$group
    )
  } else {
    ggplot2::aes(
      x = .data$label,
      y = .data$estimate,
      ymin = .data$conf.low,
      ymax = .data$conf.high
    )
  }

  dodge <- ggplot2::position_dodge(width = dodge_width)

  p <- ggplot2::ggplot(forest_data, mapping) +
    ggplot2::geom_errorbar(
      width = 0,
      linewidth = line_size,
      position = dodge
    ) +
    ggplot2::geom_point(
      size = point_size,
      position = dodge
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
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

  if (is.null(xlab)) {
    xlab <- if (isTRUE(exponentiate)) "Estimate (log scale)" else "Estimate"
  }

  if (is.null(ylab)) {
    ylab <- NULL
  }

  p + ggplot2::labs(
    x = ylab,
    y = xlab,
    title = title,
    colour = if (has_groups) "Group" else NULL
  )
}
