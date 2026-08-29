validate_favors_text <- function(value, argument) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(
      sprintf("`%s` must be a single character string.", argument),
      call. = FALSE
    )
  }

  value
}

validate_favors_number <- function(value,
                                   argument,
                                   allow_zero = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L &&
    !is.na(value) && is.finite(value)

  if (isTRUE(allow_zero)) {
    valid <- valid && value >= 0
  } else {
    valid <- valid && value > 0
  }

  if (!valid) {
    qualifier <- if (isTRUE(allow_zero)) "non-negative" else "positive"
    stop(
      sprintf("`%s` must be a single %s number.", argument, qualifier),
      call. = FALSE
    )
  }

  value
}

as_forest_composition_spec <- function(plot) {
  composition <- attr(plot, "ggforestplotR_composition", exact = TRUE)

  if (!is.null(composition)) {
    return(composition)
  }

  if (!inherits(plot, "ggplot") || is.null(plot$ggforestplotR_state)) {
    stop(
      paste0(
        "`plot` must be created by `ggforestplot()` or a supported ",
        "forest-table composition."
      ),
      call. = FALSE
    )
  }

  list(
    panels = list(plot),
    widths = 1,
    forest_index = 1L,
    forest_plot = plot
  )
}

resolve_favors_reference <- function(plot, reference = NULL) {
  if (!is.null(reference)) {
    return(reference)
  }

  state <- plot$ggforestplotR_state
  reference <- state$defaults$ref_line

  if (!is.null(reference)) {
    return(reference)
  }

  source_data <- if (is.null(state$full_forest_data)) {
    state$forest_data
  } else {
    state$full_forest_data
  }
  metadata <- forest_metadata(source_data)
  metadata$reference_value
}

transform_favors_reference <- function(panel, reference) {
  x_scale <- panel$x$scale

  if (is.null(x_scale)) {
    return(NA_real_)
  }

  if (is.function(x_scale$transform)) {
    return(as.numeric(x_scale$transform(reference)))
  }

  transformation <- if (is.function(x_scale$get_transformation)) {
    x_scale$get_transformation()
  } else if (!is.null(x_scale$transformation)) {
    x_scale$transformation
  } else {
    x_scale$trans
  }

  if (!is.null(transformation) && is.function(transformation$transform)) {
    return(as.numeric(transformation$transform(reference)))
  }

  as.numeric(reference)
}

trained_favors_reference_position <- function(plot, reference) {
  built <- tryCatch(
    ggplot2::ggplot_build(plot),
    error = function(error) {
      stop(
        paste0(
          "`add_favors()` could not train the forest plot: ",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  panels <- built$layout$panel_params

  if (length(panels) == 0L) {
    stop(
      "`add_favors()` could not find a trained forest-plot panel.",
      call. = FALSE
    )
  }

  panel_info <- lapply(panels, function(panel) {
    is_discrete <- !is.null(panel$x$scale_is_discrete) &&
      isTRUE(panel$x$scale_is_discrete)

    if (is_discrete) {
      stop(
        "`add_favors()` requires a continuous forest-plot x scale.",
        call. = FALSE
      )
    }

    x_range <- panel$x.range

    if (is.null(x_range) && !is.null(panel$x$continuous_range)) {
      x_range <- panel$x$continuous_range
    }

    transformed_reference <- transform_favors_reference(panel, reference)

    if (length(x_range) != 2L || anyNA(x_range) ||
        any(!is.finite(x_range)) || diff(x_range) <= 0 ||
        length(transformed_reference) != 1L ||
        is.na(transformed_reference) || !is.finite(transformed_reference)) {
      stop(
        paste0(
          "`add_favors()` could not resolve the reference on the trained ",
          "forest-plot x scale."
        ),
        call. = FALSE
      )
    }

    list(
      range = as.numeric(x_range),
      position = (transformed_reference - x_range[[1L]]) / diff(x_range)
    )
  })

  first_range <- panel_info[[1L]]$range
  compatible_ranges <- vapply(
    panel_info,
    function(info) {
      isTRUE(all.equal(
        info$range,
        first_range,
        tolerance = sqrt(.Machine$double.eps)
      ))
    },
    logical(1)
  )

  if (!all(compatible_ranges)) {
    stop(
      paste0(
        "`add_favors()` requires facets to share one trained x scale; ",
        "free or incompatible facet x scales cannot share one annotation."
      ),
      call. = FALSE
    )
  }

  positions <- vapply(panel_info, `[[`, numeric(1), "position")

  if (any(positions <= 0 | positions >= 1)) {
    stop(
      "The resolved `reference` must be inside the visible x range.",
      call. = FALSE
    )
  }

  if (!isTRUE(all.equal(
    positions,
    rep(positions[[1L]], length(positions)),
    tolerance = sqrt(.Machine$double.eps)
  ))) {
    stop(
      paste0(
        "`add_favors()` requires facets to share one reference position; ",
        "free or incompatible facet x scales cannot share one annotation."
      ),
      call. = FALSE
    )
  }

  positions[[1L]]
}

build_favors_footer <- function(left,
                                right,
                                reference_position,
                                gap,
                                text_size,
                                linewidth,
                                arrow_length,
                                arrow_type) {
  if (gap >= min(reference_position, 1 - reference_position)) {
    stop(
      paste0(
        "`gap` is too large for the resolved reference position. ",
        "Reduce `gap` so both arrow segments remain visible."
      ),
      call. = FALSE
    )
  }

  segment_data <- data.frame(
    x = c(reference_position - gap, reference_position + gap),
    xend = c(0, 1),
    y = c(0.72, 0.72),
    yend = c(0.72, 0.72)
  )
  text_data <- data.frame(
    x = c(
      (reference_position - gap) / 2,
      (1 + reference_position + gap) / 2
    ),
    y = c(0.2, 0.2),
    label = c(left, right)
  )
  arrow <- grid::arrow(
    length = grid::unit(arrow_length, "in"),
    type = arrow_type
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = segment_data,
      mapping = ggplot2::aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend
      ),
      inherit.aes = FALSE,
      linewidth = linewidth,
      arrow = arrow
    ) +
    ggplot2::geom_text(
      data = text_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$label
      ),
      inherit.aes = FALSE,
      size = text_size,
      lineheight = 0.95
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}

compose_favors_footer <- function(composition,
                                  footer,
                                  footer_height,
                                  favors_state) {
  column_count <- length(composition$panels)
  footer_panels <- lapply(seq_len(column_count), function(index) {
    if (index == composition$forest_index) {
      footer
    } else {
      patchwork::plot_spacer()
    }
  })
  panels <- c(composition$panels, footer_panels)
  legend_position <- resolved_plot_legend_position(composition$forest_plot)
  collect_outer_legend <- legend_position %in% c("top", "bottom")

  out <- patchwork::wrap_plots(
    panels,
    ncol = column_count,
    nrow = 2,
    byrow = TRUE,
    widths = composition$widths,
    heights = grid::unit(c(1, footer_height), c("null", "in")),
    guides = if (collect_outer_legend) "collect" else "keep"
  )

  if (collect_outer_legend) {
    out <- out & ggplot2::theme(legend.position = legend_position)
  }

  attr(out, "ggforestplotR_composition") <- composition
  attr(out, "ggforestplotR_favors") <- favors_state
  out
}

.compose_favors <- function(plot,
                            left,
                            right,
                            reference = NULL,
                            gap = 0.02,
                            footer_height = 0.4,
                            text_size = 3.2,
                            linewidth = 0.5,
                            arrow_length = 0.08,
                            arrow_type = c("closed", "open")) {
  left <- validate_favors_text(left, "left")
  right <- validate_favors_text(right, "right")
  gap <- validate_favors_number(gap, "gap", allow_zero = TRUE)
  footer_height <- validate_favors_number(footer_height, "footer_height")
  text_size <- validate_favors_number(text_size, "text_size")
  linewidth <- validate_favors_number(linewidth, "linewidth")
  arrow_length <- validate_favors_number(arrow_length, "arrow_length")
  arrow_type <- match.arg(arrow_type)

  if (!is.null(reference) &&
      (!is.numeric(reference) || length(reference) != 1L ||
       is.na(reference) || !is.finite(reference))) {
    stop("`reference` must be `NULL` or a single finite number.", call. = FALSE)
  }

  composition <- as_forest_composition_spec(plot)
  forest_plot <- composition$forest_plot
  reference <- resolve_favors_reference(forest_plot, reference)

  if (!is.numeric(reference) || length(reference) != 1L ||
      is.na(reference) || !is.finite(reference)) {
    stop(
      paste0(
        "`add_favors()` could not resolve a reference value. ",
        "Supply `reference` explicitly."
      ),
      call. = FALSE
    )
  }

  reference_position <- trained_favors_reference_position(
    forest_plot,
    reference
  )
  footer <- build_favors_footer(
    left = left,
    right = right,
    reference_position = reference_position,
    gap = gap,
    text_size = text_size,
    linewidth = linewidth,
    arrow_length = arrow_length,
    arrow_type = arrow_type
  )
  favors_state <- list(
    footer = footer,
    reference = reference,
    reference_position = reference_position,
    gap = gap,
    footer_height = footer_height,
    text_size = text_size,
    linewidth = linewidth,
    arrow_length = arrow_length,
    arrow_type = arrow_type,
    forest_index = composition$forest_index,
    widths = composition$widths
  )

  compose_favors_footer(
    composition = composition,
    footer = footer,
    footer_height = footer_height,
    favors_state = favors_state
  )
}

#' Add directional favors labels beneath a forest plot
#'
#' Compose a two-sided arrow annotation beneath the trained forest-plot x
#' panel. The annotation is a separate footer plot, so it does not alter the
#' forest plot's scales, limits, confidence intervals, or reference line.
#'
#' `add_favors()` is designed as the final composition step. It works with a
#' bare [ggforestplot()] result and with layouts returned by
#' [add_forest_table()] and [add_split_table()].
#'
#' @param plot A plot created by [ggforestplot()] or a supported forest-table
#'   composition. Leave as `NULL` to use `+ add_favors(...)` syntax.
#' @param left,right Single strings shown beneath the left and right arrows.
#' @param reference Optional numeric reference value. `NULL` uses the reference
#'   line resolved by [ggforestplot()], falling back to the null value stored in
#'   the forest-data metadata.
#' @param gap Gap on each side of the trained reference position, expressed as
#'   a fraction of the forest panel width.
#' @param footer_height Footer height in inches.
#' @param text_size Text size passed to [ggplot2::geom_text()].
#' @param linewidth Line width passed to [ggplot2::geom_segment()].
#' @param arrow_length Arrowhead length in inches.
#' @param arrow_type Whether arrowheads are `"closed"` or `"open"`.
#'
#' @return A patchwork-composed plot with a footer beneath only the forest-plot
#'   column, or a ggplot add-on object when `plot = NULL`.
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
#' ggforestplot(coefs) +
#'   add_favors(
#'     left = "Treatment A better",
#'     right = "Treatment B better"
#'   )
#'
#' add_favors(
#'   ggforestplot(coefs),
#'   left = "Treatment A better",
#'   right = "Treatment B better"
#' )
add_favors <- function(plot = NULL,
                       left,
                       right,
                       reference = NULL,
                       gap = 0.02,
                       footer_height = 0.4,
                       text_size = 3.2,
                       linewidth = 0.5,
                       arrow_length = 0.08,
                       arrow_type = c("closed", "open")) {
  if (missing(left)) {
    stop("`left` must be supplied.", call. = FALSE)
  }

  if (missing(right)) {
    stop("`right` must be supplied.", call. = FALSE)
  }

  arrow_type <- match.arg(arrow_type)

  if (is.null(plot)) {
    return(structure(
      list(
        left = left,
        right = right,
        reference = reference,
        gap = gap,
        footer_height = footer_height,
        text_size = text_size,
        linewidth = linewidth,
        arrow_length = arrow_length,
        arrow_type = arrow_type
      ),
      class = "ggforestplot_favors_adder"
    ))
  }

  .compose_favors(
    plot = plot,
    left = left,
    right = right,
    reference = reference,
    gap = gap,
    footer_height = footer_height,
    text_size = text_size,
    linewidth = linewidth,
    arrow_length = arrow_length,
    arrow_type = arrow_type
  )
}

#' @export
#' @keywords internal
ggplot_add.ggforestplot_favors_adder <- function(object, plot, ...) {
  do.call(
    .compose_favors,
    c(list(plot = plot), object)
  )
}
