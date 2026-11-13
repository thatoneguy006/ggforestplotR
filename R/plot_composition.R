# --- Final assembly ----------------------------------------------------------

resolved_plot_legend_position <- function(plot) {
  position <- plot$theme$legend.position

  if (is.null(position)) {
    position <- ggplot2::theme_get()$legend.position
  }

  if (!is.character(position) || length(position) != 1L || is.na(position)) {
    return(NULL)
  }

  position
}

wrap_forest_composition <- function(panels, widths, plot) {
  legend_position <- resolved_plot_legend_position(plot)
  collect_outer_legend <- legend_position %in% c("top", "bottom")

  out <- patchwork::wrap_plots(
    panels,
    nrow = 1,
    widths = widths,
    guides = if (collect_outer_legend) "collect" else "keep"
  )

  if (collect_outer_legend) {
    out <- out & ggplot2::theme(legend.position = legend_position)
  }

  forest_index <- which(vapply(
    panels,
    function(panel) {
      inherits(panel, "ggplot") &&
        !is.null(panel$ggforestplotR_state)
    },
    logical(1)
  ))

  if (length(forest_index) != 1L) {
    abort_forest(
      "A forest composition must contain exactly one forest-plot panel.",
      class = "ggforestplotR_invalid_composition"
    )
  }

  attr(out, "ggforestplotR_composition") <- list(
    panels = panels,
    widths = widths,
    forest_index = forest_index,
    forest_plot = plot
  )

  out
}

#' Combine a forest plot with one table on either side.
#'
#' Each table panel receives its own measured content width; the forest plot
#' receives `plot_width`.
#'
#' @param plot The forest plot (ggplot2 object).
#' @param left_table Left-side table plot, or NULL if none.
#' @param right_table Right-side table plot, or NULL if none.
#' @param left_spec Left table_spec (needs `content_width`), or NULL.
#' @param right_spec Right table_spec (needs `content_width`), or NULL.
#' @param plot_width Width ratio for the forest plot panel.
#' @keywords internal
#' @noRd
combine_split_forest_plot <- function(plot,
                                      left_table = NULL,
                                      right_table = NULL,
                                      left_spec = NULL,
                                      right_spec = NULL,
                                      plot_width = 2.5) {
  left_w <- if (!is.null(left_spec)) left_spec$content_width else 0
  right_w <- if (!is.null(right_spec)) right_spec$content_width else 0

  panels <- list()
  widths <- numeric()

  if (!is.null(left_table)) {
    panels <- c(panels, list(left_table))
    widths <- c(widths, left_w)
  }

  panels <- c(panels, list(plot))
  widths <- c(widths, plot_width)

  if (!is.null(right_table)) {
    panels <- c(panels, list(right_table))
    widths <- c(widths, right_w)
  }

  wrap_forest_composition(panels, widths = widths, plot = plot)
}

#' Convenience wrapper for single-table layouts (table on one side only).
#' @keywords internal
#' @noRd
combine_forest_plot_and_table <- function(plot, table_plot,
                                          table_position = c("left", "right"),
                                          table_width = 2.2,
                                          plot_width = 2.4) {
  table_position <- match.arg(table_position)
  widths <- c(table_width, plot_width)

  if (table_position == "left") {
    panels <- list(table_plot, plot)
  } else {
    panels <- list(plot, table_plot)
    widths <- rev(widths)
  }

  wrap_forest_composition(panels, widths = widths, plot = plot)
}
