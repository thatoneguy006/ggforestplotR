#' @export
#' @keywords internal
ggplot_add.ScaleDiscretePosition <- function(object, plot, ...) {
  if (is_ggforestplot_faceted_y_scale(object, plot)) {
    object <- align_discrete_y_scale_to_forest_facets(object, plot)
  }

  NextMethod()
}

is_ggforestplot_faceted_y_scale <- function(scale, plot) {
  state <- plot$ggforestplotR_state

  if (is.null(state) || !isTRUE(state$has_groupings)) {
    return(FALSE)
  }

  if (is.null(scale$aesthetics) || !"y" %in% scale$aesthetics) {
    return(FALSE)
  }

  is.character(scale$limits) && length(scale$limits) > 0L
}

align_discrete_y_scale_to_forest_facets <- function(scale, plot) {
  forest_data <- plot$ggforestplotR_state$forest_data
  row_lookup <- unique(data.frame(
    row_key = as.character(forest_data$row_key),
    label = as.character(forest_data$label),
    stringsAsFactors = FALSE
  ))
  requested_limits <- as.character(scale$limits)

  scale$limits <- function(x) {
    panel_row_keys <- as.character(x)
    panel_lookup <- row_lookup[row_lookup$row_key %in% panel_row_keys, , drop = FALSE]

    matched <- unlist(lapply(requested_limits, function(limit) {
      panel_lookup$row_key[
        panel_lookup$label == limit | panel_lookup$row_key == limit
      ]
    }), use.names = FALSE)

    unique(matched)
  }

  if (inherits(scale$labels, "waiver")) {
    scale$labels <- function(x) {
      matched_labels <- row_lookup$label[match(as.character(x), row_lookup$row_key)]
      ifelse(is.na(matched_labels), as.character(x), matched_labels)
    }
  }

  scale
}
