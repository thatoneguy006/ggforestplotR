#' @export
#' @keywords internal
ggplot_add.ScaleDiscretePosition <- function(object, plot, ...) {
  if (is_ggforestplot_faceted_y_scale(object, plot)) {
    object <- align_discrete_y_scale_to_forest_facets(object, plot)
  }

  NextMethod()
}

#' @export
#' @keywords internal
ggplot_add.ScaleContinuousPosition <- function(object, plot, ...) {
  if (is_ggforestplot_exponentiated_x_scale(object, plot)) {
    object <- zero_default_x_scale_expansion(object)
    plot <- align_striped_rows_to_x_scale(object, plot)
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

is_ggforestplot_exponentiated_x_scale <- function(scale, plot) {
  state <- plot$ggforestplotR_state

  if (is.null(state) || !isTRUE(state$defaults$exponentiate)) {
    return(FALSE)
  }

  if (is.null(scale$aesthetics) || !"x" %in% scale$aesthetics) {
    return(FALSE)
  }

  TRUE
}

zero_default_x_scale_expansion <- function(scale) {
  if (inherits(scale$expand, "waiver")) {
    scale$expand <- ggplot2::expansion(mult = 0)
  }

  scale
}

align_striped_rows_to_x_scale <- function(scale, plot) {
  state <- plot$ggforestplotR_state

  if (!isTRUE(state$defaults$striped_rows) || is.null(state$stripe_layer_index)) {
    return(plot)
  }

  stripe_layer_index <- state$stripe_layer_index

  if (stripe_layer_index > length(plot$layers)) {
    return(plot)
  }

  stripe_data <- plot$layers[[stripe_layer_index]]$data
  stripe_limits <- x_scale_limits_for_stripes(scale, stripe_data)

  if (is.null(stripe_limits)) {
    return(plot)
  }

  plot$layers[[stripe_layer_index]]$data$xmin <- stripe_limits[1]
  plot$layers[[stripe_layer_index]]$data$xmax <- stripe_limits[2]

  plot
}

x_scale_limits_for_stripes <- function(scale, stripe_data) {
  scale_limits <- scale$limits

  if (is.null(scale_limits) || is.function(scale_limits) || inherits(scale_limits, "waiver")) {
    return(NULL)
  }

  if (!is.numeric(scale_limits) || length(scale_limits) != 2L) {
    return(NULL)
  }

  stripe_limits <- inverse_continuous_scale_limits(scale, scale_limits)

  current_limits <- c(
    min(stripe_data$xmin, na.rm = TRUE),
    max(stripe_data$xmax, na.rm = TRUE)
  )
  stripe_limits[is.na(stripe_limits)] <- current_limits[is.na(stripe_limits)]

  if (any(!is.finite(stripe_limits))) {
    return(NULL)
  }

  stripe_limits
}

inverse_continuous_scale_limits <- function(scale, limits) {
  transformation <- scale$trans

  if (is.null(transformation)) {
    transformation <- scale$transformation
  }

  if (!is.null(transformation) && is.function(transformation$inverse)) {
    return(transformation$inverse(limits))
  }

  limits
}
