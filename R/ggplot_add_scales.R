#' @export
#' @keywords internal
ggplot_add.ScaleDiscretePosition <- function(object, plot, ...) {
  if (is_ggforestplot_faceted_y_scale(object, plot)) {
    has_character_limits <- is.character(object$limits)
    requested_row_levels <- if (has_character_limits) {
      resolve_forest_requested_row_levels(
        object$limits,
        plot$ggforestplotR_state
      )
    } else if (is.function(object$limits)) {
      resolve_forest_function_row_levels(
        object$limits,
        plot$ggforestplotR_state
      )
    } else {
      forest_full_row_levels(plot$ggforestplotR_state)
    }
    if (length(requested_row_levels) == 0L) {
      stop(
        "The discrete y-scale limits did not match any forest-plot rows.",
        call. = FALSE
      )
    }
    object <- align_discrete_y_scale_to_forest_facets(object, plot)
    if (!is.null(requested_row_levels)) {
      state <- align_forest_state_to_row_levels(
        plot$ggforestplotR_state,
        requested_row_levels
      )
      plot <- align_forest_row_layers_to_state(plot, state)
    }
  }

  NextMethod()
}

forest_full_display_data <- function(state) {
  if (!is.null(state$full_display_data)) {
    state$full_display_data
  } else if (!is.null(state$display_data)) {
    state$display_data
  } else {
    state$forest_data
  }
}

forest_full_row_levels <- function(state) {
  levels(forest_full_display_data(state)$row_key)
}

resolve_forest_function_row_levels <- function(limit_function, state) {
  forest_data <- forest_full_display_data(state)
  panel_values <- observed_grouping_panels(forest_data, state$has_groupings)
  row_levels <- levels(forest_data$row_key)
  resolved <- unlist(lapply(panel_values, function(panel) {
    panel_rows <- if (isTRUE(state$has_groupings)) {
      forest_data$grouping_panel == panel
    } else {
      rep(TRUE, nrow(forest_data))
    }
    panel_levels <- row_levels[
      row_levels %in% as.character(forest_data$row_key[panel_rows])
    ]
    as.character(limit_function(panel_levels))
  }), use.names = FALSE)
  unique(resolved[resolved %in% row_levels])
}

resolve_forest_requested_row_levels <- function(requested_limits, state) {
  forest_data <- forest_full_display_data(state)
  display_label <- if ("display_label" %in% names(forest_data)) {
    as.character(forest_data$display_label)
  } else {
    as.character(forest_data$label)
  }
  row_lookup <- unique(data.frame(
    row_key = as.character(forest_data$row_key),
    term = as.character(forest_data$term),
    label = as.character(forest_data$label),
    display_label = display_label,
    grouping_panel = as.character(forest_data$grouping_panel),
    stringsAsFactors = FALSE
  ))
  panel_values <- observed_grouping_panels(forest_data, state$has_groupings)
  matched <- unlist(lapply(panel_values, function(panel) {
    panel_lookup <- if (isTRUE(state$has_groupings)) {
      row_lookup[row_lookup$grouping_panel == panel, , drop = FALSE]
    } else {
      row_lookup
    }
    match_forest_requested_limits(requested_limits, panel_lookup)
  }), use.names = FALSE)
  unique(matched)
}

match_forest_requested_limits <- function(requested_limits, row_lookup) {
  matched <- unlist(lapply(as.character(requested_limits), function(limit) {
    row_lookup$row_key[
      row_lookup$term == limit |
        row_lookup$label == limit |
        row_lookup$display_label == limit |
        row_lookup$row_key == limit
    ]
  }), use.names = FALSE)
  matched <- as.character(matched)
  unique(matched[!is.na(matched) & nzchar(matched)])
}

forest_visible_y_labels <- function(row_keys, row_lookup) {
  matched <- match(as.character(row_keys), row_lookup$row_key)
  visible <- row_lookup$display_label[matched]
  ifelse(is.na(visible), as.character(row_keys), visible)
}

wrap_forest_y_scale_labels <- function(labels,
                                       requested_limits,
                                       row_lookup) {
  if (inherits(labels, "waiver")) {
    return(function(x) forest_visible_y_labels(x, row_lookup))
  }

  if (is.null(labels)) {
    return(NULL)
  }

  if (is.function(labels)) {
    return(function(x) {
      labels(forest_visible_y_labels(x, row_lookup))
    })
  }

  if (!is.character(labels) && !is.expression(labels)) {
    return(labels)
  }

  label_values <- labels
  label_names <- names(label_values)

  if (!is.null(label_names) && any(nzchar(label_names))) {
    return(function(x) {
      keys <- as.character(x)
      matched <- match(keys, row_lookup$row_key)
      lookup <- row_lookup[matched, , drop = FALSE]
      out <- lapply(seq_along(keys), function(i) {
        candidates <- c(
          keys[[i]], lookup$term[[i]], lookup$label[[i]],
          lookup$display_label[[i]]
        )
        matched_label <- match(candidates, label_names, nomatch = 0L)
        matched_label <- matched_label[matched_label > 0L]
        if (length(matched_label) == 0L) {
          return(NULL)
        }
        label_values[[matched_label[[1L]]]]
      })
      missing <- lengths(out) == 0L
      out[missing] <- as.list(forest_visible_y_labels(keys[missing], row_lookup))
      if (is.expression(label_values)) {
        return(as.expression(out))
      }
      unlist(out, use.names = FALSE)
    })
  }

  if (length(label_values) == length(requested_limits)) {
    key_labels <- list()
    for (i in seq_along(requested_limits)) {
      keys <- match_forest_requested_limits(requested_limits[[i]], row_lookup)
      for (key in keys) {
        key_labels[[key]] <- label_values[[i]]
      }
    }
    return(function(x) {
      keys <- as.character(x)
      out <- lapply(seq_along(keys), function(i) {
        value <- key_labels[[keys[[i]]]]
        if (is.null(value)) {
          forest_visible_y_labels(keys[[i]], row_lookup)
        } else {
          value
        }
      })
      if (is.expression(label_values)) {
        return(as.expression(out))
      }
      unlist(out, use.names = FALSE)
    })
  }

  labels
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

  if (is.null(state)) {
    return(FALSE)
  }

  display_data <- forest_full_display_data(state)
  has_subgroup_headers <- !is.null(display_data) &&
    "row_type" %in% names(display_data) &&
    any(display_data$row_type == "subgroup_header")

  if (!isTRUE(state$has_groupings) && !isTRUE(has_subgroup_headers)) {
    return(FALSE)
  }

  if (is.null(scale$aesthetics) || !"y" %in% scale$aesthetics) {
    return(FALSE)
  }

  TRUE
}

align_discrete_y_scale_to_forest_facets <- function(scale, plot) {
  state <- plot$ggforestplotR_state
  forest_data <- if (is.null(state$display_data)) {
    state$forest_data
  } else if (!is.null(state$full_display_data)) {
    state$full_display_data
  } else {
    state$display_data
  }
  display_label <- if ("display_label" %in% names(forest_data)) {
    as.character(forest_data$display_label)
  } else {
    as.character(forest_data$label)
  }
  row_lookup <- unique(data.frame(
    row_key = as.character(forest_data$row_key),
    term = as.character(forest_data$term),
    label = as.character(forest_data$label),
    display_label = display_label,
    stringsAsFactors = FALSE
  ))
  has_character_limits <- is.character(scale$limits)
  requested_limits <- if (has_character_limits) {
    as.character(scale$limits)
  } else {
    NULL
  }

  if (has_character_limits) {
    scale$limits <- function(x) {
      panel_row_keys <- as.character(x)
      panel_lookup <- row_lookup[
        row_lookup$row_key %in% panel_row_keys,
        ,
        drop = FALSE
      ]

      match_forest_requested_limits(requested_limits, panel_lookup)
    }
  }

  scale$labels <- wrap_forest_y_scale_labels(
    scale$labels,
    requested_limits,
    row_lookup
  )

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
