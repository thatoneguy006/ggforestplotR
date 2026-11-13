# --- Plot limits -------------------------------------------------------------

default_plot_background_limits <- function(forest_data,
                                           exponentiate = FALSE,
                                           include_zero = TRUE,
                                           ref_line = NULL) {
  xmin <- min(forest_data$conf.low, na.rm = TRUE)
  xmax <- max(forest_data$conf.high, na.rm = TRUE)

  if (isTRUE(include_zero)) {
    null_value <- if (is.null(ref_line)) {
      if (isTRUE(exponentiate)) 1 else 0
    } else {
      ref_line
    }
    xmin <- min(xmin, null_value)
    xmax <- max(xmax, null_value)
  }

  if (isTRUE(exponentiate)) {
    pad_mult <- 1.08
    c(xmin / pad_mult, xmax * pad_mult)
  } else {
    span <- xmax - xmin
    pad <- if (is.finite(span) && span > 0) span * 0.08 else max(abs(xmax), 1) * 0.08
    c(xmin - pad, xmax + pad)
  }
}

# --- Confidence interval truncation -----------------------------------------

validate_ci_limits <- function(ci_limits = NULL, exponentiate = FALSE) {
  if (is.null(ci_limits)) {
    return(NULL)
  }

  if (!is.numeric(ci_limits) || length(ci_limits) != 2L ||
    anyNA(ci_limits) || any(!is.finite(ci_limits))) {
    abort_forest(
      "`ci_limits` must be `NULL` or a numeric vector of length 2.",
      class = "ggforestplotR_invalid_scale"
    )
  }

  ci_limits <- sort(ci_limits)

  if (ci_limits[[1]] == ci_limits[[2]]) {
    abort_forest(
      "`ci_limits` must contain two distinct values.",
      class = "ggforestplotR_invalid_scale"
    )
  }

  if (isTRUE(exponentiate) && any(ci_limits <= 0)) {
    abort_forest(
      "`ci_limits` must be positive for exponentiated plots.",
      class = "ggforestplotR_invalid_scale"
    )
  }

  ci_limits
}

build_ci_plot_data <- function(data, ci_limits = NULL, exponentiate = FALSE) {
  data$ci_low <- data$conf.low
  data$ci_high <- data$conf.high
  data$ci_estimate <- data$estimate
  data$ci_truncated_left <- FALSE
  data$ci_truncated_right <- FALSE
  data$ci_arrow_left_start <- NA_real_
  data$ci_arrow_right_start <- NA_real_

  if (is.null(ci_limits)) {
    return(data)
  }

  lower <- ci_limits[[1]]
  upper <- ci_limits[[2]]
  data$ci_truncated_left <- data$conf.low < lower
  data$ci_truncated_right <- data$conf.high > upper
  data$ci_low <- pmax(data$conf.low, lower)
  data$ci_high <- pmin(data$conf.high, upper)
  data$ci_estimate <- pmin(pmax(data$estimate, lower), upper)

  if (isTRUE(exponentiate)) {
    arrow_ratio <- exp(log(upper / lower) * 0.025)
    data$ci_arrow_left_start[data$ci_truncated_left] <- lower * arrow_ratio
    data$ci_arrow_right_start[data$ci_truncated_right] <- upper / arrow_ratio
  } else {
    arrow_offset <- (upper - lower) * 0.025
    data$ci_arrow_left_start[data$ci_truncated_left] <- lower + arrow_offset
    data$ci_arrow_right_start[data$ci_truncated_right] <- upper - arrow_offset
  }

  data
}

# Point sizes

resolve_point_size_column <- function(data, point_size) {
  if (is.numeric(point_size) && !is.complex(point_size) &&
    length(point_size) == 1L && is.finite(point_size) && point_size >= 0) {
    return(NULL)
  }

  if (!is.character(point_size) || length(point_size) != 1L ||
    is.na(point_size) || !nzchar(point_size)) {
    abort_forest(
      "`point_size` must be a finite non-negative number or a single column name.",
      class = "ggforestplotR_invalid_plot"
    )
  }

  source_columns <- forest_source_columns(data)
  column <- if (point_size %in% names(source_columns)) {
    unname(source_columns[[point_size]])
  } else {
    resolve_column(data, point_size, "point_size")
  }
  values <- data[[column]]
  geometry <- data$row_type %in% forest_geometry_row_types()

  if (!is.numeric(values) || is.complex(values) ||
    any(!is.finite(values[geometry])) || any(values[geometry] < 0)) {
    abort_forest(
      sprintf(
        paste0(
          "Column `%s` mapped to `point_size` must contain finite ",
          "non-negative numeric values for estimate and summary rows."
        ),
        point_size
      ),
      class = "ggforestplotR_invalid_plot"
    )
  }

  column
}

# Position adjustments

# Keep every estimate in the dodge calculation, then select the geometry for
# this layer. Truncation must not change a group's slot within a row.
position_forest_dodge <- function(width, keep = NULL) {
  ggplot2::ggproto(
    NULL,
    ggplot2::PositionDodge,
    width = width,
    keep = keep,
    setup_params = function(self, data) {
      data$flipped_aes <- TRUE
      ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)
    },
    compute_layer = function(self, data, params, layout) {
      if (!is.null(self$keep)) {
        data$.forest_keep <- self$keep
      }
      data <- ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_layer(
        data, params, layout
      )
      if ("yend" %in% names(data)) {
        data$yend <- data$y
      }
      if (!is.null(self$keep)) {
        data <- data[data$.forest_keep, , drop = FALSE]
        data$.forest_keep <- NULL
      }
      data
    }
  )
}
