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
