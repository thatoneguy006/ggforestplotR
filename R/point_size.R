resolve_point_size_column <- function(data, point_size) {
  if (is.numeric(point_size) && !is.complex(point_size) &&
      length(point_size) == 1L && is.finite(point_size) && point_size >= 0) {
    return(NULL)
  }

  if (!is.character(point_size) || length(point_size) != 1L ||
      is.na(point_size) || !nzchar(point_size)) {
    stop(
      "`point_size` must be a finite non-negative number or a single column name.",
      call. = FALSE
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
    stop(
      sprintf(
        paste0(
          "Column `%s` mapped to `point_size` must contain finite ",
          "non-negative numeric values for estimate and summary rows."
        ),
        point_size
      ),
      call. = FALSE
    )
  }

  column
}
