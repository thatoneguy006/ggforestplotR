forest_estimate_scales <- function() {
  c("identity", "log", "ratio", "probability", "risk_difference")
}

default_axis_transform <- function(estimate_scale) {
  if (identical(estimate_scale, "ratio")) "log10" else "identity"
}

default_reference_value <- function(estimate_scale) {
  switch(
    estimate_scale,
    identity = 0,
    log = 0,
    ratio = 1,
    probability = NULL,
    risk_difference = 0
  )
}

default_effect_label <- function(estimate_scale) {
  switch(
    estimate_scale,
    identity = "Estimate",
    log = "Log estimate",
    ratio = "Ratio",
    probability = "Probability",
    risk_difference = "RD"
  )
}

new_forest_metadata <- function(estimate_scale = "identity",
                                axis_transform = default_axis_transform(estimate_scale),
                                effect_label = default_effect_label(estimate_scale),
                                conf_level = NA_real_,
                                reference_value = default_reference_value(estimate_scale),
                                source_model = NULL,
                                source_package = NULL,
                                source_columns = NULL,
                                column_mapping = NULL,
                                grouping_levels = NULL) {
  list(
    version = 1L,
    estimate_scale = estimate_scale,
    axis_transform = axis_transform,
    effect_label = effect_label,
    conf_level = conf_level,
    reference_value = reference_value,
    source_model = source_model,
    source_package = source_package,
    source_columns = source_columns,
    column_mapping = column_mapping,
    grouping_levels = grouping_levels
  )
}

validate_forest_metadata <- function(data, metadata) {
  if (!is.list(metadata)) {
    stop("Forest metadata must be a named list.", call. = FALSE)
  }

  required <- c(
    "version", "estimate_scale", "axis_transform", "effect_label",
    "conf_level", "reference_value", "source_model", "source_package",
    "source_columns", "column_mapping", "grouping_levels"
  )
  missing <- setdiff(required, names(metadata))

  if (length(missing) > 0L) {
    stop(
      sprintf("Missing required forest metadata: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  scale <- metadata$estimate_scale
  if (!is.character(scale) || length(scale) != 1L || is.na(scale) ||
      !scale %in% forest_estimate_scales()) {
    stop(
      sprintf(
        "`estimate_scale` must be one of: %s.",
        paste(sprintf('"%s"', forest_estimate_scales()), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.character(metadata$axis_transform) ||
      length(metadata$axis_transform) != 1L ||
      is.na(metadata$axis_transform) ||
      !metadata$axis_transform %in% c("identity", "log10")) {
    stop("`axis_transform` must be either \"identity\" or \"log10\".", call. = FALSE)
  }

  if (!is.character(metadata$effect_label) ||
      length(metadata$effect_label) != 1L ||
      is.na(metadata$effect_label) ||
      !nzchar(metadata$effect_label)) {
    stop("`effect_label` must be a single non-empty string.", call. = FALSE)
  }

  conf_level <- metadata$conf_level
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      (!is.na(conf_level) && (conf_level <= 0 || conf_level >= 1))) {
    stop("`conf_level` must be `NA` or a number strictly between 0 and 1.", call. = FALSE)
  }

  reference_value <- metadata$reference_value
  if (!is.null(reference_value) &&
      (!is.numeric(reference_value) || length(reference_value) != 1L ||
       is.na(reference_value) || !is.finite(reference_value))) {
    stop("`reference_value` must be `NULL` or a single finite number.", call. = FALSE)
  }

  if (identical(metadata$axis_transform, "log10") &&
      !is.null(reference_value) && reference_value <= 0) {
    stop("`reference_value` must be positive when `axis_transform = \"log10\"`.", call. = FALSE)
  }

  if (!is.null(metadata$source_model) &&
      !is.character(metadata$source_model) && !is.list(metadata$source_model)) {
    stop("`source_model` must be `NULL`, a character vector, or a list.", call. = FALSE)
  }

  if (!is.null(metadata$source_package) &&
      !is.character(metadata$source_package) && !is.list(metadata$source_package)) {
    stop("`source_package` must be `NULL`, a character vector, or a list.", call. = FALSE)
  }

  source_columns <- metadata$source_columns
  valid_source_columns <- is.null(source_columns) ||
    (is.character(source_columns) && !is.null(names(source_columns)) &&
       all(!is.na(source_columns)) && all(nzchar(source_columns)) &&
       all(nzchar(names(source_columns))))
  if (!valid_source_columns) {
    stop(
      "`source_columns` must be `NULL` or a named character vector mapping source names to stored columns.",
      call. = FALSE
    )
  }
  if (!is.null(source_columns) && any(!unname(source_columns) %in% names(data))) {
    stop("Every `source_columns` value must identify a stored forest-data column.", call. = FALSE)
  }

  column_mapping <- metadata$column_mapping
  valid_column_mapping <- is.null(column_mapping) || is.list(column_mapping) ||
    (is.character(column_mapping) && !is.null(names(column_mapping)) &&
       all(nzchar(names(column_mapping))))
  if (!valid_column_mapping) {
    stop(
      "`column_mapping` must be `NULL`, a named character vector, or a list.",
      call. = FALSE
    )
  }

  validate_forest_data(
    data,
    exponentiate = identical(scale, "ratio") ||
      identical(metadata$axis_transform, "log10")
  )

  bounded_columns <- c("estimate", "conf.low", "conf.high")
  if (identical(scale, "probability") &&
      any(vapply(bounded_columns, function(column) {
        any(data[[column]] < 0 | data[[column]] > 1)
      }, logical(1)))) {
    stop("Probability estimates and intervals must be between 0 and 1.", call. = FALSE)
  }

  if (identical(scale, "risk_difference") &&
      any(vapply(bounded_columns, function(column) {
        any(data[[column]] < -1 | data[[column]] > 1)
      }, logical(1)))) {
    stop("Risk-difference estimates and intervals must be between -1 and 1.", call. = FALSE)
  }

  invisible(data)
}

forest_axis_label <- function(metadata) {
  if (is.na(metadata$conf_level)) {
    sprintf("%s (CI)", metadata$effect_label)
  } else {
    sprintf(
      "%s (%s CI)",
      metadata$effect_label,
      format_conf_level_label(metadata$conf_level)
    )
  }
}

set_forest_metadata <- function(data, metadata) {
  validate_forest_metadata(data, metadata)
  attr(data, "forest_meta") <- metadata

  # Transitional mirrors for code written against ggforestplotR <= 0.3.1.
  attr(data, "exponentiate") <- identical(metadata$estimate_scale, "ratio")
  attr(data, "estimate_label") <- metadata$effect_label
  attr(data, "axis_label") <- forest_axis_label(metadata)
  attr(data, "conf.level") <- metadata$conf_level
  attr(data, "column_mapping") <- metadata$column_mapping
  attr(data, "grouping_levels") <- metadata$grouping_levels
  data
}

new_forest_data <- function(data, metadata) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  class(data) <- unique(c("forest_data", class(data)))
  set_forest_metadata(data, metadata)
}

#' Inspect forest-data metadata
#'
#' Returns the semantic and provenance metadata attached to a `forest_data`
#' object. Plotting behavior is determined by this metadata rather than by the
#' class of the original fitted model.
#'
#' @param x A `forest_data` object.
#'
#' @return A named metadata list.
#' @export
forest_metadata <- function(x) {
  if (!inherits(x, "forest_data")) {
    stop("`x` must be a `forest_data` object.", call. = FALSE)
  }

  metadata <- attr(x, "forest_meta", exact = TRUE)
  if (is.null(metadata)) {
    stop("The `forest_data` object has no metadata contract.", call. = FALSE)
  }

  metadata
}

forest_source_columns <- function(x) {
  forest_metadata(x)$source_columns
}

forest_column_mapping <- function(x) {
  forest_metadata(x)$column_mapping
}

strip_forest_data_class <- function(x) {
  class(x) <- setdiff(class(x), "forest_data")
  for (attribute in c(
    "forest_meta", "exponentiate", "estimate_label", "axis_label",
    "conf.level", "source_columns", "column_mapping", "grouping_levels"
  )) {
    attr(x, attribute) <- NULL
  }
  x
}

#' @export
`[.forest_data` <- function(x, i, j, drop = FALSE) {
  metadata <- forest_metadata(x)
  out <- NextMethod("[")

  if (!is.data.frame(out)) {
    return(out)
  }

  required <- c("term", "estimate", "conf.low", "conf.high")
  if (!all(required %in% names(out))) {
    return(strip_forest_data_class(out))
  }

  set_forest_metadata(out, metadata)
}

#' @export
print.forest_data <- function(x, ...) {
  metadata <- forest_metadata(x)
  cat(sprintf(
    "<forest_data> %s; scale: %s; reference: %s\n",
    metadata$effect_label,
    metadata$estimate_scale,
    if (is.null(metadata$reference_value)) "none" else format(metadata$reference_value)
  ))
  display <- strip_forest_data_class(x)
  display <- display[, !startsWith(names(display), "..source.."), drop = FALSE]
  print(display, ...)
  invisible(x)
}
