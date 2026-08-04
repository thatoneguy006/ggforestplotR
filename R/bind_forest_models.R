bind_model_frames <- function(parts) {
  all_names <- unique(unlist(lapply(parts, names), use.names = FALSE))

  aligned <- lapply(parts, function(part) {
    if (inherits(part, "forest_data")) {
      part <- strip_forest_data_class(part)
    }
    part <- as.data.frame(part, stringsAsFactors = FALSE)
    missing <- setdiff(all_names, names(part))

    for (column in missing) {
      part[[column]] <- NA
    }

    part[all_names]
  })

  out <- do.call(rbind, aligned)
  rownames(out) <- NULL
  out
}

resolve_model_labels <- function(models, model_labels = NULL) {
  n_models <- length(models)

  if (is.null(model_labels)) {
    model_labels <- names(models)

    if (is.null(model_labels)) {
      model_labels <- rep("", n_models)
    }

    missing_labels <- is.na(model_labels) | !nzchar(model_labels)
    model_labels[missing_labels] <- paste("Model", which(missing_labels))
  }

  if (!is.atomic(model_labels) || length(model_labels) != n_models) {
    stop("`model_labels` must have one label per model.", call. = FALSE)
  }

  model_labels <- as.character(model_labels)

  if (anyNA(model_labels) || any(!nzchar(model_labels))) {
    stop("`model_labels` cannot contain missing or empty values.", call. = FALSE)
  }

  if (anyDuplicated(model_labels)) {
    stop("`model_labels` must be unique.", call. = FALSE)
  }

  model_labels
}

resolve_model_exponentiate <- function(exponentiate = NULL, n_models) {
  if (is.null(exponentiate)) {
    return(rep(list(NULL), n_models))
  }

  if (!is.logical(exponentiate) || !length(exponentiate) %in% c(1L, n_models) ||
      anyNA(exponentiate)) {
    stop("`exponentiate` must be `NULL`, a single logical value, or one logical value per model.", call. = FALSE)
  }

  as.list(rep(exponentiate, length.out = n_models))
}

#' Bind multiple model summaries for a grouped forest plot
#'
#' Tidies multiple fitted models and stacks their fixed-effect coefficient
#' tables into a single forest-plot data frame. The resulting data can be
#' passed directly to [ggforestplot()], where model labels are used as the
#' grouping variable for dodged, color-coded estimates.
#'
#' @param models A non-empty list of fitted model objects supported by an
#'   [as_forest_data()] method.
#' @param model_labels Optional labels used to identify each model in the
#'   forest plot. Defaults to list names when present, otherwise `"Model 1"`,
#'   `"Model 2"`, and so on.
#' @param exponentiate `NULL`, a single logical value, or one logical value per
#'   model. `NULL` uses the canonical scale inferred by [as_forest_data()]
#'   for each model.
#' @param ... Additional arguments passed to [as_forest_data()], such as
#'   `conf.level`, `intercept`, `term_labels`, or `sort_terms`.
#'
#' @return A standardized forest-plot data frame with one row per model term
#'   and a `group` column containing the model labels.
#' @export
#'
#' @examples
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   fit1 <- lm(mpg ~ wt + hp, data = mtcars)
#'   fit2 <- lm(mpg ~ wt + qsec, data = mtcars)
#'
#'   bound <- bind_forest_models(
#'     list(Base = fit1, Adjusted = fit2)
#'   )
#'
#'   ggforestplot(bound)
#' }
bind_forest_models <- function(models,
                               model_labels = NULL,
                               exponentiate = NULL,
                               ...) {
  if (!is.list(models) || length(models) == 0L || !is.null(attr(models, "class"))) {
    stop("`models` must be a non-empty list of fitted model objects.", call. = FALSE)
  }

  model_labels <- resolve_model_labels(models, model_labels = model_labels)
  exponentiate <- resolve_model_exponentiate(exponentiate, length(models))

  parts <- vector("list", length(models))

  for (i in seq_along(models)) {
    part <- as_forest_data(
      models[[i]],
      exponentiate = exponentiate[[i]],
      ...
    )
    part$group <- model_labels[[i]]

    parts[[i]] <- part
  }

  metadata <- lapply(parts, forest_metadata)
  scales <- vapply(metadata, `[[`, character(1), "estimate_scale")

  if (length(unique(scales)) > 1L) {
    stop(
      "All bound models must use the same estimate scale. ",
      "Set `exponentiate` to a single logical value to force a common scale.",
      call. = FALSE
    )
  }

  axis_transforms <- vapply(metadata, `[[`, character(1), "axis_transform")
  if (length(unique(axis_transforms)) > 1L) {
    stop("All bound models must use the same axis transformation.", call. = FALSE)
  }

  reference_values <- lapply(metadata, `[[`, "reference_value")
  compatible_references <- vapply(
    reference_values[-1L],
    identical,
    logical(1),
    reference_values[[1L]]
  )
  if (length(compatible_references) > 0L && !all(compatible_references)) {
    stop("All bound models must use the same reference value.", call. = FALSE)
  }

  conf_levels <- vapply(metadata, `[[`, numeric(1), "conf_level")
  if (length(unique(conf_levels)) > 1L) {
    stop("All bound models must use the same confidence level.", call. = FALSE)
  }

  estimate_labels <- vapply(metadata, `[[`, character(1), "effect_label")

  out <- bind_model_frames(parts)
  effect_label <- if (length(unique(estimate_labels)) == 1L) {
    estimate_labels[[1]]
  } else if (identical(scales[[1]], "ratio")) {
    "Ratio"
  } else {
    "Estimate"
  }

  column_mappings <- lapply(metadata, `[[`, "column_mapping")
  same_mapping <- length(column_mappings) == 1L || all(vapply(
    column_mappings[-1L],
    identical,
    logical(1),
    column_mappings[[1L]]
  ))
  column_mapping <- if (same_mapping) column_mappings[[1L]] else {
    stats::setNames(column_mappings, model_labels)
  }

  source_mappings <- lapply(metadata, `[[`, "source_columns")
  source_storage <- unlist(source_mappings, use.names = TRUE)
  source_storage <- source_storage[!duplicated(names(source_storage))]
  source_storage[["group"]] <- "group"

  bound_metadata <- new_forest_metadata(
    estimate_scale = scales[[1L]],
    axis_transform = axis_transforms[[1L]],
    effect_label = effect_label,
    conf_level = conf_levels[[1L]],
    reference_value = reference_values[[1L]],
    source_model = stats::setNames(lapply(metadata, `[[`, "source_model"), model_labels),
    source_package = stats::setNames(lapply(metadata, `[[`, "source_package"), model_labels),
    source_columns = source_storage,
    column_mapping = column_mapping
  )

  out <- new_forest_data(out, bound_metadata)
  class(out) <- unique(c("ggforestplot_bound_models", class(out)))
  set_forest_metadata(out, bound_metadata)
}
