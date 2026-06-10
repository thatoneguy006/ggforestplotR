bind_model_frames <- function(parts) {
  all_names <- unique(unlist(lapply(parts, names), use.names = FALSE))

  aligned <- lapply(parts, function(part) {
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

bind_forest_source_columns <- function(part, label) {
  source_columns <- attr(part, "source_columns")

  if (is.null(source_columns)) {
    source_columns <- part
  }

  source_columns <- as.data.frame(source_columns, stringsAsFactors = FALSE)
  source_columns$group <- label
  source_columns
}

#' Bind multiple model summaries for a grouped forest plot
#'
#' Tidies multiple fitted models and stacks their fixed-effect coefficient
#' tables into a single forest-plot data frame. The resulting data can be
#' passed directly to [ggforestplot()], where model labels are used as the
#' grouping variable for dodged, color-coded estimates.
#'
#' @param models A non-empty list of fitted model objects supported by
#'   [tidy_forest_model()].
#' @param model_labels Optional labels used to identify each model in the
#'   forest plot. Defaults to list names when present, otherwise `"Model 1"`,
#'   `"Model 2"`, and so on.
#' @param exponentiate `NULL`, a single logical value, or one logical value per
#'   model. `NULL` uses the canonical scale inferred by [tidy_forest_model()]
#'   for each model.
#' @param ... Additional arguments passed to [tidy_forest_model()], such as
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
  source_parts <- vector("list", length(models))

  for (i in seq_along(models)) {
    part <- tidy_forest_model(
      models[[i]],
      exponentiate = exponentiate[[i]],
      ...
    )
    part$group <- model_labels[[i]]

    parts[[i]] <- part
    source_parts[[i]] <- bind_forest_source_columns(part, model_labels[[i]])
  }

  scales <- vapply(parts, function(part) isTRUE(attr(part, "exponentiate")), logical(1))

  if (length(unique(scales)) > 1L) {
    stop(
      "All bound models must use the same estimate scale. ",
      "Set `exponentiate` to a single logical value to force a common scale.",
      call. = FALSE
    )
  }

  estimate_labels <- vapply(parts, function(part) {
    label <- attr(part, "estimate_label")
    if (is.null(label)) "Estimate" else label
  }, character(1))
  axis_labels <- vapply(parts, function(part) {
    label <- attr(part, "axis_label")
    if (is.null(label)) "Estimate" else label
  }, character(1))

  out <- bind_model_frames(parts)
  source_columns <- bind_model_frames(source_parts)

  attr(out, "source_columns") <- source_columns
  attr(out, "exponentiate") <- scales[[1]]
  attr(out, "estimate_label") <- if (length(unique(estimate_labels)) == 1L) {
    estimate_labels[[1]]
  } else if (isTRUE(scales[[1]])) {
    "Ratio"
  } else {
    "Estimate"
  }
  attr(out, "axis_label") <- if (length(unique(axis_labels)) == 1L) {
    axis_labels[[1]]
  } else {
    attr(out, "estimate_label")
  }
  attr(out, "conf.level") <- attr(parts[[1]], "conf.level")

  class(out) <- c("ggforestplot_bound_models", class(out))
  out
}
