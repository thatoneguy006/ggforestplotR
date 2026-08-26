.subgroup_fixed_terms <- function(model) {
  terms <- if (inherits(model, c("merMod", "lme"))) {
    stats::terms(model, fixed.only = TRUE)
  } else {
    stats::terms(model)
  }

  stats::delete.response(terms)
}

.subgroup_model_frame <- function(model) {
  frame <- tryCatch(
    stats::model.frame(model),
    error = function(error) NULL
  )

  if (is.data.frame(frame)) {
    frame
  } else {
    NULL
  }
}

.subgroup_term_components <- function(terms) {
  factors <- attr(terms, "factors", exact = TRUE)
  labels <- attr(terms, "term.labels", exact = TRUE)

  if (is.null(factors) || length(labels) == 0L) {
    return(rep(list(character()), length(labels)))
  }

  lapply(seq_along(labels), function(index) {
    rownames(factors)[factors[, index] > 0]
  })
}

.subgroup_component_name <- function(expression) {
  parsed <- tryCatch(
    str2lang(expression),
    error = function(error) NULL
  )

  if (is.symbol(parsed)) {
    return(as.character(parsed))
  }

  if (!is.call(parsed) || length(parsed) < 2L) {
    return(NA_character_)
  }

  function_name <- as.character(parsed[[1L]])
  argument <- parsed[[2L]]
  if (function_name %in% c("factor", "as.factor") &&
      is.symbol(argument)) {
    return(as.character(argument))
  }

  NA_character_
}

.subgroup_component_info <- function(expression,
                                     data_classes,
                                     model_frame) {
  name <- .subgroup_component_name(expression)
  data_class <- if (expression %in% names(data_classes)) {
    unname(data_classes[[expression]])
  } else {
    NULL
  }

  if (is.null(data_class) && !is.null(model_frame)) {
    frame_name <- if (expression %in% names(model_frame)) {
      expression
    } else if (!is.na(name) && name %in% names(model_frame)) {
      name
    } else {
      NULL
    }

    if (!is.null(frame_name)) {
      values <- model_frame[[frame_name]]
      data_class <- if (is.factor(values)) {
        "factor"
      } else if (is.numeric(values)) {
        "numeric"
      } else {
        class(values)[[1L]]
      }
    }
  }

  kind <- if (length(data_class) == 1L &&
      data_class %in% c("factor", "ordered")) {
    "factor"
  } else if (length(data_class) == 1L &&
      data_class %in% c("numeric", "integer")) {
    "continuous"
  } else {
    "unsupported"
  }

  list(
    expression = expression,
    name = name,
    emmeans_name = name,
    data_class = data_class,
    kind = kind,
    supported = !is.na(name) && kind != "unsupported"
  )
}

.subgroup_interaction_metadata <- function(model) {
  if (inherits(model, c("nlmerMod", "glmmTMB")) ||
      !inherits(model, c("lm", "glm", "coxph", "merMod", "lme"))) {
    stop(
      paste0(
        "Automatic subgroup effects currently support `lm`, `glm`, ",
        "`coxph`, `lmerMod`, `glmerMod`, and `lme` models."
      ),
      call. = FALSE
    )
  }

  terms <- .subgroup_fixed_terms(model)
  labels <- attr(terms, "term.labels", exact = TRUE)
  components <- .subgroup_term_components(terms)
  orders <- attr(terms, "order", exact = TRUE)
  if (is.null(orders)) {
    orders <- lengths(components)
  }

  data_classes <- attr(terms, "dataClasses", exact = TRUE)
  if (is.null(data_classes)) {
    data_classes <- character()
  }
  model_frame <- .subgroup_model_frame(model)

  records <- lapply(seq_along(labels), function(index) {
    component_info <- lapply(
      components[[index]],
      .subgroup_component_info,
      data_classes = data_classes,
      model_frame = model_frame
    )
    list(
      index = index,
      label = labels[[index]],
      order = orders[[index]],
      components = component_info
    )
  })

  list(
    terms = terms,
    labels = labels,
    records = records
  )
}

.subgroup_component_matches <- function(component, requested) {
  identical(component$name, requested) ||
    identical(component$expression, requested)
}

.subgroup_candidate <- function(record) {
  if (record$order != 2L || length(record$components) != 2L ||
      !all(vapply(record$components, `[[`, logical(1), "supported"))) {
    return(NULL)
  }

  record
}

.resolve_auto_subgroup_interaction <- function(metadata, focal) {
  interaction_records <- Filter(
    function(record) record$order > 1L,
    metadata$records
  )
  candidates <- Filter(
    Negate(is.null),
    lapply(interaction_records, .subgroup_candidate)
  )
  candidates <- Filter(function(record) {
    kinds <- vapply(record$components, `[[`, character(1), "kind")
    identical(sort(kinds), c("continuous", "factor"))
  }, candidates)

  if (!is.null(focal)) {
    candidates <- Filter(function(record) {
      continuous <- record$components[
        vapply(record$components, `[[`, character(1), "kind") ==
          "continuous"
      ][[1L]]
      .subgroup_component_matches(continuous, focal)
    }, candidates)
  }

  if (length(interaction_records) != 1L || length(candidates) != 1L) {
    stop(
      paste0(
        "`subgroup = \"auto\"` requires exactly one unambiguous ",
        "continuous-by-factor interaction. Supply explicit `focal` and ",
        "`subgroup` names for other interaction structures."
      ),
      call. = FALSE
    )
  }

  record <- candidates[[1L]]
  kinds <- vapply(record$components, `[[`, character(1), "kind")
  list(
    record = record,
    focal = record$components[[which(kinds == "continuous")]],
    subgroup = record$components[[which(kinds == "factor")]]
  )
}

.resolve_explicit_subgroup_interaction <- function(metadata,
                                                   focal,
                                                   subgroup) {
  candidates <- Filter(function(record) {
    candidate <- .subgroup_candidate(record)
    if (is.null(candidate)) {
      return(FALSE)
    }

    focal_matches <- vapply(
      candidate$components,
      .subgroup_component_matches,
      logical(1),
      requested = focal
    )
    subgroup_matches <- vapply(
      candidate$components,
      .subgroup_component_matches,
      logical(1),
      requested = subgroup
    )
    sum(focal_matches) == 1L && sum(subgroup_matches) == 1L &&
      which(focal_matches) != which(subgroup_matches)
  }, metadata$records)

  if (length(candidates) != 1L) {
    stop(
      paste0(
        "The fitted model must contain one direct two-way interaction ",
        "between `",
        focal,
        "` and `",
        subgroup,
        "`."
      ),
      call. = FALSE
    )
  }

  record <- candidates[[1L]]
  focal_component <- record$components[[which(vapply(
    record$components,
    .subgroup_component_matches,
    logical(1),
    requested = focal
  ))]]
  subgroup_component <- record$components[[which(vapply(
    record$components,
    .subgroup_component_matches,
    logical(1),
    requested = subgroup
  ))]]

  other_interactions <- Filter(function(other) {
    if (other$index == record$index || other$order <= 1L) {
      return(FALSE)
    }
    any(vapply(other$components, function(component) {
      .subgroup_component_matches(component, focal_component$name) ||
        .subgroup_component_matches(component, subgroup_component$name)
    }, logical(1)))
  }, metadata$records)
  if (length(other_interactions) > 0L) {
    stop(
      paste0(
        "The selected predictors participate in additional interaction ",
        "terms. Three-way and multiple-interaction estimands are not yet ",
        "supported."
      ),
      call. = FALSE
    )
  }

  list(
    record = record,
    focal = focal_component,
    subgroup = subgroup_component
  )
}

.resolve_subgroup_interaction <- function(model, subgroup, focal) {
  scalar_name <- function(value) {
    is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value)
  }

  if (!scalar_name(subgroup)) {
    stop(
      "`subgroup` must be `NULL`, \"auto\", or one predictor name.",
      call. = FALSE
    )
  }
  if (!is.null(focal) && !scalar_name(focal)) {
    stop("`focal` must be `NULL` or one predictor name.", call. = FALSE)
  }

  metadata <- .subgroup_interaction_metadata(model)
  resolved <- if (identical(subgroup, "auto")) {
    .resolve_auto_subgroup_interaction(metadata, focal)
  } else {
    if (is.null(focal)) {
      stop(
        "Explicit `subgroup` selection also requires `focal`.",
        call. = FALSE
      )
    }
    if (identical(focal, subgroup)) {
      stop("`focal` and `subgroup` must be different predictors.", call. = FALSE)
    }
    .resolve_explicit_subgroup_interaction(
      metadata,
      focal = focal,
      subgroup = subgroup
    )
  }

  if (resolved$subgroup$kind != "factor") {
    stop(
      paste0(
        "The selected `subgroup` must be a factor. Continuous-by-continuous ",
        "subgroup effects are not yet supported."
      ),
      call. = FALSE
    )
  }
  if (!resolved$focal$kind %in% c("continuous", "factor")) {
    stop(
      "The selected `focal` predictor must be continuous or a factor.",
      call. = FALSE
    )
  }

  main_indices <- vapply(
    c(resolved$focal$expression, resolved$subgroup$expression),
    function(expression) {
      matches <- vapply(metadata$records, function(record) {
        record$order == 1L && length(record$components) == 1L &&
          identical(record$components[[1L]]$expression, expression)
      }, logical(1))
      if (sum(matches) != 1L) {
        return(NA_integer_)
      }
      metadata$records[[which(matches)]]$index
    },
    integer(1)
  )
  if (anyNA(main_indices)) {
    stop(
      "The interaction model must include both corresponding main effects.",
      call. = FALSE
    )
  }

  resolved$metadata <- metadata
  resolved$term_indices <- c(main_indices, resolved$record$index)
  resolved$display_subgroup <- resolved$subgroup$name
  resolved
}

.subgroup_fixed_model_matrix <- function(model, terms) {
  matrix <- if (inherits(model, "lme")) {
    if (!requireNamespace("nlme", quietly = TRUE)) {
      stop(
        "The `nlme` package is required for subgroup effects from `lme` models.",
        call. = FALSE
      )
    }
    model_data <- nlme::getData(model)
    contrasts <- tryCatch(model$contrasts, error = function(error) NULL)
    stats::model.matrix(
      terms,
      data = model_data,
      contrasts.arg = contrasts
    )
  } else {
    tryCatch(
      stats::model.matrix(model),
      error = function(error) NULL
    )
  }

  if (is.null(matrix) || is.null(attr(matrix, "assign", exact = TRUE))) {
    stop(
      "Could not recover the fixed-effect model matrix for subgroup effects.",
      call. = FALSE
    )
  }
  if (length(attr(matrix, "col.dropped", exact = TRUE)) > 0L) {
    stop(
      "Subgroup effects require a full-rank fixed-effect model matrix.",
      call. = FALSE
    )
  }

  matrix
}

.subgroup_summary_columns <- function(summary) {
  estimate <- attr(summary, "estName", exact = TRUE)
  confidence <- attr(summary, "clNames", exact = TRUE)

  if (is.null(estimate) || !estimate %in% names(summary)) {
    candidates <- c(
      "estimate",
      grep("\\.trend$", names(summary), value = TRUE)
    )
    candidates <- candidates[candidates %in% names(summary)]
    estimate <- if (length(candidates) > 0L) candidates[[1L]] else NULL
  }
  if (is.null(confidence) || length(confidence) != 2L ||
      any(!confidence %in% names(summary))) {
    confidence <- if (all(c("lower.CL", "upper.CL") %in% names(summary))) {
      c("lower.CL", "upper.CL")
    } else {
      c("asymp.LCL", "asymp.UCL")
    }
  }
  if (is.null(estimate) || !estimate %in% names(summary) ||
      any(!confidence %in% names(summary))) {
    stop(
      "`emmeans` did not return recognizable estimate and interval columns.",
      call. = FALSE
    )
  }

  statistic <- intersect(c("t.ratio", "z.ratio"), names(summary))
  list(
    estimate = estimate,
    conf.low = confidence[[1L]],
    conf.high = confidence[[2L]],
    statistic = if (length(statistic) > 0L) statistic[[1L]] else NULL
  )
}

.summarize_subgroup_emmeans <- function(object, conf.level) {
  summary_data <- summary(
    object,
    infer = c(TRUE, TRUE),
    level = conf.level,
    type = "link",
    adjust = "none"
  )
  as.data.frame(summary_data)
}

.estimate_subgroup_effects <- function(model,
                                       subgroup,
                                       focal,
                                       conf.level,
                                       estimate_info) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop(
      paste0(
        "The `emmeans` package is required to estimate subgroup effects. ",
        "Install it or use `subgroup = NULL` for ordinary model coefficients."
      ),
      call. = FALSE
    )
  }

  interaction <- .resolve_subgroup_interaction(
    model,
    subgroup = subgroup,
    focal = focal
  )

  result <- tryCatch({
    if (interaction$focal$kind == "continuous") {
      emmeans::emtrends(
        model,
        specs = interaction$subgroup$emmeans_name,
        var = interaction$focal$emmeans_name
      )
    } else {
      means <- emmeans::emmeans(
        model,
        specs = interaction$focal$emmeans_name,
        by = interaction$subgroup$emmeans_name,
        type = "link"
      )
      mean_data <- as.data.frame(means)
      focal_levels <- unique(as.character(
        mean_data[[interaction$focal$emmeans_name]]
      ))
      if (length(focal_levels) < 2L) {
        stop(
          paste0(
            "Factor `focal` predictors require at least two observed levels."
          ),
          call. = FALSE
        )
      }
      emmeans::contrast(
        means,
        method = "trt.vs.ctrl",
        ref = 1L,
        by = interaction$subgroup$emmeans_name,
        adjust = "none"
      )
    }
  }, error = function(error) {
    stop(
      paste0(
        "Could not estimate subgroup effects with `emmeans`: ",
        conditionMessage(error)
      ),
      call. = FALSE
    )
  })

  summary <- .summarize_subgroup_emmeans(result, conf.level = conf.level)
  columns <- .subgroup_summary_columns(summary)
  subgroup_column <- interaction$subgroup$emmeans_name
  if (!subgroup_column %in% names(summary)) {
    stop(
      "`emmeans` did not return the requested subgroup predictor.",
      call. = FALSE
    )
  }

  estimate <- as.numeric(summary[[columns$estimate]])
  conf.low <- as.numeric(summary[[columns$conf.low]])
  conf.high <- as.numeric(summary[[columns$conf.high]])
  if (anyNA(estimate) || anyNA(conf.low) || anyNA(conf.high)) {
    stop(
      paste0(
        "One or more subgroup effects are not estimable from the fitted ",
        "model. Check sparse cells and fixed-effect rank."
      ),
      call. = FALSE
    )
  }

  if (isTRUE(estimate_info$exponentiate)) {
    estimate <- exp(estimate)
    conf.low <- exp(conf.low)
    conf.high <- exp(conf.high)
  }

  terms <- as.character(summary[[subgroup_column]])
  contrast <- if ("contrast" %in% names(summary)) {
    as.character(summary$contrast)
  } else {
    rep(paste0(interaction$focal$name, " slope"), nrow(summary))
  }
  statistic <- if (is.null(columns$statistic)) {
    rep(NA_real_, nrow(summary))
  } else {
    as.numeric(summary[[columns$statistic]])
  }

  rows <- data.frame(
    term = terms,
    label = terms,
    estimate = estimate,
    std.error = as.numeric(summary$SE),
    statistic = statistic,
    df = if ("df" %in% names(summary)) as.numeric(summary$df) else NA_real_,
    p.value = rep(NA_real_, nrow(summary)),
    effect.p.value = as.numeric(summary$p.value),
    conf.low = conf.low,
    conf.high = conf.high,
    subgroup = rep(interaction$display_subgroup, nrow(summary)),
    model_term = rep(interaction$record$label, nrow(summary)),
    contrast = contrast,
    stringsAsFactors = FALSE
  )
  if (interaction$focal$kind == "factor" &&
      length(unique(contrast)) > 1L) {
    rows$group <- contrast
  }

  matrix <- .subgroup_fixed_model_matrix(
    model,
    terms = interaction$metadata$terms
  )
  assignment <- attr(matrix, "assign", exact = TRUE)
  remove_terms <- colnames(matrix)[assignment %in% interaction$term_indices]
  if (length(remove_terms) == 0L) {
    stop(
      "Could not map the selected interaction to fixed-effect coefficients.",
      call. = FALSE
    )
  }

  list(
    rows = rows,
    remove_terms = remove_terms
  )
}

.bind_subgroup_frames <- function(left, right) {
  columns <- union(names(left), names(right))
  for (column in setdiff(columns, names(left))) {
    left[[column]] <- rep(NA, nrow(left))
  }
  for (column in setdiff(columns, names(right))) {
    right[[column]] <- rep(NA, nrow(right))
  }

  rbind(
    left[, columns, drop = FALSE],
    right[, columns, drop = FALSE]
  )
}

.splice_subgroup_effects <- function(model_rows, effects) {
  model_rows <- as.data.frame(model_rows)
  missing_terms <- setdiff(effects$remove_terms, model_rows$term)
  if (length(missing_terms) > 0L) {
    stop(
      paste0(
        "Could not align model coefficient row(s): ",
        paste(missing_terms, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  removed <- model_rows$term %in% effects$remove_terms
  insertion <- which(removed)[[1L]]
  model_rows$label <- model_rows$term
  model_rows$subgroup <- NA_character_
  model_rows$effect.p.value <- NA_real_

  subgroup_rows <- effects$rows
  if ("effect" %in% names(model_rows)) {
    subgroup_rows$effect <- "fixed"
  }

  before <- model_rows[seq_len(insertion - 1L), , drop = FALSE]
  before <- before[!before$term %in% effects$remove_terms, , drop = FALSE]
  after_indices <- if (insertion < nrow(model_rows)) {
    seq.int(insertion + 1L, nrow(model_rows))
  } else {
    integer()
  }
  after <- model_rows[after_indices, , drop = FALSE]
  after <- after[!after$term %in% effects$remove_terms, , drop = FALSE]

  out <- .bind_subgroup_frames(before, subgroup_rows)
  out <- .bind_subgroup_frames(out, after)
  rownames(out) <- NULL
  out
}
