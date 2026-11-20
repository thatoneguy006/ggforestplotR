#' Convert marginaleffects results to forest data
#'
#' Standardizes results from `marginaleffects` into `forest_data`. One method
#' covers both unit-level and averaged results because
#' [marginaleffects::avg_comparisons()], [marginaleffects::avg_predictions()],
#' and [marginaleffects::avg_slopes()] return the same classes as
#' [marginaleffects::comparisons()], [marginaleffects::predictions()], and
#' [marginaleffects::slopes()]. Results from [marginaleffects::hypotheses()]
#' are supported as well.
#'
#' @details
#' Estimates, confidence intervals, p-values, and the confidence level are
#' taken from the object exactly as `marginaleffects` computed them. Nothing
#' is re-estimated, so `conf.level` can only confirm the stored level. To
#' change the interval, recompute it with the `conf_level` argument in
#' `marginaleffects`.
#'
#' Row labels are built from the identifier columns that vary across rows:
#' `hypothesis`, `term`, `group`, `contrast`, any `by` variables, and, for
#' predictions, the variables supplied through `newdata = datagrid(...)`. A
#' single varying identifier is shown as is, for example `"F"` and `"M"`.
#' Several identifiers are combined, for example `"cyl: 6 - 4"` or
#' `"hp, am = 1"`. Columns mapped to `group`, `facet`, or `subgroup` are
#' removed from the label. Supply `label` to use one column directly.
#'
#' The effect scale is inferred from the comparison function, the prediction
#' type, any `transform`, and the model link:
#'
#' * Differences, slopes, and hypothesis tests are additive with a reference
#'   line at 0.
#' * `comparison = "ratio"` or `"ratioavg"`, and `"lnratio"` or `"lnor"`
#'   comparisons with `transform = exp`, are ratios on a log10 axis with a
#'   reference line at 1. Delta-method intervals for `"ratio"` comparisons
#'   are symmetric and can cross zero; `comparison = "lnratioavg"` with
#'   `transform = exp` gives intervals that are always positive.
#' * `"lnratio"` and `"lnor"` comparisons, and differences computed on a
#'   log or logit link with `type = "link"`, are on the log scale.
#'   `exponentiate = TRUE` converts them to ratios.
#' * Predictions have no null value, so no reference line is drawn.
#'   Response-scale predictions from binomial models are probabilities.
#'
#' Set `estimate_scale`, `effect_label`, or `reference_value` when the
#' detected scale does not describe a custom comparison or transformation.
#'
#' @param data A `comparisons`, `predictions`, `slopes`, or `hypotheses`
#'   object from `marginaleffects`.
#' @param ... Not used; must be empty.
#' @param label Optional column used as the displayed row label instead of
#'   the composed label.
#' @param group Optional column used to color and dodge estimates that share
#'   a row, such as a `by` variable.
#' @param facet Optional column used to split rows into plot sections.
#' @param subgroup Optional column used to group rows under hierarchical
#'   headers, such as `"term"` for several contrasts of one factor. Rows are
#'   gathered into contiguous blocks in order of first appearance.
#' @param term_labels Optional named vector used to relabel identifier values
#'   (for example `c(hp = "Horsepower")`) or composed row labels.
#' @param exponentiate `NULL` keeps the reported scale. `TRUE` converts
#'   log-scale estimates to ratios and `FALSE` converts ratios to the log
#'   scale.
#' @param estimate_scale Optional override for the detected effect scale. One
#'   of `"identity"`, `"log"`, `"ratio"`, `"probability"`, or
#'   `"risk_difference"`.
#' @param effect_label Optional override for the effect label used on the
#'   axis and in tables.
#' @param reference_value Optional override for the reference line value.
#'   Use `NULL` for no reference line.
#' @param conf.level `NULL` uses the confidence level stored in the object.
#'   A number must match the stored level.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#' @param p_method Subgroup p-value placement when `subgroup` is mapped.
#'   Defaults to `"level"` because `marginaleffects` reports one test per row.
#'
#' @return A `forest_data` object. The original identifier columns, such as
#'   `term`, `contrast`, and `by` variables, are retained for table helpers.
#' @seealso [as_forest_data()], [ggforestplot()]
#' @name as_forest_data_marginaleffects
#'
#' @examples
#' if (requireNamespace("marginaleffects", quietly = TRUE)) {
#'   fit <- glm(am ~ hp + wt + factor(cyl), data = mtcars, family = binomial())
#'
#'   # Average risk differences for each predictor
#'   cmp <- marginaleffects::avg_comparisons(fit)
#'   ggforestplot(cmp) +
#'     add_forest_table(columns = c("term", "estimate", "ci", "p"))
#'
#'   # Average risk ratios, with factor contrasts grouped under a header
#'   ratios <- marginaleffects::avg_comparisons(
#'     fit,
#'     comparison = "lnratioavg",
#'     transform = exp
#'   )
#'   as_forest_data(ratios, subgroup = "term")
#'
#'   # Treatment effects within subgroups
#'   by_vs <- marginaleffects::avg_comparisons(fit, variables = "wt", by = "vs")
#'   ggforestplot(by_vs, term_labels = c("0" = "V-shaped", "1" = "Straight"))
#' }
NULL

#' @rdname as_forest_data_marginaleffects
#' @export
as_forest_data.comparisons <- function(data,
                                       ...,
                                       label = NULL,
                                       group = NULL,
                                       facet = NULL,
                                       subgroup = NULL,
                                       term_labels = NULL,
                                       exponentiate = NULL,
                                       estimate_scale = NULL,
                                       effect_label = NULL,
                                       reference_value = NULL,
                                       conf.level = NULL,
                                       sort_terms = c("none", "descending", "ascending"),
                                       p_method = c("level", "overall")) {
  rlang::check_dots_empty()
  marginaleffects_as_forest_data(
    data,
    label = label,
    group = group,
    facet = facet,
    subgroup = subgroup,
    term_labels = term_labels,
    exponentiate = exponentiate,
    estimate_scale = estimate_scale,
    effect_label = effect_label,
    reference_value = reference_value,
    reference_missing = missing(reference_value),
    conf.level = conf.level,
    sort_terms = match.arg(sort_terms),
    p_method = match.arg(p_method)
  )
}

#' @rdname as_forest_data_marginaleffects
#' @export
as_forest_data.predictions <- as_forest_data.comparisons

#' @rdname as_forest_data_marginaleffects
#' @export
as_forest_data.slopes <- as_forest_data.comparisons

#' @rdname as_forest_data_marginaleffects
#' @export
as_forest_data.hypotheses <- as_forest_data.comparisons

marginaleffects_kind <- function(x) {
  kinds <- c("slopes", "comparisons", "predictions", "hypotheses")
  kinds[inherits(x, kinds, which = TRUE) > 0L][[1L]]
}

# marginaleffects >= 0.29 stores its internal information in one S4 object
# exposed through `components()`. Older releases used plain attributes, and
# "lean" objects drop both, so every lookup falls back to `NULL`.
marginaleffects_component <- function(x, name) {
  value <- if (requireNamespace("marginaleffects", quietly = TRUE) &&
    "components" %in% getNamespaceExports("marginaleffects")) {
    tryCatch(
      suppressMessages(marginaleffects::components(x, name)),
      error = function(error) NULL
    )
  } else {
    NULL
  }

  if (is.null(value)) {
    value <- attr(x, name, exact = TRUE)
  }

  value
}

marginaleffects_model_link <- function(model) {
  if (is.null(model)) {
    return(list(family = NA_character_, link = NA_character_))
  }

  family <- tryCatch(stats::family(model), error = function(error) NULL)
  if (is.null(family) || !is.list(family)) {
    return(list(family = NA_character_, link = NA_character_))
  }

  list(
    family = if (is.character(family$family)) family$family[[1L]] else NA_character_,
    link = if (is.character(family$link)) family$link[[1L]] else NA_character_
  )
}

marginaleffects_call_argument <- function(call, name) {
  if (is.null(call) || !(is.call(call) || is.list(call))) {
    return(NULL)
  }

  value <- tryCatch(call[[name]], error = function(error) NULL)
  if (is.null(value)) {
    return(NULL)
  }

  paste(deparse(value, width.cutoff = 500L), collapse = " ")
}

marginaleffects_scale <- function(x, table) {
  kind <- marginaleffects_kind(x)
  comparison <- marginaleffects_component(x, "comparison")
  type <- marginaleffects_component(x, "type")
  type <- if (is.character(type) && length(type) >= 1L) type[[1L]] else NA_character_
  call <- marginaleffects_component(x, "call")
  model <- marginaleffects_component(x, "model")
  hypothesis <- marginaleffects_component(x, "hypothesis")
  model_link <- marginaleffects_model_link(model)

  transform <- marginaleffects_call_argument(call, "transform")
  exp_transform <- !is.null(transform) &&
    gsub("[\"' ]", "", transform) %in% c("exp", "base::exp")
  other_transform <- !is.null(transform) && !exp_transform &&
    !identical(transform, "NULL")

  if (identical(kind, "hypotheses") || !is.null(hypothesis)) {
    hypothesis_text <- paste(
      deparse(hypothesis, width.cutoff = 500L),
      collapse = " "
    )
    if (grepl("ratio", hypothesis_text, fixed = TRUE)) {
      return(result_scale("ratio", "Ratio"))
    }
    return(result_scale("identity", "Estimate"))
  }

  if (other_transform) {
    return(result_scale("identity", "Estimate", reference_value = NULL))
  }

  link_scale <- !is.na(type) && type %in% c("link", "lp", "linear.predictor")
  log_link <- link_scale && (
    model_link$link %in% c("log", "logit") || inherits(model, "coxph")
  )
  log_label <- if (inherits(model, "coxph")) {
    "log(HR)"
  } else if (identical(model_link$link, "logit")) {
    "log(OR)"
  } else {
    "log(Ratio)"
  }

  if (identical(kind, "predictions")) {
    binomial <- model_link$family %in% c("binomial", "quasibinomial")
    estimates <- c(table$estimate, table$conf.low, table$conf.high)
    estimates <- estimates[is.finite(estimates)]
    if (!link_scale && isTRUE(binomial) && length(estimates) > 0L &&
      all(estimates >= 0 & estimates <= 1)) {
      return(result_scale("probability", "Probability", reference_value = NULL))
    }
    label <- if (link_scale) "Prediction (link scale)" else "Prediction"
    if (exp_transform) {
      label <- "Prediction"
    }
    return(result_scale("identity", label, reference_value = NULL))
  }

  comparison <- if (is.character(comparison) && length(comparison) >= 1L) {
    comparison[[1L]]
  } else {
    NA_character_
  }

  if (identical(kind, "slopes") || comparison %in% c("dydx", "eyex", "eydx", "dyex", "dydxavg", "eyexavg", "eydxavg", "dyexavg")) {
    label <- if (grepl("^eyex", comparison)) {
      "Elasticity"
    } else if (grepl("^(eydx|dyex)", comparison)) {
      "Semi-elasticity"
    } else {
      "Slope"
    }
    return(result_scale("identity", label))
  }

  if (is.na(comparison)) {
    return(result_scale("identity", "Estimate"))
  }

  if (grepl("^lnor", comparison)) {
    if (exp_transform) {
      return(result_scale("ratio", "OR"))
    }
    return(result_scale("log", "log(OR)"))
  }

  if (grepl("^lnratio", comparison)) {
    if (exp_transform) {
      return(result_scale("ratio", "Ratio"))
    }
    return(result_scale("log", "log(Ratio)"))
  }

  if (grepl("^ratio", comparison)) {
    return(result_scale("ratio", "Ratio"))
  }

  if (grepl("^lift", comparison)) {
    return(result_scale("identity", "Lift"))
  }

  if (grepl("^difference", comparison)) {
    if (log_link) {
      if (exp_transform) {
        return(result_scale("ratio", unlog_effect_label(log_label)))
      }
      return(result_scale("log", log_label))
    }
    binomial <- model_link$family %in% c("binomial", "quasibinomial")
    if (!link_scale && isTRUE(binomial)) {
      return(result_scale("identity", "Risk difference"))
    }
    return(result_scale("identity", "Difference"))
  }

  result_scale("identity", "Estimate")
}

marginaleffects_id_columns <- function(x, table) {
  kind <- marginaleffects_kind(x)
  by <- marginaleffects_component(x, "variable_names_by")
  datagrid <- if (identical(kind, "predictions")) {
    marginaleffects_component(x, "variable_names_datagrid")
  } else {
    NULL
  }
  by <- if (is.character(by)) by else character()
  datagrid <- if (is.character(datagrid)) datagrid else character()

  stat_columns <- c(
    "estimate", "std.error", "statistic", "p.value", "s.value",
    "conf.low", "conf.high", "df", "rowid", "rowidcf",
    "predicted", "predicted_lo", "predicted_hi"
  )
  ids <- c("hypothesis", "term", "group", "contrast", by, datagrid)
  setdiff(unique(intersect(ids, names(table))), stat_columns)
}

marginaleffects_as_forest_data <- function(x,
                                           label = NULL,
                                           group = NULL,
                                           facet = NULL,
                                           subgroup = NULL,
                                           term_labels = NULL,
                                           exponentiate = NULL,
                                           estimate_scale = NULL,
                                           effect_label = NULL,
                                           reference_value = NULL,
                                           reference_missing = TRUE,
                                           conf.level = NULL,
                                           sort_terms = "none",
                                           p_method = "level",
                                           call = rlang::caller_env()) {
  kind <- marginaleffects_kind(x)
  source <- sprintf("`%s`", kind)
  table <- as.data.frame(x, stringsAsFactors = FALSE)
  check_result_columns(
    table,
    c("estimate", "conf.low", "conf.high"),
    source,
    hint = "Request confidence intervals in `marginaleffects`, for example with `vcov = TRUE`.",
    call = call
  )

  # Log-scale comparisons label contrasts as "ln(...)". Once the estimates are
  # ratios, through `transform = exp` or `exponentiate = TRUE`, drop the "ln".
  comparison <- marginaleffects_component(x, "comparison")
  transform <- marginaleffects_call_argument(
    marginaleffects_component(x, "call"),
    "transform"
  )
  exp_transform <- !is.null(transform) &&
    gsub("[\"' ]", "", transform) %in% c("exp", "base::exp")
  if ("contrast" %in% names(table) && is.character(comparison) &&
    length(comparison) >= 1L && grepl("^ln", comparison[[1L]]) &&
    (exp_transform || isTRUE(exponentiate))) {
    table$contrast <- sub("^ln\\((.*)\\)$", "\\1", as.character(table$contrast))
  }

  stored_level <- marginaleffects_component(x, "conf_level")
  conf.level <- resolve_result_conf_level(
    conf.level,
    stored = stored_level,
    source = "`marginaleffects`",
    call = call
  )
  model <- marginaleffects_component(x, "model")

  result_table_as_forest_data(
    table,
    id_columns = marginaleffects_id_columns(x, table),
    bare_columns = c("hypothesis", "term", "group", "contrast"),
    scale = marginaleffects_scale(x, table),
    source = source,
    label = label,
    group = group,
    facet = facet,
    subgroup = subgroup,
    term_labels = term_labels,
    exponentiate = exponentiate,
    estimate_scale = estimate_scale,
    effect_label = effect_label,
    reference_value = reference_value,
    reference_missing = reference_missing,
    conf.level = conf.level,
    sort_terms = sort_terms,
    p_method = p_method,
    row_id = "rowid",
    source_model = c(kind, if (!is.null(model)) class(model)),
    source_package = "marginaleffects",
    ratio_hint = paste0(
      "Delta-method intervals for `comparison = \"ratio\"` are symmetric on ",
      "the ratio scale. Use `comparison = \"lnratioavg\"` with ",
      "`transform = exp` for intervals computed on the log scale, or set ",
      "`estimate_scale = \"identity\"`."
    ),
    call = call
  )
}
