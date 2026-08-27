is_mixed_model <- function(model) {
  inherits(
    model,
    c("merMod", "lmerMod", "glmerMod", "nlmerMod", "lme", "glmmTMB")
  )
}

tidy_model_coefficients <- function(model,
                                    conf.int = TRUE,
                                    conf.level = 0.95,
                                    exponentiate = FALSE) {
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop(
      "The `broom` package is required to tidy model objects. ",
      "Install it or pass a coefficient data frame to `ggforestplot()` instead.",
      call. = FALSE
    )
  }

  if (is_mixed_model(model)) {
    if (!requireNamespace("broom.mixed", quietly = TRUE)) {
      stop(
        "The `broom.mixed` package is required to tidy mixed model objects. ",
        "Install it or pass a coefficient data frame to `ggforestplot()` instead.",
        call. = FALSE
      )
    }

    return(broom::tidy(
      x = model,
      effects = "fixed",
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate
    ))
  }

  broom::tidy(
    x = model,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate
  )
}

keep_fixed_effects <- function(out) {
  if (!"effect" %in% names(out)) {
    return(out)
  }

  fixed <- !is.na(out$effect) & as.character(out$effect) == "fixed"
  if (!any(fixed)) {
    stop("`tidy()` returned no fixed-effect rows to plot.", call. = FALSE)
  }

  out[fixed, , drop = FALSE]
}

#' Tidy a model object for forest plotting
#'
#' Uses [broom::tidy()] to convert a fitted model into forest-plot data. When
#' subgroup effects are requested, [marginaleffects::avg_slopes()] or
#' [marginaleffects::avg_comparisons()] derives conditional average effects
#' from the original fitted model and its covariance matrix. Mixed models are
#' supported through `broom.mixed` tidy methods when that package is installed.
#'
#' @details
#' With `subgroup = NULL`, the function retains its ordinary coefficient-tidy
#' behavior. When subgroup effects are requested, interaction selection uses
#' the fitted model's terms and model frame rather than parsing coefficient
#' names. The selected focal main effect, subgroup main-effect coefficients,
#' and raw interaction coefficients are replaced at their original position by
#' one hierarchical subgroup block. Unrelated coefficient rows stay in formula
#' order.
#'
#' Continuous focal predictors use
#' [marginaleffects::avg_slopes()] within each observed subgroup. Factor focal
#' predictors use [marginaleffects::avg_comparisons()] and compare each
#' non-reference level with the first factor level. Both functions use the
#' original fitted model and its variance-covariance matrix; no subgroup models
#' are refitted.
#'
#' Automatic selection is deliberately conservative. It accepts one
#' unambiguous continuous-by-factor interaction. Factor-by-factor interactions
#' require explicit `focal` and `subgroup` names. Continuous subgroups,
#' transformed focal terms, multiple interactions involving the selected
#' predictors, and three-way interactions are rejected.
#'
#' Linear and identity-link effects remain additive. Logit and log-link effects
#' are estimated on the link scale and use the existing `exponentiate`
#' semantics to return odds ratios or ratios by default. Cox effects are
#' estimated on the linear-predictor scale and returned as hazard ratios by
#' default. Other links fail rather than silently returning a response-scale
#' estimand with a different interpretation.
#'
#' @param model A fitted model object supported by [broom::tidy()] or, for
#'   mixed models, a `broom.mixed` tidy method.
#' @param conf.int Logical; if `TRUE`, request confidence intervals from
#'   [broom::tidy()].
#' @param conf.level Confidence level for intervals.
#' @param exponentiate `NULL` uses the model's conventional coefficient scale,
#'   such as odds ratios for logistic models and hazard ratios for Cox models.
#'   `TRUE` or `FALSE` overrides that behavior.
#' @param intercept Logical; if `FALSE`, drop the intercept term.
#' @param term_labels Optional named vector used to relabel displayed terms.
#'   Names should match model term names and values are the labels to display.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#' @param subgroup `NULL` for ordinary coefficient rows, `"auto"` to detect one
#'   unambiguous continuous-by-factor interaction, or the name of a factor
#'   defining subgroup levels. Explicit selection also requires `focal`.
#' @param focal Optional predictor whose conditional effect is estimated within
#'   each subgroup level. It may be continuous or a factor. For factors, each
#'   non-reference level is contrasted with the first factor level.
#'
#' @return A `forest_data` object ready for [ggforestplot()]. Derived rows add
#'   `subgroup_level`, `focal`, `model_term`, `contrast`, `estimand`,
#'   `effect_scale`, and `effect.p.value` columns. The canonical `p.value`
#'   remains missing for these
#'   rows because it is reserved for a future parent-header interaction test;
#'   `effect.p.value` stores the subgroup-specific slope or comparison test.
#' @export
#'
#' @examples
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)
#'   tidy_forest_model(fit)
#'
#'   if (requireNamespace("marginaleffects", quietly = TRUE)) {
#'     interaction_fit <- lm(wt ~ mpg * factor(cyl), data = mtcars)
#'     tidy_forest_model(
#'       interaction_fit,
#'       subgroup = "auto",
#'       focal = "mpg"
#'     )
#'   }
#'
#'   set.seed(123)
#'   logit_data <- data.frame(
#'     age = rnorm(250, mean = 62, sd = 8),
#'     bmi = rnorm(250, mean = 28, sd = 4),
#'     treatment = factor(rbinom(250, 1, 0.45), labels = c("Control", "Treatment"))
#'   )
#'   linpred <- -9 + 0.09 * logit_data$age + 0.11 * logit_data$bmi +
#'     0.9 * (logit_data$treatment == "Treatment")
#'   logit_data$event <- rbinom(250, 1, plogis(linpred))
#'   logit_fit <- glm(event ~ age + bmi + treatment, data = logit_data, family = binomial())
#'
#'   tidy_forest_model(logit_fit, exponentiate = TRUE)
#' }
tidy_forest_model <- function(model,
                              conf.int = TRUE,
                              conf.level = 0.95,
                              exponentiate = NULL,
                              intercept = FALSE,
                              term_labels = NULL,
                              sort_terms = c("none", "descending", "ascending"),
                              subgroup = NULL,
                              focal = NULL) {
  as_forest_data(
    model,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal
  )
}

tidy_forest_model_impl <- function(model,
                                   conf.int = TRUE,
                                   conf.level = 0.95,
                                   exponentiate = NULL,
                                   intercept = FALSE,
                                   term_labels = NULL,
                                   sort_terms = c("none", "descending", "ascending"),
                                   source_package = NULL,
                                   subgroup = NULL,
                                   focal = NULL) {
  if (!is.logical(conf.int) || length(conf.int) != 1L || is.na(conf.int)) {
    stop("`conf.int` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!isTRUE(conf.int)) {
    stop("Forest data require confidence intervals; use `conf.int = TRUE`.", call. = FALSE)
  }

  sort_terms <- match.arg(sort_terms)
  if (is.null(subgroup) && !is.null(focal)) {
    stop("`focal` requires `subgroup`.", call. = FALSE)
  }
  if (!is.null(subgroup) && !identical(sort_terms, "none")) {
    stop(
      "Subgroup-effect rows require `sort_terms = \"none\"`.",
      call. = FALSE
    )
  }
  has_subgroup <- !is.null(subgroup)
  subgroup_interaction <- if (has_subgroup) {
    interaction <- .resolve_subgroup_interaction(
      model,
      subgroup = subgroup,
      focal = focal
    )
    .subgroup_effect_dispatch(model)
    interaction
  } else {
    NULL
  }
  estimate_info <- infer_model_estimate_info(
    model,
    exponentiate = exponentiate,
    conf.level = conf.level
  )

  out <- tidy_model_coefficients(
    model = model,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = estimate_info$exponentiate
  )
  out <- keep_fixed_effects(out)

  if (!"term" %in% names(out) || !"estimate" %in% names(out)) {
    stop(
      "`broom::tidy()` returned an unsupported result. ",
      "Expected at least `term` and `estimate` columns.",
      call. = FALSE
    )
  }

  if (conf.int && (!"conf.low" %in% names(out) || !"conf.high" %in% names(out))) {
    stop(
      "`broom::tidy()` did not return confidence interval columns.",
      call. = FALSE
    )
  }

  if (!isTRUE(intercept)) {
    out <- out[out$term != "(Intercept)", , drop = FALSE]
  }

  if (has_subgroup) {
    effects <- .estimate_subgroup_effects(
      model,
      subgroup = subgroup,
      focal = focal,
      conf.level = conf.level,
      estimate_info = estimate_info,
      interaction = subgroup_interaction
    )
    out <- .splice_subgroup_effects(out, effects)
  }

  group_column <- if (has_subgroup && "group" %in% names(out) &&
      any(!is.na(out$group) & nzchar(out$group))) {
    "group"
  } else {
    NULL
  }

  out <- as_forest_data(
    data = out,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high",
    label = if (has_subgroup) "label" else "term",
    group = group_column,
    subgroup = if (has_subgroup) "subgroup" else NULL,
    term_labels = term_labels,
    n = NULL,
    p.value = if ("p.value" %in% names(out)) "p.value" else NULL,
    estimate_scale = estimate_info$estimate_scale,
    axis_transform = estimate_info$axis_transform,
    effect_label = estimate_info$effect_label,
    conf.level = conf.level,
    reference_value = estimate_info$reference_value,
    source_model = class(model),
    source_package = source_package,
    sort_terms = sort_terms
  )
  out
}

model_as_forest_data <- function(data,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 exponentiate = NULL,
                                 intercept = FALSE,
                                 term_labels = NULL,
                                 sort_terms = c("none", "descending", "ascending"),
                                 source_package = NULL,
                                 subgroup = NULL,
                                 focal = NULL,
                                 ...) {
  tidy_forest_model_impl(
    model = data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = source_package,
    subgroup = subgroup,
    focal = focal
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.lm <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                              exponentiate = NULL, intercept = FALSE,
                              term_labels = NULL,
                              sort_terms = c("none", "descending", "ascending"),
                              subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = "stats",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.glm <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                               exponentiate = NULL, intercept = FALSE,
                               term_labels = NULL,
                               sort_terms = c("none", "descending", "ascending"),
                               subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = "stats",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.coxph <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                 exponentiate = NULL, intercept = FALSE,
                                 term_labels = NULL,
                                 sort_terms = c("none", "descending", "ascending"),
                                 subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = "survival",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.merMod <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                  exponentiate = NULL, intercept = FALSE,
                                  term_labels = NULL,
                                  sort_terms = c("none", "descending", "ascending"),
                                  subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = "lme4",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.lme <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                               exponentiate = NULL, intercept = FALSE,
                               term_labels = NULL,
                               sort_terms = c("none", "descending", "ascending"),
                               subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = "nlme",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.glmmTMB <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                   exponentiate = NULL, intercept = FALSE,
                                   term_labels = NULL,
                                   sort_terms = c("none", "descending", "ascending"),
                                   subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = "glmmTMB",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.default <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                   exponentiate = NULL, intercept = FALSE,
                                   term_labels = NULL,
                                   sort_terms = c("none", "descending", "ascending"),
                                   subgroup = NULL, focal = NULL) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    subgroup = subgroup,
    focal = focal,
    source_package = NULL,
    ...
  )
}
