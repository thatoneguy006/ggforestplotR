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
#' Uses [broom::tidy()] to convert a fitted model into forest-plot data. Mixed
#' models are supported through `broom.mixed` tidy methods when that package is
#' installed.
#'
#' @param model A fitted model object supported by [broom::tidy()] or, for
#'   mixed models, a `broom.mixed` tidy method.
#' @param conf.int Logical; if `TRUE`, request confidence intervals from
#'   [broom::tidy()].
#' @param conf.level Confidence level for intervals.
#' @param exponentiate Logical; passed through to [broom::tidy()].
#' @param intercept Logical; if `FALSE`, drop the intercept term.
#' @param term_labels Optional named vector used to relabel displayed terms.
#'   Names should match model term names and values are the labels to display.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#'
#' @return A `forest_data` object ready for [ggforestplot()].
#' @export
#'
#' @examples
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)
#'   tidy_forest_model(fit)
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
                              sort_terms = c("none", "descending", "ascending")) {
  as_forest_data(
    model,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms
  )
}

tidy_forest_model_impl <- function(model,
                                   conf.int = TRUE,
                                   conf.level = 0.95,
                                   exponentiate = NULL,
                                   intercept = FALSE,
                                   term_labels = NULL,
                                   sort_terms = c("none", "descending", "ascending"),
                                   source_package = NULL) {
  if (!is.logical(conf.int) || length(conf.int) != 1L || is.na(conf.int)) {
    stop("`conf.int` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!isTRUE(conf.int)) {
    stop("Forest data require confidence intervals; use `conf.int = TRUE`.", call. = FALSE)
  }

  sort_terms <- match.arg(sort_terms)
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

  out <- as_forest_data(
    data = out,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high",
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
                                 ...) {
  tidy_forest_model_impl(
    model = data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = source_package
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.lm <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                              exponentiate = NULL, intercept = FALSE,
                              term_labels = NULL,
                              sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = "stats",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.glm <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                               exponentiate = NULL, intercept = FALSE,
                               term_labels = NULL,
                               sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = "stats",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.coxph <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                 exponentiate = NULL, intercept = FALSE,
                                 term_labels = NULL,
                                 sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = "survival",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.merMod <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                  exponentiate = NULL, intercept = FALSE,
                                  term_labels = NULL,
                                  sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = "lme4",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.lme <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                               exponentiate = NULL, intercept = FALSE,
                               term_labels = NULL,
                               sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = "nlme",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.glmmTMB <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                   exponentiate = NULL, intercept = FALSE,
                                   term_labels = NULL,
                                   sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = "glmmTMB",
    ...
  )
}

#' @rdname as_forest_data
#' @export
as_forest_data.default <- function(data, ..., conf.int = TRUE, conf.level = 0.95,
                                   exponentiate = NULL, intercept = FALSE,
                                   term_labels = NULL,
                                   sort_terms = c("none", "descending", "ascending")) {
  model_as_forest_data(
    data,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    intercept = intercept,
    term_labels = term_labels,
    sort_terms = sort_terms,
    source_package = NULL,
    ...
  )
}
