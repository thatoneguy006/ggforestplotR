#' Tidy a model object for forest plotting
#'
#' Uses [broom::tidy()] to convert a fitted model into forest-plot data.
#'
#' @param model A fitted model object supported by [broom::tidy()].
#' @param conf.int Logical; if `TRUE`, request confidence intervals from
#'   [broom::tidy()].
#' @param conf.level Confidence level for intervals.
#' @param exponentiate Logical; passed through to [broom::tidy()].
#' @param intercept Logical; if `FALSE`, drop the intercept term.
#' @param sort_terms How to sort rows: `"none"`, `"descending"`, or
#'   `"ascending"`.
#'
#' @return A standardized coefficient data frame ready for [ggforestplot()].
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
                              exponentiate = FALSE,
                              intercept = FALSE,
                              sort_terms = c("none", "descending", "ascending")) {
  sort_terms <- match.arg(sort_terms)

  if (!requireNamespace("broom", quietly = TRUE)) {
    stop(
      "The `broom` package is required to tidy model objects. ",
      "Install it or pass a coefficient data frame to `ggforestplot()` instead.",
      call. = FALSE
    )
  }

  out <- broom::tidy(
    x = model,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate
  )

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

  as_forest_data(
    data = out,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high",
    n = NULL,
    p.value = if ("p.value" %in% names(out)) "p.value" else NULL,
    exponentiate = exponentiate,
    sort_terms = sort_terms
  )
}
