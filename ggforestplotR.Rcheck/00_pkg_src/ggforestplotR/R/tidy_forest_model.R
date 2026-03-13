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