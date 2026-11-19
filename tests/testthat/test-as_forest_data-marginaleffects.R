make_logit_fit <- function() {
  suppressWarnings(stats::glm(
    am ~ hp + wt + factor(cyl),
    data = mtcars,
    family = stats::binomial()
  ))
}

test_that("avg_comparisons() differences keep marginaleffects estimates", {
  skip_if_not_installed("marginaleffects")

  cmp <- suppressWarnings(marginaleffects::avg_comparisons(make_logit_fit()))
  out <- as_forest_data(cmp)
  raw <- as.data.frame(cmp)

  expect_s3_class(out, "forest_data")
  expect_equal(out$estimate, raw$estimate)
  expect_equal(out$conf.low, raw$conf.low)
  expect_equal(out$conf.high, raw$conf.high)
  expect_equal(out$p.value, raw$p.value)
  expect_equal(out$label, paste0(raw$term, ": ", raw$contrast))
  expect_false(anyDuplicated(out$term) > 0L)

  metadata <- forest_metadata(out)
  expect_equal(metadata$estimate_scale, "identity")
  expect_equal(metadata$effect_label, "Risk difference")
  expect_equal(metadata$reference_value, 0)
  expect_equal(metadata$conf_level, 0.95)
  expect_equal(metadata$source_package, "marginaleffects")
  expect_equal(metadata$p_method, "level")
})

test_that("ratio comparisons use a ratio scale and subgroup blocks", {
  skip_if_not_installed("marginaleffects")

  cmp <- suppressWarnings(marginaleffects::avg_comparisons(
    make_logit_fit(),
    comparison = "lnratioavg",
    transform = exp
  ))
  out <- as_forest_data(cmp, subgroup = "term")
  metadata <- forest_metadata(out)

  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$axis_transform, "log10")
  expect_equal(metadata$reference_value, 1)
  contrasts <- as.character(as.data.frame(cmp)$contrast)
  expect_equal(out$label, sub("^ln\\((.*)\\)$", "\\1", contrasts))
  expect_false(any(startsWith(out$label, "ln(")))
  expect_equal(out$subgroup, as.character(as.data.frame(cmp)$term))

  p <- ggforestplot(cmp, subgroup = "term")
  expect_s3_class(p, "ggplot")
})

test_that("ratio comparisons explain intervals that cross zero", {
  skip_if_not_installed("marginaleffects")

  cmp <- suppressWarnings(marginaleffects::avg_comparisons(
    make_logit_fit(),
    comparison = "ratio"
  ))
  raw <- as.data.frame(cmp)
  if (all(raw$conf.low > 0)) {
    expect_equal(forest_metadata(as_forest_data(cmp))$estimate_scale, "ratio")
  } else {
    expect_error(as_forest_data(cmp), "lnratioavg")
    expect_equal(
      forest_metadata(as_forest_data(cmp, estimate_scale = "identity"))$estimate_scale,
      "identity"
    )
  }
})

test_that("log comparisons can be exponentiated", {
  skip_if_not_installed("marginaleffects")

  fit <- make_logit_fit()
  log_or <- suppressWarnings(marginaleffects::avg_comparisons(
    fit,
    variables = "wt",
    comparison = "lnoravg"
  ))
  out <- as_forest_data(log_or)
  expect_equal(forest_metadata(out)$estimate_scale, "log")
  expect_equal(forest_metadata(out)$effect_label, "log(OR)")

  ratio <- as_forest_data(log_or, exponentiate = TRUE)
  expect_equal(ratio$estimate, exp(as.data.frame(log_or)$estimate))
  expect_equal(forest_metadata(ratio)$estimate_scale, "ratio")
  expect_equal(forest_metadata(ratio)$effect_label, "OR")

  transformed <- suppressWarnings(marginaleffects::avg_comparisons(
    fit,
    variables = "wt",
    comparison = "lnoravg",
    transform = exp
  ))
  expect_equal(forest_metadata(as_forest_data(transformed))$estimate_scale, "ratio")
})

test_that("link-scale differences from a logit model are log odds ratios", {
  skip_if_not_installed("marginaleffects")

  cmp <- suppressWarnings(marginaleffects::avg_comparisons(
    make_logit_fit(),
    variables = "hp",
    type = "link"
  ))
  out <- as_forest_data(cmp)
  expect_equal(forest_metadata(out)$estimate_scale, "log")
  expect_equal(forest_metadata(out)$effect_label, "log(OR)")
})

test_that("by variables become labels or mapped groups", {
  skip_if_not_installed("marginaleffects")

  fit <- stats::lm(mpg ~ wt * factor(am) + hp, data = mtcars)
  cmp <- suppressWarnings(marginaleffects::avg_comparisons(
    fit,
    variables = "wt",
    by = "am"
  ))

  labelled <- as_forest_data(cmp, term_labels = c("0" = "Automatic", "1" = "Manual"))
  expect_equal(labelled$label, c("Automatic", "Manual"))

  grouped <- as_forest_data(cmp, group = "am")
  expect_equal(grouped$label, c("wt", "wt"))
  expect_equal(levels(grouped$group), c("0", "1"))

  faceted <- as_forest_data(cmp, facet = "am")
  expect_equal(faceted$grouping, c("0", "1"))
})

test_that("predictions have no reference line and binomial probabilities", {
  skip_if_not_installed("marginaleffects")

  pred <- suppressWarnings(marginaleffects::avg_predictions(make_logit_fit(), by = "cyl"))
  out <- as_forest_data(pred)
  metadata <- forest_metadata(out)

  expect_equal(metadata$estimate_scale, "probability")
  expect_null(metadata$reference_value)
  expect_equal(out$label, as.character(as.data.frame(pred)$cyl))

  p <- ggforestplot(pred)
  expect_s3_class(p, "ggplot")
})

test_that("unit-level predictions stay distinct rows", {
  skip_if_not_installed("marginaleffects")

  fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
  pred <- suppressWarnings(marginaleffects::predictions(
    fit,
    newdata = marginaleffects::datagrid(wt = c(2, 3, 4))
  ))
  out <- as_forest_data(pred)
  expect_equal(out$label, c("2", "3", "4"))
  expect_equal(forest_metadata(out)$effect_label, "Prediction")
})

test_that("avg_slopes() and hypotheses() are supported", {
  skip_if_not_installed("marginaleffects")

  fit <- stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
  slopes <- suppressWarnings(marginaleffects::avg_slopes(fit))
  out <- as_forest_data(slopes)
  expect_equal(forest_metadata(out)$effect_label, "Slope")
  expect_equal(out$label, as.data.frame(slopes)$term)

  hyp <- suppressWarnings(marginaleffects::hypotheses(fit, hypothesis = "b2 = b3"))
  hyp_out <- as_forest_data(hyp)
  expect_equal(nrow(hyp_out), 1L)
  expect_equal(forest_metadata(hyp_out)$reference_value, 0)
})

test_that("stored confidence levels are respected", {
  skip_if_not_installed("marginaleffects")

  cmp <- suppressWarnings(marginaleffects::avg_comparisons(
    make_logit_fit(),
    variables = "hp",
    conf_level = 0.9
  ))
  expect_equal(forest_metadata(as_forest_data(cmp))$conf_level, 0.9)
  expect_equal(forest_metadata(as_forest_data(cmp, conf.level = 0.9))$conf_level, 0.9)
  expect_error(as_forest_data(cmp, conf.level = 0.95), "does not match")
  expect_error(
    ggforestplot(cmp, conf.level = 0.95),
    "does not match"
  )
  expect_s3_class(ggforestplot(cmp), "ggplot")
})

test_that("marginaleffects methods validate their mappings", {
  skip_if_not_installed("marginaleffects")

  cmp <- suppressWarnings(marginaleffects::avg_comparisons(make_logit_fit()))
  expect_error(as_forest_data(cmp, group = "missing"), "was not found")
  expect_error(as_forest_data(cmp, group = "estimate"), "was not found")
  expect_error(as_forest_data(cmp, foo = 1))
  expect_error(as_forest_data(cmp, exponentiate = TRUE), "log-scale")
  expect_equal(
    forest_metadata(as_forest_data(cmp, estimate_scale = "identity", effect_label = "AME"))$effect_label,
    "AME"
  )
  expect_null(forest_metadata(as_forest_data(cmp, reference_value = NULL))$reference_value)
})

test_that("bind_forest_models() combines marginaleffects results", {
  skip_if_not_installed("marginaleffects")

  base <- suppressWarnings(stats::glm(am ~ hp + wt, data = mtcars, family = stats::binomial()))
  adjusted <- make_logit_fit()
  bound <- bind_forest_models(list(
    Base = suppressWarnings(marginaleffects::avg_comparisons(base, variables = c("hp", "wt"))),
    Adjusted = suppressWarnings(marginaleffects::avg_comparisons(adjusted, variables = c("hp", "wt")))
  ))

  expect_s3_class(bound, "ggforestplot_bound_models")
  expect_equal(levels(bound$group), c("Base", "Adjusted"))
  expect_s3_class(ggforestplot(bound), "ggplot")
})
