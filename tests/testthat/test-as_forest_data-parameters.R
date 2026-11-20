test_that("model_parameters() tables keep reported estimates", {
  skip_if_not_installed("parameters")

  fit <- suppressWarnings(stats::glm(
    am ~ hp + wt + factor(cyl),
    data = mtcars,
    family = stats::binomial()
  ))
  params <- suppressWarnings(parameters::model_parameters(fit, exponentiate = TRUE))
  out <- as_forest_data(params)
  raw <- as.data.frame(params)
  raw <- raw[raw$Parameter != "(Intercept)", ]

  expect_s3_class(out, "forest_data")
  expect_equal(out$estimate, raw$Coefficient)
  expect_equal(out$conf.low, raw$CI_low)
  expect_equal(out$p.value, raw$p)
  expect_equal(nrow(out), nrow(raw))

  metadata <- forest_metadata(out)
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$effect_label, "Odds Ratio")
  expect_equal(metadata$conf_level, 0.95)

  log_scale <- as_forest_data(suppressWarnings(parameters::model_parameters(fit)))
  expect_equal(forest_metadata(log_scale)$estimate_scale, "log")

  with_intercept <- as_forest_data(params, intercept = TRUE)
  expect_equal(nrow(with_intercept), nrow(out) + 1L)

  raw_names <- as_forest_data(params, pretty_names = FALSE)
  expect_equal(raw_names$label, raw$Parameter)
  expect_s3_class(ggforestplot(params), "ggplot")
})

test_that("linear model_parameters() tables are additive", {
  skip_if_not_installed("parameters")

  params <- parameters::model_parameters(stats::lm(mpg ~ wt + hp, data = mtcars))
  out <- as_forest_data(params, term_labels = c(wt = "Weight"))
  expect_equal(forest_metadata(out)$estimate_scale, "identity")
  expect_equal(out$label[[1L]], "Weight")
})
