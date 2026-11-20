make_warp_fit <- function() {
  stats::lm(breaks ~ wool * tension, data = warpbreaks)
}

test_that("emmeans() means use primary levels and no reference line", {
  skip_if_not_installed("emmeans")

  means <- emmeans::emmeans(make_warp_fit(), ~ tension)
  out <- suppressMessages(as_forest_data(means))
  raw <- as.data.frame(stats::confint(means))

  expect_s3_class(out, "forest_data")
  expect_equal(out$label, as.character(raw$tension))
  expect_equal(out$estimate, raw$emmean)
  expect_equal(out$conf.low, raw$lower.CL)
  expect_equal(out$conf.high, raw$upper.CL)
  expect_true(all(is.na(out$p.value)))

  metadata <- forest_metadata(out)
  expect_equal(metadata$effect_label, "EMM")
  expect_null(metadata$reference_value)
  expect_equal(metadata$conf_level, 0.95)
  expect_equal(metadata$source_package, "emmeans")
})

test_that("by variables are labelled or mapped to facets", {
  skip_if_not_installed("emmeans")

  means <- emmeans::emmeans(make_warp_fit(), ~ tension | wool)
  out <- suppressMessages(as_forest_data(means))
  expect_equal(
    out$label,
    paste0("tension = ", c("L", "M", "H"), ", wool = ", rep(c("A", "B"), each = 3))
  )

  faceted <- suppressMessages(as_forest_data(means, facet = "wool"))
  expect_equal(faceted$label, rep(c("L", "M", "H"), 2))
  expect_equal(faceted$grouping, rep(c("A", "B"), each = 3))
  expect_s3_class(suppressMessages(ggforestplot(means, facet = "wool")), "ggplot")
})

test_that("pairs() contrasts are additive and keep adjusted tests", {
  skip_if_not_installed("emmeans")

  contrasts <- pairs(emmeans::emmeans(make_warp_fit(), ~ tension))
  out <- suppressMessages(as_forest_data(contrasts))
  raw <- as.data.frame(summary(contrasts, infer = c(TRUE, TRUE)))

  expect_equal(out$label, as.character(raw$contrast))
  expect_equal(out$estimate, raw$estimate)
  expect_equal(out$p.value, raw$p.value)
  expect_equal(forest_metadata(out)$reference_value, 0)
  expect_equal(forest_metadata(out)$effect_label, "Estimate")

  unadjusted <- suppressMessages(as_forest_data(contrasts, adjust = "none"))
  expect_false(isTRUE(all.equal(unadjusted$conf.low, out$conf.low)))
})

test_that("logit contrasts convert between log odds ratios and odds ratios", {
  skip_if_not_installed("emmeans")

  fit <- suppressWarnings(stats::glm(
    vs ~ factor(gear) + mpg,
    data = mtcars,
    family = stats::binomial()
  ))
  contrasts <- pairs(emmeans::emmeans(fit, ~ gear), adjust = "none")

  log_scale <- suppressMessages(as_forest_data(contrasts))
  expect_equal(forest_metadata(log_scale)$estimate_scale, "log")
  expect_equal(forest_metadata(log_scale)$effect_label, "log(OR)")

  ratios <- suppressMessages(as_forest_data(contrasts, exponentiate = TRUE))
  response <- suppressMessages(as_forest_data(contrasts, type = "response"))
  expect_equal(forest_metadata(ratios)$estimate_scale, "ratio")
  expect_equal(forest_metadata(response)$estimate_scale, "ratio")
  expect_equal(forest_metadata(response)$effect_label, "OR")
  expect_equal(ratios$estimate, response$estimate, tolerance = 1e-8)
  expect_equal(ratios$conf.high, response$conf.high, tolerance = 1e-8)

  probabilities <- suppressMessages(as_forest_data(
    emmeans::emmeans(fit, ~ gear),
    type = "response"
  ))
  expect_equal(forest_metadata(probabilities)$estimate_scale, "probability")
})

test_that("summary_emm and emm_list inputs are supported", {
  skip_if_not_installed("emmeans")

  means <- emmeans::emmeans(make_warp_fit(), ~ tension)
  intervals <- stats::confint(means, level = 0.9)
  out <- as_forest_data(intervals)
  expect_equal(forest_metadata(out)$conf_level, 0.9)
  expect_error(as_forest_data(intervals, conf.level = 0.95), "does not match")

  expect_error(
    as_forest_data(emmeans::test(pairs(means))),
    "no confidence intervals"
  )

  both <- emmeans::emmeans(make_warp_fit(), pairwise ~ tension)
  expect_equal(suppressMessages(as_forest_data(both))$label, c("L", "M", "H"))
  expect_equal(
    nrow(suppressMessages(as_forest_data(both, which = "contrasts"))),
    3L
  )
  expect_error(as_forest_data(both, which = "missing"), "must identify")
})

test_that("emmGrid conf.level is passed to emmeans", {
  skip_if_not_installed("emmeans")

  means <- emmeans::emmeans(make_warp_fit(), ~ tension)
  out <- suppressMessages(as_forest_data(means, conf.level = 0.8))
  raw <- as.data.frame(stats::confint(means, level = 0.8))
  expect_equal(out$conf.low, raw$lower.CL)
  expect_equal(forest_metadata(out)$conf_level, 0.8)
  expect_equal(
    forest_metadata(suppressMessages(ggforestplot(means, conf.level = 0.8))$ggforestplotR_state$forest_data)$conf_level,
    0.8
  )
})
