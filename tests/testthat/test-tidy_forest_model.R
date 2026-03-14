make_logistic_example_data <- function() {
  set.seed(123)

  logit_data <- data.frame(
    age = rnorm(250, mean = 62, sd = 8),
    bmi = rnorm(250, mean = 28, sd = 4),
    treatment = factor(rbinom(250, 1, 0.45), labels = c("Control", "Treatment"))
  )

  linpred <- -9 +
    0.09 * logit_data$age +
    0.11 * logit_data$bmi +
    0.9 * (logit_data$treatment == "Treatment")

  logit_data$event <- rbinom(250, 1, plogis(linpred))
  logit_data
}

test_that("tidy_forest_model exponentiates logistic regression coefficients", {
  fit <- glm(
    event ~ age + bmi + treatment,
    data = make_logistic_example_data(),
    family = binomial()
  )

  out <- tidy_forest_model(fit, exponentiate = TRUE)

  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "p.value") %in% names(out)))
  expect_true(all(out$estimate > 0))
  expect_true(all(out$conf.low > 0))
  expect_true(all(out$conf.high > 0))
  expect_true(all(!is.na(out$p.value)))
})

test_that("ggforestplot supports exponentiated logistic regression models", {
  fit <- glm(
    event ~ age + bmi + treatment,
    data = make_logistic_example_data(),
    family = binomial()
  )

  p <- ggforestplot(fit, exponentiate = TRUE) +
    ggplot2::labs(x = "Odds ratio")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Odds ratio")
  expect_equal(p$scales$get_scales("x")$trans$name, "log-10")
  expect_true(all(p$ggforestplotR_state$forest_data$estimate > 0))
  expect_true(all(p$ggforestplotR_state$forest_data$conf.low > 0))
  expect_true(all(p$ggforestplotR_state$forest_data$conf.high > 0))
})

test_that("add_forest_table works for exponentiated logistic regression output", {
  fit <- glm(
    event ~ age + bmi + treatment,
    data = make_logistic_example_data(),
    family = binomial()
  )

  out <- ggforestplot(fit, exponentiate = TRUE) +
    ggplot2::labs(x = "Odds ratio") +
    add_forest_table(position = "right", estimate_label = "OR", show_p = TRUE)

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
})
