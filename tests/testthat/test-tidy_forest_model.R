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

test_that("mixed-model tidy output is filtered to fixed effects", {
  mixed_tidy <- data.frame(
    effect = c("fixed", "fixed", "ran_pars"),
    term = c("(Intercept)", "Days", "sd__(Intercept)"),
    estimate = c(251.4, 10.5, 24.7),
    conf.low = c(238.0, 8.9, NA),
    conf.high = c(264.8, 12.1, NA),
    stringsAsFactors = FALSE
  )

  out <- keep_fixed_effects(mixed_tidy)

  expect_equal(out$term, c("(Intercept)", "Days"))
  expect_true(all(as.character(out$effect) == "fixed"))
})

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

test_that("tidy_forest_model supports lme4 mixed models", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  out <- tidy_forest_model(fit)

  expect_equal(out$term, "Days")
  expect_true("effect" %in% names(out))
  expect_true(all(as.character(out$effect) == "fixed"))
  expect_true(all(!is.na(out$estimate)))
  expect_true(all(!is.na(out$conf.low)))
  expect_true(all(!is.na(out$conf.high)))

  p <- ggforestplot(fit)

  expect_equal(p$ggforestplotR_state$forest_data$term, "Days")
  expect_s3_class(p, "ggplot")
})

test_that("ggforestplot labels logistic mixed models as odds ratios", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial()
  )

  p <- ggforestplot(fit)

  expect_equal(p$labels$x, "OR (95% CI)")
  expect_equal(p$ggforestplotR_state$defaults$estimate_label, "OR")
  expect_true(all(p$ggforestplotR_state$forest_data$estimate > 0))
  expect_true(all(as.character(p$ggforestplotR_state$forest_data$effect) == "fixed"))
})

test_that("ggforestplot supports exponentiated logistic regression models", {
  fit <- glm(
    event ~ age + bmi + treatment,
    data = make_logistic_example_data(),
    family = binomial()
  )

  p <- ggforestplot(fit, exponentiate = TRUE) +
    ggplot2::labs(x = "Odds ratio")

  expect_equal(p$labels$x, "Odds ratio")
  expect_equal(p$scales$get_scales("x")$trans$name, "log-10")
  expect_true(all(p$ggforestplotR_state$forest_data$estimate > 0))
  expect_true(all(p$ggforestplotR_state$forest_data$conf.low > 0))
  expect_true(all(p$ggforestplotR_state$forest_data$conf.high > 0))
})

test_that("ggforestplot uses canonical labels for model objects", {
  fit <- glm(
    event ~ age + bmi + treatment,
    data = make_logistic_example_data(),
    family = binomial()
  )

  p <- ggforestplot(fit)

  expect_equal(p$labels$x, "OR (95% CI)")
  expect_equal(p$ggforestplotR_state$defaults$estimate_label, "OR")
  expect_true(all(p$ggforestplotR_state$forest_data$estimate > 0))
})

test_that("ggforestplot labels Cox models as hazard ratios", {
  skip_if_not_installed("survival")

  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )

  p <- ggforestplot(fit)

  expect_equal(p$labels$x, "HR (95% CI)")
  expect_equal(p$ggforestplotR_state$defaults$estimate_label, "HR")
  expect_equal(p$ggforestplotR_state$defaults$ref_line, 1)
  expect_true(all(p$ggforestplotR_state$forest_data$estimate > 0))
})

test_that("add_forest_table works for exponentiated logistic regression output", {
  fit <- glm(
    event ~ age + bmi + treatment,
    data = make_logistic_example_data(),
    family = binomial()
  )

  out <- ggforestplot(fit, exponentiate = TRUE) +
    ggplot2::labs(x = "Odds ratio") +
    add_forest_table(position = "right", estimate_label = "OR", columns = c("term", "estimate", "p"))

  expect_s3_class(out, "patchwork")
})
