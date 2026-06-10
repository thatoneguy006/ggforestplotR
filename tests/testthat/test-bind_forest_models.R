test_that("bind_forest_models stacks model terms with model labels", {
  skip_if_not_installed("broom")

  fit1 <- lm(mpg ~ wt + hp, data = mtcars)
  fit2 <- lm(mpg ~ wt + qsec, data = mtcars)

  out <- bind_forest_models(list(Base = fit1, Adjusted = fit2))

  expect_s3_class(out, "ggforestplot_bound_models")
  expect_equal(unique(out$group), c("Base", "Adjusted"))
  expect_false("(Intercept)" %in% out$term)
  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "group") %in% names(out)))
  expect_equal(attr(out, "estimate_label"), "Estimate")
  expect_false(isTRUE(attr(out, "exponentiate")))
})

test_that("ggforestplot uses bound model labels as groups", {
  skip_if_not_installed("broom")

  fit1 <- lm(mpg ~ wt + hp, data = mtcars)
  fit2 <- lm(mpg ~ wt + qsec, data = mtcars)
  bound <- bind_forest_models(
    list(fit1, fit2),
    model_labels = c("Clinical", "Adjusted")
  )

  p <- ggforestplot(bound)

  expect_equal(p$labels$colour, "Group")
  expect_equal(unique(p$ggforestplotR_state$forest_data$group), c("Clinical", "Adjusted"))
  expect_s3_class(p, "ggplot")
})

test_that("bound model tables label terms that appear in only one model", {
  skip_if_not_installed("broom")

  fit1 <- lm(mpg ~ cyl, data = mtcars)
  fit2 <- lm(mpg ~ cyl + disp, data = mtcars)
  fit3 <- lm(mpg ~ cyl + disp + wt, data = mtcars)
  bound <- bind_forest_models(
    list(fit1, fit2, fit3),
    model_labels = c("Unadjusted", "Adjusted", "Fully Adjusted")
  )
  p <- ggforestplot(bound)
  table_spec <- build_forest_table_data(p$ggforestplotR_state$forest_data)
  estimate_text <- table_spec$table_data$text[
    table_spec$table_data$column_key == "estimate" &
      as.character(table_spec$table_data$row_key) == "wt"
  ]

  expect_match(estimate_text, "^Fully Adjusted: ")
})

test_that("bound model tables format p.value aliases with p_digits", {
  skip_if_not_installed("broom")

  fit1 <- lm(mpg ~ cyl, data = mtcars)
  fit2 <- lm(mpg ~ cyl + disp, data = mtcars)
  fit3 <- lm(mpg ~ cyl + disp + wt, data = mtcars)
  bound <- bind_forest_models(
    list(fit1, fit2, fit3),
    model_labels = c("Unadjusted", "Adjusted", "Fully Adjusted")
  )
  p <- ggforestplot(bound, p.value = "p.value")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("term", "estimate", "p.value"),
    p_digits = 3
  )
  p_text <- table_spec$table_data$text[
    table_spec$table_data$column_key == "p" &
      as.character(table_spec$table_data$row_key) == "wt"
  ]

  expect_true("p" %in% table_spec$column_keys)
  expect_false("p.value" %in% table_spec$column_keys)
  expect_match(p_text, "^Fully Adjusted: ")
  expect_equal(p_text, "Fully Adjusted: 0.0016")
})

test_that("bind_forest_models supports common exponentiated scales", {
  skip_if_not_installed("broom")

  set.seed(123)
  dat <- data.frame(
    x = rnorm(120),
    z = rnorm(120),
    group = factor(rep(c("A", "B"), length.out = 120))
  )
  dat$y <- rbinom(120, 1, stats::plogis(-0.2 + 0.8 * dat$x - 0.4 * dat$z))

  fit1 <- glm(y ~ x, data = dat, family = binomial())
  fit2 <- glm(y ~ x + z, data = dat, family = binomial())

  out <- bind_forest_models(
    list(Unadjusted = fit1, Adjusted = fit2),
    exponentiate = TRUE
  )

  expect_true(isTRUE(attr(out, "exponentiate")))
  expect_equal(attr(out, "estimate_label"), "OR")
  expect_true(all(out$estimate > 0))
  expect_true(all(out$conf.low > 0))
  expect_true(all(out$conf.high > 0))
})

test_that("bind_forest_models validates model labels and scales", {
  skip_if_not_installed("broom")

  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ hp, data = mtcars)

  expect_error(
    bind_forest_models(fit1),
    "`models` must be a non-empty list"
  )
  expect_error(
    bind_forest_models(list(fit1, fit2), model_labels = "Only one"),
    "`model_labels` must have one label per model"
  )
  expect_error(
    bind_forest_models(list(fit1, fit2), model_labels = c("A", "A")),
    "`model_labels` must be unique"
  )
  expect_error(
    bind_forest_models(list(fit1, fit2), exponentiate = c(TRUE, FALSE, TRUE)),
    "`exponentiate` must be `NULL`, a single logical value, or one logical value per model"
  )
})

test_that("bind_forest_models rejects mixed estimate scales", {
  skip_if_not_installed("broom")

  set.seed(123)
  dat <- data.frame(x = rnorm(80))
  dat$y <- rbinom(80, 1, stats::plogis(0.2 + dat$x))

  linear <- lm(mpg ~ wt, data = mtcars)
  logistic <- glm(y ~ x, data = dat, family = binomial())

  expect_error(
    bind_forest_models(list(linear, logistic)),
    "same estimate scale"
  )
})

test_that("ggforestplot rejects exponentiate overrides for bound models", {
  skip_if_not_installed("broom")

  bound <- bind_forest_models(list(Base = lm(mpg ~ wt, data = mtcars)))

  expect_error(
    ggforestplot(bound, exponentiate = TRUE),
    "`exponentiate` is set by `bind_forest_models\\(\\)`"
  )
})
