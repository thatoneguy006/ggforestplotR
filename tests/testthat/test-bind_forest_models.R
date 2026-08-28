test_that("bind_forest_models stacks model terms with model labels", {
  skip_if_not_installed("broom")

  fit1 <- lm(mpg ~ wt + hp, data = mtcars)
  fit2 <- lm(mpg ~ wt + qsec, data = mtcars)

  out <- bind_forest_models(list(Base = fit1, Adjusted = fit2))

  expect_s3_class(out, "ggforestplot_bound_models")
  expect_s3_class(out, "forest_data")
  expect_equal(unique(out$group), c("Base", "Adjusted"))
  expect_false("(Intercept)" %in% out$term)
  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "group") %in% names(out)))
  expect_equal(attr(out, "estimate_label"), "Beta")
  expect_false(isTRUE(attr(out, "exponentiate")))

  metadata <- forest_metadata(out)
  expect_equal(metadata$estimate_scale, "identity")
  expect_equal(metadata$effect_label, "Beta")
  expect_equal(metadata$reference_value, 0)
  expect_equal(metadata$p_method, "overall")
  expect_named(metadata$source_model, c("Base", "Adjusted"))
  expect_named(metadata$source_package, c("Base", "Adjusted"))
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

test_that("bound model tables use a dedicated model column", {
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
  model_text <- table_spec$table_data$text[
    table_spec$table_data$column_key == "group" &
      as.character(table_spec$table_data$row_key) == "wt"
  ]

  expect_equal(table_spec$column_keys, c("term", "group", "estimate"))
  expect_equal(table_spec$headers[[2L]], "Model")
  expect_equal(model_text, "Fully Adjusted")
  expect_false(grepl("Fully Adjusted:", estimate_text, fixed = TRUE))
})

test_that("dedicated model columns preserve multiline value alignment", {
  data <- data.frame(
    term = rep("Age", 2),
    estimate = c(0.2, 0.4),
    conf.low = c(0.1, 0.3),
    conf.high = c(0.3, 0.5),
    model = c("Base", "Adjusted"),
    sample_size = c(100, 100),
    note = c(NA, "Primary")
  )
  p <- ggforestplot(data, group = "model", n = "sample_size")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("term", "group", "n", "note")
  )
  table_text <- stats::setNames(
    table_spec$table_data$text,
    table_spec$table_data$column_key
  )

  expect_equal(table_text[["group"]], "Base\nAdjusted")
  expect_equal(table_text[["n"]], "100\n100")
  expect_equal(table_text[["note"]], "\nPrimary")

  estimate_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = "estimate"
  )
  expect_false(grepl("Base:", estimate_spec$table_data$text, fixed = TRUE))
  expect_equal(length(strsplit(estimate_spec$table_data$text, "\n", fixed = TRUE)[[1L]]), 2L)
})

test_that("grouped table text uses the same vertical dodge as plot points", {
  data <- data.frame(
    term = rep(c("Age", "BMI"), each = 2),
    estimate = c(0.2, 0.4, -0.1, 0.1),
    conf.low = c(0.1, 0.3, -0.2, 0),
    conf.high = c(0.3, 0.5, 0, 0.2),
    model = rep(c("A", "B"), 2)
  )
  plot <- ggforestplot(data, group = "model", dodge_width = 0.8)
  out <- add_forest_table(plot, columns = c("term", "group", "estimate"))
  table_plot <- out$patches$plots[[1L]]
  plot_build <- ggplot2::ggplot_build(plot)
  table_build <- ggplot2::ggplot_build(table_plot)
  point_layer <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  ))[[1L]]
  text_layers <- which(vapply(
    table_plot$layers,
    function(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  ))
  point_data <- plot_build$data[[point_layer]]
  text_data <- table_build$data[text_layers]
  group_layer <- which(vapply(
    text_data,
    function(layer) any(layer$label %in% c("A", "B")),
    logical(1)
  ))[[1L]]
  group_text <- text_data[[group_layer]]
  colour_scale <- plot_build$plot$scales$get_scales("colour")

  for (model in c("A", "B")) {
    point_y <- point_data$y[point_data$colour == colour_scale$map(model)]
    group_y <- group_text$y[group_text$label == model]
    expect_equal(sort(group_y), sort(point_y))
  }
  expect_false(any(grepl("\n", group_text$label, fixed = TRUE)))
})

test_that("bound model tables omit prefixes when the model column is omitted", {
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
  expect_false(grepl("Fully Adjusted:", p_text, fixed = TRUE))
  expect_equal(p_text, "0.002")
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

  metadata <- forest_metadata(out)
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$axis_transform, "log10")
  expect_equal(metadata$reference_value, 1)
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
