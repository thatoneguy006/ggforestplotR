test_that("ggforestplot returns a ggplot object for tidy input", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

  p <- ggforestplot(raw)

  expect_s3_class(p, "ggplot")
})

test_that("ggforestplot can facet grouped rows and add stripes", {
  raw <- data.frame(
    term = c("Age", "BMI", "Stage II", "Stage III"),
    estimate = c(0.3, -0.2, 0.5, 0.8),
    conf.low = c(0.1, -0.4, 0.2, 0.4),
    conf.high = c(0.5, 0.0, 0.8, 1.2),
    section = c("Clinical", "Clinical", "Tumor", "Tumor")
  )

  p <- ggforestplot(raw, grouping = "section", striped_rows = TRUE)
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(built$data[[1]]), 2L)
  expect_equal(nrow(built$layout$layout), 2L)
})

test_that("ggforestplot can attach a left-side summary table", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98)
  )

  p <- ggforestplot(
    raw,
    n = "sample_size",
    table_position = "left",
    table_estimate_label = "Beta"
  )

  expect_s3_class(p, "gtable")
})

test_that("ggforestplot can attach a right-side grouped summary table", {
  raw <- data.frame(
    term = rep(c("Age", "BMI"), 2),
    estimate = c(0.3, -0.2, 0.2, -0.1),
    conf.low = c(0.1, -0.4, 0.0, -0.3),
    conf.high = c(0.5, 0.0, 0.4, 0.1),
    model = rep(c("Model A", "Model B"), each = 2),
    section = rep(c("Clinical", "Clinical"), 2),
    sample_size = c(120, 118, 120, 118)
  )

  p <- ggforestplot(
    raw,
    group = "model",
    grouping = "section",
    n = "sample_size",
    striped_rows = TRUE,
    table_position = "right",
    table_estimate_label = "HR"
  )

  expect_s3_class(p, "gtable")
})

test_that("ggforestplot validates N table requests", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_error(
    ggforestplot(raw, table_position = "left", table_show_n = TRUE),
    "requires an `n` column"
  )
})

test_that("ggforestplot enforces positive values for exponentiated plots", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_s3_class(ggforestplot(raw, exponentiate = TRUE), "ggplot")
})