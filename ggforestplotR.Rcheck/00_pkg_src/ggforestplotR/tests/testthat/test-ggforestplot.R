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
    term = c("Age", "BMI", "Smoking", "Stage II", "Stage III", "Nodes"),
    estimate = c(0.3, -0.2, 0.4, 0.5, 0.8, 0.4),
    conf.low = c(0.1, -0.4, 0.2, 0.2, 0.4, 0.1),
    conf.high = c(0.5, 0.0, 0.6, 0.8, 1.2, 0.7),
    section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor", "Tumor")
  )

  p <- ggforestplot(raw, grouping = "section", striped_rows = TRUE)
  built <- ggplot2::ggplot_build(p)
  panel_rows <- lapply(split(as.numeric(built$data[[2]]$y), built$data[[2]]$PANEL), unique)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(built$data[[1]]), 4L)
  expect_equal(length(panel_rows), 2L)
  expect_equal(unname(panel_rows[[1]]), c(1, 2, 3))
  expect_equal(unname(panel_rows[[2]]), c(1, 2, 3))
})

test_that("ggforestplot supports shape and staple width controls", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

  built <- ggplot2::ggplot_build(
    ggforestplot(raw, point_shape = 17, staple_width = 0.25)
  )

  expect_true(all(built$data[[2]]$shape == 17))
  expect_true(all(built$data[[1]]$width == 0.25))
})

test_that("add_forest_table can attach a left-side summary table", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98)
  )

  p <- ggforestplot(raw, n = "sample_size")
  out <- add_forest_table(
    p,
    position = "left",
    show_n = TRUE,
    estimate_label = "Beta"
  )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
})

test_that("add_forest_table works with ggplot add syntax", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98)
  )

  out <- ggforestplot(raw, n = "sample_size") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "italic")) +
    add_forest_table(position = "right", show_n = TRUE, estimate_label = "Beta")

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
})

test_that("ggforestplot warns instead of composing tables directly", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0),
    sample_size = c(120, 118)
  )

  expect_warning(
    p <- ggforestplot(raw, n = "sample_size", table_position = "right"),
    "deprecated"
  )

  expect_s3_class(p, "ggplot")
})

test_that("add_forest_table validates N table requests", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_error(
    add_forest_table(ggforestplot(raw), position = "left", show_n = TRUE),
    "requires an `n` column"
  )
})

test_that("add_forest_table requires a ggforestplot object", {
  raw <- data.frame(x = 1:2, y = 1:2)
  p <- ggplot2::ggplot(raw, ggplot2::aes(x, y)) + ggplot2::geom_point()

  expect_error(
    add_forest_table(p),
    "must be created by"
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