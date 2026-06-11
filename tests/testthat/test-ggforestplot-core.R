test_that("ggforestplot defaults staple width to 0.2", {
  raw <- make_simple_forest_data()

  built <- ggplot2::ggplot_build(ggforestplot(raw))

  expect_true(all(built$data[[1]]$width == 0.2))
})

test_that("ggforestplot can facet grouped rows and add stripes", {
  raw <- data.frame(
    term = c("Age", "BMI", "Smoking", "Stage II", "Stage III", "Nodes"),
    estimate = c(0.3, -0.2, 0.4, 0.5, 0.8, 0.4),
    conf.low = c(0.1, -0.4, 0.2, 0.2, 0.4, 0.1),
    conf.high = c(0.5, 0.0, 0.6, 0.8, 1.2, 0.7),
    section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor", "Tumor")
  )

  p <- ggforestplot(raw, facet = "section", striped_rows = TRUE, stripe_alpha = 0.35)
  built <- ggplot2::ggplot_build(p)
  panel_rows <- lapply(split(as.numeric(built$data[[2]]$y), built$data[[2]]$PANEL), unique)

  expect_equal(nrow(built$data[[1]]), 4L)
  expect_true(all(built$data[[1]]$alpha == 0.35))
  expect_equal(p$ggforestplotR_state$defaults$stripe_alpha, 0.35)
  expect_equal(length(panel_rows), 2L)
  expect_equal(unname(panel_rows[[1]]), c(1, 2, 3))
  expect_equal(unname(panel_rows[[2]]), c(1, 2, 3))
})

test_that("ggforestplot respects factor level order for facets", {
  raw <- data.frame(
    term = c("Age", "BMI", "Stage II", "Stage III"),
    estimate = c(0.3, -0.2, 0.5, 0.8),
    conf.low = c(0.1, -0.4, 0.2, 0.4),
    conf.high = c(0.5, 0.0, 0.8, 1.2),
    section = factor(
      c("Clinical", "Clinical", "Tumor", "Tumor"),
      levels = c("Tumor", "Clinical")
    )
  )

  p <- ggforestplot(raw, facet = "section")
  built <- ggplot2::ggplot_build(p)
  panel_order <- as.character(built$layout$layout$grouping_panel)
  out <- p + add_forest_table()
  table_plot <- out$patches$plots[[1]]

  expect_equal(panel_order, c("Tumor", "Clinical"))
  expect_equal(levels(p$ggforestplotR_state$forest_data$grouping_panel), c("Tumor", "Clinical"))
  expect_equal(levels(p$ggforestplotR_state$stripe_data$grouping_panel), c("Tumor", "Clinical"))
  expect_equal(levels(table_plot$data$grouping_panel), c("Tumor", "Clinical"))
})

test_that("faceted ggforestplot supports visible labels in scale_y_discrete limits", {
  raw <- data.frame(
    term = c("Age", "BMI", "Smoking", "Stage II", "Stage III", "Nodes"),
    estimate = c(0.3, -0.2, 0.4, 0.5, 0.8, 0.4),
    conf.low = c(0.1, -0.4, 0.2, 0.2, 0.4, 0.1),
    conf.high = c(0.5, 0.0, 0.6, 0.8, 1.2, 0.7),
    section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor", "Tumor")
  )

  p <- suppressMessages(
    ggforestplot(raw, facet = "section") +
      ggplot2::scale_y_discrete(limits = c("Smoking", "Age", "Stage II"))
  )
  built <- ggplot2::ggplot_build(p)
  panel_limits <- lapply(built$layout$panel_params, function(panel) panel$y$get_limits())
  aligned_state <- align_forest_state_to_plot_y_scale(p$ggforestplotR_state, p)
  out <- p + add_forest_table()
  table_plot <- out$patches$plots[[1]]

  expect_equal(sum(!is.na(built$data[[1]]$y)), 3L)
  expect_equal(sum(!is.na(built$data[[2]]$y)), 3L)
  expect_equal(panel_limits[[1]], c("Smoking", "Age"))
  expect_equal(panel_limits[[2]], "Stage II")
  expect_equal(
    levels(aligned_state$forest_data$row_key),
    c("Smoking", "Age", "Stage II")
  )
  expect_equal(levels(table_plot$data$row_key), c("Smoking", "Age", "Stage II"))
})

test_that("ggforestplot supports point and interval geometry controls", {
  raw <- make_simple_forest_data()

  built <- ggplot2::ggplot_build(
    ggforestplot(raw, point_shape = 17, linewidth = 0.8, staple_width = 0.25)
  )

  expect_true(all(built$data[[2]]$shape == 17))
  expect_true(all(built$data[[1]]$linewidth == 0.8))
  expect_true(all(built$data[[1]]$width == 0.25))
})

test_that("ggforestplot truncates confidence intervals and draws arrows", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(-2.0, -0.4, 0.2),
    conf.high = c(0.5, 2.0, 1.8)
  )

  p <- ggforestplot(raw, ci_limits = c(-1, 1), ci_arrow_length = 0.05)
  built <- ggplot2::ggplot_build(p)
  errorbar_layers <- Filter(
    function(layer) all(c("xmin", "xmax", "width") %in% names(layer)),
    built$data
  )
  segment_layers <- Filter(
    function(layer) all(c("x", "xend", "y", "yend") %in% names(layer)),
    built$data
  )
  staple_layers <- Filter(
    function(layer) all(layer$width == 0.2) && all(layer$xmin == layer$xmax),
    errorbar_layers
  )
  staple_positions <- unlist(lapply(staple_layers, function(layer) layer$xmin), use.names = FALSE)

  expect_equal(p$scales$get_scales("x")$limits, c(-1, 1))
  expect_equal(built$data[[1]]$xmin, c(-1, -0.4, 0.2))
  expect_equal(built$data[[1]]$xmax, c(0.5, 1, 1))
  expect_true(all(built$data[[1]]$width == 0))
  expect_equal(sort(staple_positions), c(-0.4, 0.2, 0.5))
  expect_false(any(staple_positions %in% c(-1, 1)))
  expect_length(segment_layers, 2L)
  expect_true(any(segment_layers[[1]]$xend == -1))
  expect_true(all(segment_layers[[2]]$xend == 1))
  expect_equal(p$ggforestplotR_state$forest_data$conf.low, raw$conf.low)
  expect_equal(p$ggforestplotR_state$forest_data$conf.high, raw$conf.high)
  expect_equal(p$ggforestplotR_state$defaults$ci_limits, c(-1, 1))
})

test_that("ggforestplot can truncate confidence intervals without arrows", {
  raw <- data.frame(
    term = "Age",
    estimate = 0.3,
    conf.low = -2,
    conf.high = 2
  )

  built <- ggplot2::ggplot_build(
    ggforestplot(raw, ci_limits = c(-1, 1), ci_arrows = FALSE)
  )
  segment_layers <- Filter(
    function(layer) all(c("x", "xend", "y", "yend") %in% names(layer)),
    built$data
  )

  expect_equal(built$data[[1]]$xmin, -1)
  expect_equal(built$data[[1]]$xmax, 1)
  expect_length(segment_layers, 0L)
})

test_that("ggforestplot validates confidence interval truncation limits", {
  raw <- data.frame(
    term = "Age",
    estimate = 1.2,
    conf.low = 0.1,
    conf.high = 3
  )

  expect_error(
    ggforestplot(raw, ci_limits = 1),
    "`ci_limits` must be `NULL` or a numeric vector of length 2."
  )
  expect_error(
    ggforestplot(raw, ci_limits = c(1, 1)),
    "`ci_limits` must contain two distinct values."
  )
  expect_error(
    ggforestplot(raw, exponentiate = TRUE, ci_limits = c(0, 2)),
    "`ci_limits` must be positive for exponentiated plots."
  )
  expect_error(
    ggforestplot(raw, ci_limits = c(0, 2), ci_arrows = NA),
    "`ci_arrows` must be `TRUE` or `FALSE`."
  )
  expect_error(
    ggforestplot(raw, ci_limits = c(0, 2), ci_arrow_length = 0),
    "`ci_arrow_length` must be a single positive number."
  )
})

test_that("ggforestplot can draw separator lines for each labeled variable block", {
  raw <- data.frame(
    term = c("race_black", "race_white", "race_other", "age"),
    label = c("Black", "White", "Other", "Age"),
    estimate = c(0.3, 0.1, 0.05, -0.2),
    conf.low = c(0.1, -0.1, -0.15, -0.4),
    conf.high = c(0.5, 0.3, 0.25, 0.0),
    block = c("Race", "Race", "Race", "Age")
  )

  built <- ggplot2::ggplot_build(
    ggforestplot(
      raw,
      label = "label",
      separate_groups = "block",
      separate_lines = TRUE,
      ref_line = NULL
    )
  )

  line_layers <- Filter(function(x) "yintercept" %in% names(x), built$data)

  expect_length(line_layers, 1L)
  expect_equal(line_layers[[1]]$yintercept, c(0.5, 3.5, 4.5))
  expect_true(all(line_layers[[1]]$linetype == 2))
  expect_true(all(line_layers[[1]]$colour == "black"))
})
test_that("ggforestplot prefixes labels for multi-level separator groups", {
  raw <- data.frame(
    term = c("race_black", "race_white", "race_other", "age"),
    label = c("Black", "White", "Other", "Age"),
    estimate = c(0.3, 0.1, 0.05, -0.2),
    conf.low = c(0.1, -0.1, -0.15, -0.4),
    conf.high = c(0.5, 0.3, 0.25, 0.0),
    block = c("Race", "Race", "Race", "Age")
  )

  p <- ggforestplot(raw, label = "label", separate_groups = "block")

  expect_equal(
    as.character(p$ggforestplotR_state$forest_data$label),
    c("Race: Black", "Race: White", "Race: Other", "Age")
  )
})

