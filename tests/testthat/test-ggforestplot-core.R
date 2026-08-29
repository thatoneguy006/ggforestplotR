test_that("ggforestplot defaults staple width to 0.2", {
  raw <- make_simple_forest_data()

  built <- ggplot2::ggplot_build(ggforestplot(raw))

  expect_true(all(built$data[[1]]$width == 0.2))
})

test_that("deprecated ggforestplot line_size argument warns", {
  raw <- make_simple_forest_data()

  expect_warning(
    ggforestplot(raw, line_size = 0.8),
    "`line_size` is deprecated"
  )
  expect_error(
    ggforestplot(raw, linewidth = 0.8, line_size = 0.6),
    "Use only one of"
  )
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

test_that("ggforestplot can draw striped rows on exponentiated plots", {
  raw <- data.frame(
    term = c("Treatment", "Biomarker"),
    estimate = c(1.2, 0.8),
    conf.low = c(0.9, 0.6),
    conf.high = c(1.6, 1.1)
  )

  expect_no_warning({
    built <- ggplot2::ggplot_build(
      ggforestplot(raw, exponentiate = TRUE, striped_rows = TRUE)
    )
  })

  stripe_layers <- Filter(
    function(x) all(c("xmin", "xmax", "ymin", "ymax") %in% names(x)),
    built$data
  )

  expect_true(length(stripe_layers) >= 1L)
  expect_true(all(is.finite(stripe_layers[[1]]$xmin)))
  expect_true(all(is.finite(stripe_layers[[1]]$xmax)))

  p <- ggforestplot(raw, exponentiate = TRUE, striped_rows = TRUE)
  expected_limits <- default_plot_background_limits(
    p$ggforestplotR_state$forest_data,
    exponentiate = TRUE,
    include_zero = TRUE
  )

  expect_equal(p$scales$get_scales("x")$limits, log10(expected_limits))

  p_custom <- suppressMessages(
    ggforestplot(raw, exponentiate = TRUE, striped_rows = TRUE) +
      ggplot2::scale_x_log10(limits = c(0.5, 2), breaks = c(0.5, 1, 2))
  )
  stripe_index <- p_custom$ggforestplotR_state$stripe_layer_index
  custom_stripe_data <- p_custom$layers[[stripe_index]]$data

  expect_equal(p_custom$scales$get_scales("x")$limits, log10(c(0.5, 2)))
  expect_equal(p_custom$scales$get_scales("x")$breaks, c(0.5, 1, 2))
  expect_equal(unique(custom_stripe_data$xmin), 0.5)
  expect_equal(unique(custom_stripe_data$xmax), 2)

  p_partial <- suppressMessages(
    p + ggplot2::scale_x_log10(limits = c(NA, 2))
  )
  partial_index <- p_partial$ggforestplotR_state$stripe_layer_index
  partial_stripe_data <- p_partial$layers[[partial_index]]$data
  original_index <- p$ggforestplotR_state$stripe_layer_index

  expect_equal(p_partial$scales$get_scales("x")$limits[2], log10(2))
  expect_equal(partial_stripe_data$xmin, p$layers[[original_index]]$data$xmin)
  expect_equal(partial_stripe_data$xmax, 2)

  p_breaks <- suppressMessages(
    p + ggplot2::scale_x_log10(breaks = c(0.5, 1, 2))
  )

  expect_no_warning(ggplot2::ggplot_build(p_breaks))
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

test_that("ggforestplot allows facet strip labels on the right", {
  raw <- data.frame(
    term = c("Age", "BMI", "Stage II", "Stage III"),
    estimate = c(0.3, -0.2, 0.5, 0.8),
    conf.low = c(0.1, -0.4, 0.2, 0.4),
    conf.high = c(0.5, 0.0, 0.8, 1.2),
    section = c("Clinical", "Clinical", "Tumor", "Tumor")
  )

  p <- ggforestplot(raw, facet = "section", facet_strip_position = "right")
  table_spec <- build_forest_table_data(p$ggforestplotR_state$forest_data)
  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = p$ggforestplotR_state$stripe_data,
    has_groupings = p$ggforestplotR_state$has_groupings,
    facet_strip_position = p$ggforestplotR_state$facet_strip_position
  )

  expect_equal(p$facet$params$strip.position, "right")
  expect_equal(p$ggforestplotR_state$facet_strip_position, "right")
  expect_equal(table_plot$facet$params$strip.position, "right")
})

test_that("ggforestplot can sort terms with grouped sections", {
  raw <- data.frame(
    term = c("Age", "BMI", "Stage II", "Stage III"),
    estimate = c(0.3, -0.2, 0.5, 0.8),
    conf.low = c(0.1, -0.4, 0.2, 0.4),
    conf.high = c(0.5, 0.0, 0.8, 1.2),
    section = c("Clinical", "Clinical", "Tumor", "Tumor")
  )

  p <- ggforestplot(
    raw,
    facet = "section",
    striped_rows = TRUE,
    stripe_fill = "grey94",
    facet_strip_position = "right",
    sort_terms = "descending"
  )

  expect_equal(
    as.character(p$ggforestplotR_state$forest_data$term),
    c("Age", "BMI", "Stage III", "Stage II")
  )
  expect_equal(p$facet$params$strip.position, "right")
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

test_that("ggforestplot truncates confidence intervals on exponentiated plots", {
  raw <- data.frame(
    term = c("Treatment", "Biomarker"),
    estimate = c(1.2, 0.8),
    conf.low = c(0.2, 0.6),
    conf.high = c(4.0, 1.1)
  )

  p <- ggforestplot(raw, exponentiate = TRUE, ci_limits = c(0.5, 2))
  built <- ggplot2::ggplot_build(p)
  errorbar_layers <- Filter(
    function(layer) all(c("xmin", "xmax", "width") %in% names(layer)),
    built$data
  )
  truncated_interval <- Filter(
    function(layer) all(layer$width == 0),
    errorbar_layers
  )[[1]]
  complete_interval <- Filter(
    function(layer) all(layer$width == 0.2) && any(layer$xmin != layer$xmax),
    errorbar_layers
  )[[1]]

  expect_equal(p$scales$get_scales("x")$limits, log10(c(0.5, 2)))
  expect_equal(truncated_interval$xmin, log10(0.5))
  expect_equal(truncated_interval$xmax, log10(2.0))
  expect_equal(complete_interval$xmin, log10(0.6))
  expect_equal(complete_interval$xmax, log10(1.1))
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

test_that("ggforestplot relabels terms with a named vector", {
  raw <- data.frame(
    term = c("age", "bmi", "treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6)
  )

  p <- ggforestplot(
    raw,
    term_labels = c(age = "Age, years", treatment = "Treatment arm")
  )

  expect_equal(
    as.character(p$ggforestplotR_state$forest_data$label),
    c("Age, years", "bmi", "Treatment arm")
  )
})

test_that("ggforestplot supports reference line naming and values", {
  raw <- make_simple_forest_data()

  p <- ggforestplot(
    raw,
    ref_line = 0.25,
    ref_label = "Null",
    ref_linetype = 3,
    ref_color = "red"
  )
  built <- ggplot2::ggplot_build(p)
  vline_layers <- Filter(function(x) "xintercept" %in% names(x), built$data)
  label_layers <- Filter(
    function(x) "label" %in% names(x) && any(x$label == "Null"),
    built$data
  )

  expect_equal(vline_layers[[1]]$xintercept, 0.25)
  expect_equal(vline_layers[[1]]$linetype, 3)
  expect_equal(vline_layers[[1]]$colour, "red")
  expect_equal(label_layers[[1]]$label, "Null")
  expect_equal(p$ggforestplotR_state$defaults$ref_line, 0.25)

  hidden <- ggplot2::ggplot_build(ggforestplot(raw, ref_line = NULL))
  hidden_vline_layers <- Filter(
    function(x) "xintercept" %in% names(x),
    hidden$data
  )

  expect_length(hidden_vline_layers, 0L)
  expect_error(
    ggforestplot(raw, ref_line = "Null"),
    "`ref_line` must be a single numeric value or `NULL`."
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

