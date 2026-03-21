test_that("ggforestplot defaults staple width to 0.2", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

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
      zero_line = FALSE
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

  stripe_layers <- Filter(function(x) all(c("xmin", "xmax", "ymin", "ymax") %in% names(x)), built$data)

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
})

test_that("ggforestplot allows grouping strip labels on the right", {
  raw <- data.frame(
    term = c("Age", "BMI", "Stage II", "Stage III"),
    estimate = c(0.3, -0.2, 0.5, 0.8),
    conf.low = c(0.1, -0.4, 0.2, 0.4),
    conf.high = c(0.5, 0.0, 0.8, 1.2),
    section = c("Clinical", "Clinical", "Tumor", "Tumor")
  )

  p <- ggforestplot(raw, grouping = "section", grouping_strip_position = "right")
  table_spec <- build_forest_table_data(p$ggforestplotR_state$forest_data)
  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = p$ggforestplotR_state$stripe_data,
    has_groupings = p$ggforestplotR_state$has_groupings,
    grouping_strip_position = p$ggforestplotR_state$grouping_strip_position
  )

  expect_equal(p$facet$params$strip.position, "right")
  expect_equal(p$ggforestplotR_state$grouping_strip_position, "right")
  expect_equal(table_plot$facet$params$strip.position, "right")
})

test_that("forest table centers the Term header and text", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

  p <- ggforestplot(raw)
  table_spec <- build_forest_table_data(p$ggforestplotR_state$forest_data)
  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = p$ggforestplotR_state$stripe_data
  )

  expect_equal(table_spec$header_positions[1], table_spec$positions[1])
  expect_equal(table_plot$layers[[1]]$aes_params$hjust, 0.5)
  expect_equal(table_plot$theme$axis.text.x.top$hjust, 0.5)
})

test_that("add_forest_table supports explicit side-table column order", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )

  p <- ggforestplot(raw, n = "sample_size", p.value = "p_value")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    term_header = "Term",
    n_header = "N",
    estimate_label = "Beta",
    p_header = "P-value",
    columns = c("n", "term", "estimate", "p")
  )

  expect_equal(table_spec$column_keys, c("n", "term", "estimate", "p"))
  expect_equal(table_spec$headers, c("N", "Term", "Beta (95% CI)", "P-value"))
  expect_s3_class(
    add_forest_table(
      p,
      position = "left",
      columns = c("n", "term", "estimate", "p")
    ),
    "patchwork"
  )
})

test_that("forest table can draw horizontal separator lines only", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98)
  )

  p <- ggforestplot(raw, n = "sample_size")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    show_n = TRUE
  )
  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = p$ggforestplotR_state$stripe_data,
    grid_lines = TRUE,
    grid_line_size = 0.4,
    grid_line_linetype = 2
  )

  built <- ggplot2::ggplot_build(table_plot)
  hline_layers <- Filter(function(x) "yintercept" %in% names(x), built$data)
  segment_layers <- Filter(function(x) all(c("x", "xend", "y", "yend") %in% names(x)), built$data)
  vline_layers <- Filter(function(x) "xintercept" %in% names(x), built$data)

  expect_equal(length(hline_layers), 1L)
  expect_equal(length(segment_layers), 0L)
  expect_equal(length(vline_layers), 0L)
  expect_equal(hline_layers[[1]]$yintercept, c(0.5, 1.5, 2.5, 3.5))
  expect_true(all(hline_layers[[1]]$colour == "black"))
  expect_true(all(hline_layers[[1]]$linetype == 2))
})






test_that("add_forest_table can show p-values to the right of estimates", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    p_value = c(0.012, 0.031, 0.004)
  )

  p <- ggforestplot(raw, p.value = "p_value")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    show_p = TRUE,
    estimate_label = "Beta"
  )

  expect_equal(table_spec$headers, c("Term", "Beta (95% CI)", "P-value"))
  expect_true(any(table_spec$table_data$column_key == "p"))
  expect_true(any(grepl("0\\.012", table_spec$table_data$text, fixed = FALSE)))
})

test_that("add_forest_table validates p-value table requests", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_error(
    add_forest_table(ggforestplot(raw), position = "left", show_p = TRUE),
    "requires a `p.value` column"
  )
})

test_that("add_split_table requires left and right table columns", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0),
    sample_size = c(120, 115)
  )

  p <- ggforestplot(raw, n = "sample_size")

  expect_error(
    add_split_table(p, show_terms = FALSE, show_n = FALSE),
    "left-side column"
  )

  expect_error(
    add_split_table(p, show_estimate = FALSE, show_p = FALSE),
    "right-side column"
  )
})

test_that("add_split_table accepts explicit left and right columns by name", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )

  out <- ggforestplot(raw, n = "sample_size", p.value = "p_value") +
    add_split_table(
      left_columns = c("term", "n"),
      right_columns = c("estimate", "p")
    )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
})

test_that("add_split_table accepts explicit left and right columns by position", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )

  out <- ggforestplot(raw, n = "sample_size", p.value = "p_value") +
    add_split_table(
      left_columns = c(1, 2),
      right_columns = c(3, 4)
    )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
})

test_that("add_split_table removes panel border and keeps x-axis line", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )

  out <- add_split_table(
    ggforestplot(raw, n = "sample_size", p.value = "p_value") + ggplot2::labs(title = "Split"),
    left_columns = c("term", "n"),
    right_columns = c("estimate", "p")
  )

  center_plot <- out$patches$plots[[2]]

  expect_s3_class(center_plot$theme$panel.border, "element_blank")
  expect_s3_class(center_plot$theme$axis.line.x, "element_line")
  expect_s3_class(center_plot$theme$axis.line.y, "element_blank")
  expect_s3_class(center_plot$theme$panel.grid.major, "element_blank")
  expect_s3_class(center_plot$theme$panel.grid.minor, "element_blank")
})

test_that("add_split_table uses split-specific alignment and no grid lines", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )

  p <- ggforestplot(raw, n = "sample_size", p.value = "p_value")
  state <- p$ggforestplotR_state
  left_spec <- build_forest_table_data(state$forest_data, columns = c("term", "n"))
  right_spec <- build_forest_table_data(state$forest_data, columns = c("estimate", "p"))
  left_spec <- layout_split_table_spec(left_spec, text_size = 3.2, alignment = "left")
  right_spec <- layout_split_table_spec(right_spec, text_size = 3.2, alignment = "right")
  left_plot <- build_forest_table_plot(
    table_spec = left_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = "left",
    striped_rows = TRUE,
    grid_lines = FALSE,
    x_expand = c(0.05, 0.35),
    plot_margin = ggplot2::margin(5.5, 0, 5.5, 5.5),
    text_hjust = 0,
    header_hjust = 0
  )
  right_plot <- build_forest_table_plot(
    table_spec = right_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = "right",
    striped_rows = TRUE,
    grid_lines = FALSE,
    x_expand = c(0.1, 0.35),
    plot_margin = ggplot2::margin(5.5, 5.5, 5.5, 0),
    text_hjust = 1,
    header_hjust = 1
  )

  left_built <- ggplot2::ggplot_build(left_plot)
  right_built <- ggplot2::ggplot_build(right_plot)

  expect_equal(left_plot$layers[[2]]$aes_params$hjust, 0)
  expect_equal(right_plot$layers[[2]]$aes_params$hjust, 1)
  expect_equal(left_plot$theme$axis.text.x.top$hjust, 0)
  expect_equal(right_plot$theme$axis.text.x.top$hjust, 1)
  expect_length(Filter(function(x) "yintercept" %in% names(x), left_built$data), 0L)
  expect_length(Filter(function(x) "yintercept" %in% names(x), right_built$data), 0L)
})


test_that("add_split_table sizes panels from split column counts", {
  raw <- data.frame(
    term = c("Very long predictor name", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )

  out_equal <- ggforestplot(raw, n = "sample_size", p.value = "p_value") +
    add_split_table(left_columns = c("term", "n"), right_columns = c("estimate", "p"))
  widths_equal <- out_equal$patches$layout$widths

  out_unequal <- ggforestplot(raw, n = "sample_size", p.value = "p_value") +
    add_split_table(left_columns = "term", right_columns = c("estimate", "p"))
  widths_unequal <- out_unequal$patches$layout$widths

  out_three <- ggforestplot(raw, n = "sample_size", p.value = "p_value") +
    add_split_table(left_columns = c("term", "n", "p"), right_columns = "estimate")
  widths_three <- out_three$patches$layout$widths

  expect_equal(widths_equal, c(2.5, 2.5, 2.5))
  expect_equal(widths_unequal, c(1.25, 2.5, 2.5))
  expect_equal(widths_three, c(2.5 * (4 / 3), 2.5, 1.25))
  expect_equal(split_table_width_multiplier(1), 0.5)
  expect_equal(split_table_width_multiplier(2), 1)
  expect_equal(split_table_width_multiplier(3), 4 / 3)
})

