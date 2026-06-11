test_that("add_split_table requires left and right table columns", {
  raw <- make_simple_forest_data(
    sample_size = c(120, 115)
  )

  p <- ggforestplot(raw, n = "sample_size")

  expect_error(
    add_split_table(p, left_columns = character()),
    "left-side column"
  )

  expect_error(
    add_split_table(p, right_columns = character()),
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
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )

  out <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(
      left_columns = c("term", "n", "events"),
      right_columns = c("estimate", "p")
    )

  expect_s3_class(out, "patchwork")

  out_with_preserved_columns <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(
      left_columns = c(1, 5, 6),
      right_columns = c(2, 7)
    )

  expect_s3_class(out_with_preserved_columns, "patchwork")
})

test_that("add_split_table supports custom column labels", {
  raw <- make_simple_forest_data(
    sample_size = c(120, 115),
    adjustment = c("Clinical", "Demographic")
  )

  p <- ggforestplot(raw, n = "sample_size")
  state <- p$ggforestplotR_state
  left_spec <- build_forest_table_data(
    state$forest_data,
    columns = c("term", "adjustment"),
    column_labels = c(term = "Variable", adjustment = "Adjusted for")
  )
  right_spec <- build_forest_table_data(
    state$forest_data,
    columns = "estimate",
    column_labels = c(estimate = "Beta (95% CI)")
  )

  expect_equal(left_spec$headers, c("Variable", "Adjusted for"))
  expect_equal(right_spec$headers, "Beta (95% CI)")
  expect_s3_class(
    add_split_table(
      p,
      left_columns = c("term", "adjustment"),
      right_columns = "estimate",
      column_labels = c(term = "Variable", adjustment = "Adjusted for", estimate = "Beta (95% CI)")
    ),
    "patchwork"
  )
})

test_that("add_split_table can split estimates and confidence intervals", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3456, -0.2345),
    conf.low = c(0.1234, -0.4321),
    conf.high = c(0.5678, -0.0432),
    sample_size = c(120, 115)
  )

  p <- ggforestplot(raw, n = "sample_size")
  state <- p$ggforestplotR_state
  right_spec <- build_forest_table_data(
    state$forest_data,
    columns = c("estimate", "ci"),
    estimate_digits = 1,
    interval_digits = 3,
    ci_fmt = "{conf.low} to {conf.high}"
  )

  expect_equal(right_spec$headers, c("Estimate", "95% CI"))
  expect_true(any(right_spec$table_data$column_key == "estimate" & right_spec$table_data$text == "0.3"))
  expect_true(any(right_spec$table_data$column_key == "ci" & right_spec$table_data$text == "0.123 to 0.568"))
  expect_s3_class(
    add_split_table(
      p,
      left_columns = c("term", "n"),
      right_columns = c("estimate", "ci"),
      estimate_digits = 1,
      interval_digits = 3,
      ci_fmt = "{conf.low} to {conf.high}",
      column_labels = c(estimate = "Beta", ci = "95% CI")
    ),
    "patchwork"
  )
})

test_that("add_split_table accepts confidence bound names as CI aliases", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3456, -0.2345),
    conf.low = c(0.1234, -0.4321),
    conf.high = c(0.5678, -0.0432),
    sample_size = c(120, 115)
  )

  p <- ggforestplot(raw, n = "sample_size")
  right_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("estimate", "conf.low", "conf.high"),
    estimate_digits = 1,
    interval_digits = 3
  )

  expect_equal(right_spec$column_keys, c("estimate", "ci"))
  expect_true(any(right_spec$table_data$column_key == "ci" & right_spec$table_data$text == "(0.123, 0.568)"))
  expect_s3_class(
    add_split_table(
      p,
      left_columns = c("term", "n"),
      right_columns = c("estimate", "conf.low", "conf.high"),
      estimate_digits = 1,
      interval_digits = 3
    ),
    "patchwork"
  )
})

test_that("add_split_table accepts explicit left and right columns by position", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )

  out <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(
      left_columns = c(1, 5, 6),
      right_columns = c(2, 7)
    )

  expect_s3_class(out, "patchwork")
})

test_that("add_split_table removes panel border and keeps x-axis line", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )

  out <- add_split_table(
    ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") + ggplot2::labs(title = "Split"),
    left_columns = c("term", "n", "events"),
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
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )

  p <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value")
  state <- p$ggforestplotR_state
  left_spec <- build_forest_table_data(state$forest_data, columns = c("term", "n", "events"))
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

test_that("add_split_table supports header size and font customization", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )

  p <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value")
  state <- p$ggforestplotR_state
  left_spec <- build_forest_table_data(state$forest_data, columns = c("term", "n", "events"))
  right_spec <- build_forest_table_data(state$forest_data, columns = c("estimate", "p"))
  left_spec <- layout_split_table_spec(
    left_spec,
    text_size = 3.2,
    header_text_size = 13,
    header_fontface = "plain",
    header_family = "serif",
    alignment = "left"
  )
  right_spec <- layout_split_table_spec(
    right_spec,
    text_size = 3.2,
    header_text_size = 13,
    header_fontface = "plain",
    header_family = "serif",
    alignment = "right"
  )
  left_plot <- build_forest_table_plot(
    table_spec = left_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = "left",
    header_text_size = 13,
    header_fontface = "plain",
    header_family = "serif"
  )
  right_plot <- build_forest_table_plot(
    table_spec = right_spec,
    stripe_data = state$stripe_data,
    has_groupings = state$has_groupings,
    grouping_strip_position = state$grouping_strip_position,
    table_position = "right",
    header_text_size = 13,
    header_fontface = "plain",
    header_family = "serif"
  )

  expect_equal(left_plot$theme$axis.text.x.top$size, 13)
  expect_equal(left_plot$theme$axis.text.x.top$face, "plain")
  expect_equal(left_plot$theme$axis.text.x.top$family, "serif")
  expect_equal(right_plot$theme$axis.text.x.top$size, 13)
  expect_equal(right_plot$theme$axis.text.x.top$face, "plain")
  expect_equal(right_plot$theme$axis.text.x.top$family, "serif")
})


test_that("add_split_table sizes panels from split column counts", {
  raw <- data.frame(
    term = c("Very long predictor name", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )

  out_equal <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(left_columns = c("term", "events"), right_columns = c("estimate", "p"))
  widths_equal <- out_equal$patches$layout$widths

  out_unequal <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(left_columns = "term", right_columns = c("estimate", "p"))
  widths_unequal <- out_unequal$patches$layout$widths

  out_three <- ggforestplot(raw, n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(left_columns = c("term", "n", "events"), right_columns = "estimate")
  widths_three <- out_three$patches$layout$widths

  expect_equal(widths_equal, c(2.5, 2.5, 2.5))
  expect_equal(widths_unequal, c(1.25, 2.5, 2.5))
  expect_equal(widths_three, c(2.5 * (4 / 3), 2.5, 1.25))
  expect_equal(split_table_width_multiplier(1), 0.5)
  expect_equal(split_table_width_multiplier(2), 1)
  expect_equal(split_table_width_multiplier(3), 4 / 3)
})

test_that("add_split_table validates events requests", {
  raw <- make_simple_forest_data()

  p <- ggforestplot(raw)

  expect_error(
    add_split_table(p, left_columns = c("term", "events"), right_columns = "estimate"),
    "An `events` column is required"
  )
})

