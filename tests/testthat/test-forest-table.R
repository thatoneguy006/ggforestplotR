test_that("add_forest_table validates N table requests", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_error(
    add_forest_table(ggforestplot(raw), position = "left", columns = "n"),
    "requires an `n` column"
  )
})

test_that("deprecated ggforestplot facet arguments warn", {
  raw <- make_simple_forest_data(
    section = c("Clinical", "Tumor")
  )

  expect_warning(
    ggforestplot(raw, grouping = "section"),
    "`grouping` is deprecated"
  )
  expect_warning(
    ggforestplot(raw, facet = "section", grouping_strip_position = "right"),
    "`grouping_strip_position` is deprecated"
  )
  expect_error(
    ggforestplot(raw, facet = "section", grouping = "section"),
    "Use only one of"
  )
  expect_error(
    ggforestplot(raw, facet_strip_position = "right", grouping_strip_position = "right"),
    "Use only one of"
  )
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

test_that("table helpers use stripe alpha from plots and overrides", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6)
  )

  p <- ggforestplot(raw, striped_rows = TRUE, stripe_alpha = 0.35)
  table_out <- add_forest_table(p, position = "left")
  table_plot <- table_out$patches$plots[[1]]
  split_out <- add_split_table(p, stripe_alpha = 0.6)
  left_table <- split_out$patches$plots[[1]]

  expect_true(all(ggplot2::ggplot_build(table_plot)$data[[1]]$alpha == 0.35))
  expect_true(all(ggplot2::ggplot_build(left_table)$data[[1]]$alpha == 0.6))
})

test_that("add_forest_table requires a ggforestplot object", {
  raw <- data.frame(x = 1:2, y = 1:2)
  p <- ggplot2::ggplot(raw, ggplot2::aes(x, y)) + ggplot2::geom_point()

  expect_error(
    add_forest_table(p),
    "must be created by"
  )
})

test_that("forest tables inherit y-axis order from the plot scale", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6)
  )

  p <- suppressMessages(
    ggforestplot(raw) +
      ggplot2::scale_y_discrete(limits = c("Treatment", "Age"))
  )
  aligned_state <- align_forest_state_to_plot_y_scale(p$ggforestplotR_state, p)
  table_spec <- build_forest_table_data(aligned_state$forest_data)
  out <- p + add_forest_table()
  table_plot <- out$patches$plots[[1]]

  expect_equal(
    levels(aligned_state$forest_data$row_key),
    c("Treatment", "Age")
  )
  expect_equal(
    levels(table_spec$table_data$row_key),
    c("Treatment", "Age")
  )
  expect_false(any(table_spec$table_data$text == "BMI"))
  expect_equal(levels(table_plot$data$row_key), c("Treatment", "Age"))
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

  p_custom <- suppressMessages(
    ggforestplot(raw, exponentiate = TRUE, striped_rows = TRUE) +
      ggplot2::scale_x_log10(limits = c(0.5, 2), breaks = c(0.5, 1, 2))
  )
  custom_stripe_data <- p_custom$layers[[p_custom$ggforestplotR_state$stripe_layer_index]]$data

  expect_equal(p_custom$scales$get_scales("x")$limits, log10(c(0.5, 2)))
  expect_equal(p_custom$scales$get_scales("x")$breaks, c(0.5, 1, 2))
  expect_equal(unique(custom_stripe_data$xmin), 0.5)
  expect_equal(unique(custom_stripe_data$xmax), 2)

  p_partial <- suppressMessages(
    p + ggplot2::scale_x_log10(limits = c(NA, 2))
  )
  partial_stripe_data <- p_partial$layers[[p_partial$ggforestplotR_state$stripe_layer_index]]$data

  expect_equal(p_partial$scales$get_scales("x")$limits[2], log10(2))
  expect_equal(partial_stripe_data$xmin, p$layers[[p$ggforestplotR_state$stripe_layer_index]]$data$xmin)
  expect_equal(partial_stripe_data$xmax, 2)

  p_breaks <- suppressMessages(
    p + ggplot2::scale_x_log10(breaks = c(0.5, 1, 2))
  )

  expect_no_warning(
    ggplot2::ggplot_build(p_breaks)
  )
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
    grouping_strip_position = p$ggforestplotR_state$facet_strip_position
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

test_that("forest table centers the Term header and text", {
  raw <- make_simple_forest_data()

  p <- ggforestplot(raw)
  table_spec <- layout_center_table_spec(
    build_forest_table_data(p$ggforestplotR_state$forest_data)
  )
  table_plot <- build_forest_table_plot(
    table_spec = table_spec,
    stripe_data = p$ggforestplotR_state$stripe_data
  )

  expect_equal(table_spec$header_positions[1], table_spec$positions[1])
  expect_equal(table_plot$layers[[1]]$aes_params$hjust, 0.5)
  expect_equal(table_plot$theme$axis.text.x.top$hjust, 0.5)
})

test_that("forest table supports header size and font customization", {
  raw <- make_simple_forest_data()

  p <- ggforestplot(raw)
  out <- add_forest_table(
    p,
    position = "left",
    header_text_size = 14,
    header_fontface = "italic",
    header_family = "mono"
  )

  table_plot <- out$patches$plots[[1]]

  expect_equal(table_plot$theme$axis.text.x.top$size, 14)
  expect_equal(table_plot$theme$axis.text.x.top$face, "italic")
  expect_equal(table_plot$theme$axis.text.x.top$family, "mono")
})

test_that("add_forest_table supports explicit side-table column order", {
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
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    term_header = "Term",
    n_header = "N",
    events_header = "Events",
    estimate_label = "Beta",
    p_header = "P-value",
    columns = c("n", "events", "term", "estimate", "p")
  )

  expect_equal(table_spec$column_keys, c("n", "events", "term", "estimate", "p"))
  expect_equal(table_spec$headers, c("N", "Events", "Term", "Beta (95% CI)", "P-value"))
  expect_s3_class(
    add_forest_table(
      p,
      position = "left",
      columns = c("n", "events", "term", "estimate", "p")
    ),
    "patchwork"
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
  label_layers <- Filter(function(x) "label" %in% names(x) && any(x$label == "Null"), built$data)

  expect_equal(vline_layers[[1]]$xintercept, 0.25)
  expect_equal(vline_layers[[1]]$linetype, 3)
  expect_equal(vline_layers[[1]]$colour, "red")
  expect_equal(label_layers[[1]]$label, "Null")
  expect_equal(p$ggforestplotR_state$defaults$ref_line, 0.25)

  hidden <- ggplot2::ggplot_build(ggforestplot(raw, ref_line = NULL))
  hidden_vline_layers <- Filter(function(x) "xintercept" %in% names(x), hidden$data)

  expect_length(hidden_vline_layers, 0L)
  expect_error(
    ggforestplot(raw, ref_line = "Null"),
    "`ref_line` must be a single numeric value or `NULL`."
  )
})

test_that("add_forest_table supports arbitrary preserved columns", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.345, -0.234),
    conf.low = c(0.12, -0.43),
    conf.high = c(0.57, -0.04),
    upper_bound = c(0.57, -0.04),
    adjustment = c("Clinical", "Clinical")
  )

  p <- ggforestplot(raw)
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("term", "adjustment", "upper_bound", "estimate")
  )

  expect_equal(table_spec$column_keys, c("term", "adjustment", "upper_bound", "estimate"))
  expect_equal(table_spec$headers, c("Term", "adjustment", "upper_bound", "Estimate (95% CI)"))
  expect_true(any(table_spec$table_data$column_key == "adjustment"))
  expect_true(any(table_spec$table_data$text == "Clinical"))
  expect_true(any(table_spec$table_data$column_key == "upper_bound"))
  expect_true(any(table_spec$table_data$text == "0.57"))
  positioned_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c(1, 5, 6)
  )

  expect_equal(positioned_spec$column_keys, c("term", "upper_bound", "adjustment"))
  expect_s3_class(
    add_forest_table(p, columns = c("term", "upper_bound", "adjustment")),
    "patchwork"
  )
  expect_s3_class(
    add_forest_table(p, columns = c(1, 5, 6)),
    "patchwork"
  )
})

test_that("forest table supports custom column labels", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.345, -0.234),
    conf.low = c(0.12, -0.43),
    conf.high = c(0.57, -0.04),
    adjustment = c("Clinical", "Demographic"),
    p_value = c(0.012, 0.031)
  )

  p <- ggforestplot(raw, p.value = "p_value")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("term", "adjustment", "estimate", "p"),
    column_labels = c(
      term = "Variable",
      adjustment = "Adjusted for",
      estimate = "Beta (95% CI)",
      p = "P"
    )
  )

  expect_equal(table_spec$headers, c("Variable", "Adjusted for", "Beta (95% CI)", "P"))
  expect_s3_class(
    add_forest_table(
      p,
      columns = c("term", "adjustment", "estimate", "p"),
      column_labels = c(adjustment = "Adjusted for", estimate = "Beta (95% CI)")
    ),
    "patchwork"
  )
})

test_that("forest table validates column labels", {
  raw <- data.frame(
    term = "Age",
    estimate = 0.345,
    conf.low = 0.12,
    conf.high = 0.57
  )

  p <- ggforestplot(raw)

  expect_error(
    build_forest_table_data(
      p$ggforestplotR_state$forest_data,
      columns = "term",
      column_labels = c("Variable")
    ),
    "`column_labels` must be a named vector."
  )
  expect_error(
    build_forest_table_data(
      p$ggforestplotR_state$forest_data,
      columns = "term",
      column_labels = c(missing_column = "Missing")
    ),
    "Unsupported table columns"
  )
})

test_that("forest table columns can use original dataframe vectors", {
  raw <- data.frame(
    variable = c("Age", "BMI"),
    beta = c(0.345, -0.234),
    lower = c(0.12, -0.43),
    upper = c(0.57, -0.04),
    adjustment = factor(c("Clinical", "Demographic")),
    review_date = as.Date(c("2026-01-15", "2026-02-20")),
    selected = c(TRUE, FALSE),
    group = c("Reviewer A", "Reviewer B")
  )

  p <- ggforestplot(
    raw,
    term = "variable",
    estimate = "beta",
    conf.low = "lower",
    conf.high = "upper"
  )
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("variable", "beta", "lower", "upper", "adjustment", "review_date", "selected", "group")
  )

  expect_equal(
    table_spec$column_keys,
    c("variable", "beta", "lower", "upper", "adjustment", "review_date", "selected", "group")
  )
  expect_true(any(table_spec$table_data$text == "Age"))
  expect_true(any(table_spec$table_data$text == "0.345"))
  expect_true(any(table_spec$table_data$text == "0.12"))
  expect_true(any(table_spec$table_data$text == "0.57"))
  expect_true(any(table_spec$table_data$text == "Clinical"))
  expect_true(any(table_spec$table_data$text == "2026-01-15"))
  expect_true(any(table_spec$table_data$text == "TRUE"))
  expect_true(any(table_spec$table_data$text == "Reviewer A"))
  expect_s3_class(
    add_forest_table(
      p,
      columns = c("variable", "beta", "lower", "upper", "adjustment", "review_date", "selected", "group")
    ),
    "patchwork"
  )
})

test_that("forest table formats estimates, intervals, and p-values separately", {
  raw <- data.frame(
    term = "Age",
    estimate = 0.3456,
    conf.low = 0.1234,
    conf.high = 0.5678,
    p_value = 0.01234
  )

  p <- ggforestplot(raw, p.value = "p_value")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("estimate", "p"),
    estimate_digits = 1,
    interval_digits = 3,
    p_digits = 4
  )

  expect_true(any(table_spec$table_data$text == "0.3 (0.123, 0.568)"))
  expect_true(any(table_spec$table_data$text == "0.0123"))
})

test_that("forest table supports custom estimate format strings", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3456, -0.2345),
    conf.low = c(0.1234, -0.4321),
    conf.high = c(0.5678, -0.0432)
  )

  p <- ggforestplot(raw)
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = "estimate",
    estimate_digits = 1,
    interval_digits = 3,
    estimate_fmt = "{estimate} [{conf.low}, {conf.high}]"
  )
  shorthand_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = "estimate",
    estimate_digits = 1,
    interval_digits = 3,
    estimate_fmt = "{estimate} ({conf.low, conf.high})"
  )

  expect_true(any(table_spec$table_data$text == "0.3 [0.123, 0.568]"))
  expect_true(any(shorthand_spec$table_data$text == "0.3 (0.123, 0.568)"))
  expect_s3_class(
    add_forest_table(
      p,
      columns = "estimate",
      estimate_digits = 1,
      interval_digits = 3,
      estimate_fmt = "{estimate} [{conf.low}, {conf.high}]"
    ),
    "patchwork"
  )
})

test_that("forest table can split estimates and confidence intervals", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3456, -0.2345),
    conf.low = c(0.1234, -0.4321),
    conf.high = c(0.5678, -0.0432)
  )

  p <- ggforestplot(raw)
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("estimate", "ci"),
    estimate_digits = 1,
    interval_digits = 3
  )
  custom_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("estimate", "ci"),
    estimate_digits = 1,
    interval_digits = 3,
    ci_fmt = "{conf.low} to {conf.high}"
  )

  expect_equal(table_spec$column_keys, c("estimate", "ci"))
  expect_equal(table_spec$headers, c("Estimate", "95% CI"))
  expect_true(any(table_spec$table_data$column_key == "estimate" & table_spec$table_data$text == "0.3"))
  expect_true(any(table_spec$table_data$column_key == "ci" & table_spec$table_data$text == "(0.123, 0.568)"))
  expect_true(any(custom_spec$table_data$column_key == "ci" & custom_spec$table_data$text == "0.123 to 0.568"))
  expect_s3_class(
    add_forest_table(
      p,
      columns = c("term", "estimate", "ci"),
      estimate_digits = 1,
      interval_digits = 3,
      column_labels = c(estimate = "Beta", ci = "95% CI")
    ),
    "patchwork"
  )
})

test_that("confidence bound column names alias to the CI table column", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3456, -0.2345),
    conf.low = c(0.1234, -0.4321),
    conf.high = c(0.5678, -0.0432)
  )

  p <- ggforestplot(raw)
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("estimate", "conf.low", "conf.high"),
    estimate_digits = 1,
    interval_digits = 3,
    column_labels = c(conf.high = "CI")
  )

  expect_equal(table_spec$column_keys, c("estimate", "ci"))
  expect_equal(table_spec$headers, c("Estimate", "CI"))
  expect_true(any(table_spec$table_data$column_key == "estimate" & table_spec$table_data$text == "0.3"))
  expect_true(any(table_spec$table_data$column_key == "ci" & table_spec$table_data$text == "(0.123, 0.568)"))
  expect_s3_class(
    add_forest_table(
      p,
      columns = c("term", "estimate", "conf.low", "conf.high"),
      column_labels = c(conf.high = "CI")
    ),
    "patchwork"
  )
})

test_that("forest table validates confidence interval format strings", {
  raw <- data.frame(
    term = "Age",
    estimate = 0.345,
    conf.low = 0.12,
    conf.high = 0.57
  )

  p <- ggforestplot(raw)

  expect_error(
    build_forest_table_data(
      p$ggforestplotR_state$forest_data,
      columns = "ci",
      ci_fmt = c("{conf.low}", "{conf.high}")
    ),
    "`ci_fmt` must be a single character string."
  )
})

test_that("add_forest_table can show an events column", {
  raw <- data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    event_count = c(42, 39, 31)
  )

  p <- ggforestplot(raw, events = "event_count")
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("term", "events", "estimate")
  )

  expect_true(any(table_spec$column_keys == "events"))
  expect_true(any(table_spec$table_data$column_key == "events"))
  expect_true(any(table_spec$table_data$text == "42"))
})

test_that("add_forest_table validates events table requests", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_error(
    add_forest_table(ggforestplot(raw), position = "left", columns = "events"),
    "requires an `events` column"
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
    columns = c("term", "n", "estimate")
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
    columns = c("term", "estimate", "p"),
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
    add_forest_table(ggforestplot(raw), position = "left", columns = "p"),
    "requires a `p.value` column"
  )
})

test_that("deprecated table digit argument warns", {
  raw <- make_simple_forest_data(
    p_value = c(0.012, 0.031)
  )

  p <- ggforestplot(raw, p.value = "p_value")

  expect_warning(
    add_forest_table(p, digits = 3),
    "`digits` is deprecated"
  )
  expect_warning(
    add_split_table(p, digits = 3),
    "`digits` is deprecated"
  )
})

