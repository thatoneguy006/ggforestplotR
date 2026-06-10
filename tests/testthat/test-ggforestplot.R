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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

  built <- ggplot2::ggplot_build(
    ggforestplot(raw, point_shape = 17, linewidth = 0.8, staple_width = 0.25)
  )

  expect_true(all(built$data[[2]]$shape == 17))
  expect_true(all(built$data[[1]]$linewidth == 0.8))
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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0),
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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

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
  expect_true(any(table_spec$table_data$text == "0.01234"))
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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0),
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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0),
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
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

  p <- ggforestplot(raw)

  expect_error(
    add_split_table(p, left_columns = c("term", "events"), right_columns = "estimate"),
    "An `events` column is required"
  )
})

