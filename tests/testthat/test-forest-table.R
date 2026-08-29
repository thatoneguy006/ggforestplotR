test_that("forest table validates requested mapped columns", {
  p <- ggforestplot(make_simple_forest_data())

  expect_error(
    add_forest_table(p, columns = "n"),
    "requires an `n` column"
  )
  expect_error(
    add_forest_table(p, columns = "events"),
    "requires an `events` column"
  )
  expect_error(
    add_forest_table(p, columns = "p"),
    "requires a `p.value` column"
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
})

test_that("mapped row-label columns use the term header and suppress duplicate y-axis labels", {
  raw <- data.frame(
    name = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6)
  )

  p <- suppressMessages(
    ggforestplot(raw, term = "name") +
      ggplot2::scale_y_discrete(limits = c("Treatment", "BMI", "Age"))
  )
  out <- p + add_forest_table(
    columns = c("name", "estimate"),
    column_labels = c(name = "Variable")
  )
  table_plot <- out$patches$plots[[1]]

  expect_s3_class(out$theme$axis.text.y, "element_blank")
  expect_s3_class(out$theme$axis.ticks.y, "element_blank")
  expect_equal(
    table_plot$scales$get_scales("x")$labels,
    c("Variable", "Estimate (95% CI)")
  )
  expect_equal(
    out$scales$get_scales("y")$limits,
    c("Treatment", "BMI", "Age")
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

test_that("forest table renders mapped count and p-value columns", {
  raw <- make_table_forest_data()
  p <- ggforestplot(
    raw,
    events = "event_count",
    p.value = "p_value"
  )
  table_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = c("term", "events", "estimate", "p")
  )
  alias_spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    columns = "p_value"
  )

  expect_equal(
    table_spec$column_keys,
    c("term", "events", "estimate", "p")
  )
  expect_true(any(table_spec$table_data$text == "42"))
  expect_true(any(table_spec$table_data$text == "0.012"))
  expect_equal(alias_spec$column_keys, "p")
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

test_that("dedicated table header arguments are deprecated", {
  header_columns <- c(
    term_header = "term",
    n_header = "n",
    events_header = "events",
    p_header = "p"
  )

  for (argument in names(header_columns)) {
    args <- stats::setNames(list("Custom"), argument)
    replacement <- sprintf(
      "column_labels = c\\(%s =",
      header_columns[[argument]]
    )

    expect_warning(
      do.call(add_forest_table, args),
      replacement,
      class = "ggforestplotR_deprecated_argument"
    )
    expect_warning(
      do.call(add_split_table, args),
      replacement,
      class = "ggforestplotR_deprecated_argument"
    )
  }

  expect_no_warning(add_forest_table(
    column_labels = c(term = "Variable", n = "N")
  ))
  expect_no_warning(add_split_table(
    column_labels = c(term = "Variable", p = "P")
  ))
})
