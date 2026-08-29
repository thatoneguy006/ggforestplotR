test_that("add_forest_table returns a two-panel patchwork", {
  p <- ggforestplot(
    make_table_forest_data(),
    n = "sample_size",
    events = "event_count",
    p.value = "p_value"
  )

  out <- add_forest_table(
    p,
    position = "left",
    columns = c("term", "n", "events", "estimate", "p"),
    estimate_label = "Beta"
  )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 2L)
  expect_length(out$patches$plots, 1L)
  expect_s3_class(out$patches$plots[[1]], "ggplot")
})

test_that("add_forest_table supports ggplot add syntax as a terminal step", {
  out <- ggforestplot(
    make_table_forest_data(),
    n = "sample_size",
    events = "event_count",
    p.value = "p_value"
  ) +
    ggplot2::labs(title = "Contract") +
    add_forest_table(position = "right", columns = c("term", "n", "events", "estimate", "p"))

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 2L)
  expect_length(out$patches$plots, 1L)
})

test_that("add_forest_table supports explicit table and plot widths", {
  p <- ggforestplot(make_table_forest_data())

  left <- add_forest_table(
    p,
    position = "left",
    table_width = 3.5,
    plot_width = 5
  )
  right <- p + add_forest_table(
    position = "right",
    table_width = 2,
    plot_width = 4
  )

  expect_equal(left$patches$layout$widths, c(3.5, 5))
  expect_equal(right$patches$layout$widths, c(4, 2))
  expect_error(add_forest_table(p, table_width = 0), "single positive number")
  expect_error(add_forest_table(p, plot_width = c(1, 2)), "single positive number")
})

test_that("add_split_table returns left plot right panels in order", {
  p <- ggforestplot(
    make_table_forest_data(),
    n = "sample_size",
    events = "event_count",
    p.value = "p_value"
  )

  out <- add_split_table(
    p,
    left_columns = c("term", "n", "events"),
    right_columns = c("estimate", "p"),
    estimate_label = "HR",
    estimate_fmt = "{estimate} [{conf.low}, {conf.high}]"
  )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 3L)
  expect_length(out$patches$plots, 2L)
  expect_s3_class(out$patches$plots[[2]], "ggplot")
})

test_that("add_split_table ggplot add syntax preserves a forest-plot center panel", {
  out <- ggforestplot(
    make_table_forest_data(),
    n = "sample_size",
    events = "event_count",
    p.value = "p_value"
  ) +
    add_split_table(left_columns = c("term", "n", "events"), right_columns = c("estimate", "p"))

  center_plot <- out$patches$plots[[2]]

  expect_s3_class(out, "patchwork")
  expect_s3_class(center_plot, "ggplot")
  expect_true(!is.null(center_plot$ggforestplotR_state))
})

test_that("group columns use the standard table column syntax", {
  data <- make_table_forest_data()
  data$model <- c("Base", "Adjusted", "Base")
  p <- ggforestplot(
    data,
    group = "model",
    n = "sample_size",
    events = "event_count"
  )
  forest_data <- p$ggforestplotR_state$forest_data
  default_columns <- default_forest_table_columns(forest_data)
  default_spec <- build_forest_table_data(forest_data, columns = "group")
  relabeled_spec <- build_forest_table_data(
    forest_data,
    column_labels = c(
      term = "Term",
      model = "Model",
      estimate = "Estimate (95% CI)"
    )
  )

  expect_equal(default_columns, c("term", "group", "n", "events", "estimate"))
  expect_equal(default_spec$headers, "model")
  expect_equal(
    relabeled_spec$headers,
    c("Term", "Model", "N", "Events", "Estimate (95% CI)")
  )

  single_table <- add_forest_table(
    p,
    columns = c("term", "n", "events", "group", "estimate"),
    column_labels = c(model = "Analysis")
  )
  split_table <- add_split_table(
    p,
    left_columns = c("term", "n", "events"),
    right_columns = c("group", "estimate"),
    column_labels = c(group = "Analysis")
  )

  expect_s3_class(single_table, "patchwork")
  expect_equal(
    unique(single_table$patches$plots[[1L]]$data$column_key),
    c("term", "n", "events", "group", "estimate")
  )
  expect_s3_class(split_table, "patchwork")
  expect_equal(
    unique(split_table$patches$plots[[1L]]$data$column_key),
    c("term", "n", "events")
  )
  expect_equal(unique(split_table$data$column_key), c("group", "estimate"))
  expect_equal(single_table$patches$plots[[1L]]$scales$scales[[1L]]$labels[[4L]], "Analysis")
  expect_equal(split_table$scales$scales[[1L]]$labels[[1L]], "Analysis")

  omitted_table <- add_forest_table(
    p,
    columns = c("term", "n", "events", "estimate")
  )
  omitted_estimates <- omitted_table$patches$plots[[1L]]$data$text[
    omitted_table$patches$plots[[1L]]$data$column_key == "estimate"
  ]
  expect_false(any(grepl("Base:|Adjusted:", omitted_estimates)))
})

test_that("outer legends span the full table and plot composition", {
  data <- make_table_forest_data()
  data$cohort <- c("Base", "Adjusted", "Base")

  top_plot <- ggforestplot(data, group = "cohort") +
    ggplot2::theme(legend.position = "top")
  bottom_plot <- ggforestplot(data, group = "cohort") +
    ggplot2::theme(legend.position = "bottom")

  compositions <- list(
    top = top_plot + add_forest_table(position = "left"),
    bottom = bottom_plot + add_split_table(
      left_columns = "term",
      right_columns = "estimate"
    )
  )

  for (position in names(compositions)) {
    out <- compositions[[position]]
    expect_equal(out$patches$layout$guides, "collect")
    expect_equal(out$patches$annotation$theme$legend.position, position)

    layout <- patchwork::patchworkGrob(out)$layout
    panels <- layout[grepl("^panel-[0-9]+$", layout$name), , drop = FALSE]
    guide <- layout[layout$name == "guide-box", , drop = FALSE]

    expect_equal(nrow(guide), 1L)
    expect_equal(guide$l, min(panels$l))
    expect_equal(guide$r, max(panels$r))
  }
})
