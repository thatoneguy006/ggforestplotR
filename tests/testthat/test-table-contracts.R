make_contract_data <- function() {
  data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.30, -0.20, 0.40),
    conf.low = c(0.10, -0.40, 0.20),
    conf.high = c(0.50, 0.00, 0.60),
    sample_size = c(120, 115, 98),
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )
}

test_that("add_forest_table returns a two-panel patchwork", {
  p <- ggforestplot(make_contract_data(), n = "sample_size", events = "event_count", p.value = "p_value")

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
  out <- ggforestplot(make_contract_data(), n = "sample_size", events = "event_count", p.value = "p_value") +
    ggplot2::labs(title = "Contract") +
    add_forest_table(position = "right", columns = c("term", "n", "events", "estimate", "p"))

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 2L)
  expect_length(out$patches$plots, 1L)
})

test_that("add_split_table returns left plot right panels in order", {
  p <- ggforestplot(make_contract_data(), n = "sample_size", events = "event_count", p.value = "p_value")

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
  out <- ggforestplot(make_contract_data(), n = "sample_size", events = "event_count", p.value = "p_value") +
    add_split_table(left_columns = c("term", "n", "events"), right_columns = c("estimate", "p"))

  center_plot <- out$patches$plots[[2]]

  expect_s3_class(out, "patchwork")
  expect_s3_class(center_plot, "ggplot")
  expect_true(!is.null(center_plot$ggforestplotR_state))
})

test_that("group_position moves the dedicated group column", {
  data <- make_contract_data()
  data$model <- c("Base", "Adjusted", "Base")
  p <- ggforestplot(
    data,
    group = "model",
    n = "sample_size",
    events = "event_count"
  )
  forest_data <- p$ggforestplotR_state$forest_data
  default_columns <- default_forest_table_columns(forest_data)

  expect_equal(default_columns, c("term", "group", "n", "events", "estimate"))
  expect_equal(
    resolve_group_position(default_columns, 4, forest_data),
    c("term", "n", "events", "group", "estimate")
  )
  expect_equal(
    resolve_group_position(default_columns, FALSE, forest_data),
    c("term", "n", "events", "estimate")
  )

  split_columns <- resolve_split_group_position(
    default_split_left_columns(forest_data),
    default_split_right_columns(forest_data),
    c(right = 1),
    forest_data
  )
  expect_equal(split_columns$left, c("term", "n", "events"))
  expect_equal(split_columns$right, c("group", "estimate"))

  single_table <- add_forest_table(p, group_position = 4)
  split_table <- add_split_table(p, group_position = c(right = 1))

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

  omitted_table <- add_forest_table(p, group_position = FALSE)
  omitted_estimates <- omitted_table$patches$plots[[1L]]$data$text[
    omitted_table$patches$plots[[1L]]$data$column_key == "estimate"
  ]
  expect_false(any(grepl("Base:|Adjusted:", omitted_estimates)))
})

test_that("group_position validates grouped data and table positions", {
  p <- ggforestplot(make_contract_data())
  forest_data <- p$ggforestplotR_state$forest_data

  expect_error(
    resolve_group_position(c("term", "estimate"), 1, forest_data),
    "requires grouped forest data"
  )

  grouped <- make_contract_data()
  grouped$model <- c("Base", "Adjusted", "Base")
  grouped_data <- ggforestplot(grouped, group = "model")$ggforestplotR_state$forest_data

  expect_error(
    resolve_group_position(c("term", "estimate"), 4, grouped_data),
    "between 1 and 3"
  )
  expect_error(
    resolve_split_group_position(
      c("term"),
      c("estimate"),
      c(center = 1),
      grouped_data
    ),
    "name `group_position` either `left` or `right`"
  )
})

test_that("outer legends span the full table and plot composition", {
  data <- make_contract_data()
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
