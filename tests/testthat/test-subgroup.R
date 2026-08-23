make_mixed_subgroup_data <- function() {
  data.frame(
    term = c("Age", "White", "Black", "BMI", "Female", "Male"),
    subgroup_name = c(NA, "Race", "Race", "", "Sex", "Sex"),
    estimate = c(1.03, 1.01, 0.89, 0.97, 0.96, 0.98),
    conf.low = c(1.01, 0.95, 0.80, 0.94, 0.89, 0.92),
    conf.high = c(1.05, 1.07, 0.99, 1.00, 1.04, 1.06),
    sample_size = c(500, 310, 190, 500, 280, 220),
    event_count = c(120, 80, 40, 120, 68, 52),
    p_value = c(0.002, 0.72, 0.03, 0.06, 0.31, 0.58),
    note = c("linear", "level", "level", "linear", "level", "level"),
    stringsAsFactors = FALSE
  )
}

subgroup_plot <- function(data = make_mixed_subgroup_data(), ...) {
  ggforestplot(
    data,
    subgroup = "subgroup_name",
    n = "sample_size",
    events = "event_count",
    p.value = "p_value",
    ...
  )
}

subgroup_layer_data <- function(plot, layer) {
  layer_data <- layer$data

  if (is.null(layer_data) || inherits(layer_data, "waiver")) {
    return(plot$data)
  }

  layer_data
}

subgroup_layer_indices <- function(plot, geom_class) {
  which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, geom_class),
    logical(1)
  ))
}

subgroup_component_plots <- function(composition) {
  components <- list(composition)

  if (!is.null(composition$patches$plots)) {
    components <- c(components, composition$patches$plots)
  }

  components
}

subgroup_table_plots <- function(composition) {
  Filter(
    function(component) {
      inherits(component, "ggplot") &&
        is.data.frame(component$data) &&
        all(c("row_key", "column_key", "text") %in% names(component$data))
    },
    subgroup_component_plots(composition)
  )
}

subgroup_axis_lookup <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  panel_y <- built$layout$panel_params[[1L]]$y
  stats::setNames(panel_y$get_labels(), panel_y$get_limits())
}

subgroup_is_indented <- function(display_label, original_label) {
  display_label != original_label && endsWith(display_label, original_label)
}

expect_subgroup_header_blanks <- function(table_data, display_data) {
  header_rows <- display_data$row_type == "subgroup_header"
  header_keys <- as.character(display_data$row_key[header_rows])
  expected_labels <- stats::setNames(
    display_data$display_label[header_rows],
    header_keys
  )

  for (row_key in header_keys) {
    cells <- table_data[as.character(table_data$row_key) == row_key, , drop = FALSE]
    term_cells <- cells[cells$column_key == "term", , drop = FALSE]
    other_cells <- cells[cells$column_key != "term", , drop = FALSE]

    if (nrow(term_cells) > 0L) {
      expect_equal(term_cells$text, unname(expected_labels[[row_key]]))
    }
    if (nrow(other_cells) > 0L) {
      expect_true(all(is.na(other_cells$text) | !nzchar(other_cells$text)))
    }
  }
}

test_that("subgroup is an explicit canonical mapping and is not inferred", {
  raw <- make_mixed_subgroup_data()
  raw$categorical_variable <- c(NA, "Race", "Race", NA, "Sex", "Sex")
  raw$subgroup <- raw$subgroup_name

  explicit <- as_forest_data(
    raw,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high",
    subgroup = "subgroup_name"
  )
  implicit <- as_forest_data(
    raw,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high"
  )

  expect_equal(as.character(explicit$subgroup), raw$subgroup_name)
  expect_equal(nrow(explicit), nrow(raw))
  expect_false("row_type" %in% names(explicit))
  expect_equal(
    unname(forest_metadata(explicit)$column_mapping[["subgroup"]]),
    "subgroup_name"
  )
  expect_true(all(is.na(implicit$subgroup)))

  implicit_plot <- ggforestplot(raw)
  expect_true(all(implicit_plot$ggforestplotR_state$display_data$row_type == "estimate"))
})

test_that("mixed standalone and subgroup rows preserve source order", {
  raw <- make_mixed_subgroup_data()
  p <- subgroup_plot(raw)
  forest_data <- p$ggforestplotR_state$forest_data
  display_data <- p$ggforestplotR_state$display_data

  expect_equal(nrow(forest_data), nrow(raw))
  expect_equal(as.character(forest_data$term), raw$term)
  expect_true(all(stats::complete.cases(
    forest_data[c("estimate", "conf.low", "conf.high")]
  )))
  expect_false(any(forest_data$term %in% c("Race", "Sex")))

  expect_equal(
    display_data$row_type,
    c(
      "estimate", "subgroup_header", "estimate", "estimate",
      "estimate", "subgroup_header", "estimate", "estimate"
    )
  )
  expect_equal(
    display_data$display_label[display_data$row_type == "subgroup_header"],
    c("Race", "Sex")
  )
  expect_equal(
    as.character(display_data$term[display_data$row_type == "estimate"]),
    raw$term
  )
  expect_equal(
    names(subgroup_axis_lookup(p)),
    levels(display_data$row_key)
  )
  expect_equal(
    rev(unname(subgroup_axis_lookup(p))),
    display_data$display_label
  )
  expect_equal(
    display_data$display_label[display_data$row_type == "estimate" &
      (is.na(display_data$subgroup) | !nzchar(display_data$subgroup))],
    c("Age", "BMI")
  )

  child_rows <- display_data$row_type == "estimate" &
    !is.na(display_data$subgroup) & nzchar(display_data$subgroup)
  expect_true(all(vapply(
    which(child_rows),
    function(i) subgroup_is_indented(
      display_data$display_label[[i]],
      display_data$label[[i]]
    ),
    logical(1)
  )))
})

test_that("multiple subgroup blocks are inserted immediately before their members", {
  raw <- make_mixed_subgroup_data()
  display_data <- subgroup_plot(raw)$ggforestplotR_state$display_data

  visible_labels <- ifelse(
    display_data$row_type == "subgroup_header",
    display_data$display_label,
    as.character(display_data$term)
  )

  expect_equal(
    visible_labels,
    c("Age", "Race", "White", "Black", "BMI", "Sex", "Female", "Male")
  )
  expect_equal(which(display_data$row_type == "subgroup_header"), c(2L, 6L))
})

test_that("explicit all-standalone subgroup values do not add headers", {
  raw <- make_mixed_subgroup_data()[c(1L, 4L), , drop = FALSE]
  display_data <- subgroup_plot(raw)$ggforestplotR_state$display_data

  expect_equal(nrow(display_data), nrow(raw))
  expect_true(all(display_data$row_type == "estimate"))
  expect_equal(display_data$display_label, raw$term)
  expect_equal(as.character(display_data$term), raw$term)
})

test_that("all-subgrouped data retain every block and estimate", {
  raw <- make_mixed_subgroup_data()[c(2L, 3L, 5L, 6L), , drop = FALSE]
  display_data <- subgroup_plot(raw)$ggforestplotR_state$display_data

  expect_equal(
    display_data$row_type,
    c(
      "subgroup_header", "estimate", "estimate",
      "subgroup_header", "estimate", "estimate"
    )
  )
  expect_equal(
    display_data$display_label[display_data$row_type == "subgroup_header"],
    c("Race", "Sex")
  )
  expect_equal(
    as.character(display_data$term[display_data$row_type == "estimate"]),
    raw$term
  )
})

test_that("noncontiguous repeated subgroup blocks are rejected", {
  raw <- make_mixed_subgroup_data()[c(2L, 5L, 3L), , drop = FALSE]

  expect_error(
    subgroup_plot(raw),
    "contiguous|noncontiguous"
  )
})

test_that("headers and estimates have unique row keys and mapped axis labels", {
  raw <- make_mixed_subgroup_data()[c(1L, 2L, 3L), , drop = FALSE]
  raw$term[[1L]] <- "Race"
  p <- subgroup_plot(raw)
  display_data <- p$ggforestplotR_state$display_data
  row_keys <- as.character(display_data$row_key)
  axis_lookup <- subgroup_axis_lookup(p)

  expect_length(row_keys, 4L)
  expect_length(unique(row_keys), 4L)
  expect_equal(levels(display_data$row_key), rev(row_keys))
  expect_equal(names(axis_lookup), levels(display_data$row_key))
  expect_equal(
    unname(axis_lookup[row_keys]),
    display_data$display_label
  )
  expect_equal(sum(display_data$display_label == "Race"), 2L)

  child_rows <- display_data$row_type == "estimate" &
    !is.na(display_data$subgroup) & nzchar(display_data$subgroup)
  expect_true(all(vapply(
    which(child_rows),
    function(i) subgroup_is_indented(
      display_data$display_label[[i]],
      display_data$label[[i]]
    ),
    logical(1)
  )))
})

test_that("point and confidence interval geoms exclude subgroup headers", {
  p <- subgroup_plot()
  display_data <- p$ggforestplotR_state$display_data
  header_keys <- as.character(
    display_data$row_key[display_data$row_type == "subgroup_header"]
  )
  point_indices <- subgroup_layer_indices(p, "GeomPoint")
  interval_indices <- subgroup_layer_indices(p, "GeomErrorbar")
  reference_indices <- subgroup_layer_indices(p, "GeomVline")

  expect_gt(length(point_indices), 0L)
  expect_gt(length(interval_indices), 0L)
  expect_length(reference_indices, 1L)

  for (layer_index in c(point_indices, interval_indices)) {
    layer_data <- subgroup_layer_data(p, p$layers[[layer_index]])

    expect_true("row_type" %in% names(layer_data))
    expect_true(all(layer_data$row_type == "estimate"))
    expect_false(any(as.character(layer_data$row_key) %in% header_keys))
  }

  built <- ggplot2::ggplot_build(p)
  expect_true(all(vapply(
    built$data[point_indices],
    nrow,
    integer(1)
  ) == nrow(p$ggforestplotR_state$forest_data)))
  expect_true(all(vapply(
    built$data[interval_indices],
    nrow,
    integer(1)
  ) == nrow(p$ggforestplotR_state$forest_data)))
})

test_that("striping counts subgroup headers and aligns with a forest table", {
  p <- subgroup_plot(striped_rows = TRUE)
  display_data <- p$ggforestplotR_state$display_data
  stripe_data <- p$ggforestplotR_state$stripe_data

  expect_equal(nrow(stripe_data), nrow(display_data))
  expect_equal(stripe_data$stripe_id, seq_len(nrow(display_data)))
  expect_equal(
    stripe_data$fill_key,
    ifelse(seq_len(nrow(display_data)) %% 2L == 1L, "stripe", "base")
  )

  plot_rect_indices <- subgroup_layer_indices(p, "GeomRect")
  expect_gt(length(plot_rect_indices), 0L)
  plot_rect_data <- subgroup_layer_data(
    p,
    p$layers[[plot_rect_indices[[1L]]]]
  )

  out <- add_forest_table(p, columns = c("term", "estimate"))
  table_plots <- subgroup_table_plots(out)
  expect_length(table_plots, 1L)
  table_plot <- table_plots[[1L]]
  table_rect_indices <- subgroup_layer_indices(table_plot, "GeomRect")
  expect_gt(length(table_rect_indices), 0L)
  table_rect_data <- subgroup_layer_data(
    table_plot,
    table_plot$layers[[table_rect_indices[[1L]]]]
  )

  expect_equal(plot_rect_data$ymin, table_rect_data$ymin)
  expect_equal(plot_rect_data$ymax, table_rect_data$ymax)
  expect_equal(plot_rect_data$stripe_id, c(1L, 3L, 5L, 7L))
})

test_that("add_forest_table aligns headers and blanks non-term cells", {
  p <- subgroup_plot(striped_rows = TRUE)
  display_data <- p$ggforestplotR_state$display_data
  out <- add_forest_table(
    p,
    position = "left",
    columns = c("term", "estimate", "ci", "n", "events", "p", "note")
  )
  table_plots <- subgroup_table_plots(out)

  expect_length(table_plots, 1L)
  table_data <- table_plots[[1L]]$data
  expect_equal(
    levels(table_data$row_key),
    levels(display_data$row_key)
  )
  expect_subgroup_header_blanks(table_data, display_data)

  term_cells <- table_data[table_data$column_key == "term", , drop = FALSE]
  term_lookup <- stats::setNames(term_cells$text, as.character(term_cells$row_key))
  expect_equal(
    unname(term_lookup[as.character(display_data$row_key)]),
    display_data$display_label
  )

  header_keys <- as.character(
    display_data$row_key[display_data$row_type == "subgroup_header"]
  )
  arbitrary_header_cells <- table_data[
    table_data$column_key == "note" &
      as.character(table_data$row_key) %in% header_keys,
    ,
    drop = FALSE
  ]
  expect_true(all(is.na(arbitrary_header_cells$text) |
    !nzchar(arbitrary_header_cells$text)))
})

test_that("add_split_table keeps both tables aligned with subgroup rows", {
  p <- subgroup_plot(striped_rows = TRUE)
  display_data <- p$ggforestplotR_state$display_data
  out <- add_split_table(
    p,
    left_columns = c("term", "n", "events", "note"),
    right_columns = c("estimate", "ci", "p")
  )
  table_plots <- subgroup_table_plots(out)
  plot_rect_indices <- subgroup_layer_indices(p, "GeomRect")
  expect_gt(length(plot_rect_indices), 0L)
  plot_rect_data <- subgroup_layer_data(
    p,
    p$layers[[plot_rect_indices[[1L]]]]
  )

  expect_length(table_plots, 2L)
  for (table_plot in table_plots) {
    expect_equal(
      levels(table_plot$data$row_key),
      levels(display_data$row_key)
    )
    expect_subgroup_header_blanks(table_plot$data, display_data)

    table_rect_indices <- subgroup_layer_indices(table_plot, "GeomRect")
    expect_gt(length(table_rect_indices), 0L)
    table_rect_data <- subgroup_layer_data(
      table_plot,
      table_plot$layers[[table_rect_indices[[1L]]]]
    )
    expect_equal(table_rect_data$ymin, plot_rect_data$ymin)
    expect_equal(table_rect_data$ymax, plot_rect_data$ymax)
  }

  left_table <- table_plots[[which(vapply(
    table_plots,
    function(table_plot) "term" %in% table_plot$data$column_key,
    logical(1)
  ))]]
  right_table <- table_plots[[which(vapply(
    table_plots,
    function(table_plot) "estimate" %in% table_plot$data$column_key,
    logical(1)
  ))]]
  term_cells <- left_table$data[
    left_table$data$column_key == "term",
    ,
    drop = FALSE
  ]
  term_lookup <- stats::setNames(term_cells$text, as.character(term_cells$row_key))

  expect_equal(
    unname(term_lookup[as.character(display_data$row_key)]),
    display_data$display_label
  )
  expect_subgroup_header_blanks(right_table$data, display_data)
})

test_that("subgroup NULL retains ordinary forest-plot row behavior", {
  raw <- make_mixed_subgroup_data()
  raw$subgroup_name <- NULL

  omitted <- ggforestplot(raw)
  explicit_null <- ggforestplot(raw, subgroup = NULL)
  omitted_display <- omitted$ggforestplotR_state$display_data
  null_display <- explicit_null$ggforestplotR_state$display_data

  expect_equal(
    null_display[c("row_type", "display_label", "row_key")],
    omitted_display[c("row_type", "display_label", "row_key")]
  )
  expect_true(all(null_display$row_type == "estimate"))
  expect_equal(null_display$display_label, raw$term)
  expect_equal(levels(null_display$row_key), raw$term)
  expect_equal(
    explicit_null$ggforestplotR_state$forest_data$term,
    raw$term
  )
  expect_equal(
    normalize_table_columns(
      "subgroup",
      data = explicit_null$ggforestplotR_state$forest_data
    ),
    "term"
  )
})

test_that("custom y limits preserve hierarchy and attached row layers", {
  raw <- make_mixed_subgroup_data()[1:4, , drop = FALSE]
  p <- suppressMessages(
    subgroup_plot(raw, striped_rows = TRUE) +
      ggplot2::scale_y_discrete(
        limits = c("BMI", "Race", "White", "Age")
      )
  )
  built <- ggplot2::ggplot_build(p)

  expect_equal(
    built$layout$panel_params[[1L]]$y$get_labels(),
    c("BMI", "Race", "   White", "Age")
  )

  out <- add_forest_table(p, columns = c("term", "estimate"))
  table_plot <- subgroup_table_plots(out)[[1L]]
  plot_rect <- out$layers[[subgroup_layer_indices(out, "GeomRect")[[1L]]]]$data
  table_rect <- table_plot$layers[[
    subgroup_layer_indices(table_plot, "GeomRect")[[1L]]
  ]]$data

  expect_equal(plot_rect[c("ymin", "ymax")], table_rect[c("ymin", "ymax")])
  expect_equal(levels(table_plot$data$row_key), c("BMI", "Race", "White", "Age"))
})

test_that("subgroup headers join common separate_groups blocks", {
  raw <- make_mixed_subgroup_data()
  raw$block <- c("Age", "Race", "Race", "BMI", "Sex", "Sex")
  p <- ggforestplot(
    raw,
    subgroup = "subgroup_name",
    separate_groups = "block",
    separate_lines = TRUE,
    ref_line = NULL
  )

  expect_true(all(c(0.5, 3.5, 4.5, 7.5, 8.5) %in%
    p$ggforestplotR_state$separator_data$yintercept))
})

test_that("display-only names do not shadow mapped source columns", {
  raw <- data.frame(
    row_type = c("Age", "White", "Black"),
    display_label = c("Age label", "White label", "Black label"),
    parent = c(NA, "Race", "Race"),
    beta = c(0.1, 0.2, 0.3),
    lower = c(0.0, 0.1, 0.2),
    upper = c(0.2, 0.3, 0.4)
  )
  p <- ggforestplot(
    raw,
    term = "row_type",
    label = "display_label",
    subgroup = "parent",
    estimate = "beta",
    conf.low = "lower",
    conf.high = "upper",
    ref_line = NULL
  )
  out <- add_forest_table(p, columns = c("row_type", "display_label"))
  table_data <- subgroup_table_plots(out)[[1L]]$data

  source_terms <- table_data$text[table_data$column_key == "row_type"]
  source_labels <- table_data$text[table_data$column_key == "display_label"]
  expect_equal(trimws(source_terms), c("Black", "White", "Race", "Age"))
  expect_equal(
    trimws(source_labels),
    c("Black label", "White label", "Race", "Age label")
  )
  expect_false(any(source_terms %in% c("estimate", "subgroup_header")))
})

test_that("facet-qualified row keys cannot collide with natural labels", {
  raw <- data.frame(
    term = c("White", "White", "A___Race"),
    subgroup_name = c("Race", "Race", NA),
    section = c("A", "B", "B"),
    estimate = c(0.1, 0.2, 0.3),
    conf.low = c(0.0, 0.1, 0.2),
    conf.high = c(0.2, 0.3, 0.4)
  )

  p <- ggforestplot(
    raw,
    subgroup = "subgroup_name",
    facet = "section",
    ref_line = NULL
  )
  row_keys <- levels(p$ggforestplotR_state$display_data$row_key)

  expect_length(row_keys, 5L)
  expect_length(unique(row_keys), 5L)
})

test_that("subgroup source order cannot be overridden by sorting", {
  expect_error(
    subgroup_plot(sort_terms = "ascending"),
    "sort_terms.*none"
  )
})

test_that("arbitrary columns cannot be shadowed by table internals", {
  raw <- data.frame(
    code = c("Age", "White", "Black"),
    parent = c(NA, "Race", "Race"),
    beta = c(0.1, 0.2, 0.3),
    lower = c(0.0, 0.1, 0.2),
    upper = c(0.2, 0.3, 0.4),
    row_key = c("rk-age", "rk-white", "rk-black"),
    grouping_panel = c("gp-age", "gp-white", "gp-black"),
    term_text = c("tt-age", "tt-white", "tt-black"),
    estimate_text = c("et-age", "et-white", "et-black")
  )
  p <- ggforestplot(
    raw,
    term = "code",
    subgroup = "parent",
    estimate = "beta",
    conf.low = "lower",
    conf.high = "upper",
    ref_line = NULL
  )
  display_data <- p$ggforestplotR_state$display_data
  out <- add_forest_table(
    p,
    columns = c("row_key", "grouping_panel", "term_text", "estimate_text")
  )
  table_data <- subgroup_table_plots(out)[[1L]]$data
  estimate_keys <- as.character(
    display_data$row_key[display_data$row_type == "estimate"]
  )
  header_keys <- as.character(
    display_data$row_key[display_data$row_type == "subgroup_header"]
  )

  for (column in c("row_key", "grouping_panel", "term_text", "estimate_text")) {
    cells <- table_data[table_data$column_key == column, , drop = FALSE]
    lookup <- stats::setNames(cells$text, as.character(cells$row_key))
    expect_equal(unname(lookup[estimate_keys]), raw[[column]])
    expect_true(all(!nzchar(unname(lookup[header_keys]))))
  }
})

test_that("header-only y limits still support forest and split tables", {
  raw <- make_mixed_subgroup_data()[1:3, , drop = FALSE]
  p <- suppressMessages(
    subgroup_plot(raw) + ggplot2::scale_y_discrete(limits = "Race")
  )

  expect_equal(nrow(p$ggforestplotR_state$forest_data), 0L)
  forest_out <- add_forest_table(p, columns = c("term", "n"))
  forest_table <- subgroup_table_plots(forest_out)[[1L]]$data
  expect_equal(forest_table$text[forest_table$column_key == "term"], "Race")
  expect_equal(forest_table$text[forest_table$column_key == "n"], "")

  split_out <- add_split_table(
    p,
    left_columns = c("term", "n"),
    right_columns = "estimate"
  )
  split_tables <- subgroup_table_plots(split_out)
  expect_length(split_tables, 2L)
  expect_true(all(vapply(
    split_tables,
    function(table) all(table$data$row_type == "subgroup_header"),
    logical(1)
  )))
})

test_that("custom y limits immediately realign separator layers", {
  raw <- make_mixed_subgroup_data()[1:4, , drop = FALSE]
  raw$block <- c("Age", "Race", "Race", "BMI")
  p <- ggforestplot(
    raw,
    subgroup = "subgroup_name",
    separate_groups = "block",
    separate_lines = TRUE,
    striped_rows = TRUE,
    ref_line = NULL
  )
  p <- suppressMessages(
    p + ggplot2::scale_y_discrete(
      limits = c("BMI", "Race", "White", "Age")
    )
  )
  state <- p$ggforestplotR_state
  separator_layer <- p$layers[[state$separator_layer_index]]$data
  stripe_layer <- p$layers[[state$stripe_layer_index]]$data

  expect_equal(separator_layer, state$separator_data)
  expect_equal(
    stripe_layer[c("ymin", "ymax", "stripe_id")],
    state$stripe_data[state$stripe_data$fill_key == "stripe",
      c("ymin", "ymax", "stripe_id"),
      drop = FALSE
    ],
    ignore_attr = TRUE
  )
})

test_that("custom y limits must match at least one forest row", {
  expect_error(
    suppressMessages(
      subgroup_plot() + ggplot2::scale_y_discrete(limits = "No such row")
    ),
    "did not match any forest-plot rows"
  )
  expect_error(
    suppressMessages(
      subgroup_plot() + ggplot2::scale_y_discrete(limits = character())
    ),
    "did not match any forest-plot rows"
  )
  expect_error(
    suppressMessages(
      subgroup_plot() + ggplot2::scale_y_discrete(limits = NA_character_)
    ),
    "did not match any forest-plot rows"
  )

  matched_with_na <- suppressMessages(
    subgroup_plot() +
      ggplot2::scale_y_discrete(limits = c("White", NA_character_))
  )
  expect_equal(
    ggplot2::ggplot_build(matched_with_na)$layout$panel_params[[1L]]$y$get_limits(),
    "White"
  )
})

test_that("faceted tables retain panels emptied by custom y limits", {
  raw <- data.frame(
    term = c("White", "Female"),
    parent = c("Race", "Sex"),
    section = c("A", "B"),
    estimate = c(0.1, 0.2),
    conf.low = c(0.0, 0.1),
    conf.high = c(0.2, 0.3)
  )
  p <- ggforestplot(
    raw,
    subgroup = "parent",
    facet = "section",
    ref_line = NULL
  )
  p <- suppressMessages(
    p + ggplot2::scale_y_discrete(limits = c("Race", "White"))
  )
  out <- add_forest_table(p, columns = "term")
  table_plot <- subgroup_table_plots(out)[[1L]]

  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  expect_equal(nrow(ggplot2::ggplot_build(table_plot)$layout$layout), 2L)
})

test_that("custom discrete scales retain visible hierarchical labels", {
  raw <- make_mixed_subgroup_data()[1:3, , drop = FALSE]
  expanded <- suppressMessages(
    subgroup_plot(raw) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.2))
  )
  expect_equal(
    rev(ggplot2::ggplot_build(expanded)$layout$panel_params[[1L]]$y$get_labels()),
    c("Age", "Race", "   White", "   Black")
  )

  filtered <- suppressMessages(
    subgroup_plot(raw) +
      ggplot2::scale_y_discrete(limits = c("White", "Age"))
  )
  restored <- suppressMessages(
    filtered +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.2))
  )
  expect_equal(
    rev(ggplot2::ggplot_build(restored)$layout$panel_params[[1L]]$y$get_labels()),
    c("Age", "Race", "   White", "   Black")
  )
  expect_equal(
    nrow(restored$ggforestplotR_state$display_data),
    nrow(restored$ggforestplotR_state$full_display_data)
  )

  function_limits <- suppressMessages(
    subgroup_plot(raw) +
      ggplot2::scale_y_discrete(limits = function(x) x)
  )
  expect_equal(
    rev(ggplot2::ggplot_build(function_limits)$layout$panel_params[[1L]]$y$get_labels()),
    c("Age", "Race", "   White", "   Black")
  )

  faceted <- data.frame(
    term = c("White", "White"),
    parent = c("Race", "Race"),
    section = c("A", "B"),
    estimate = c(0.1, 0.2),
    conf.low = c(0.0, 0.1),
    conf.high = c(0.2, 0.3)
  )
  named_labels <- suppressMessages(
    ggforestplot(
      faceted,
      subgroup = "parent",
      facet = "section",
      ref_line = NULL
    ) + ggplot2::scale_y_discrete(
      limits = "White",
      labels = c(White = "W")
    )
  )
  panel_labels <- lapply(
    ggplot2::ggplot_build(named_labels)$layout$panel_params,
    function(panel) panel$y$get_labels()
  )
  expect_true(all(vapply(panel_labels, identical, logical(1), "W")))
})

test_that("plural subgroup source columns remain selectable when unmapped", {
  raw <- make_mixed_subgroup_data()
  raw$subgroup_name <- NULL
  raw$subgroups <- paste0("source-", seq_len(nrow(raw)))
  p <- ggforestplot(raw)
  out <- add_forest_table(p, columns = "subgroups")
  table_data <- subgroup_table_plots(out)[[1L]]$data

  expect_equal(table_data$column_key, rep("subgroups", nrow(raw)))
  expect_equal(table_data$text, raw$subgroups)
})

test_that("adding subgroup preserves existing positional argument slots", {
  raw <- make_mixed_subgroup_data()
  raw$section <- rep(c("A", "B"), each = 3L)
  raw$subgroup_name <- NULL

  positional_plot <- ggforestplot(
    raw,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    "term",
    NULL,
    NULL,
    "section"
  )
  positional_data <- as_forest_data.data.frame(
    raw,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    "term",
    NULL,
    NULL,
    "section"
  )

  expect_true(positional_plot$ggforestplotR_state$has_groupings)
  expect_true(all(is.na(positional_plot$ggforestplotR_state$forest_data$subgroup)))
  expect_equal(positional_data$grouping, raw$section)
  expect_true(all(is.na(positional_data$subgroup)))
})

test_that("legacy forest_data subgroup columns are not inferred", {
  raw <- make_mixed_subgroup_data()
  raw$subgroup <- raw$subgroup_name
  forest <- as_forest_data(
    raw,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high"
  )
  metadata <- forest_metadata(forest)
  stored_subgroup <- unname(metadata$source_columns[["subgroup"]])
  legacy <- strip_forest_data_class(forest)
  legacy$subgroup <- legacy[[stored_subgroup]]
  legacy[[stored_subgroup]] <- NULL
  metadata$source_columns[["subgroup"]] <- "subgroup"
  legacy <- new_forest_data(legacy, metadata)

  p <- ggforestplot(legacy)
  expect_true(all(is.na(p$ggforestplotR_state$forest_data$subgroup)))
  expect_true(all(p$ggforestplotR_state$display_data$row_type == "estimate"))

  out <- add_forest_table(p, columns = "subgroup")
  table_data <- subgroup_table_plots(out)[[1L]]$data
  expect_equal(table_data$text, ifelse(is.na(raw$subgroup), "", raw$subgroup))
})

test_that("mapped row labels are not repeated across grouped estimates", {
  raw <- data.frame(
    code = c("Age", "Age", "White", "White"),
    pretty = c("Age label", "Age label", "White label", "White label"),
    parent = c(NA, NA, "Race", "Race"),
    model = rep(c("A", "B"), 2L),
    estimate = c(0.1, 0.2, 0.3, 0.4),
    conf.low = c(0.0, 0.1, 0.2, 0.3),
    conf.high = c(0.2, 0.3, 0.4, 0.5)
  )
  p <- ggforestplot(
    raw,
    term = "code",
    label = "pretty",
    group = "model",
    subgroup = "parent",
    ref_line = NULL
  )
  out <- add_forest_table(
    p,
    columns = c("code", "pretty", "group", "estimate")
  )
  table_data <- subgroup_table_plots(out)[[1L]]$data

  code_text <- table_data$text[table_data$column_key == "code"]
  pretty_text <- table_data$text[table_data$column_key == "pretty"]
  expect_false(any(grepl("\n", code_text, fixed = TRUE)))
  expect_false(any(grepl("\n", pretty_text, fixed = TRUE)))
  expect_equal(trimws(code_text), c("White", "Race", "Age"))
  expect_equal(trimws(pretty_text), c("White label", "Race", "Age label"))
})
