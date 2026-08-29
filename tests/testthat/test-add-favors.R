make_favors_data <- function() {
  data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(-0.2, 0.1, 0.4),
    conf.low = c(-0.4, -0.1, 0.2),
    conf.high = c(0.0, 0.3, 0.6),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )
}

favors_state <- function(plot) {
  attr(plot, "ggforestplotR_favors", exact = TRUE)
}

test_that("add_favors places an identity-scale reference at zero", {
  forest <- ggforestplot(make_favors_data()) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1),
      expand = ggplot2::expansion(mult = 0)
    )
  out <- forest + add_favors(left = "A better", right = "B better")
  state <- favors_state(out)
  composed_forest <- attr(out, "ggforestplotR_composition")$forest_plot

  expect_s3_class(out, "patchwork")
  expect_equal(state$reference, 0)
  expect_equal(state$reference_position, 0.5)
  expect_equal(
    composed_forest$ggforestplotR_state$forest_data,
    forest$ggforestplotR_state$forest_data
  )
})

test_that("add_favors derives a ratio reference from a trained log scale", {
  ratio_data <- make_favors_data()
  ratio_data[c("estimate", "conf.low", "conf.high")] <- lapply(
    ratio_data[c("estimate", "conf.low", "conf.high")],
    exp
  )
  forest <- ggforestplot(ratio_data, exponentiate = TRUE) +
    ggplot2::scale_x_log10(
      limits = c(0.25, 2),
      expand = ggplot2::expansion(mult = 0)
    )
  out <- add_favors(
    forest,
    left = "Lower is better",
    right = "Higher is better"
  )
  state <- favors_state(out)

  expect_equal(state$reference, 1)
  expect_equal(state$reference_position, 2 / 3)
})

test_that("add_favors respects asymmetric limits and scale expansion", {
  forest <- ggforestplot(make_favors_data(), ref_line = 0.5) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 3),
      expand = ggplot2::expansion(mult = 0.1)
    )
  out <- forest + add_favors(left = "Left", right = "Right")
  state <- favors_state(out)

  expect_equal(state$reference, 0.5)
  expect_equal(state$reference_position, 1.9 / 4.8)

  override <- forest + add_favors(
    left = "Left",
    right = "Right",
    reference = 0
  )
  expect_equal(favors_state(override)$reference_position, 1.4 / 4.8)
})

test_that("add_favors falls back to forest metadata when no line is drawn", {
  forest <- ggforestplot(make_favors_data(), ref_line = NULL) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1),
      expand = ggplot2::expansion(mult = 0)
    )
  out <- forest + add_favors(left = "Left", right = "Right")

  expect_null(forest$ggforestplotR_state$defaults$ref_line)
  expect_equal(favors_state(out)$reference, 0)
})

test_that("add_favors supports bare and side-table forest compositions", {
  forest <- ggforestplot(
    make_favors_data(),
    n = "sample_size",
    p.value = "p_value"
  )
  bare <- forest + add_favors(left = "Left", right = "Right")
  single <- forest +
    add_forest_table(
      position = "left",
      columns = c("term", "n", "estimate", "p")
    )
  single <- add_favors(single, left = "Left", right = "Right")
  split <- forest +
    add_split_table(
      left_columns = c("term", "n"),
      right_columns = c("estimate", "p")
    ) +
    add_favors(left = "Left", right = "Right")

  expect_equal(favors_state(bare)$forest_index, 1L)
  expect_equal(favors_state(single)$forest_index, 2L)
  expect_equal(favors_state(split)$forest_index, 2L)
  expect_length(favors_state(bare)$widths, 1L)
  expect_length(favors_state(single)$widths, 2L)
  expect_length(favors_state(split)$widths, 3L)
})

test_that("the footer panel aligns with only the central forest column", {
  forest <- ggforestplot(
    make_favors_data(),
    n = "sample_size",
    p.value = "p_value"
  )
  out <- forest +
    add_split_table(
      left_columns = c("term", "n"),
      right_columns = c("estimate", "p")
    ) +
    add_favors(left = "Left", right = "Right")
  layout <- patchwork::patchworkGrob(out)$layout
  panels <- layout[
    grepl("^panel-[0-9]+$", layout$name),
    c("name", "t", "l", "b", "r"),
    drop = FALSE
  ]
  top_panels <- panels[panels$t == min(panels$t), , drop = FALSE]
  footer_panel <- panels[panels$t == max(panels$t), , drop = FALSE]
  forest_panel <- top_panels[favors_state(out)$forest_index, , drop = FALSE]

  expect_equal(nrow(top_panels), 3L)
  expect_equal(nrow(footer_panel), 1L)
  expect_equal(footer_panel$l, forest_panel$l)
  expect_equal(footer_panel$r, forest_panel$r)
})

test_that("add_favors rejects references outside the visible range", {
  forest <- ggforestplot(make_favors_data()) +
    ggplot2::scale_x_continuous(
      limits = c(1, 2),
      expand = ggplot2::expansion(mult = 0)
    )

  expect_error(
    forest + add_favors(left = "Left", right = "Right"),
    "inside the visible x range"
  )
})

test_that("add_favors rejects incompatible facet x scales", {
  data <- rbind(
    transform(
      make_favors_data(),
      section = "Low",
      estimate = estimate - 3,
      conf.low = conf.low - 3,
      conf.high = conf.high - 3
    ),
    transform(
      make_favors_data(),
      section = "High",
      estimate = estimate + 3,
      conf.low = conf.low + 3,
      conf.high = conf.high + 3
    )
  )
  forest <- ggforestplot(data, facet = "section") +
    ggplot2::facet_wrap(
      ggplot2::vars(grouping_panel),
      scales = "free_x"
    )

  expect_error(
    forest + add_favors(left = "Left", right = "Right"),
    "free or incompatible facet x scales"
  )
})

test_that("add_favors exposes arrow and text customization", {
  forest <- ggforestplot(make_favors_data()) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1),
      expand = ggplot2::expansion(mult = 0)
    )
  out <- forest + add_favors(
    left = "Custom left",
    right = "Custom right",
    gap = 0.05,
    footer_height = 0.55,
    text_size = 4,
    linewidth = 0.8,
    arrow_length = 0.12,
    arrow_type = "open"
  )
  state <- favors_state(out)
  footer <- state$footer

  expect_equal(state$gap, 0.05)
  expect_equal(state$footer_height, 0.55)
  expect_equal(state$text_size, 4)
  expect_equal(state$linewidth, 0.8)
  expect_equal(state$arrow_length, 0.12)
  expect_equal(state$arrow_type, "open")
  expect_equal(footer$layers[[1L]]$data$x, c(0.45, 0.55))
  expect_equal(footer$layers[[1L]]$data$xend, c(0, 1))
  expect_equal(footer$layers[[1L]]$data$y, c(0.78, 0.78))
  expect_equal(footer$layers[[1L]]$data$yend, c(0.78, 0.78))
  expect_equal(
    footer$layers[[2L]]$data$label,
    c("Custom left", "Custom right")
  )
  expect_equal(footer$layers[[2L]]$data$y, c(0.26, 0.26))
  expect_false(identical(footer$coordinates$clip, "off"))
})

test_that("add_favors validates composition and footer inputs", {
  plain <- ggplot2::ggplot(
    make_favors_data(),
    ggplot2::aes(estimate, term)
  ) +
    ggplot2::geom_point()
  forest <- ggforestplot(make_favors_data()) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1),
      expand = ggplot2::expansion(mult = 0)
    )

  expect_error(
    plain + add_favors(left = "Left", right = "Right"),
    "must be created by `ggforestplot\\(\\)`"
  )
  expect_error(
    forest + add_favors(left = "Left", right = "Right", gap = 0.5),
    "gap.*too large"
  )
  expect_error(
    add_favors(forest, left = NA_character_, right = "Right"),
    "`left` must be a single character string"
  )
  expect_error(
    add_favors(
      forest,
      left = "Left",
      right = "Right",
      reference = Inf
    ),
    "`reference` must be `NULL` or a single finite number"
  )
})
