point_size_data <- function() {
  raw <- make_table_forest_data()
  raw$N <- c(40, 100, 250)
  raw$section <- c("Clinical", "Clinical", "Treatment")
  raw
}

point_size_layer <- function(plot) {
  index <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  ))
  ggplot2::ggplot_build(plot)$data[[index]]
}

test_that("point_size maps a numeric source column and supports size scales", {
  raw <- point_size_data()
  plot <- ggforestplot(raw, point_size = "N")
  built <- ggplot2::ggplot_build(plot)
  size_scale <- built$plot$scales$get_scales("size")

  expect_equal(point_size_layer(plot)$size, size_scale$map(raw$N))
  expect_equal(plot$labels$size, "N")
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")

  scaled <- plot + ggplot2::scale_size_continuous(range = c(2, 8))
  expect_equal(range(point_size_layer(scaled)$size), c(2, 8))
})

test_that("numeric point_size retains constant marker sizes", {
  raw <- point_size_data()
  for (size in c(0, 2.3, 4)) {
    plot <- ggforestplot(raw, point_size = size)
    expect_equal(point_size_layer(plot)$size, rep(size, nrow(raw)))
    expect_null(ggplot2::ggplot_build(plot)$plot$scales$get_scales("size"))
  }
})

test_that("point sizes follow source rows through sorting and facets", {
  raw <- point_size_data()
  plot <- ggforestplot(
    raw, point_size = "N", facet = "section", sort_terms = "ascending"
  )
  points <- point_size_layer(plot)
  size_scale <- ggplot2::ggplot_build(plot)$plot$scales$get_scales("size")
  rows <- match(points$x, raw$estimate)

  expect_equal(points$size, size_scale$map(raw$N[rows]))
  expect_equal(length(unique(points$PANEL)), 2L)
  out <- add_split_table(
    plot, left_columns = "section", right_columns = c("term", "estimate")
  )
  forest <- attr(out, "ggforestplotR_composition")$panels[[2L]]
  expect_equal(point_size_layer(forest)$size, points$size)
  expect_s3_class(patchwork::patchworkGrob(out), "gtable")
})

test_that("point sizes resolve preserved source names and canonical collisions", {
  raw <- point_size_data()
  raw$n <- raw$N
  raw$row_key <- rev(raw$N)
  raw$ci_estimate <- rev(raw$N)
  raw$beta <- raw$estimate
  raw$estimate <- raw$N
  forest <- as_forest_data(
    raw, term = "term", estimate = "beta", conf.low = "conf.low",
    conf.high = "conf.high", n = "n"
  )

  for (column in c("n", "row_key", "estimate", "ci_estimate")) {
    plot <- ggforestplot(forest, point_size = column)
    size_scale <- ggplot2::ggplot_build(plot)$plot$scales$get_scales("size")
    expect_equal(point_size_layer(plot)$size, size_scale$map(raw[[column]]))
  }
})

test_that("point_size can map numeric columns from fitted model output", {
  skip_if_not_installed("broom")
  plot <- ggforestplot(lm(mpg ~ wt + hp, data = mtcars),
                       point_size = "std.error")
  size_scale <- ggplot2::ggplot_build(plot)$plot$scales$get_scales("size")
  values <- plot$ggforestplotR_state$forest_data$std.error

  expect_equal(point_size_layer(plot)$size, size_scale$map(values))
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("non-geometry rows do not require point sizes", {
  raw <- structured_rows()
  plot <- make_structured_plot(raw, point_size = "n")

  expect_equal(nrow(point_size_layer(plot)), 2L)
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("point sizes survive grouped subgroup composition and row filtering", {
  raw <- make_grouped_subgroup_data()
  raw$N <- seq_len(nrow(raw)) * 10
  plot <- suppressMessages(
    ggforestplot(
      raw, group = "model", subgroup = "subgroup_name", point_size = "N"
    ) + ggplot2::scale_y_discrete(limits = c("Black", "Race", "Age"))
  )
  out <- add_split_table(
    plot, left_columns = c("term", "group"), right_columns = "estimate"
  )
  forest <- attr(out, "ggforestplotR_composition")$panels[[2L]]
  points <- point_size_layer(forest)
  size_scale <- ggplot2::ggplot_build(forest)$plot$scales$get_scales("size")
  points <- points[is.finite(points$y), ]
  rows <- match(points$x, raw$estimate)

  expect_equal(nrow(points), 4L)
  expect_equal(points$size, size_scale$map(raw$N[rows]))
})

test_that("point_size rejects invalid inputs before rendering", {
  raw <- point_size_data()
  for (value in list(NULL, TRUE, NA_real_, Inf, -1, c(1, 2),
                     NA_character_, "", c("N", "section"))) {
    expect_error(ggforestplot(raw, point_size = value), "`point_size`")
  }
  expect_error(ggforestplot(raw, point_size = "missing"), "not found")

  for (values in list(c("40", "100", "250"), factor(c(40, 100, 250)),
                      c(40, NA, 250), c(40, Inf, 250), c(40, -1, 250))) {
    raw$N <- values
    expect_error(ggforestplot(raw, point_size = "N"), "numeric values")
  }
})

test_that("split tables render points sized by a source column", {
  skip_if_not_installed("vdiffr")
  plot <- ggforestplot(point_size_data(), point_size = "N") +
    ggplot2::scale_size_continuous(range = c(2, 6)) +
    ggplot2::theme(legend.position = "bottom")
  out <- add_split_table(
    plot,
    left_columns = "section",
    right_columns = c("term", "estimate"),
    left_width = 1,
    right_width = 3
  )

  vdiffr::expect_doppelganger("split table with mapped point sizes", out)
})
