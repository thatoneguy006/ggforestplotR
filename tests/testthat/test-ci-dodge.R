make_ci_dodge_data <- function() {
  data.frame(
    term = rep(c("Left", "Right", "Both", "Complete", "Single"),
               c(3, 3, 3, 3, 1)),
    parent = "Measures",
    model = factor(
      c(rep(c("B", "A", "C"), 4), "A"),
      levels = c("B", "A", "C")
    ),
    estimate = seq(-0.6, 0.6, length.out = 13),
    conf.low = c(-2, -0.8, -0.8, -0.8, -2, -0.8,
                 -2, -0.8, -2, -0.8, -0.8, -0.8, -2),
    conf.high = c(0.8, 0.8, 0.8, 2, 0.8, 0.8,
                  2, 0.8, 2, 0.8, 0.8, 0.8, 2)
  )
}

expect_ci_dodge_alignment <- function(plot, limits, exponentiate = FALSE) {
  built <- ggplot2::ggplot_build(plot)
  point_index <- which(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomPoint")
  }, logical(1)))
  points <- built$data[[point_index]]
  key <- function(data) paste(data$PANEL, data$colour, round(data$y))
  point_y <- stats::setNames(as.numeric(points$y), key(points))
  ci_indices <- which(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomSegment") ||
      inherits(layer$geom, "GeomErrorbar")
  }, logical(1)))
  expect_gt(length(ci_indices), 2L)

  for (i in ci_indices) {
    geometry <- built$data[[i]]
    expect_equal(as.numeric(geometry$y), unname(point_y[key(geometry)]))
    if ("yend" %in% names(geometry)) {
      expect_equal(as.numeric(geometry$yend), as.numeric(geometry$y))
      boundaries <- if (exponentiate) log10(limits) else limits
      expect_true(all(geometry$xend %in% boundaries))
    }
  }

  # CI clipping must leave the point positions and group order unchanged.
  unclipped <- ggplot2::ggplot_build(ggforestplot(
    make_ci_dodge_data(), group = "model", subgroup = "parent",
    dodge_width = plot$ggforestplotR_state$defaults$dodge_width
  ))
  plain_points <- Filter(function(data) "shape" %in% names(data),
                         unclipped$data)[[1L]]
  expect_equal(sort(unique(as.numeric(points$y))),
               sort(unique(as.numeric(plain_points$y))))
}

test_that("CI arrows and caps keep point dodge slots with groups and subgroups", {
  raw <- make_ci_dodge_data()
  for (width in c(0, 0.6, 0.9)) {
    plot <- ggforestplot(
      raw, group = "model", subgroup = "parent",
      ci_limits = c(-1, 1), dodge_width = width
    )
    expect_ci_dodge_alignment(plot, c(-1, 1))
  }
})

test_that("CI dodging aligns across facets and on log scales", {
  raw <- make_ci_dodge_data()
  raw <- rbind(transform(raw, section = "One"),
               transform(raw, section = "Two"))
  raw[c("estimate", "conf.low", "conf.high")] <- lapply(
    raw[c("estimate", "conf.low", "conf.high")], exp
  )
  plot <- ggforestplot(
    raw, group = "model", subgroup = "parent", facet = "section",
    exponentiate = TRUE, ci_limits = exp(c(-1, 1))
  )
  expect_ci_dodge_alignment(plot, exp(c(-1, 1)), exponentiate = TRUE)
})

test_that("CI dodging supports ordinary grouped and ungrouped plots", {
  raw <- make_ci_dodge_data()
  plot <- ggforestplot(raw, group = "model", ci_limits = c(-1, 1))
  # Use a single estimate per row for the ungrouped case.
  single <- raw[!duplicated(raw$term), ]
  ungrouped <- ggforestplot(single, ci_limits = c(-1, 1))
  for (p in list(plot, ungrouped)) {
    built <- ggplot2::ggplot_build(p)
    points <- Filter(function(data) "shape" %in% names(data), built$data)[[1L]]
    for (geometry in Filter(function(data) "yend" %in% names(data), built$data)) {
      expect_equal(as.numeric(geometry$y), as.numeric(geometry$yend))
      expect_true(all(geometry$y %in% points$y))
    }
  }
})
