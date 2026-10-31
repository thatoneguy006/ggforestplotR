grouped_subgroup_plot <- function(raw = make_grouped_subgroup_data(), ...) {
  ggforestplot(
    raw,
    group = "model",
    subgroup = "subgroup_name",
    p.value = "p_value",
    ref_line = NULL,
    ...
  ) + ggplot2::theme(legend.position = "bottom")
}

layer_data_for_geom <- function(plot, geom) {
  indices <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, geom),
    logical(1)
  ))
  data <- lapply(ggplot2::ggplot_build(plot)$data[indices], function(layer) {
    layer[intersect(c("x", "y", "label", "PANEL"), names(layer))]
  })
  do.call(rbind, data)
}

expect_grouped_table_alignment <- function(composition) {
  panels <- attr(composition, "ggforestplotR_composition")$panels
  forest <- Filter(function(p) !is.null(p$ggforestplotR_state), panels)[[1L]]
  forest_build <- ggplot2::ggplot_build(forest)
  points <- layer_data_for_geom(forest, "GeomPoint")
  points <- points[is.finite(points$x) & is.finite(points$y), ]
  point_keys <- paste(points$PANEL, sprintf("%.2f", points$x))

  for (table in Filter(function(p) is.null(p$ggforestplotR_state), panels)) {
    table_build <- ggplot2::ggplot_build(table)
    expect_equal(
      lapply(table_build$layout$panel_params, function(p) p$y$get_limits()),
      lapply(forest_build$layout$panel_params, function(p) p$y$get_limits())
    )
    expect_equal(
      lapply(table_build$layout$panel_params, function(p) p$y.range),
      lapply(forest_build$layout$panel_params, function(p) p$y.range)
    )

    text <- layer_data_for_geom(table, "GeomText")
    estimate_x <- unique(table$data$column_position[
      table$data$column_key == "estimate"
    ])
    if (length(estimate_x) == 0L) next

    estimates <- text[text$x %in% estimate_x & nzchar(text$label), ]
    estimate_keys <- paste(estimates$PANEL, estimates$label)
    matched <- match(estimate_keys, point_keys)
    expect_equal(nrow(estimates), nrow(points))
    expect_false(anyNA(matched))
    expect_equal(as.numeric(estimates$y), as.numeric(points$y[matched]))
  }
}

test_that("split tables align grouped subgroup estimates on either side", {
  for (striped_rows in c(FALSE, TRUE)) {
    for (dodge_width in c(0.6, 0.9)) {
      for (p_method in c("overall", "level")) {
        plot <- grouped_subgroup_plot(
          striped_rows = striped_rows,
          dodge_width = dodge_width,
          p_method = p_method
        )
        for (estimate_side in c("left", "right")) {
          columns <- list(c("term", "group"), c("estimate", "ci", "p"))
          if (estimate_side == "left") columns <- rev(columns)
          out <- add_split_table(
            plot,
            left_columns = columns[[1L]],
            right_columns = columns[[2L]]
          )
          expect_grouped_table_alignment(out)
        }
      }
    }
  }
})

test_that("grouped subgroup tables align in free-y facets and sparse groups", {
  first <- make_grouped_subgroup_data()
  first$section <- "First"
  second <- first[1:7, ]
  second$section <- "Second"
  raw <- rbind(first, second)
  plot <- grouped_subgroup_plot(raw, facet = "section", dodge_width = 0.8)

  expect_grouped_table_alignment(add_split_table(
    plot,
    left_columns = c("term", "group"),
    right_columns = c("estimate", "ci", "p")
  ))
  expect_grouped_table_alignment(add_forest_table(
    plot,
    columns = c("estimate", "ci", "p")
  ))
})

test_that("grouped subgroup tables respect custom row selections", {
  raw <- make_grouped_subgroup_data()
  raw$model <- as.character(raw$model)
  plot <- suppressMessages(
    grouped_subgroup_plot(raw) +
      ggplot2::scale_y_discrete(
        limits = c("Black", "Race", "Age", "Female", "Sex")
      )
  )

  expect_grouped_table_alignment(add_split_table(
    plot,
    left_columns = c("term", "group"),
    right_columns = c("estimate", "ci", "p")
  ))
})

test_that("split tables render aligned grouped subgroup rows", {
  skip_if_not_installed("vdiffr")
  plot <- grouped_subgroup_plot(striped_rows = TRUE, dodge_width = 0.8)
  out <- add_split_table(
    plot,
    left_columns = c("term", "group"),
    right_columns = c("estimate", "p")
  )

  vdiffr::expect_doppelganger("split table grouped subgroups", out)
})

test_that("split tables render aligned grouped subgroup facets", {
  skip_if_not_installed("vdiffr")
  first <- make_grouped_subgroup_data()
  first$section <- "First"
  second <- first[1:7, ]
  second$section <- "Second"
  plot <- grouped_subgroup_plot(
    rbind(first, second), facet = "section", dodge_width = 0.8
  )
  out <- add_split_table(
    plot,
    left_columns = c("term", "group"),
    right_columns = c("estimate", "ci", "p")
  )

  vdiffr::expect_doppelganger("split table grouped subgroup facets", out)
})
