test_that("ref_color aliases ref_colour with a classed warning", {
  d <- make_simple_forest_data()
  expect_warning(
    old <- ggforestplot(d, ref_color = "red", ref_label = "Null"),
    class = "ggforestplotR_deprecated_argument"
  )
  new <- ggforestplot(d, ref_colour = "red", ref_label = "Null")
  expect_identical(ggplot2::ggplot_build(old)$data,
                   ggplot2::ggplot_build(new)$data)
  expect_equal(match("ref_colour", names(formals(ggforestplot))), 39L)
  expect_error(
    ggforestplot(d, ref_colour = "red", ref_color = "blue"),
    "Use only one of"
  )
})

test_that("separator_line_size aliases separator_linewidth", {
  d <- make_table_forest_data()
  expect_warning(
    old <- ggforestplot(d, separate_groups = "term", separate_lines = TRUE,
                 separator_line_size = 0.8),
    class = "ggforestplotR_deprecated_argument"
  )
  new <- ggforestplot(d, separate_groups = "term", separate_lines = TRUE,
                      separator_linewidth = 0.8)
  expect_identical(ggplot2::ggplot_build(old)$data,
                   ggplot2::ggplot_build(new)$data)
  expect_equal(match("separator_linewidth", names(formals(ggforestplot))), 31L)
  expect_error(
    ggforestplot(d, separator_linewidth = 0.8, separator_line_size = 0.3),
    "Use only one of"
  )
})

test_that("grid_line_size aliases grid_linewidth in both table call styles", {
  expect_warning(
    old <- add_forest_table(grid_lines = TRUE, grid_line_size = 0.8),
    class = "ggforestplotR_deprecated_argument"
  )
  new <- add_forest_table(grid_lines = TRUE, grid_linewidth = 0.8)
  expect_identical(old, new)
  expect_equal(match("grid_linewidth", names(formals(add_forest_table))), 28L)

  p <- ggforestplot(make_table_forest_data())
  expect_warning(
    direct <- add_forest_table(p, grid_lines = TRUE, grid_line_size = 0.8),
    class = "ggforestplotR_deprecated_argument"
  )
  added <- p + new
  expect_identical(ggplot2::ggplot_build(direct[[1]])$data,
                   ggplot2::ggplot_build(added[[1]])$data)
  expect_error(
    add_forest_table(grid_linewidth = 0.8, grid_line_size = 0.3),
    "Use only one of"
  )
})

test_that("data-frame grouping aliases facet without renaming stored columns", {
  d <- make_table_forest_data()
  d$section <- factor(c("Clinical", "Clinical", "Treatment"))
  expect_warning(
    old <- as_forest_data(d, grouping = "section"),
    class = "ggforestplotR_deprecated_argument"
  )
  new <- as_forest_data(d, facet = "section")
  expect_identical(old, new)
  expect_equal(match("facet", names(formals(as_forest_data.data.frame))), 9L)
  expect_equal(new$grouping, as.character(d$section))
  expect_equal(unname(forest_metadata(new)$column_mapping[["grouping"]]),
               "section")
  expect_error(
    as_forest_data(d, facet = "section", grouping = "section"),
    "Use only one of"
  )
})
