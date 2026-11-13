styled_table_layers <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  indices <- which(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomText")
  }, logical(1)))
  columns <- c("label", "size", "fontface", "family", "colour", "y", "PANEL")
  do.call(rbind, lapply(built$data[indices], function(data) data[, columns]))
}

test_that("themes inherit and explicit table settings win without mutating plots", {
  original <- make_structured_plot()
  themed <- original + forest_theme(
    base_family = "serif", text_colour = "navy",
    reference_fontface = "italic", row_header_fontface = "bold",
    header_fontface = "italic"
  )
  expect_null(original$ggforestplotR_state$table_text_style)
  for (adder in list(
    add_forest_table(columns = c("term", "estimate")),
    add_split_table(left_columns = "term", right_columns = "estimate")
  )) {
    composition <- themed + adder
    panels <- attr(composition, "ggforestplotR_composition")$panels
    tables <- panels[vapply(panels, function(p) {
      is.null(p$ggforestplotR_state)
    }, logical(1))]
    for (table in tables) {
      cells <- styled_table_layers(table)
      expect_true(all(cells$family == "serif"))
      expect_true(all(cells$colour == "navy"))
      expect_equal(table$theme$axis.text.x.top$face, "italic")
      expect_true(all(cells$fontface[cells$label %in% c("Female", "Reference")] ==
                        "italic"))
      expect_true(all(cells$fontface[cells$label == "Section"] == "bold"))
    }
    expect_s3_class(patchwork::patchworkGrob(composition), "gtable")
  }
  override <- add_forest_table(
    themed, text_colour = "red", header_fontface = "plain",
    reference_fontface = "bold.italic", reference_family = "mono",
    reference_text_size = 5, row_header_colour = "darkgreen"
  )
  cells <- styled_table_layers(override[[1]])
  ref <- cells$label == "Reference"
  expect_true(any(ref))
  expect_equal(cells$size[ref], 5)
  expect_equal(cells$fontface[ref], "bold.italic")
  expect_equal(cells$family[ref], "mono")
  expect_equal(cells$colour[ref], "red")
  expect_equal(cells$colour[cells$label == "Section"], "darkgreen")
  expect_equal(override[[1]]$theme$axis.text.x.top$face, "plain")
  expect_equal(themed$ggforestplotR_state$table_text_style$text_colour, "navy")
})

test_that("direct and add-on syntax produce the same styled tables", {
  p <- make_structured_plot()
  for (fun in list(add_forest_table, add_split_table)) {
    args <- list(text_size = 4, text_family = "serif",
                 text_fontface = "italic", text_colour = "navy",
                 header_colour = "red", reference_fontface = 2,
                 row_header_text_size = 5)
    direct <- do.call(fun, c(list(plot = p), args))
    added <- p + do.call(fun, args)
    for (i in seq_along(attr(direct, "ggforestplotR_composition")$panels)) {
      expect_equal(ggplot2::ggplot_build(direct[[i]])$data,
                   ggplot2::ggplot_build(added[[i]])$data)
    }
  }
})

test_that("font measurement uses row-specific face family and size", {
  p <- make_structured_plot()
  spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    display_data = p$ggforestplotR_state$display_data,
    columns = "term"
  )
  normal <- layout_center_table_spec(spec)
  spec$text_style <- resolve_forest_text_style(overrides = list(
    reference_text_size = 20, reference_fontface = "bold",
    reference_family = "mono"
  ))
  large <- layout_center_table_spec(spec)
  expect_gt(large$content_width, normal$content_width)
})

test_that("text styles remain aligned through grouped facets and row filtering", {
  d <- make_grouped_subgroup_data()
  d$panel <- ifelse(d$term %in% c("Age", "White", "Black"), "First", "Second")
  p <- ggforestplot(d, group = "model", subgroup = "subgroup_name",
                    facet = "panel") +
    forest_theme(text_colour = "navy", row_header_colour = "red",
                 row_header_fontface = "bold")
  filtered <- p + ggplot2::scale_y_discrete(
    limits = c("Black", "Race", "Age", "BMI")
  )
  out <- filtered + add_split_table(left_columns = "term", right_columns = "estimate")
  for (i in c(1, 3)) {
    table <- out[[i]]
    cells <- styled_table_layers(table)
    expect_true(all(cells$colour[cells$label == "Race"] == "red"))
    expect_true(all(cells$fontface[cells$label == "Race"] == "bold"))
  }
  complete <- p + add_split_table(left_columns = "term", right_columns = "estimate")
  expect_s3_class(patchwork::patchworkGrob(complete + add_favors(
    left = "Lower", right = "Higher"
  )), "gtable")
})

test_that("invalid styles and unsupported theme placement fail clearly", {
  for (fun in list(forest_theme, add_forest_table, add_split_table)) {
    expect_error(fun(text_size = -1), "text_size")
    expect_error(fun(reference_text_size = Inf), "reference_text_size")
    expect_error(fun(text_fontface = "heavy"), "text_fontface")
    expect_error(fun(header_family = NA_character_), "header_family")
    expect_error(fun(reference_colour = "not-a-colour"), "reference_colour")
    expect_error(fun(row_header_colour = c("red", "blue")), "row_header_colour")
  }
  p <- make_structured_plot()
  expect_error(p + add_forest_table() + forest_theme(), "before composing")
  expect_error(ggplot2::ggplot() + forest_theme(), "forest plot")
})
