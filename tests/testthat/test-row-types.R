test_that("row types normalize and require appropriate geometry", {
  raw <- structured_rows()
  raw$row_type[2] <- " Reference "
  out <- as_structured_data(raw, row_type = "row_type")
  expect_equal(out$row_type[2], "reference")
  expect_equal(out$label[4:5], c("", ""))
  expect_equal(unname(forest_metadata(out)$column_mapping[["row_type"]]), "row_type")

  raw$row_type[2] <- "ref"
  expect_error(as_structured_data(raw, row_type = "row_type"), "Unsupported.*ref")
  raw$row_type[2] <- NA_character_
  expect_error(as_structured_data(raw, row_type = "row_type"), "Unsupported.*NA")
  raw$row_type[2] <- "reference"
  raw$estimate[2] <- 1
  expect_error(as_structured_data(raw, row_type = "row_type"), "cannot contain estimate")
  raw$estimate[2] <- NA_real_
  raw$conf.low[3] <- NA_real_
  expect_error(as_structured_data(raw, row_type = "row_type"), "conf.low.*finite")
  raw$conf.low[3] <- 1.1
  raw$estimate[6] <- NA_real_
  expect_error(as_structured_data(raw, row_type = "row_type"), "estimate.*finite")
  raw <- structured_rows()
  expect_error(as_structured_data(raw[1:2, ], row_type = "row_type"),
               "at least one")
  expect_error(as_structured_data(raw, row_type = "row_type",
                              sort_terms = "ascending"), "sort_terms.*none")
  raw$term[2] <- ""
  expect_error(as_structured_data(raw, row_type = "row_type"), "non-empty")
})

test_that("structured rows preserve order and draw geometry only where required", {
  p <- make_structured_plot()
  display <- p$ggforestplotR_state$display_data
  expect_equal(display$row_type, structured_rows()$row_type)
  expect_equal(length(unique(as.character(display$row_key))), 6L)
  expect_equal(rev(levels(display$row_key)), as.character(display$row_key))
  expect_equal(display$display_label[4:5], c("", ""))
  expect_true(all(!is.na(p$ggforestplotR_state$forest_data$row_key)))
  built <- ggplot2::ggplot_build(p)
  point_index <- which(vapply(p$layers, function(x) inherits(x$geom, "GeomPoint"), logical(1)))
  expect_equal(nrow(built$data[[point_index]]), 2L)
  expect_true(any(vapply(p$layers, function(x) inherits(x$geom, "GeomBlank"), logical(1))))
})

test_that("reference, header, summary, and spacer table cells follow semantics", {
  p <- make_structured_plot()
  spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    display_data = p$ggforestplotR_state$display_data,
    columns = c("term", "n", "estimate", "ci", "p", "extra")
  )
  expect_equal(table_cell(spec, "estimate", "reference"), "Reference")
  expect_equal(table_cell(spec, "ci", "reference"), "")
  expect_equal(table_cell(spec, "p", "reference"), "")
  expect_equal(table_cell(spec, "n", "reference"), "60")
  expect_equal(table_cell(spec, "extra", "reference"), "Ref")
  expect_equal(table_cell(spec, "estimate", "summary"), "1.18")
  expect_equal(table_cell(spec, "p", "header"), "0.030")
  expect_equal(table_cell(spec, "extra", "header"), "Head")
  expect_true(all(spec$table_data$text[spec$table_data$row_type == "spacer"] == ""))
  spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    display_data = p$ggforestplotR_state$display_data,
    columns = "estimate", reference_text = "Ref."
  )
  expect_equal(table_cell(spec, "estimate", "reference"), "Ref.")
})

test_that("subgroup reference children are indented and generated headers stay marked", {
  raw <- structured_rows()[2:3, ]
  raw$subgroup <- "Sex"
  p <- ggforestplot(raw, row_type = "row_type", subgroup = "subgroup", ref_line = NULL)
  display <- p$ggforestplotR_state$display_data
  expect_equal(display$row_type, c("header", "reference", "estimate"))
  expect_equal(display$.forest_generated_subgroup_header, c(TRUE, FALSE, FALSE))
  expect_equal(display$display_label[2], "   Female")
  expect_equal(display$display_label[3], "   Male")
})

test_that("semantic scale checks apply to geometry rows only", {
  raw <- structured_rows()
  expect_s3_class(as_structured_data(raw, row_type = "row_type",
                                      estimate_scale = "ratio"), "forest_data")
  raw$estimate[3] <- 0
  expect_error(as_structured_data(raw, row_type = "row_type",
                                  estimate_scale = "ratio"), "strictly positive")
  raw <- structured_rows()
  raw$estimate[3] <- 1.2
  expect_error(as_structured_data(raw, row_type = "row_type",
                                  estimate_scale = "probability"), "between 0 and 1")
  raw <- structured_rows()
  raw$estimate[3] <- 1.2
  expect_error(as_structured_data(raw, row_type = "row_type",
                                  estimate_scale = "risk_difference"), "between -1 and 1")
})

test_that("grouped estimates still share rows while reference rows remain separate", {
  raw <- data.frame(
    term = c("Female", "Female", "Female", "Female"),
    estimate = c(NA, NA, 1.2, 1.4),
    conf.low = c(NA, NA, 1.0, 1.2),
    conf.high = c(NA, NA, 1.4, 1.6),
    type = c("reference", "reference", "estimate", "estimate"),
    model = c("A", "B", "A", "B")
  )
  p <- ggforestplot(raw, row_type = "type", group = "model", ref_line = NULL)
  display <- p$ggforestplotR_state$display_data
  expect_equal(length(unique(as.character(display$row_key))), 2L)
  expect_equal(length(unique(as.character(display$row_key[1:2]))), 1L)
  expect_equal(length(unique(as.character(display$row_key[3:4]))), 1L)
  spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    display_data = display, columns = "estimate"
  )
  expect_equal(table_cell(spec, "estimate", "reference"), "Reference\nReference")
})
