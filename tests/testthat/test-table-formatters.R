test_that("formatter specifications and results are validated", {
  raw <- structured_rows()
  p <- make_structured_plot(raw)
  data <- p$ggforestplotR_state$forest_data
  display <- p$ggforestplotR_state$display_data
  build <- function(formatters) build_forest_table_data(
    data, display_data = display, columns = c("estimate", "p"),
    formatters = formatters
  )
  expect_error(build(list(function(x) x)), "named list")
  expect_error(build("bad"), "named list")
  expect_error(build(stats::setNames(list(identity), "")), "non-empty")
  expect_error(build(list(p = identity, p.value = identity)), "unique")
  expect_error(build(list(p = "bad")), "function")
  expect_error(build(list(unknown = identity)), "Unknown formatter")
  expect_error(build(list(estimate = function(x) stop("boom"))),
               "Formatter for `estimate` failed: boom")
  expect_error(apply_table_formatter(1:2, function(x) "fixed", key = "estimate"),
               "one value per input")
  expect_error(apply_table_formatter(1:2, function(x) list("a", "b"), key = "n"),
               "atomic vector")
  expect_error(build_forest_table_data(data, display_data = display,
                                       reference_text = NA_character_), "reference_text")
})

test_that("estimate, CI, and p formatters precede templates and digits", {
  p <- make_structured_plot()
  args <- list(
    data = p$ggforestplotR_state$forest_data,
    display_data = p$ggforestplotR_state$display_data,
    columns = c("estimate", "ci", "p"),
    p_digits = 1,
    estimate_fmt = "{estimate} [{conf.low}, {conf.high}]",
    ci_fmt = "<{conf.low}|{conf.high}>",
    formatters = list(
      estimate = function(x) sprintf("%.1f", x),
      ci = function(x) sprintf("%.3f", x),
      conf.low = function(x) paste0("L", sprintf("%.2f", x)),
      p.value = function(x) sprintf("P%.3f", x)
    )
  )
  spec <- do.call(build_forest_table_data, args)
  expect_equal(table_cell(spec, "estimate", "estimate"), "1.4 [L1.10, 1.840]")
  expect_equal(table_cell(spec, "ci", "estimate"), "<L1.10|1.840>")
  expect_equal(table_cell(spec, "p", "estimate"), "P0.008")
  expect_equal(table_cell(spec, "p", "header"), "P0.030")
  expect_equal(table_cell(spec, "estimate", "reference"), "Reference")
  args$columns <- "estimate"
  spec <- do.call(build_forest_table_data, args)
  expect_equal(table_cell(spec, "estimate", "estimate"), "1.4 [L1.10, 1.840]")
})

test_that("custom source columns and non-geometric values are formatted", {
  raw <- structured_rows()
  raw$prevalence <- c(NA, 0.2, 0.33, NA, NA, 0.4)
  raw$date <- as.Date("2026-01-01") + seq_len(nrow(raw))
  p <- make_structured_plot(raw)
  spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    display_data = p$ggforestplotR_state$display_data,
    columns = c("term", "n", "prevalence", "date"),
    formatters = list(
      n = function(x) paste0("N=", x),
      prevalence = function(x) ifelse(is.na(x), NA_character_,
                                      paste0(round(100 * x), "%")),
      date = function(x) format(x, "%Y/%m/%d")
    )
  )
  expect_equal(table_cell(spec, "n", "reference"), "N=60")
  expect_equal(table_cell(spec, "prevalence", "reference"), "20%")
  expect_match(table_cell(spec, "date", "reference"), "^2026/")
  expect_true(all(spec$table_data$text[spec$table_data$row_type == "spacer"] == ""))
})

test_that("both public table adders accept formatters and reference text", {
  skip_if_not_installed("patchwork")
  p <- make_structured_plot()
  formatter <- list(estimate = function(x) sprintf("%.1f", x))
  expect_s3_class(
    p + add_forest_table(columns = c("term", "estimate"),
                         formatters = formatter, reference_text = "Ref."),
    "patchwork"
  )
  expect_s3_class(
    p + add_split_table(left_columns = "term", right_columns = "estimate",
                        formatters = formatter, reference_text = "Ref."),
    "patchwork"
  )
})

test_that("generated subgroup p-values use the custom formatter", {
  raw <- structured_rows()[2:3, ]
  raw$subgroup <- "Sex"
  raw$p.value[2] <- 0.007
  p <- ggforestplot(raw, row_type = "row_type", subgroup = "subgroup",
                    p.value = "p.value", ref_line = NULL)
  spec <- build_forest_table_data(
    p$ggforestplotR_state$forest_data,
    display_data = p$ggforestplotR_state$display_data,
    columns = c("estimate", "p"),
    formatters = list(p = function(x) sprintf("P%.4f", x))
  )
  expect_equal(table_cell(spec, "p", "header"), "P0.0070")
  expect_equal(table_cell(spec, "p", "reference"), "")
  expect_equal(table_cell(spec, "p", "estimate"), "")
})
