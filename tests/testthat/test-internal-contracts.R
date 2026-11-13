test_that("text options retain explicit inheritance and normalize at resolution", {
  options <- forest_text_settings(text_family = "serif", reference_fontface = 2)
  expect_null(options$reference_family)
  expect_equal(options$reference_fontface, 2)
  resolved <- resolve_forest_text_style(
    inherited = options, overrides = list(text_family = "mono")
  )
  expect_equal(resolved$reference_family, "mono")
  expect_equal(resolved$header_family, "mono")
  expect_equal(resolved$reference_fontface, "bold")
  expect_null(options$reference_family)
  expect_equal(options$text_family, "serif")
})

test_that("style errors identify the public call even for unusual input types", {
  for (bad in list(environment(), identity, list("bold"), NA, numeric())) {
    error <- tryCatch(add_forest_table(text_fontface = bad), error = identity)
    expect_s3_class(error, "ggforestplotR_invalid_text_style")
    expect_s3_class(error, "ggforestplotR_error")
    expect_equal(error$call[[1]], as.name("add_forest_table"))
  }
  expect_error(
    resolve_forest_text_style(overrides = list(typo = "red")),
    class = "ggforestplotR_invalid_text_style"
  )
  expect_error(
    resolve_forest_text_style(overrides = list(text_size = 3, text_size = 4)),
    class = "ggforestplotR_invalid_text_style"
  )
})

test_that("formatter errors retain the original condition and result context", {
  original <- structure(
    list(message = "source formatter failed", call = NULL, detail = "source"),
    class = c("source_formatter_error", "error", "condition")
  )
  error <- tryCatch(
    apply_table_formatter(1:2, function(x) stop(original), key = "estimate"),
    error = identity
  )
  expect_s3_class(error, "ggforestplotR_formatter_error")
  expect_identical(error$parent, original)
  expect_match(conditionMessage(error), "source formatter failed")
  expect_error(
    apply_table_formatter(1:3, function(x) "one", key = "p"),
    "Expected 3 values, but received 1",
    class = "ggforestplotR_invalid_formatter_result"
  )
})

test_that("cell text styles preserve column types for empty inputs", {
  empty <- data.frame(row_type = character(), text = character())
  style <- resolve_forest_text_style()
  cells <- forest_cell_text_style(empty, style)
  expect_identical(cells$size, numeric())
  expect_identical(cells$fontface, character())
  expect_identical(cells$family, character())
  expect_identical(cells$colour, character())
  expect_identical(measure_styled_table_cells(empty, style), 0)
})
