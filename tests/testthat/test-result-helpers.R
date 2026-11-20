test_that("result labels use bare values for one varying identifier", {
  table <- data.frame(
    term = c("trt", "trt"),
    contrast = c("B - A", "B - A"),
    sex = c("F", "M")
  )

  expect_equal(
    compose_result_labels(table, c("term", "contrast", "sex"), c("term", "contrast")),
    c("F", "M")
  )
})

test_that("result labels combine several varying identifiers", {
  table <- data.frame(
    term = c("hp", "cyl", "cyl", "cyl"),
    contrast = c("+1", "6 - 4", "8 - 4", "8 - 4"),
    am = c(0, 0, 0, 1)
  )

  expect_equal(
    compose_result_labels(table, c("term", "contrast", "am"), c("term", "contrast")),
    c("hp: +1, am = 0", "cyl: 6 - 4, am = 0", "cyl: 8 - 4, am = 0", "cyl: 8 - 4, am = 1")
  )
  expect_equal(
    compose_result_labels(
      table[1:3, ],
      c("term", "contrast"),
      c("term", "contrast"),
      term_labels = c(cyl = "Cylinders")
    ),
    c("hp: +1", "Cylinders: 6 - 4", "Cylinders: 8 - 4")
  )
})

test_that("constant identifiers still produce a label", {
  table <- data.frame(term = "trt", contrast = "1 - 0")
  expect_equal(compose_result_labels(table, c("term", "contrast"), "term"), "trt")
  expect_equal(compose_result_labels(data.frame(x = 1), character()), "Estimate")
})

test_that("duplicated labels gain row identifiers", {
  expect_equal(
    deduplicate_result_labels(c("A", "A", "B"), row_ids = c(10, 11, 12)),
    c("A [10]", "A [11]", "B")
  )
  expect_equal(
    deduplicate_result_labels(c("A", "A"), keys = c("x", "y")),
    c("A", "A")
  )
})

test_that("subgroup blocks are gathered in order of first appearance", {
  subgroup <- c("a", NA, "b", "a", "b")
  expect_equal(contiguous_subgroup_order(subgroup), c(1L, 4L, 2L, 3L, 5L))
})

test_that("exponentiate converts between log and ratio scales", {
  table <- data.frame(estimate = log(2), conf.low = log(1.5), conf.high = log(3))
  converted <- apply_result_exponentiate(
    table,
    result_scale("log", "log(OR)"),
    exponentiate = TRUE
  )
  expect_equal(converted$table$estimate, 2)
  expect_equal(converted$scale$estimate_scale, "ratio")
  expect_equal(converted$scale$effect_label, "OR")
  expect_equal(converted$scale$reference_value, 1)

  back <- apply_result_exponentiate(converted$table, converted$scale, FALSE)
  expect_equal(back$table$conf.high, log(3))
  expect_equal(back$scale$effect_label, "log(OR)")

  expect_error(
    apply_result_exponentiate(table, result_scale("identity", "Difference"), TRUE),
    class = "ggforestplotR_invalid_data"
  )
  expect_identical(
    apply_result_exponentiate(table, result_scale("identity", "Difference"), NULL)$table,
    table
  )
})

test_that("stored confidence levels can only be confirmed", {
  expect_equal(resolve_result_conf_level(NULL, 0.9, "`x`"), 0.9)
  expect_true(is.na(resolve_result_conf_level(NULL, NULL, "`x`")))
  expect_equal(resolve_result_conf_level(0.9, 0.9, "`x`"), 0.9)
  expect_equal(resolve_result_conf_level(0.8, NA_real_, "`x`"), 0.8)
  expect_error(
    resolve_result_conf_level(0.95, 0.9, "`x`"),
    "does not match",
    class = "ggforestplotR_invalid_data"
  )
})

test_that("incomplete result rows are dropped with a warning", {
  table <- data.frame(
    estimate = c(1, NA),
    conf.low = c(0.5, NA),
    conf.high = c(1.5, NA)
  )
  expect_warning(
    out <- drop_incomplete_result_rows(table, "`x`", labels = c("a", "b")),
    class = "ggforestplotR_dropped_rows"
  )
  expect_equal(nrow(out), 1L)
  expect_error(
    suppressWarnings(drop_incomplete_result_rows(table[2, ], "`x`")),
    class = "ggforestplotR_invalid_data"
  )
})
