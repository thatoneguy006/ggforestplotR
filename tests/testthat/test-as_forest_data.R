test_that("as_forest_data standardizes coefficient columns", {
  raw <- data.frame(
    variable = c("Age", "BMI"),
    beta = c(0.3, -0.2),
    lower = c(0.1, -0.4),
    upper = c(0.5, 0.0),
    cohort = c("A", "A")
  )

  out <- as_forest_data(
    data = raw,
    term = "variable",
    estimate = "beta",
    conf.low = "lower",
    conf.high = "upper",
    group = "cohort"
  )

  expect_equal(names(out), c("term", "estimate", "conf.low", "conf.high", "label", "group", "p.value"))
  expect_equal(as.character(out$term), c("Age", "BMI"))
  expect_equal(as.character(out$group), c("A", "A"))
})

test_that("as_forest_data rejects reversed confidence intervals", {
  raw <- data.frame(
    term = "Age",
    estimate = 0.3,
    conf.low = 0.5,
    conf.high = 0.1
  )

  expect_error(
    as_forest_data(raw, "term", "estimate", "conf.low", "conf.high"),
    "conf.low"
  )
})
