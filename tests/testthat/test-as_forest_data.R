test_that("as_forest_data standardizes coefficient columns", {
  raw <- data.frame(
    variable = c("Age", "BMI"),
    beta = c(0.3, -0.2),
    lower = c(0.1, -0.4),
    upper = c(0.5, 0.0),
    cohort = c("A", "A"),
    section = c("Clinical", "Clinical"),
    block = c("Anthropometrics", "Anthropometrics"),
    sample_size = c(120, 120),
    event_count = c(32, 28)
  )

  out <- as_forest_data(
    data = raw,
    term = "variable",
    estimate = "beta",
    conf.low = "lower",
    conf.high = "upper",
    group = "cohort",
    grouping = "section",
    separate_groups = "block",
    n = "sample_size",
    events = "event_count"
  )

  expect_equal(
    names(out)[seq_len(11)],
    c("term", "estimate", "conf.low", "conf.high", "label", "group", "grouping", "separate_groups", "n", "events", "p.value")
  )
  expect_true(all(c("variable", "beta", "lower", "upper", "cohort", "section", "block", "sample_size", "event_count") %in% names(out)))
  expect_equal(as.character(out$term), c("Age", "BMI"))
  expect_equal(as.character(out$group), c("A", "A"))
  expect_equal(as.character(out$grouping), c("Clinical", "Clinical"))
  expect_equal(as.character(out$separate_groups), c("Anthropometrics", "Anthropometrics"))
  expect_equal(as.character(out$n), c("120", "120"))
  expect_equal(as.character(out$events), c("32", "28"))
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

test_that("as_forest_data sorts terms within grouping sections", {
  raw <- data.frame(
    term = c("Age", "BMI", "Stage II", "Stage III"),
    estimate = c(0.3, -0.2, 0.5, 0.8),
    conf.low = c(0.1, -0.4, 0.2, 0.4),
    conf.high = c(0.5, 0.0, 0.8, 1.2),
    section = c("Clinical", "Clinical", "Tumor", "Tumor")
  )

  descending <- as_forest_data(
    raw,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    grouping = "section",
    sort_terms = "descending"
  )
  ascending <- as_forest_data(
    raw,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    grouping = "section",
    sort_terms = "ascending"
  )

  expect_equal(descending$term, c("Age", "BMI", "Stage III", "Stage II"))
  expect_equal(ascending$term, c("BMI", "Age", "Stage II", "Stage III"))
  expect_equal(descending$grouping, c("Clinical", "Clinical", "Tumor", "Tumor"))
})
