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
  expect_s3_class(out, "forest_data")

  metadata <- forest_metadata(out)
  expect_equal(metadata$estimate_scale, "identity")
  expect_equal(metadata$axis_transform, "identity")
  expect_equal(metadata$effect_label, "Estimate")
  expect_equal(metadata$conf_level, 0.95)
  expect_equal(metadata$reference_value, 0)
  expect_equal(names(metadata$source_columns), names(raw))
  expect_equal(
    unname(metadata$column_mapping[c("term", "estimate", "conf.low", "conf.high")]),
    c("variable", "beta", "lower", "upper")
  )
})

test_that("as_forest_data accepts tibble and data.table subclasses", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0)
  )
  tibble_input <- tibble::as_tibble(raw)
  data_table_input <- structure(raw, class = c("data.table", "data.frame"))

  for (input in list(tibble_input, data_table_input)) {
    out <- as_forest_data(
      input,
      term = "term",
      estimate = "estimate",
      conf.low = "conf.low",
      conf.high = "conf.high"
    )

    expect_s3_class(out, "forest_data")
    expect_equal(out$term, c("Age", "BMI"))
    expect_equal(out$estimate, c(0.3, -0.2))
  }
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

test_that("as_forest_data validates semantic estimate scales", {
  ratio <- data.frame(
    term = "Treatment",
    estimate = 1.4,
    conf.low = 1.1,
    conf.high = 1.8
  )

  out <- as_forest_data(
    ratio,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    estimate_scale = "ratio",
    effect_label = "RR",
    conf.level = 0.9
  )
  metadata <- forest_metadata(out)

  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$axis_transform, "log10")
  expect_equal(metadata$effect_label, "RR")
  expect_equal(metadata$conf_level, 0.9)
  expect_equal(metadata$reference_value, 1)
  plot <- ggforestplot(out)
  expect_equal(plot$labels$x, "RR (90% CI)")
  expect_equal(
    build_forest_table_data(
      plot$ggforestplotR_state$forest_data,
      estimate_label = "RR"
    )$headers,
    c("Term", "RR (90% CI)")
  )

  expect_error(
    as_forest_data(
      transform(ratio, conf.low = -0.1),
      "term", "estimate", "conf.low", "conf.high",
      estimate_scale = "ratio"
    ),
    "strictly positive"
  )

  expect_error(
    as_forest_data(
      transform(ratio, estimate = 1.2, conf.low = 0.8, conf.high = 1.1),
      "term", "estimate", "conf.low", "conf.high",
      estimate_scale = "probability"
    ),
    "between 0 and 1"
  )
})

test_that("probability forest data do not invent a reference value", {
  raw <- data.frame(
    term = "Predicted risk",
    estimate = 0.25,
    conf.low = 0.18,
    conf.high = 0.34
  )

  out <- as_forest_data(
    raw,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    estimate_scale = "probability"
  )
  metadata <- forest_metadata(out)
  plot <- ggforestplot(out)

  expect_null(metadata$reference_value)
  expect_equal(metadata$axis_transform, "identity")
  expect_null(plot$ggforestplotR_state$defaults$ref_line)
})

test_that("forest_data preserves metadata during row operations", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.2, -0.1),
    conf.low = c(0.1, -0.2),
    conf.high = c(0.3, 0),
    note = c("A", "B")
  )
  out <- as_forest_data(
    raw,
    "term",
    "estimate",
    "conf.low",
    "conf.high",
    effect_label = "Beta",
    conf.level = 0.95
  )

  subset <- out[2, , drop = FALSE]
  expect_s3_class(subset, "forest_data")
  expect_equal(forest_metadata(subset)$effect_label, "Beta")
  expect_equal(names(forest_metadata(subset)$source_columns), names(raw))

  incomplete <- out[, c("term", "estimate"), drop = FALSE]
  expect_false(inherits(incomplete, "forest_data"))
})
