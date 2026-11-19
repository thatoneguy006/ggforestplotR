skip_if_no_gtsummary <- function() {
  skip_if_not_installed("gtsummary", minimum_version = "2.0.0")
  skip_if_not_installed("broom.helpers")
}

make_trial_logit <- function() {
  trial <- gtsummary::trial
  stats::glm(response ~ age + grade + trt, data = trial, family = stats::binomial())
}

test_that("tbl_regression() keeps categorical blocks and reference rows", {
  skip_if_no_gtsummary()

  tbl <- gtsummary::tbl_regression(make_trial_logit(), exponentiate = TRUE)
  out <- as_forest_data(tbl)
  body <- tbl$table_body

  expect_s3_class(out, "forest_data")
  expect_equal(out$label, c("Age", "I", "II", "III", "Drug A", "Drug B"))
  expect_equal(out$row_type, c("estimate", "reference", "estimate", "estimate", "reference", "estimate"))
  expect_equal(out$subgroup, c(NA, "Grade", "Grade", "Grade", "Chemotherapy Treatment", "Chemotherapy Treatment"))

  estimates <- body[!is.na(body$estimate), ]
  expect_equal(out$estimate[out$row_type == "estimate"], estimates$estimate)
  expect_equal(out$conf.low[out$row_type == "estimate"], estimates$conf.low)
  expect_equal(out$p.value[out$row_type == "estimate"], estimates$p.value)
  expect_true(all(is.na(out$estimate[out$row_type == "reference"])))
  expect_equal(out$n[[1L]], as.character(body$n_obs[body$variable == "age"][[1L]]))

  metadata <- forest_metadata(out)
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$effect_label, "OR")
  expect_equal(metadata$reference_value, 1)
  expect_equal(metadata$conf_level, 0.95)
  expect_equal(metadata$p_method, "level")
  expect_equal(metadata$source_package, "gtsummary")

  p <- ggforestplot(tbl)
  expect_s3_class(p, "ggplot")
  display <- p$ggforestplotR_state$display_data
  expect_equal(
    display$display_label,
    c("Age", "Grade", "   I", "   II", "   III", "Chemotherapy Treatment", "   Drug A", "   Drug B")
  )
})

test_that("unexponentiated logistic tables use the log scale", {
  skip_if_no_gtsummary()

  tbl <- gtsummary::tbl_regression(make_trial_logit())
  out <- as_forest_data(tbl)
  expect_equal(forest_metadata(out)$estimate_scale, "log")
  expect_equal(forest_metadata(out)$effect_label, "log(OR)")

  ratio <- as_forest_data(tbl, exponentiate = TRUE)
  expect_equal(forest_metadata(ratio)$estimate_scale, "ratio")
  expect_equal(forest_metadata(ratio)$effect_label, "OR")
  expect_equal(ratio$estimate, exp(out$estimate))

  linear <- gtsummary::tbl_regression(stats::lm(marker ~ age + trt, data = gtsummary::trial))
  expect_equal(forest_metadata(as_forest_data(linear))$estimate_scale, "identity")
  expect_equal(forest_metadata(as_forest_data(linear))$effect_label, "Beta")
})

test_that("global p-values move to subgroup headers", {
  skip_if_no_gtsummary()
  skip_if_not_installed("cardx")
  skip_if_not_installed("car")
  skip_if_not_installed("parameters")

  tbl <- gtsummary::tbl_regression(make_trial_logit(), exponentiate = TRUE) |>
    gtsummary::add_global_p()
  out <- as_forest_data(tbl)
  expect_equal(forest_metadata(out)$p_method, "overall")

  global <- tbl$table_body$p.value[tbl$table_body$variable == "grade" &
    tbl$table_body$row_type == "label"]
  expect_true(all(out$p.value[out$subgroup %in% "Grade"] == global))

  p <- ggforestplot(tbl)
  display <- p$ggforestplotR_state$display_data
  expect_equal(display$p.value[display$label == "Grade"], global)

  level <- as_forest_data(tbl, p_method = "level")
  expect_equal(forest_metadata(level)$p_method, "level")
})

test_that("term_labels relabel variables and levels", {
  skip_if_no_gtsummary()

  tbl <- gtsummary::tbl_regression(make_trial_logit(), exponentiate = TRUE)
  out <- as_forest_data(
    tbl,
    term_labels = c(age = "Age (years)", grade = "Tumor grade", "Drug B" = "Treatment B")
  )
  expect_equal(out$label[[1L]], "Age (years)")
  expect_true(all(out$subgroup[2:4] == "Tumor grade"))
  expect_equal(out$label[[6L]], "Treatment B")
})

test_that("tbl_uvregression() and tbl_merge() become grouped estimates", {
  skip_if_no_gtsummary()

  trial <- gtsummary::trial
  crude <- gtsummary::tbl_uvregression(
    trial[c("response", "age", "grade", "trt")],
    method = stats::glm,
    y = response,
    method.args = list(family = stats::binomial),
    exponentiate = TRUE
  )
  uv <- as_forest_data(crude)
  expect_equal(uv$label, c("Age", "I", "II", "III", "Drug A", "Drug B"))
  expect_equal(forest_metadata(uv)$estimate_scale, "ratio")

  adjusted <- gtsummary::tbl_regression(make_trial_logit(), exponentiate = TRUE)
  merged <- gtsummary::tbl_merge(
    list(crude, adjusted),
    tab_spanner = c("**Crude**", "**Adjusted**")
  )
  out <- as_forest_data(merged)

  expect_equal(levels(out$group), c("Crude", "Adjusted"))
  expect_equal(nrow(out), 12L)
  expect_equal(
    out$estimate[out$group == "Adjusted" & out$row_type == "estimate"],
    adjusted$table_body$estimate[!is.na(adjusted$table_body$estimate)]
  )
  expect_equal(forest_metadata(out)$effect_label, "OR")

  p <- ggforestplot(merged)
  expect_s3_class(p, "ggplot")
  expect_equal(default_group_table_header(out), "Model")
})

test_that("merged tables with different scales are rejected", {
  skip_if_no_gtsummary()

  ratio <- gtsummary::tbl_regression(make_trial_logit(), exponentiate = TRUE)
  log_scale <- gtsummary::tbl_regression(make_trial_logit())
  merged <- gtsummary::tbl_merge(list(ratio, log_scale))
  expect_error(as_forest_data(merged), "same scale")
})

test_that("tbl_stack() group headers become facets", {
  skip_if_no_gtsummary()

  trial <- gtsummary::trial
  first <- gtsummary::tbl_regression(
    stats::glm(response ~ age + grade, data = trial[trial$trt == "Drug A", ], family = stats::binomial()),
    exponentiate = TRUE
  )
  second <- gtsummary::tbl_regression(
    stats::glm(response ~ age + grade, data = trial[trial$trt == "Drug B", ], family = stats::binomial()),
    exponentiate = TRUE
  )

  stacked <- gtsummary::tbl_stack(list(first, second), group_header = c("Drug A", "Drug B"))
  out <- as_forest_data(stacked)
  expect_equal(unique(out$grouping), c("Drug A", "Drug B"))
  expect_s3_class(ggforestplot(stacked), "ggplot")

  expect_error(
    as_forest_data(gtsummary::tbl_stack(list(first, second))),
    "group_header"
  )
})

test_that("gtsummary inputs are validated", {
  skip_if_no_gtsummary()

  tbl <- gtsummary::tbl_regression(make_trial_logit(), exponentiate = TRUE)
  expect_error(as_forest_data(tbl, conf.level = 0.9), "does not match")
  expect_error(as_forest_data(tbl, sort_terms = "ascending"), "row order")
  expect_error(as_forest_data(tbl, group = "x"))

  summary_table <- gtsummary::tbl_summary(gtsummary::trial[c("age", "grade")])
  expect_error(as_forest_data(summary_table), "no `estimate` column")
})
