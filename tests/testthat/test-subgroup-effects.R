make_interaction_lm <- function(include_covariates = FALSE) {
  formula <- if (isTRUE(include_covariates)) {
    wt ~ hp + mpg * factor(cyl) + qsec
  } else {
    wt ~ mpg * factor(cyl)
  }
  stats::lm(formula, data = mtcars)
}

emmeans_interval_data <- function(object, conf.level = 0.95) {
  as.data.frame(summary(
    object,
    infer = c(TRUE, TRUE),
    level = conf.level,
    type = "link",
    adjust = "none"
  ))
}

test_that("lm subgroup slopes use the fitted coefficient covariance", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  fit <- make_interaction_lm()
  out <- tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "mpg"
  )

  coefficients <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  contrast_matrix <- matrix(
    0,
    nrow = 3L,
    ncol = length(coefficients),
    dimnames = list(c("4", "6", "8"), names(coefficients))
  )
  contrast_matrix["4", "mpg"] <- 1
  contrast_matrix["6", c("mpg", "mpg:factor(cyl)6")] <- 1
  contrast_matrix["8", c("mpg", "mpg:factor(cyl)8")] <- 1

  expected_estimate <- drop(contrast_matrix %*% coefficients)
  expected_se <- sqrt(diag(contrast_matrix %*% covariance %*%
    t(contrast_matrix)))
  critical_value <- stats::qt(0.975, stats::df.residual(fit))

  expect_equal(out$term, c("4", "6", "8"))
  expect_equal(out$label, out$term)
  expect_equal(out$subgroup, rep("cyl", 3L))
  expect_equal(out$estimate, unname(expected_estimate), tolerance = 1e-10)
  expect_equal(out$std.error, unname(expected_se), tolerance = 1e-10)
  expect_equal(
    out$conf.low,
    unname(expected_estimate - critical_value * expected_se),
    tolerance = 1e-10
  )
  expect_equal(
    out$conf.high,
    unname(expected_estimate + critical_value * expected_se),
    tolerance = 1e-10
  )
  expect_true(all(is.na(out$p.value)))
  expect_true(all(!is.na(out$effect.p.value)))
  expect_equal(out$model_term, rep("mpg:factor(cyl)", 3L))
  expect_equal(out$contrast, rep("mpg slope", 3L))
})

test_that("automatic and explicit continuous-by-factor selection agree", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  fit <- make_interaction_lm()
  automatic <- tidy_forest_model(fit, subgroup = "auto", focal = "mpg")
  inferred_focal <- tidy_forest_model(fit, subgroup = "auto")
  explicit <- tidy_forest_model(fit, subgroup = "cyl", focal = "mpg")

  columns <- c(
    "term", "subgroup", "estimate", "std.error", "conf.low", "conf.high"
  )
  expect_equal(as.data.frame(automatic)[columns], as.data.frame(explicit)[columns])
  expect_equal(
    as.data.frame(automatic)[columns],
    as.data.frame(inferred_focal)[columns]
  )
})

test_that("interaction detection is independent of coefficient coding", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  data <- mtcars
  data$cyl_group <- factor(data$cyl)
  contrasts(data$cyl_group) <- stats::contr.sum(nlevels(data$cyl_group))
  fit <- stats::lm(wt ~ mpg * cyl_group, data = data)

  out <- tidy_forest_model(fit, subgroup = "auto", focal = "mpg")
  expected <- emmeans_interval_data(
    emmeans::emtrends(fit, specs = "cyl_group", var = "mpg")
  )

  expect_equal(out$term, levels(data$cyl_group))
  expect_equal(out$estimate, expected$mpg.trend, tolerance = 1e-10)
  expect_equal(out$std.error, expected$SE, tolerance = 1e-10)
  expect_false(any(grepl("cyl_group[12]", out$term)))
})

test_that("derived blocks replace selected coefficients and preserve order", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  fit <- make_interaction_lm(include_covariates = TRUE)
  out <- tidy_forest_model(fit, subgroup = "auto", focal = "mpg")

  expect_equal(out$term, c("hp", "4", "6", "8", "qsec"))
  expect_equal(out$subgroup, c(NA, "cyl", "cyl", "cyl", NA))
  expect_false(any(out$term %in% c(
    "mpg", "factor(cyl)6", "factor(cyl)8",
    "mpg:factor(cyl)6", "mpg:factor(cyl)8"
  )))

  metadata <- forest_metadata(out)
  expect_equal(unname(metadata$column_mapping[["subgroup"]]), "subgroup")
  expect_true(all(unname(metadata$source_columns) %in% names(out)))
})

test_that("model-derived subgroup rows align with plots and tables", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("patchwork")

  out <- tidy_forest_model(
    make_interaction_lm(),
    subgroup = "auto",
    focal = "mpg"
  )
  plot <- ggforestplot(out, striped_rows = TRUE)
  display_data <- plot$ggforestplotR_state$display_data

  expect_equal(
    display_data$row_type,
    c("subgroup_header", rep("estimate", 3L))
  )
  expect_equal(display_data$display_label[[1L]], "cyl")
  expect_equal(
    trimws(display_data$display_label[-1L]),
    c("4", "6", "8")
  )
  expect_true(all(is.na(
    display_data$p.value[display_data$row_type == "subgroup_header"]
  )))

  table_spec <- build_forest_table_data(
    plot$ggforestplotR_state$forest_data,
    columns = c("term", "estimate", "p", "effect.p.value"),
    display_data = display_data
  )
  table_data <- table_spec$table_data
  header_key <- as.character(
    display_data$row_key[display_data$row_type == "subgroup_header"]
  )
  header_cells <- table_data[
    as.character(table_data$row_key) == header_key,
    ,
    drop = FALSE
  ]

  expect_equal(header_cells$text[header_cells$column_key == "term"], "cyl")
  expect_true(all(
    header_cells$text[header_cells$column_key != "term"] == ""
  ))
  expect_s3_class(plot + add_forest_table(), "patchwork")
})

test_that("subgroup NULL retains ordinary model-coefficient behavior", {
  skip_if_not_installed("broom")

  fit <- make_interaction_lm()
  implicit <- tidy_forest_model(fit)
  explicit <- tidy_forest_model(fit, subgroup = NULL)

  expect_equal(as.data.frame(implicit), as.data.frame(explicit))
  expect_true(any(grepl(":", implicit$term, fixed = TRUE)))
  expect_true(all(is.na(implicit$subgroup)))
})

test_that("automatic selection rejects ambiguous interaction structures", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  data <- transform(
    mtcars,
    cyl_group = factor(cyl),
    gear_group = factor(gear)
  )

  expect_error(
    tidy_forest_model(
      stats::lm(wt ~ mpg + cyl_group, data = data),
      subgroup = "auto"
    ),
    "exactly one unambiguous"
  )
  expect_error(
    tidy_forest_model(
      stats::lm(wt ~ mpg * cyl_group + hp * gear_group, data = data),
      subgroup = "auto"
    ),
    "exactly one unambiguous"
  )
  expect_error(
    tidy_forest_model(
      stats::lm(wt ~ cyl_group * gear_group, data = data),
      subgroup = "auto"
    ),
    "exactly one unambiguous"
  )
  expect_error(
    tidy_forest_model(
      stats::lm(wt ~ mpg * hp, data = data),
      subgroup = "hp",
      focal = "mpg"
    ),
    "must be a factor"
  )
  expect_error(
    tidy_forest_model(
      stats::lm(wt ~ mpg * cyl_group * gear_group, data = data),
      subgroup = "cyl_group",
      focal = "mpg"
    ),
    "additional interaction"
  )
})

test_that("subgroup argument validation is conservative", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  fit <- make_interaction_lm()

  expect_error(
    tidy_forest_model(fit, focal = "mpg"),
    "requires `subgroup`"
  )
  expect_error(
    tidy_forest_model(fit, subgroup = "cyl"),
    "also requires `focal`"
  )
  expect_error(
    tidy_forest_model(
      fit,
      subgroup = "auto",
      focal = "mpg",
      sort_terms = "ascending"
    ),
    "require `sort_terms = \\\"none\\\"`"
  )
  expect_error(
    ggforestplot(fit, subgroup = "auto"),
    "tidy_forest_model"
  )
})

test_that("explicit factor-by-factor effects use reference contrasts", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(1001)
  data <- expand.grid(
    replicate = seq_len(40L),
    treatment = factor(c("Control", "Treatment")),
    sex = factor(c("Female", "Male"))
  )
  data$age <- stats::rnorm(nrow(data))
  data$outcome <- 0.2 * data$age +
    0.5 * (data$treatment == "Treatment") +
    0.3 * (data$sex == "Male") +
    0.7 * (data$treatment == "Treatment") * (data$sex == "Male") +
    stats::rnorm(nrow(data))
  fit <- stats::lm(outcome ~ age + treatment * sex, data = data)

  out <- tidy_forest_model(
    fit,
    subgroup = "sex",
    focal = "treatment"
  )
  expected_means <- emmeans::emmeans(
    fit,
    specs = "treatment",
    by = "sex",
    type = "link"
  )
  expected <- emmeans_interval_data(emmeans::contrast(
    expected_means,
    method = "trt.vs.ctrl",
    ref = 1L,
    by = "sex",
    adjust = "none"
  ))

  expect_equal(out$term, levels(data$sex))
  expect_equal(out$estimate, expected$estimate, tolerance = 1e-10)
  expect_equal(out$conf.low, expected$lower.CL, tolerance = 1e-10)
  expect_equal(out$conf.high, expected$upper.CL, tolerance = 1e-10)
  expect_equal(out$contrast, rep("Treatment - Control", 2L))
  expect_true(all(is.na(out$group)))
})

test_that("multi-level factor effects become grouped reference contrasts", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(1002)
  data <- expand.grid(
    replicate = seq_len(30L),
    treatment = factor(c("A", "B", "C")),
    sex = factor(c("Female", "Male"))
  )
  data$outcome <-
    (as.integer(data$treatment) - 1) * (1 + 0.5 * (data$sex == "Male")) +
    stats::rnorm(nrow(data))
  fit <- stats::lm(outcome ~ treatment * sex, data = data)

  out <- tidy_forest_model(
    fit,
    subgroup = "sex",
    focal = "treatment"
  )
  plot <- ggforestplot(out)

  expect_equal(out$term, rep(levels(data$sex), each = 2L))
  expect_equal(out$group, rep(c("B - A", "C - A"), 2L))
  expect_equal(
    plot$ggforestplotR_state$display_data$row_type,
    c("subgroup_header", rep("estimate", 4L))
  )
  expect_equal(
    length(unique(plot$ggforestplotR_state$display_data$row_key)),
    3L
  )
})

test_that("logistic subgroup slopes retain odds-ratio scale", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")

  set.seed(1003)
  data <- data.frame(
    focal = stats::rnorm(1200L),
    adjustment = stats::rnorm(1200L),
    subgroup = factor(rep(c("A", "B", "C"), each = 400L))
  )
  linear_predictor <- -0.4 + 0.2 * data$adjustment +
    0.25 * data$focal +
    0.35 * data$focal * (data$subgroup == "B") -
    0.2 * data$focal * (data$subgroup == "C")
  data$outcome <- stats::rbinom(
    nrow(data),
    size = 1L,
    prob = stats::plogis(linear_predictor)
  )
  fit <- stats::glm(
    outcome ~ adjustment + focal * subgroup,
    data = data,
    family = stats::binomial()
  )

  out <- tidy_forest_model(fit, subgroup = "auto", focal = "focal")
  expected <- emmeans_interval_data(
    emmeans::emtrends(fit, specs = "subgroup", var = "focal")
  )
  metadata <- forest_metadata(out)

  expect_equal(out$estimate[-1L], exp(expected$focal.trend), tolerance = 1e-10)
  expect_equal(out$conf.low[-1L], exp(expected$asymp.LCL), tolerance = 1e-10)
  expect_equal(out$conf.high[-1L], exp(expected$asymp.UCL), tolerance = 1e-10)
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$effect_label, "OR")
  expect_equal(metadata$reference_value, 1)
})

test_that("Cox subgroup slopes retain hazard-ratio scale", {
  skip_if_not_installed("broom")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("survival")

  fit <- survival::coxph(
    survival::Surv(time, status) ~ ph.ecog + age * factor(sex),
    data = survival::lung
  )
  out <- tidy_forest_model(fit, subgroup = "auto", focal = "age")
  expected <- emmeans_interval_data(
    emmeans::emtrends(fit, specs = "sex", var = "age")
  )
  subgroup_rows <- !is.na(out$subgroup)
  metadata <- forest_metadata(out)

  expect_equal(
    out$estimate[subgroup_rows],
    exp(expected$age.trend),
    tolerance = 1e-10
  )
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$effect_label, "HR")
})

test_that("lmer, glmer, and lme use their fitted fixed-effect covariance", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")

  lmer_fit <- lme4::lmer(
    weight ~ Time * Diet + (1 | Chick),
    data = ChickWeight
  )
  lmer_out <- suppressMessages(tidy_forest_model(
    lmer_fit,
    subgroup = "auto",
    focal = "Time"
  ))
  lmer_expected <- suppressMessages(emmeans_interval_data(
    emmeans::emtrends(lmer_fit, specs = "Diet", var = "Time")
  ))
  expect_equal(lmer_out$estimate, lmer_expected$Time.trend, tolerance = 1e-8)

  glmer_fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ size * period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial()
  )
  glmer_out <- suppressMessages(tidy_forest_model(
    glmer_fit,
    subgroup = "auto",
    focal = "size"
  ))
  glmer_expected <- suppressMessages(emmeans_interval_data(
    emmeans::emtrends(glmer_fit, specs = "period", var = "size")
  ))
  expect_equal(
    glmer_out$estimate,
    exp(glmer_expected$size.trend),
    tolerance = 1e-8
  )
  expect_equal(forest_metadata(glmer_out)$effect_label, "OR")

  lme_fit <- nlme::lme(
    distance ~ age * Sex,
    random = ~ 1 | Subject,
    data = nlme::Orthodont
  )
  lme_out <- tidy_forest_model(lme_fit, subgroup = "auto", focal = "age")
  lme_expected <- emmeans_interval_data(
    emmeans::emtrends(lme_fit, specs = "Sex", var = "age")
  )
  expect_equal(lme_out$estimate, lme_expected$age.trend, tolerance = 1e-8)
})

test_that("unsupported mixed-model structures fail clearly", {
  fake_glmm_tmb <- structure(list(), class = "glmmTMB")
  fake_nlmer <- structure(list(), class = c("nlmerMod", "merMod"))

  expect_error(
    .subgroup_interaction_metadata(fake_glmm_tmb),
    "currently support"
  )
  expect_error(
    .subgroup_interaction_metadata(fake_nlmer),
    "currently support"
  )
})
