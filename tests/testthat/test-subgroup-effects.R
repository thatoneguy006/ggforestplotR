make_interaction_lm <- function(include_covariates = FALSE) {
  formula <- if (isTRUE(include_covariates)) {
    wt ~ hp + mpg * factor(cyl) + qsec
  } else {
    wt ~ mpg * factor(cyl)
  }
  stats::lm(formula, data = mtcars)
}

average_slopes <- function(model, focal, subgroup, type = "response") {
  suppressWarnings(as.data.frame(marginaleffects::avg_slopes(
    model,
    variables = focal,
    by = subgroup,
    type = type,
    conf_level = 0.95
  )))
}

average_comparisons <- function(model, focal, subgroup, type = "response") {
  suppressWarnings(as.data.frame(marginaleffects::avg_comparisons(
    model,
    variables = focal,
    by = subgroup,
    type = type,
    conf_level = 0.95
  )))
}

joint_interaction_p <- function(model, term, joint_test = "f") {
  model_terms <- stats::delete.response(stats::terms(model))
  term_index <- match(term, attr(model_terms, "term.labels", exact = TRUE))
  model_matrix <- stats::model.matrix(model)
  interaction_terms <- colnames(model_matrix)[
    attr(model_matrix, "assign", exact = TRUE) == term_index
  ]
  coefficient_indices <- match(
    interaction_terms,
    names(marginaleffects::get_coef(model))
  )
  test <- suppressWarnings(marginaleffects::hypotheses(
    model,
    joint = coefficient_indices,
    joint_test = joint_test
  ))

  as.data.frame(test)$p.value[[1L]]
}

test_that("lm subgroup rows match marginaleffects average slopes", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  fit <- make_interaction_lm()
  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "mpg"
  ))
  expected <- average_slopes(fit, "mpg", "cyl")
  expected_p <- joint_interaction_p(fit, "mpg:factor(cyl)")

  expect_equal(out$term, as.character(expected$cyl))
  expect_equal(out$label, out$term)
  expect_equal(out$subgroup, rep("cyl", nrow(expected)))
  expect_equal(out$subgroup_level, out$term)
  expect_equal(out$estimate, expected$estimate, tolerance = 1e-8)
  expect_equal(out$std.error, expected$std.error, tolerance = 1e-8)
  expect_equal(out$conf.low, expected$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, expected$conf.high, tolerance = 1e-8)
  expect_equal(out$p.value, rep(expected_p, nrow(expected)), tolerance = 1e-8)
  expect_false("effect.p.value" %in% names(out))
  expect_equal(out$model_term, rep("mpg:factor(cyl)", nrow(expected)))
  expect_equal(out$contrast, expected$contrast)
  expect_equal(out$estimand, rep("average_slope", nrow(expected)))
  expect_equal(out$focal, rep("mpg", nrow(expected)))
  expect_equal(out$effect_scale, rep("identity", nrow(expected)))
})

test_that("automatic and explicit interaction selection agree", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  fit <- make_interaction_lm()
  automatic <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "mpg"
  ))
  inferred_focal <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto"
  ))
  explicit <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "cyl",
    focal = "mpg"
  ))
  method_output <- suppressWarnings(as_forest_data(
    fit,
    subgroup = "auto",
    focal = "mpg"
  ))

  columns <- c(
    "term", "subgroup", "estimate", "std.error", "conf.low", "conf.high"
  )
  expect_equal(as.data.frame(automatic)[columns], as.data.frame(explicit)[columns])
  expect_equal(
    as.data.frame(automatic)[columns],
    as.data.frame(inferred_focal)[columns]
  )
  expect_equal(
    as.data.frame(automatic)[columns],
    as.data.frame(method_output)[columns]
  )
})

test_that("interaction detection does not depend on coefficient names", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  data <- mtcars
  data$cyl_group <- factor(data$cyl)
  contrasts(data$cyl_group) <- stats::contr.sum(nlevels(data$cyl_group))
  fit <- stats::lm(wt ~ mpg * cyl_group, data = data)

  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "mpg"
  ))
  expected <- average_slopes(fit, "mpg", "cyl_group")

  expect_equal(out$term, levels(data$cyl_group))
  expect_equal(out$estimate, expected$estimate, tolerance = 1e-8)
  expect_equal(out$conf.low, expected$conf.low, tolerance = 1e-8)
  expect_false(any(grepl("cyl_group[12]", out$term)))
})

test_that("derived blocks replace raw terms and preserve formula order", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  fit <- make_interaction_lm(include_covariates = TRUE)
  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "mpg"
  ))

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

test_that("interaction and covariate p-values share the canonical column", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  fit <- make_interaction_lm(include_covariates = TRUE)
  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "mpg"
  ))
  coefficient_rows <- broom::tidy(fit)
  expected_interaction <- joint_interaction_p(fit, "mpg:factor(cyl)")

  expect_equal(
    out$p.value[out$term == "hp"],
    coefficient_rows$p.value[coefficient_rows$term == "hp"]
  )
  expect_equal(
    out$p.value[out$term == "qsec"],
    coefficient_rows$p.value[coefficient_rows$term == "qsec"]
  )
  expect_equal(
    out$p.value[!is.na(out$subgroup)],
    rep(expected_interaction, 3L)
  )

  plot <- ggforestplot(out)
  display_data <- plot$ggforestplotR_state$display_data
  header <- display_data$row_type == "subgroup_header"
  children <- display_data$row_type == "estimate" &
    !is.na(display_data$subgroup)
  standalone <- display_data$row_type == "estimate" &
    is.na(display_data$subgroup)

  expect_equal(display_data$p.value[header], expected_interaction)
  expect_true(all(is.na(display_data$p.value[children])))
  expect_true(all(!is.na(display_data$p.value[standalone])))

  table_spec <- build_forest_table_data(
    plot$ggforestplotR_state$forest_data,
    columns = c("term", "p"),
    display_data = display_data
  )
  p_cells <- table_spec$table_data[
    table_spec$table_data$column_key == "p",
    ,
    drop = FALSE
  ]
  p_lookup <- stats::setNames(
    p_cells$text,
    as.character(p_cells$row_key)
  )

  expect_true(all(nzchar(unname(p_lookup[
    as.character(display_data$row_key[header | standalone])
  ]))))
  expect_true(all(!nzchar(unname(p_lookup[
    as.character(display_data$row_key[children])
  ]))))
})

test_that("model-derived rows stay aligned with plots and tables", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("patchwork")

  out <- suppressWarnings(tidy_forest_model(
    make_interaction_lm(),
    subgroup = "auto",
    focal = "mpg"
  ))
  plot <- ggforestplot(out, striped_rows = TRUE)
  display_data <- plot$ggforestplotR_state$display_data

  expect_equal(
    display_data$row_type,
    c("subgroup_header", rep("estimate", 3L))
  )
  expect_equal(display_data$display_label[[1L]], "cyl")
  expect_equal(trimws(display_data$display_label[-1L]), c("4", "6", "8"))
  expect_true(all(!is.na(
    display_data$p.value[display_data$row_type == "subgroup_header"]
  )))

  table_spec <- build_forest_table_data(
    plot$ggforestplotR_state$forest_data,
    columns = c("term", "estimate", "p"),
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
  expect_equal(
    header_cells$text[header_cells$column_key == "estimate"],
    ""
  )
  expect_true(nzchar(header_cells$text[header_cells$column_key == "p"]))
  expect_s3_class(plot + add_forest_table(), "patchwork")
})

test_that("subgroup NULL retains ordinary coefficient behavior", {
  skip_if_not_installed("broom")

  fit <- make_interaction_lm()
  implicit <- tidy_forest_model(fit)
  explicit <- tidy_forest_model(fit, subgroup = NULL)

  expect_equal(as.data.frame(implicit), as.data.frame(explicit))
  expect_true(any(grepl(":", implicit$term, fixed = TRUE)))
  expect_true(all(is.na(implicit$subgroup)))
})

test_that("automatic selection rejects ambiguous interactions", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

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

test_that("unsupported estimands fail instead of changing interpretation", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  data <- transform(mtcars, cyl_group = factor(cyl), am = as.integer(am))
  nonlinear_fit <- stats::lm(
    wt ~ mpg + I(mpg^2) + cyl_group + mpg:cyl_group,
    data = data
  )
  multivariate_fit <- stats::lm(
    cbind(wt, disp) ~ mpg * cyl_group,
    data = data
  )
  probit_fit <- stats::glm(
    am ~ mpg * cyl_group,
    data = data,
    family = stats::binomial(link = "probit")
  )

  expect_error(
    tidy_forest_model(
      nonlinear_fit,
      subgroup = "cyl_group",
      focal = "mpg"
    ),
    "transformed or nonlinear"
  )
  expect_error(
    tidy_forest_model(multivariate_fit, subgroup = "auto", focal = "mpg"),
    "univariate fitted model"
  )
  expect_error(
    tidy_forest_model(probit_fit, subgroup = "auto", focal = "mpg"),
    "do not yet support the `probit` link"
  )
})

test_that("subgroup argument validation is conservative", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  fit <- make_interaction_lm()

  expect_error(tidy_forest_model(fit, focal = "mpg"), "requires `subgroup`")
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

test_that("factor focal variables use subgroup average comparisons", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  set.seed(1001)
  data <- expand.grid(
    replicate = seq_len(30L),
    treatment = factor(c("A", "B", "C")),
    sex = factor(c("Female", "Male"))
  )
  data$outcome <-
    (as.integer(data$treatment) - 1) * (1 + 0.5 * (data$sex == "Male")) +
    stats::rnorm(nrow(data))
  fit <- stats::lm(outcome ~ treatment * sex, data = data)

  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "sex",
    focal = "treatment"
  ))
  expected <- average_comparisons(fit, "treatment", "sex")
  plot <- ggforestplot(out)

  expect_equal(out$term, as.character(expected$sex))
  expect_equal(out$estimate, expected$estimate, tolerance = 1e-8)
  expect_equal(out$conf.low, expected$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, expected$conf.high, tolerance = 1e-8)
  expect_equal(out$contrast, expected$contrast)
  expect_equal(out$group, expected$contrast)
  expect_equal(out$estimand, rep("average_comparison", nrow(expected)))
  expect_equal(
    length(unique(plot$ggforestplotR_state$display_data$row_key)),
    3L
  )

  table_spec <- build_forest_table_data(
    plot$ggforestplotR_state$forest_data,
    columns = c("term", "p"),
    display_data = plot$ggforestplotR_state$display_data
  )
  header_p <- table_spec$table_data[
    table_spec$table_data$row_type == "subgroup_header" &
      table_spec$table_data$column_key == "p",
    "text"
  ]
  expect_length(header_p, 1L)
  expect_true(nzchar(header_p))
  expect_false(grepl("\n", header_p, fixed = TRUE))
})

test_that("logistic subgroup slopes retain odds-ratio scale", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")

  set.seed(1002)
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

  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "focal"
  ))
  expected <- average_slopes(fit, "focal", "subgroup", type = "link")
  subgroup_rows <- !is.na(out$subgroup)
  metadata <- forest_metadata(out)

  expect_equal(
    out$estimate[subgroup_rows],
    exp(expected$estimate),
    tolerance = 1e-8
  )
  expect_equal(
    out$conf.low[subgroup_rows],
    exp(expected$conf.low),
    tolerance = 1e-8
  )
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$effect_label, "OR")
  expect_equal(metadata$reference_value, 1)
})

test_that("Cox subgroup slopes retain hazard-ratio scale", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("survival")

  fit <- survival::coxph(
    survival::Surv(time, status) ~ ph.ecog + age * factor(sex),
    data = survival::lung
  )
  out <- suppressWarnings(tidy_forest_model(
    fit,
    subgroup = "auto",
    focal = "age"
  ))
  expected <- average_slopes(fit, "age", "sex", type = "lp")
  subgroup_rows <- !is.na(out$subgroup)
  metadata <- forest_metadata(out)

  expect_equal(
    out$estimate[subgroup_rows],
    exp(expected$estimate),
    tolerance = 1e-8
  )
  expect_equal(metadata$estimate_scale, "ratio")
  expect_equal(metadata$effect_label, "HR")
})

test_that("mixed models delegate subgroup inference to marginaleffects", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")

  lmer_fit <- lme4::lmer(
    weight ~ Time * Diet + (1 | Chick),
    data = ChickWeight
  )
  lmer_out <- suppressWarnings(tidy_forest_model(
    lmer_fit,
    subgroup = "auto",
    focal = "Time"
  ))
  lmer_expected <- average_slopes(lmer_fit, "Time", "Diet")
  expect_equal(lmer_out$estimate, lmer_expected$estimate, tolerance = 1e-8)

  glmer_fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ size * period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial()
  )
  glmer_out <- suppressWarnings(tidy_forest_model(
    glmer_fit,
    subgroup = "auto",
    focal = "size"
  ))
  glmer_expected <- average_slopes(
    glmer_fit,
    "size",
    "period",
    type = "link"
  )
  expect_equal(
    glmer_out$estimate,
    exp(glmer_expected$estimate),
    tolerance = 1e-8
  )
  expect_equal(forest_metadata(glmer_out)$effect_label, "OR")

  lme_fit <- nlme::lme(
    distance ~ age * Sex,
    random = ~ 1 | Subject,
    data = nlme::Orthodont
  )
  lme_out <- suppressWarnings(tidy_forest_model(
    lme_fit,
    subgroup = "auto",
    focal = "age"
  ))
  lme_expected <- average_slopes(lme_fit, "age", "Sex")
  expect_equal(
    as.numeric(lme_out$estimate),
    as.numeric(lme_expected$estimate),
    tolerance = 1e-8
  )
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
