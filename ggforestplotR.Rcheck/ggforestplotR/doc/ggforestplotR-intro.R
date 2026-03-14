## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(ggforestplotR)
library(ggplot2)

## ----basic-data---------------------------------------------------------------
basic_coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.10, -0.08, 0.34),
  conf.low = c(0.02, -0.16, 0.12),
  conf.high = c(0.18, 0.00, 0.56)
)

basic_coefs

## ----basic-plot---------------------------------------------------------------
ggforestplot(
  basic_coefs,
  title = "Basic forest plot"
)

## ----basic-remap--------------------------------------------------------------
renamed_coefs <- data.frame(
  variable = c("Age", "BMI", "Treatment"),
  beta = c(0.10, -0.08, 0.34),
  lower = c(0.02, -0.16, 0.12),
  upper = c(0.18, 0.00, 0.56)
)

ggforestplot(
  renamed_coefs,
  term = "variable",
  estimate = "beta",
  conf.low = "lower",
  conf.high = "upper",
  title = "Basic forest plot with remapped columns"
)

## ----grouped-data-------------------------------------------------------------
sectioned_coefs <- data.frame(
  term = c("Age", "BMI", "Smoking", "Stage II", "Stage III", "Nodes"),
  estimate = c(0.10, -0.08, 0.20, 0.34, 0.52, 0.28),
  conf.low = c(0.02, -0.16, 0.05, 0.12, 0.20, 0.06),
  conf.high = c(0.18, 0.00, 0.35, 0.56, 0.84, 0.50),
  section = c(
    "Clinical", "Clinical", "Clinical",
    "Tumor", "Tumor", "Tumor"
  )
)

sectioned_coefs

## ----grouped-plot-------------------------------------------------------------
ggforestplot(
  sectioned_coefs,
  grouping = "section",
  title = "Forest plot with grouped sections"
)

## ----grouped-striped----------------------------------------------------------
ggforestplot(
  sectioned_coefs,
  grouping = "section",
  striped_rows = TRUE,
  stripe_fill = "grey94",
  title = "Grouped sections with striped rows"
)

## ----table-data---------------------------------------------------------------
tabled_coefs <- data.frame(
  term = c("Age", "BMI", "Smoking", "Stage II", "Stage III"),
  estimate = c(0.12, -0.10, 0.18, 0.30, 0.46),
  conf.low = c(0.03, -0.18, 0.04, 0.10, 0.18),
  conf.high = c(0.21, -0.02, 0.32, 0.50, 0.74),
  sample_size = c(120, 115, 98, 87, 83),
  section = c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor")
)

## ----table-left---------------------------------------------------------------
ggforestplot(
  tabled_coefs,
  grouping = "section",
  n = "sample_size",
  striped_rows = TRUE,
  title = "Forest plot with a left-side summary table"
) +
  add_forest_table(
  position = "left",
  show_n = TRUE,
  estimate_label = "Beta"
)

## ----table-right--------------------------------------------------------------
ggforestplot(
  tabled_coefs,
  grouping = "section",
  n = "sample_size",
  title = "Forest plot with a right-side summary table"
) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title.position = "plot"
  ) +
  add_forest_table(
  position = "right",
  show_n = TRUE,
  estimate_label = "HR"
)

## ----comparison-data----------------------------------------------------------
comparison_coefs <- data.frame(
  term = rep(c("Age", "BMI", "Smoking", "Stage II", "Stage III"), 2),
  estimate = c(
    0.12, -0.10, 0.18, 0.30, 0.46,
    0.08, -0.05, 0.24, 0.40, 0.58
  ),
  conf.low = c(
    0.03, -0.18, 0.04, 0.10, 0.18,
    0.00, -0.13, 0.10, 0.20, 0.30
  ),
  conf.high = c(
    0.21, -0.02, 0.32, 0.50, 0.74,
    0.16, 0.03, 0.38, 0.60, 0.86
  ),
  model = rep(c("Model A", "Model B"), each = 5),
  section = rep(c("Clinical", "Clinical", "Clinical", "Tumor", "Tumor"), 2),
  sample_size = rep(c(120, 115, 98, 87, 83), 2)
)

comparison_coefs

## ----comparison-plot----------------------------------------------------------
ggforestplot(
  comparison_coefs,
  group = "model",
  grouping = "section",
  n = "sample_size",
  striped_rows = TRUE,
  dodge_width = 0.5,
  title = "Comparing two model specifications"
) +
  add_forest_table(
  position = "right",
  show_n = TRUE,
  estimate_label = "HR"
)

## ----model-plot---------------------------------------------------------------
fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)

ggforestplot(
  fit,
  sort_terms = "descending",
  title = "Forest plot directly from an lm() object"
)

## ----tidy-model---------------------------------------------------------------
tidy_forest_model(fit)

## ----advanced-style-----------------------------------------------------------
ggforestplot(
  comparison_coefs,
  group = "model",
  grouping = "section",
  striped_rows = TRUE,
  stripe_fill = "#F3F4F6",
  zero_line_colour = "#9CA3AF",
  zero_line_linetype = 3,
  point_size = 2.8,
  line_size = 0.6,
  title = "Customized grouped forest plot",
  xlab = "Effect estimate",
  ylab = NULL
) +
  ggplot2::scale_colour_manual(
    values = c("Model A" = "#1D4ED8", "Model B" = "#D97706")
  ) +
  ggplot2::labs(
    subtitle = "Clinical and tumor terms shown in separate sections",
    caption = "Example data created for the vignette"
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.major.y = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "#E5E7EB", colour = NA),
    strip.text.y.left = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  )

