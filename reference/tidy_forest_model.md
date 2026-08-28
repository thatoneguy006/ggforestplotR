# Tidy a model object for forest plotting

Uses [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
to convert a fitted model into forest-plot data. When subgroup effects
are requested,
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
or
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
derives conditional average effects from the original fitted model and
its covariance matrix. Mixed models are supported through `broom.mixed`
tidy methods when that package is installed.

## Usage

``` r
tidy_forest_model(
  model,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = NULL,
  intercept = FALSE,
  term_labels = NULL,
  sort_terms = c("none", "descending", "ascending"),
  subgroup = NULL,
  focal = NULL,
  p_method = c("overall", "level")
)
```

## Arguments

- model:

  A fitted model object supported by
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) or,
  for mixed models, a `broom.mixed` tidy method.

- conf.int:

  Logical; if `TRUE`, request confidence intervals from
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- conf.level:

  Confidence level for intervals.

- exponentiate:

  `NULL` uses the model's conventional coefficient scale, such as odds
  ratios for logistic models and hazard ratios for Cox models. `TRUE` or
  `FALSE` overrides that behavior.

- intercept:

  Logical; if `FALSE`, drop the intercept term.

- term_labels:

  Optional named vector used to relabel displayed terms. Names should
  match model term names and values are the labels to display.

- sort_terms:

  How to sort rows: `"none"`, `"descending"`, or `"ascending"`.

- subgroup:

  `NULL` for ordinary coefficient rows, `"auto"` to detect one
  unambiguous continuous-by-factor interaction, or the name of a factor
  defining subgroup levels. Explicit selection also requires `focal`.

- focal:

  Optional predictor whose conditional effect is estimated within each
  subgroup level. It may be continuous or a factor. For factors, each
  non-reference level is contrasted with the first factor level.

- p_method:

  Subgroup p-value method. `"overall"` uses an omnibus Wald test of the
  selected interaction on the subgroup header. `"level"` uses the test
  returned for each subgroup-specific slope or comparison.

## Value

A `forest_data` object ready for
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).
Derived rows add `subgroup_level`, `focal`, `model_term`, `contrast`,
`estimand`, and `effect_scale` columns. Their canonical `p.value`
follows `p_method` and shares one table column with ordinary covariate
p-values.

## Details

With `subgroup = NULL`, the function retains its ordinary
coefficient-tidy behavior. When subgroup effects are requested,
interaction selection uses the fitted model's terms and model frame
rather than parsing coefficient names. The selected focal main effect,
subgroup main-effect coefficients, and raw interaction coefficients are
replaced at their original position by one hierarchical subgroup block.
Unrelated coefficient rows stay in formula order.

Continuous focal predictors use
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
within each observed subgroup. Factor focal predictors use
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
and compare each non-reference level with the first factor level. Both
functions use the original fitted model and its variance-covariance
matrix; no subgroup models are refitted.

Automatic selection is deliberately conservative. It accepts one
unambiguous continuous-by-factor interaction. Factor-by-factor
interactions require explicit `focal` and `subgroup` names. Continuous
subgroups, transformed focal terms, multiple interactions involving the
selected predictors, and three-way interactions are rejected.

Linear and identity-link effects remain additive. Logit and log-link
effects are estimated on the link scale and use the existing
`exponentiate` semantics to return odds ratios or ratios by default. Cox
effects are estimated on the linear-predictor scale and returned as
hazard ratios by default. Other links fail rather than silently
returning a response-scale estimand with a different interpretation.

The canonical `p.value` column always contains both ordinary-covariate
and interaction-related tests. With `p_method = "overall"`, subgroup
rows store an omnibus Wald test of the selected interaction, which the
display layer promotes to the parent header. With `p_method = "level"`,
subgroup rows retain the post-estimation p-value for each slope or
comparison and display it alongside that estimate.

## Examples

``` r
if (requireNamespace("broom", quietly = TRUE)) {
  fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)
  tidy_forest_model(fit)

  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    interaction_fit <- lm(wt ~ mpg * factor(cyl), data = mtcars)
    tidy_forest_model(
      interaction_fit,
      subgroup = "auto",
      focal = "mpg"
    )
  }

  set.seed(123)
  logit_data <- data.frame(
    age = rnorm(250, mean = 62, sd = 8),
    bmi = rnorm(250, mean = 28, sd = 4),
    treatment = factor(rbinom(250, 1, 0.45), labels = c("Control", "Treatment"))
  )
  linpred <- -9 + 0.09 * logit_data$age + 0.11 * logit_data$bmi +
    0.9 * (logit_data$treatment == "Treatment")
  logit_data$event <- rbinom(250, 1, plogis(linpred))
  logit_fit <- glm(event ~ age + bmi + treatment, data = logit_data, family = binomial())

  tidy_forest_model(logit_fit, exponentiate = TRUE)
}
#> <forest_data> OR; scale: ratio; reference: 1
#>                 term estimate conf.low conf.high              label group
#> 1                age 1.103890 1.061512  1.151411                age  <NA>
#> 2                bmi 1.118858 1.043220  1.204389                bmi  <NA>
#> 3 treatmentTreatment 3.133149 1.782915  5.613038 treatmentTreatment  <NA>
#>   subgroup grouping separate_groups    n events      p.value  std.error
#> 1     <NA>     <NA>            <NA> <NA>   <NA> 1.719452e-06 0.02066110
#> 2     <NA>     <NA>            <NA> <NA>   <NA> 2.093412e-03 0.03650339
#> 3     <NA>     <NA>            <NA> <NA>   <NA> 9.087778e-05 0.29180238
#>   statistic
#> 1  4.783878
#> 2  3.076650
#> 3  3.913739
```
