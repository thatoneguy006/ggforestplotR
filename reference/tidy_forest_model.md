# Tidy a model object for forest plotting

Uses [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
to convert a fitted model into forest-plot data.

## Usage

``` r
tidy_forest_model(
  model,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  intercept = FALSE,
  sort_terms = c("none", "descending", "ascending")
)
```

## Arguments

- model:

  A fitted model object supported by
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- conf.int:

  Logical; if TRUE, request confidence intervals from
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- conf.level:

  Confidence level for intervals.

- exponentiate:

  Logical; passed through to
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- intercept:

  Logical; if FALSE, drop the intercept term.

- sort_terms:

  How to sort rows: "none", "descending", or "ascending".

## Value

A standardized coefficient data frame ready for
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

## Examples

``` r
fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)

tidy_forest_model(fit)
#>   term    estimate   conf.low   conf.high label group grouping separator_group
#> 1   wt -4.35879720 -5.9006341 -2.81696034    wt  <NA>     <NA>            <NA>
#> 2   hp -0.01782227 -0.0485098  0.01286526    hp  <NA>     <NA>            <NA>
#> 3 qsec  0.51083369 -0.3888708  1.41053822  qsec  <NA>     <NA>            <NA>
#>      n      p.value
#> 1 <NA> 3.217222e-06
#> 2 <NA> 2.441762e-01
#> 3 <NA> 2.546284e-01

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
#>                 term estimate conf.low conf.high              label group
#> 1                age 1.103890 1.061512  1.151411                age  <NA>
#> 2                bmi 1.118858 1.043220  1.204389                bmi  <NA>
#> 3 treatmentTreatment 3.133149 1.782915  5.613038 treatmentTreatment  <NA>
#>   grouping separator_group    n      p.value
#> 1     <NA>            <NA> <NA> 1.719452e-06
#> 2     <NA>            <NA> <NA> 2.093412e-03
#> 3     <NA>            <NA> <NA> 9.087778e-05
```
