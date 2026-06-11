# Bind multiple model summaries for a grouped forest plot

Tidies multiple fitted models and stacks their fixed-effect coefficient
tables into a single forest-plot data frame. The resulting data can be
passed directly to
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md),
where model labels are used as the grouping variable for dodged,
color-coded estimates.

## Usage

``` r
bind_forest_models(models, model_labels = NULL, exponentiate = NULL, ...)
```

## Arguments

- models:

  A non-empty list of fitted model objects supported by
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md).

- model_labels:

  Optional labels used to identify each model in the forest plot.
  Defaults to list names when present, otherwise `"Model 1"`,
  `"Model 2"`, and so on.

- exponentiate:

  `NULL`, a single logical value, or one logical value per model. `NULL`
  uses the canonical scale inferred by
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md)
  for each model.

- ...:

  Additional arguments passed to
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md),
  such as `conf.level`, `intercept`, `term_labels`, or `sort_terms`.

## Value

A standardized forest-plot data frame with one row per model term and a
`group` column containing the model labels.

## Examples

``` r
if (requireNamespace("broom", quietly = TRUE)) {
  fit1 <- lm(mpg ~ wt + hp, data = mtcars)
  fit2 <- lm(mpg ~ wt + qsec, data = mtcars)

  bound <- bind_forest_models(
    list(Base = fit1, Adjusted = fit2)
  )

  ggforestplot(bound)
}
```
