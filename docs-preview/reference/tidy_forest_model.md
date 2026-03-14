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
