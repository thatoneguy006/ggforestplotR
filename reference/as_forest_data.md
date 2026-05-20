# Standardize coefficient data for forest plots

Standardizes a coefficient table into the internal forest-plot data
structure used throughout `ggforestplotR`.

## Usage

``` r
as_forest_data(
  data,
  term,
  estimate,
  conf.low,
  conf.high,
  label = term,
  term_labels = NULL,
  group = NULL,
  grouping = NULL,
  separate_groups = NULL,
  n = NULL,
  events = NULL,
  p.value = NULL,
  exponentiate = FALSE,
  sort_terms = c("none", "descending", "ascending")
)
```

## Arguments

- data:

  A data frame containing coefficient estimates and intervals.

- term:

  Column name holding the model term identifier.

- estimate:

  Column name holding the point estimate.

- conf.low:

  Column name holding the lower confidence bound.

- conf.high:

  Column name holding the upper confidence bound.

- label:

  Optional column name used for the displayed row label.

- term_labels:

  Optional named vector used to relabel displayed terms. Names should
  match values in the term column and values are the labels to display.

- group:

  Optional column name used for color-grouping multiple estimates per
  row.

- grouping:

  Optional column name used to split rows into grouped plot sections.

- separate_groups:

  Optional column name used to identify labeled variable blocks that can
  be outlined with separator lines.

- n:

  Optional column name holding sample sizes or other N labels for table
  helpers.

- events:

  Optional column name holding event counts or event labels for table
  helpers.

- p.value:

  Optional column name holding p-values.

- exponentiate:

  Logical; if `TRUE`, require positive values for estimates and
  intervals.

- sort_terms:

  How to sort rows: `"none"`, `"descending"`, or `"ascending"`.

## Value

A standardized data frame ready for
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
and the table composition helpers. Original dataframe columns are
retained for table helpers so they can be displayed with
`add_forest_table(columns = ...)`.

## Examples

``` r
raw <- data.frame(
  variable = c("Age", "BMI", "Treatment"),
  beta = c(0.10, -0.08, 0.34),
  lower = c(0.02, -0.16, 0.12),
  upper = c(0.18, 0.00, 0.56)
)

as_forest_data(
  data = raw,
  term = "variable",
  estimate = "beta",
  conf.low = "lower",
  conf.high = "upper"
)
#>        term estimate conf.low conf.high     label group grouping
#> 1       Age     0.10     0.02      0.18       Age  <NA>     <NA>
#> 2       BMI    -0.08    -0.16      0.00       BMI  <NA>     <NA>
#> 3 Treatment     0.34     0.12      0.56 Treatment  <NA>     <NA>
#>   separate_groups    n events p.value  variable  beta lower upper
#> 1            <NA> <NA>   <NA>      NA       Age  0.10  0.02  0.18
#> 2            <NA> <NA>   <NA>      NA       BMI -0.08 -0.16  0.00
#> 3            <NA> <NA>   <NA>      NA Treatment  0.34  0.12  0.56
```
