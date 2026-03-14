# Standardize coefficient data for forest plots

Standardizes a coefficient table into the internal forest-plot data
structure.

## Usage

``` r
as_forest_data(
  data,
  term,
  estimate,
  conf.low,
  conf.high,
  label = term,
  group = NULL,
  grouping = NULL,
  separator_group = NULL,
  n = NULL,
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

- group:

  Optional column name used for color-grouping multiple estimates per
  row.

- grouping:

  Optional column name used to split rows into grouped plot sections.

- separator_group:

  Optional column name used to identify contiguous multi-row blocks that
  can be outlined with separator lines.

- n:

  Optional column name holding sample sizes or other N labels for
  attached tables.

- p.value:

  Optional column name holding p-values.

- exponentiate:

  Logical; if TRUE, require positive values for estimates and intervals.

- sort_terms:

  How to sort rows: "none", "descending", or "ascending".

## Value

A standardized data frame with columns ready for
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).
The returned structure includes `separator_group` metadata when
supplied.
