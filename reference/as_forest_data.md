# Standardize coefficient data for forest plots

Standardizes a coefficient table into the internal forest-plot data
structure used throughout `ggforestplotR`.

## Usage

``` r
as_forest_data(data, ...)

# S3 method for class 'forest_data'
as_forest_data(
  data,
  term_labels = NULL,
  sort_terms = c("none", "descending", "ascending"),
  exponentiate = NULL,
  p_method = NULL,
  ...
)

# S3 method for class 'data.frame'
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
  exponentiate = NULL,
  estimate_scale = NULL,
  axis_transform = NULL,
  effect_label = NULL,
  conf.level = 0.95,
  reference_value = NULL,
  source_model = NULL,
  source_package = NULL,
  sort_terms = c("none", "descending", "ascending"),
  subgroup = NULL,
  p_method = c("overall", "level"),
  ...
)

# S3 method for class 'lm'
as_forest_data(
  data,
  ...,
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

# S3 method for class 'glm'
as_forest_data(
  data,
  ...,
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

# S3 method for class 'coxph'
as_forest_data(
  data,
  ...,
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

# S3 method for class 'merMod'
as_forest_data(
  data,
  ...,
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

# S3 method for class 'lme'
as_forest_data(
  data,
  ...,
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

# S3 method for class 'glmmTMB'
as_forest_data(
  data,
  ...,
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

# Default S3 method
as_forest_data(
  data,
  ...,
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

- data:

  A data frame or data-frame subclass containing coefficient estimates
  and intervals. Tibbles and `data.table` objects are supported.

- ...:

  Arguments passed to an `as_forest_data()` method.

- term_labels:

  Optional named vector used to relabel displayed terms. Names should
  match values in the term column and values are the labels to display.

- sort_terms:

  How to sort rows: `"none"`, `"descending"`, or `"ascending"`. Subgroup
  hierarchies require `"none"` so their source order is preserved.

- exponentiate:

  Compatibility argument. `TRUE` is equivalent to
  `estimate_scale = "ratio"`; `FALSE` is equivalent to
  `estimate_scale = "identity"` when `estimate_scale` is not supplied.

- p_method:

  Subgroup p-value placement. `"overall"` displays one omnibus or
  block-level p-value on the subgroup header; `"level"` keeps p-values
  on the individual subgroup estimate rows. For fitted-model methods,
  this also selects whether `p.value` contains the omnibus interaction
  test or the post-estimation test for each derived effect.

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
  row. If this column is a factor, its levels control the group legend
  and vertical dodge order.

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

- estimate_scale:

  Semantic scale of the stored estimates. One of `"identity"`, `"log"`,
  `"ratio"`, `"probability"`, or `"risk_difference"`.

- axis_transform:

  Transformation used for the plotting axis. Defaults to `"log10"` for
  ratios and `"identity"` otherwise.

- effect_label:

  Short label for the effect measure, such as `"Beta"`, `"OR"`, `"HR"`,
  `"RR"`, or `"RD"`.

- conf.level:

  Confidence level represented by the interval columns, or `NA` when it
  is unknown.

- reference_value:

  Numeric null/reference value, or `NULL` when the effect measure has no
  universal reference value.

- source_model:

  Optional character vector identifying the source model class. The
  complete fitted model is not retained.

- source_package:

  Optional package name identifying the model source.

- subgroup:

  For data frames, an optional column name defining presentation-only
  hierarchical subgroup blocks. Missing or empty values identify
  ordinary standalone estimates. Rows with the same non-empty value must
  form one contiguous block within each facet. When `p.value` is mapped,
  the first nonmissing value for each subgroup and estimate group, when
  applicable, is displayed on its parent row; child p-value cells are
  suppressed. Data-frame subgroups are never inferred and do not
  calculate model contrasts. For fitted-model methods, use `"auto"` to
  detect one unambiguous continuous-by-factor interaction, or supply a
  factor predictor name together with `focal` to derive covariance-aware
  post-estimation subgroup effects through `marginaleffects`.
  Fitted-model subgroup rows use the canonical `p.value` column for
  either the omnibus interaction test or row-level effect tests,
  according to `p_method`, so they can share a table column with
  ordinary coefficient p-values.

- conf.int:

  Logical; model methods require `TRUE` because forest data include
  confidence-interval columns.

- intercept:

  Logical; for model methods, whether to retain the intercept term.

- focal:

  For fitted-model methods, the predictor whose conditional effect is
  estimated within each subgroup level. It may be continuous or a
  factor. Factor effects compare each non-reference level with the first
  level. Ignored for data-frame methods.

## Value

A `forest_data` data-frame subclass ready for
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
and the table composition helpers. Original data-frame columns are
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
#> <forest_data> Estimate; scale: identity; reference: 0
#>        term estimate conf.low conf.high     label group subgroup grouping
#> 1       Age     0.10     0.02      0.18       Age  <NA>     <NA>     <NA>
#> 2       BMI    -0.08    -0.16      0.00       BMI  <NA>     <NA>     <NA>
#> 3 Treatment     0.34     0.12      0.56 Treatment  <NA>     <NA>     <NA>
#>   separate_groups    n events p.value  variable  beta lower upper
#> 1            <NA> <NA>   <NA>      NA       Age  0.10  0.02  0.18
#> 2            <NA> <NA>   <NA>      NA       BMI -0.08 -0.16  0.00
#> 3            <NA> <NA>   <NA>      NA Treatment  0.34  0.12  0.56
```
