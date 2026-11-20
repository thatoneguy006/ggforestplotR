# Convert emmeans results to forest data

Standardizes estimated marginal means and contrasts from `emmeans` into
`forest_data`.
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
[pairs()](https://rvlenth.github.io/emmeans/reference/contrast.html),
and
[`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
all return `emmGrid` objects, so one method covers means, pairwise
comparisons, and custom contrasts. Summaries from
[summary()](https://rvlenth.github.io/emmeans/reference/summary.emmGrid.html)
and
[confint()](https://rvlenth.github.io/emmeans/reference/summary.emmGrid.html)
(`summary_emm`) and `emm_list` results, such as
`emmeans(fit, pairwise ~ treatment)`, are supported too.

## Usage

``` r
# S3 method for class 'emmGrid'
as_forest_data(
  data,
  ...,
  label = NULL,
  group = NULL,
  facet = NULL,
  subgroup = NULL,
  term_labels = NULL,
  exponentiate = NULL,
  estimate_scale = NULL,
  effect_label = NULL,
  reference_value = NULL,
  conf.level = NULL,
  type = NULL,
  adjust = NULL,
  sort_terms = c("none", "descending", "ascending"),
  p_method = c("level", "overall")
)

# S3 method for class 'summary_emm'
as_forest_data(
  data,
  ...,
  label = NULL,
  group = NULL,
  facet = NULL,
  subgroup = NULL,
  term_labels = NULL,
  exponentiate = NULL,
  estimate_scale = NULL,
  effect_label = NULL,
  reference_value = NULL,
  conf.level = NULL,
  sort_terms = c("none", "descending", "ascending"),
  p_method = c("level", "overall")
)

# S3 method for class 'emm_list'
as_forest_data(data, ..., which = 1L)
```

## Arguments

- data:

  An `emmGrid`, `summary_emm`, or `emm_list` object from `emmeans`.

- ...:

  Not used; must be empty.

- label:

  Optional column used as the displayed row label instead of the
  composed label.

- group:

  Optional column used to color and dodge estimates that share a row,
  such as a `by` variable.

- facet:

  Optional column used to split rows into plot sections.

- subgroup:

  Optional column used to group rows under hierarchical headers, such as
  `"term"` for several contrasts of one factor. Rows are gathered into
  contiguous blocks in order of first appearance.

- term_labels:

  Optional named vector used to relabel identifier values (for example
  `c(hp = "Horsepower")`) or composed row labels.

- exponentiate:

  `NULL` keeps the reported scale. `TRUE` converts log-scale estimates
  to ratios and `FALSE` converts ratios to the log scale.

- estimate_scale:

  Optional override for the detected effect scale. One of `"identity"`,
  `"log"`, `"ratio"`, `"probability"`, or `"risk_difference"`.

- effect_label:

  Optional override for the effect label used on the axis and in tables.

- reference_value:

  Optional override for the reference line value. Use `NULL` for no
  reference line.

- conf.level:

  For `emmGrid` objects, the confidence level passed to
  [`summary()`](https://rdrr.io/r/base/summary.html). `NULL` uses the
  `emmeans` default. For `summary_emm` objects, `NULL` uses the stored
  level and a number must match it.

- type:

  Optional prediction type passed to
  [`summary()`](https://rdrr.io/r/base/summary.html), such as
  `"response"` to back-transform means or obtain ratio contrasts.

- adjust:

  Optional multiplicity adjustment passed to
  [`summary()`](https://rdrr.io/r/base/summary.html).

- sort_terms:

  How to sort rows: `"none"`, `"descending"`, or `"ascending"`.

- p_method:

  Subgroup p-value placement when `subgroup` is mapped. Defaults to
  `"level"` because `emmeans` reports one test per row.

- which:

  For `emm_list` objects, the element to convert, by position or name.
  Defaults to the first element, matching `emmeans` methods.

## Value

A `forest_data` object. The original grid and `by` columns are retained
for table helpers.

## Details

`emmGrid` objects are summarized with confidence intervals at
`conf.level` and with `adjust` and `type` passed to
[`summary()`](https://rdrr.io/r/base/summary.html). Contrasts also
include their tests; means do not, because a test against zero is rarely
meaningful. `summary_emm` objects are used exactly as computed, so
`conf.level` can only confirm the stored level.

Row labels come from the primary variables, such as the factor levels or
the `contrast` column. When `by` variables vary they are added to the
label unless they are mapped to `group`, `facet`, or `subgroup`, for
example `facet = "sex"`.

The effect scale follows the estimate column chosen by `emmeans`:

- Contrasts (`estimate`) are additive with a reference line at 0. On a
  log or logit link they are log ratios or log odds ratios, and
  `exponentiate = TRUE` converts them to ratios.

- Back-transformed contrasts (`ratio` and `odds.ratio`) are ratios on a
  log10 axis with a reference line at 1.

- Marginal means (`emmean`, `response`, `rate`, and similar) have no
  null value, so no reference line is drawn. Probabilities (`prob`) use
  the probability scale.

Intervals and p-values reflect any multiplicity adjustment applied by
`emmeans`, such as the Tukey adjustment used by
[`pairs()`](https://rdrr.io/r/graphics/pairs.html).

## See also

[`as_forest_data()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data.md),
[`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/ggforestplot.md)

## Examples

``` r
if (requireNamespace("emmeans", quietly = TRUE)) {
  warp <- lm(breaks ~ wool * tension, data = warpbreaks)

  # Marginal means for each tension level, faceted by wool
  means <- emmeans::emmeans(warp, ~ tension | wool)
  ggforestplot(means, facet = "wool")

  # Tukey-adjusted pairwise differences
  ggforestplot(pairs(means), facet = "wool")

  # Odds ratios from a logistic model
  fit <- glm(vs ~ factor(gear) + mpg, data = mtcars, family = binomial())
  gear_pairs <- pairs(emmeans::emmeans(fit, ~ gear), adjust = "none")
  as_forest_data(gear_pairs, exponentiate = TRUE)
}
#> <forest_data> OR; scale: ratio; reference: 1
#>            term    estimate  conf.low    conf.high         label row_type group
#> 1 gear3 - gear4   0.6777779 0.0463030 9.921233e+00 gear3 - gear4 estimate  <NA>
#> 2 gear3 - gear5 335.5282444 0.4074475 2.763036e+05 gear3 - gear5 estimate  <NA>
#> 3 gear4 - gear5 495.0415935 1.2614350 1.942757e+05 gear4 - gear5 estimate  <NA>
#>   subgroup grouping separate_groups    n events    p.value      contrast
#> 1     <NA>     <NA>            <NA> <NA>   <NA> 0.77636646 gear3 - gear4
#> 2     <NA>     <NA>            <NA> <NA>   <NA> 0.08953678 gear3 - gear5
#> 3     <NA>     <NA>            <NA> <NA>   <NA> 0.04173212 gear4 - gear5
#>         SE  df    z.ratio
#> 1 1.369215 Inf -0.2840573
#> 2 3.425343 Inf  1.6978463
#> 3 3.047195 Inf  2.0361816
```
