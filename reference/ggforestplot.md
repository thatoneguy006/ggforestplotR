# Draw a ggplot2 forest plot

Builds a forest plot from standardized coefficient data or directly from
a fitted model.

## Usage

``` r
ggforestplot(
  data,
  term = "term",
  estimate = "estimate",
  conf.low = "conf.low",
  conf.high = "conf.high",
  label = term,
  term_labels = NULL,
  group = NULL,
  facet = NULL,
  facet_strip_position = c("left", "right"),
  separate_groups = NULL,
  n = NULL,
  events = NULL,
  p.value = NULL,
  exponentiate = NULL,
  conf.level = 0.95,
  sort_terms = c("none", "descending", "ascending"),
  point_size = 2.3,
  point_shape = 19,
  linewidth = 0.5,
  line_size = NULL,
  staple_width = 0.2,
  ci_limits = NULL,
  ci_arrows = TRUE,
  ci_arrow_length = 0.08,
  ci_arrow_type = c("closed", "open"),
  dodge_width = 0.6,
  separate_lines = FALSE,
  separator_line_linetype = 2,
  separator_line_colour = "black",
  separator_linewidth = 0.4,
  striped_rows = FALSE,
  stripe_fill = "grey95",
  stripe_colour = NA,
  stripe_alpha = 1,
  ref_line = NULL,
  ref_label = NULL,
  ref_linetype = 2,
  ref_colour = "grey60",
  subgroup = NULL,
  p_method = c("overall", "level"),
  row_type = NULL,
  ref_color = NULL,
  separator_line_size = NULL
)
```

## Arguments

- data:

  Either a tidy coefficient data frame or a model object supported by
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- term:

  Column name holding the model term identifiers.

- estimate:

  Column name holding the point estimates.

- conf.low:

  Column name holding the lower confidence bounds.

- conf.high:

  Column name holding the upper confidence bounds.

- label:

  Optional column name used for the displayed row labels.

- term_labels:

  Optional named vector used to relabel displayed terms. Names should
  match values in the term column and values are the labels to display.

- group:

  Optional column name used for color-grouping estimates. If this column
  is a factor, its levels control the group legend and vertical dodge
  order.

- facet:

  Optional column name used to split rows into faceted plot sections. If
  this column is a factor, its levels control facet order.

- facet_strip_position:

  Positioning for facet strip labels.

- separate_groups:

  Optional column name used to identify labeled variable blocks that can
  be outlined with grid lines.

- n:

  Optional column name holding sample sizes or other N labels for table
  helpers.

- events:

  Optional column name holding event counts or event labels for table
  helpers.

- p.value:

  Optional column name holding p-values.

- exponentiate:

  For fitted models, `TRUE` exponentiates the coefficients (e.g.
  log-odds to odds ratios) and `NULL` uses the model's conventional
  scale, such as hazard ratios for Cox models. For data frames,
  `exponentiate` does **not** transform anything: `TRUE` declares that
  the estimate and interval columns are already ratios, which draws a
  log10 axis with the reference line at 1. Exponentiate log-scale
  columns yourself before plotting them as ratios.

- conf.level:

  Confidence level represented by model or data-frame interval columns.
  Defaults to `0.95`.

- sort_terms:

  How to sort rows: `"none"`, `"descending"`, or `"ascending"`. Subgroup
  hierarchies require `"none"` so their source order is preserved.

- point_size:

  A finite non-negative number for a constant marker size, or a single
  column name for sizes mapped from numeric data. Mapped values must be
  finite and non-negative on estimate and summary rows. Column-based
  sizes use the standard ggplot2 size scale; customize them with
  [`ggplot2::scale_size_continuous()`](https://ggplot2.tidyverse.org/reference/scale_size.html)
  or
  [`ggplot2::scale_size_area()`](https://ggplot2.tidyverse.org/reference/scale_size.html).

- point_shape:

  Shape used for coefficient markers.

- linewidth:

  Line width for confidence intervals.

- line_size:

  Deprecated. Use `linewidth` instead.

- staple_width:

  Width of the terminal staples on confidence interval lines.

- ci_limits:

  Optional numeric vector of length 2 used to truncate displayed
  confidence intervals. Intervals extending beyond these limits are
  clipped to the boundary and marked with arrows when `ci_arrows` is
  `TRUE`.

- ci_arrows:

  Logical; if `TRUE`, draw outward-facing arrows for confidence
  intervals truncated by `ci_limits`.

- ci_arrow_length:

  Length of CI truncation arrows in inches.

- ci_arrow_type:

  Arrowhead type for truncated confidence intervals. Passed to
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- dodge_width:

  Horizontal dodging used for grouped estimates.

- separate_lines:

  Logical; if `TRUE`, draw grid lines around each labeled block
  identified by `separate_groups`.

- separator_line_linetype:

  Line type used for separator lines.

- separator_line_colour:

  Colour used for separator lines.

- separator_linewidth:

  Line width used for separator lines.

- striped_rows:

  Logical; if `TRUE`, shade alternating rows.

- stripe_fill:

  Fill color used for shaded rows.

- stripe_colour:

  Border color for shaded rows.

- stripe_alpha:

  Transparency for shaded rows.

- ref_line:

  Numeric x-value where the reference line is drawn, or `NULL` to hide
  it. When omitted, the value is taken from the `forest_data` metadata,
  such as `0` for additive effects and `1` for ratios.

- ref_label:

  Optional label drawn alongside the reference line.

- ref_linetype:

  Line type for the reference line.

- ref_colour:

  Colour for the reference line.

- subgroup:

  Optional column name defining hierarchical subgroup blocks. Missing or
  empty values identify ordinary standalone estimates. Each non-empty
  subgroup must form one contiguous block within a facet. This is a
  presentation-only mapping; subgroups and contrasts are not inferred.
  With the default `p_method = "overall"` forest-data metadata, the
  first nonmissing p-value for each subgroup and estimate group, when
  applicable, is displayed on its parent row and child p-value cells are
  suppressed. `p_method = "level"` retains p-values on child rows. For
  fitted models, first call
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md)
  with `subgroup`, `focal`, and the desired `p_method`; direct model
  calls do not perform post-estimation subgroup inference inside the
  plotting layer.

- p_method:

  Subgroup p-value placement for data-frame input. `"overall"` displays
  one p-value on each subgroup header; `"level"` displays p-values on
  the subgroup estimate rows. Fitted-model subgroup data inherit this
  choice from
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md).

- row_type:

  Optional data-frame column mapping for semantic row types.
  `"estimate"` and `"summary"` rows draw points and intervals;
  `"reference"`, `"header"`, and `"spacer"` rows occupy display space
  without geometry. Existing `forest_data` objects define this
  internally. Summary rows are currently drawn with the same point and
  interval geometry as estimate rows; the row type is recorded for
  tables and downstream styling.

- ref_color:

  Deprecated. Use `ref_colour` instead.

- separator_line_size:

  Deprecated. Use `separator_linewidth` instead.

## Value

A `ggplot` object. Use standard `ggplot2` functions such as
[`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
for plot labels, and add composition helpers after styling the main
plot.

## Examples

``` r
coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.10, -0.08, 0.34),
  conf.low = c(0.02, -0.16, 0.12),
  conf.high = c(0.18, 0.00, 0.56)
)

ggforestplot(coefs)


ggforestplot(coefs, striped_rows = TRUE, point_shape = 17)
```
