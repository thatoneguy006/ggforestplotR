# Draw a ggplot2 forest plot

Builds a forest plot from standardized coefficient data or directly from
a fitted model, with optional grouped sections, striped rows, separator
lines for labeled variable blocks, and point-shape and staple controls.

## Usage

``` r
ggforestplot(
  data,
  term = "term",
  estimate = "estimate",
  conf.low = "conf.low",
  conf.high = "conf.high",
  label = term,
  group = NULL,
  grouping = NULL,
  separator_group = NULL,
  n = NULL,
  p.value = NULL,
  exponentiate = FALSE,
  sort_terms = c("none", "descending", "ascending"),
  point_size = 2.3,
  point_shape = 19,
  line_size = 0.5,
  staple_width = 0.2,
  dodge_width = 0.6,
  separator_lines = FALSE,
  separator_line_linetype = 2,
  separator_line_colour = "black",
  separator_line_size = 0.4,
  striped_rows = FALSE,
  stripe_fill = "grey95",
  stripe_colour = NA,
  zero_line = TRUE,
  zero_line_linetype = 2,
  zero_line_colour = "grey60",
  xlab = NULL,
  ylab = NULL,
  title = NULL
)
```

## Arguments

- data:

  Either a tidy coefficient data frame or a model object supported by
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- term, estimate, conf.low, conf.high, label, group, grouping,
  separator_group, n, p.value:

  Column names used when `data` is a data frame.

- exponentiate:

  Logical; if TRUE, draw the estimate axis on a log scale with a null
  line at 1.

- sort_terms:

  How to sort rows: "none", "descending", or "ascending".

- point_size:

  Point size for coefficient markers.

- point_shape:

  Shape used for coefficient markers.

- line_size:

  Line width for confidence intervals.

- staple_width:

  Width of the terminal staples on confidence interval lines.

- dodge_width:

  Horizontal dodging used for grouped estimates.

- separator_lines:

  Logical; if TRUE, draw dashed separator lines around each labeled
  block identified by `separator_group`.

- separator_line_linetype:

  Line type used for separator lines.

- separator_line_colour:

  Colour used for separator lines.

- separator_line_size:

  Line width used for separator lines.

- striped_rows:

  Logical; if TRUE, shade alternating rows.

- stripe_fill:

  Fill color used for shaded rows.

- stripe_colour:

  Border color for shaded rows.

- zero_line:

  Logical; if TRUE, draw a null reference line.

- zero_line_linetype:

  Line type for the null reference line.

- zero_line_colour:

  Color for the null reference line.

- xlab, ylab, title:

  Optional plot labels.

## Value

A `ggplot` object. Use
[`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
to compose a side table after styling the plot.
