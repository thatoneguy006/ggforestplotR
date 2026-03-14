# Draw a ggplot2 forest plot

Builds a forest plot from standardized coefficient data or directly from
a fitted model, with optional grouped sections, striped rows, separator
lines for labeled variable blocks, and controls for point shape and
confidence-interval staples.

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
  grouping_strip_position = c("left", "right"),
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
  zero_line_colour = "grey60"
)
```

## Arguments

- data:

  Either a tidy coefficient data frame or a model object supported by
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- term, estimate, conf.low, conf.high, label, group, grouping,
  grouping_strip_position, separator_group, n, p.value:

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

## Value

A `ggplot` object. Use standard `ggplot2` functions such as
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) for plot
labels, and add composition helpers after styling the main plot.

## Examples

``` r
coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.10, -0.08, 0.34),
  conf.low = c(0.02, -0.16, 0.12),
  conf.high = c(0.18, 0.00, 0.56)
)

ggforestplot(coefs)


ggforestplot(coefs, striped_rows = TRUE, point_shape = 17) +
  ggplot2::labs(title = "Basic forest plot")
```
