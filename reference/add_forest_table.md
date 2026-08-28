# Add a summary table to a forest plot

Compose a summary table onto a forest plot.

## Usage

``` r
add_forest_table(
  plot = NULL,
  position = c("left", "right"),
  columns = NULL,
  term_header = "Term",
  n_header = "N",
  events_header = "Events",
  estimate_label = NULL,
  p_header = "P-value",
  column_labels = NULL,
  digits = NULL,
  estimate_digits = NULL,
  interval_digits = NULL,
  p_digits = NULL,
  estimate_fmt = NULL,
  ci_fmt = NULL,
  text_size = NULL,
  header_text_size = NULL,
  header_fontface = "bold",
  header_family = NULL,
  striped_rows = NULL,
  stripe_fill = NULL,
  stripe_colour = NULL,
  stripe_alpha = NULL,
  grid_lines = FALSE,
  grid_line_colour = "black",
  grid_line_size = 0.3,
  grid_line_linetype = 1,
  table_width = NULL,
  plot_width = NULL
)
```

## Arguments

- plot:

  A plot created by
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).
  Leave as `NULL` to use `+ add_forest_table(...)` syntax.

- position:

  Whether to place the table on the left or right of the forest plot.

- columns:

  Optional explicit columns to display in the side table, in the order
  they should appear. Accepts built-in names such as `"term"`,
  `"group"`, `"n"`, `"events"`, `"estimate"`, `"ci"`, and `"p"`,
  arbitrary original dataframe columns, or numeric positions in the
  supplied data. `"conf.low"` and `"conf.high"` are accepted as aliases
  for `"ci"`. The default grouped table includes `"group"`; omit it from
  an explicit selection to hide it. Its header defaults to the source
  column supplied through `group =` for data frames and to `"Model"` for
  [`bind_forest_models()`](https://thatoneguy006.github.io/ggforestplotR/reference/bind_forest_models.md)
  output.

- term_header:

  Deprecated. Use the corresponding entry in `column_labels`, such as
  `column_labels = c(term = "Variable")`.

- n_header:

  Deprecated. Use `column_labels = c(n = "N")` instead.

- events_header:

  Deprecated. Use `column_labels = c(events = "Events")` instead.

- estimate_label:

  Header label for the estimate column. Defaults to the model-derived
  label when available.

- p_header:

  Deprecated. Use `column_labels = c(p = "P-value")` instead.

- column_labels:

  Optional named vector used to relabel table column headers. Names
  should match values supplied to `columns` after column resolution,
  such as `"term"`, `"group"`, `"estimate"`, `"ci"`, `"p"`, or an
  arbitrary original dataframe column. For grouped data frames, either
  `"group"` or the source column supplied through `group =` can relabel
  the group column. Use this argument instead of the deprecated
  dedicated header arguments.

- digits:

  Deprecated. Number of digits used when formatting estimates and
  p-values. Defaults to `2`. Use `estimate_digits`, `interval_digits`,
  and `p_digits` for separate control.

- estimate_digits:

  Number of digits used for point estimates.

- interval_digits:

  Number of digits used for confidence interval bounds.

- p_digits:

  Number of digits used for p-values.

- estimate_fmt:

  Format string for the estimate column. Use `{estimate}`, `{conf.low}`,
  and `{conf.high}` as placeholders. The shorthand
  `{conf.low, conf.high}` is also supported. Defaults to
  `"{estimate} ({conf.low}, {conf.high})"`, or `"{estimate}"` when
  `columns` includes `"ci"`.

- ci_fmt:

  Format string for the confidence interval column when `columns`
  includes `"ci"`. Use `{conf.low}` and `{conf.high}` as placeholders.
  The shorthand `{conf.low, conf.high}` is also supported. Defaults to
  `"({conf.low}, {conf.high})"`.

- text_size:

  Text size for table contents. Defaults to `3.2`.

- header_text_size:

  Header text size for table column labels. Defaults to `11`.

- header_fontface:

  Font face used for table column labels. Defaults to `"bold"`.

- header_family:

  Optional font family used for table column labels.

- striped_rows:

  Whether to draw alternating row stripes behind the table. Defaults to
  the stripe setting used in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- stripe_fill:

  Fill colour used for striped rows. Defaults to the stripe fill used in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- stripe_colour:

  Outline colour for striped rows. Defaults to the stripe outline used
  in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- stripe_alpha:

  Transparency for striped rows. Defaults to the stripe alpha used in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- grid_lines:

  Whether to draw black horizontal grid lines in the table.

- grid_line_colour:

  Colour used for the table grid lines.

- grid_line_size:

  Line width used for the table grid lines.

- grid_line_linetype:

  Line type used for the table grid lines.

- table_width:

  Optional relative width allocated to the table panel. By default this
  is calculated from the displayed table content.

- plot_width:

  Optional relative width allocated to the forest-plot panel. Defaults
  to `2.4`.

## Value

A patchwork-composed plot containing the forest plot and side table, or
a ggplot add-on object when `plot = NULL`.

## Examples

``` r
coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.3, -0.2, 0.4),
  conf.low = c(0.1, -0.4, 0.2),
  conf.high = c(0.5, 0.0, 0.6),
  sample_size = c(120, 115, 98),
  p_value = c(0.012, 0.031, 0.004)
)

p <- ggforestplot(coefs, n = "sample_size", p.value = "p_value")
add_forest_table(
  p,
  position = "left",
  columns = c("term", "n", "estimate", "p"),
  estimate_label = "Beta"
)


ggforestplot(coefs, n = "sample_size", p.value = "p_value") +
  add_forest_table(
    position = "right",
    columns = c("term", "n", "estimate", "p"),
    estimate_label = "Beta"
  )
```
