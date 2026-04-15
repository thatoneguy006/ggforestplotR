# Add split tables around a forest plot

Compose split table blocks around a forest plot so that summary data
appear on both sides of the plotting panel.

## Usage

``` r
add_split_table(
  plot = NULL,
  show_terms = TRUE,
  show_n = NULL,
  show_events = NULL,
  show_estimate = TRUE,
  show_p = FALSE,
  left_columns = NULL,
  right_columns = NULL,
  term_header = "Term",
  n_header = "N",
  events_header = "Events",
  estimate_label = "Estimate",
  p_header = "P-value",
  digits = NULL,
  text_size = NULL,
  header_text_size = NULL,
  header_fontface = "bold",
  header_family = NULL,
  striped_rows = NULL,
  stripe_fill = NULL,
  stripe_colour = NULL,
  left_width = NULL,
  plot_width = NULL,
  right_width = NULL
)
```

## Arguments

- plot:

  A plot created by
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).
  Leave as `NULL` to use `+ add_split_table(...)` syntax.

- show_terms:

  Whether to include the term column in the default left-side selection
  when `left_columns` is not supplied.

- show_n:

  Whether to include the `N` column in the default left-side selection
  when `left_columns` is not supplied. Defaults to `TRUE` when the
  underlying plot data include an `n` column.

- show_events:

  Whether to include the `Events` column in the default left-side
  selection when `left_columns` is not supplied. Defaults to `TRUE` when
  the underlying plot data include an `events` column.

- show_estimate:

  Whether to include the formatted estimate and confidence interval
  column in the default right-side selection when `right_columns` is not
  supplied.

- show_p:

  Whether to include the p-value column in the default right-side
  selection when `right_columns` is not supplied.

- left_columns:

  Optional explicit columns to place on the left side of the forest
  plot. Accepts names such as `"term"`, `"n"`, and `"events"`, or
  positions `1:5` corresponding to `term`, `n`, `events`, `estimate`,
  and `p`.

- right_columns:

  Optional explicit columns to place on the right side of the forest
  plot. Accepts names such as `"estimate"` and `"p"`, or positions `1:5`
  corresponding to `term`, `n`, `events`, `estimate`, and `p`.

- term_header:

  Header text for the term column.

- n_header:

  Header text for the `N` column.

- events_header:

  Header text for the `Events` column.

- estimate_label:

  Header label for the estimate column.

- p_header:

  Header text for the p-value column.

- digits:

  Number of digits used when formatting estimates and p-values. Defaults
  to `2`.

- text_size:

  Text size for table contents. Defaults to `3.2`.

- header_text_size:

  Header text size for table column labels. Defaults to `11`.

- header_fontface:

  Font face used for table column labels. Defaults to `"bold"`.

- header_family:

  Optional font family used for table column labels.

- striped_rows:

  Whether to draw alternating row stripes behind the split table layout.
  Defaults to the stripe setting used in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- stripe_fill:

  Fill colour used for striped rows. Defaults to the stripe fill used in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- stripe_colour:

  Outline colour for striped rows. Defaults to the stripe outline used
  in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).

- left_width:

  Optional width allocated to the left table block. By default this is
  derived from the number of displayed left-side columns relative to
  `plot_width`.

- plot_width:

  Optional width allocated to the forest plot panel. Defaults to `2.5`.

- right_width:

  Optional width allocated to the right table block. By default this is
  derived from the number of displayed right-side columns relative to
  `plot_width`.

## Value

A patchwork-composed plot containing a left table, the forest plot, and
a right table, or a ggplot add-on object when `plot = NULL`.

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
add_split_table(
  p,
  left_columns = c("term", "n"),
  right_columns = c("estimate", "p"),
  estimate_label = "HR"
)


ggforestplot(coefs, n = "sample_size", p.value = "p_value") +
  add_split_table(
    left_columns = c(1, 2),
    right_columns = c(4, 5),
    estimate_label = "HR"
  )
```
