# Add a Side Table to a Forest Plot

Compose a side table onto a forest plot after the plot has been styled.

## Usage

``` r
add_forest_table(
  plot = NULL,
  position = c("left", "right"),
  show_terms = TRUE,
  show_n = NULL,
  show_estimate = TRUE,
  term_header = "Term",
  n_header = "N",
  estimate_label = "Estimate",
  digits = NULL,
  text_size = NULL,
  striped_rows = NULL,
  stripe_fill = NULL,
  stripe_colour = NULL
)
```

## Arguments

- plot:

  A plot created by
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).
  Leave as `NULL` to use `+ add_forest_table(...)` syntax.

- position:

  Whether to place the table on the left or right of the forest plot.

- show_terms:

  Whether to show the term column in the table.

- show_n:

  Whether to show the `N` column. Defaults to `TRUE` when the underlying
  plot data include an `n` column.

- show_estimate:

  Whether to show the formatted estimate and confidence interval column.

- term_header:

  Header text for the term column.

- n_header:

  Header text for the `N` column.

- estimate_label:

  Header label for the estimate column.

- digits:

  Number of digits used when formatting estimates. Defaults to `2`.

- text_size:

  Text size for table contents. Defaults to `3.2`.

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
  sample_size = c(120, 115, 98)
)

p <- ggforestplot(coefs, n = "sample_size")
add_forest_table(p, position = "left", show_n = TRUE, estimate_label = "Beta")


ggforestplot(coefs, n = "sample_size") +
  add_forest_table(position = "right", show_n = TRUE, estimate_label = "Beta")
```
