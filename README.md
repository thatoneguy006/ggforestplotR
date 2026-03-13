# ggforestplotR

`ggforestplotR` is an in-progress R package for building coefficient-based
forest plots with a `ggplot2`-first workflow.

## Current MVP

The package currently includes:

- `as_forest_data()` to standardize coefficient tables for plotting
- `tidy_forest_model()` to pull coefficients from supported model objects via `broom`
- `ggforestplot()` to render a forest plot from either a model object or a tidy coefficient frame
- grouped sections via the `grouping` argument
- alternating row striping via `striped_rows = TRUE`
- an attached summary table that can sit on the left or right side of the plot

## Example

```r
library(ggforestplotR)

coefs <- data.frame(
  term = c("Age", "BMI", "Stage II", "Stage III"),
  estimate = c(0.10, -0.08, 0.34, 0.52),
  conf.low = c(0.02, -0.16, 0.12, 0.20),
  conf.high = c(0.18, 0.00, 0.56, 0.84),
  section = c("Clinical", "Clinical", "Tumor", "Tumor"),
  sample_size = c(120, 115, 87, 83)
)

ggforestplot(
  coefs,
  grouping = "section",
  n = "sample_size",
  striped_rows = TRUE,
  table_position = "left",
  table_estimate_label = "Beta",
  title = "Example grouped forest plot"
)
```

## Next steps

Planned work includes richer theming, grouped multi-model comparisons, and
helpers for common modelling packages.