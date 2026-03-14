# ggforestplotR

`ggforestplotR` is an in-progress R package for building coefficient-based
forest plots with a `ggplot2`-first workflow.

## Current MVP

The initial package scaffold includes:

- `as_forest_data()` to standardize coefficient tables for plotting
- `tidy_forest_model()` to pull coefficients from supported model objects via `broom`
- `ggforestplot()` to render a forest plot from either a model object or a tidy coefficient frame

## Example

```r
library(ggforestplotR)

coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.10, -0.08, 0.34),
  conf.low = c(0.02, -0.16, 0.12),
  conf.high = c(0.18, 0.00, 0.56)
)

ggforestplot(coefs)
```

## Next steps

Planned work includes richer theming, grouped/multi-model comparisons, and
helpers for common modelling packages.
