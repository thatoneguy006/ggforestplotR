# ggforestplotR

`ggforestplotR` provides a `ggplot2`-first workflow for building forest plots
from tidy coefficient tables or fitted model objects.

## Installation

Install the current development version from GitHub.

```r
install.packages("remotes")
remotes::install_github("thatoneguy006/ggforestplotR")
```

## Core workflows

The package currently supports three common workflows:

- Build a forest plot directly from a coefficient table
- Start from a fitted model and tidy it for plotting with `broom`
- Add side tables or split-table layouts for reporting-ready output

## Quick example

```r
library(ggforestplotR)
library(ggplot2)

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
  striped_rows = TRUE
) +
  labs(title = "Grouped forest plot") +
  add_forest_table(
    position = "left",
    show_n = TRUE,
    estimate_label = "Beta"
  )
```

## Learn more

- Package site: <https://thatoneguy006.github.io/ggforestplotR/>
- Get started article: <https://thatoneguy006.github.io/ggforestplotR/articles/ggforestplotR-get-started.html>
- Plot customization article: <https://thatoneguy006.github.io/ggforestplotR/articles/ggforestplotR-plot-customization.html>
- Data helpers article: <https://thatoneguy006.github.io/ggforestplotR/articles/ggforestplotR-data-helpers.html>

## Main functions

- `ggforestplot()` builds the plotting panel from a data frame or supported model object.
- `add_forest_table()` attaches a summary table to the left or right side of the plot.
- `add_split_table()` places table columns on both sides of the forest plot.
- `as_forest_data()` standardizes custom coefficient data.
- `tidy_forest_model()` converts fitted models into plotting-ready coefficient data.
