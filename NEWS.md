# ggforestplotR 0.1.0

## Major changes

- Finalized the table-composition workflow with `add_forest_table()` and `add_split_table()` as post-plot helpers.
- Added configurable side-table and split-table columns, including `N`, formatted estimates with 95% confidence intervals, and optional p-values.
- Refined split-table layout logic, including dynamic panel widths from column counts, alignment controls, and cleaner stripe/background handling.
- Added grouped sections, striped rows, separator lines for labeled blocks, exponentiated forest plot support, point-shape control, and confidence-interval staple-width control.
- Expanded regression coverage with contract tests for table composition and exponentiated plotting behavior.
- Converted package documentation to roxygen-generated `NAMESPACE` and `.Rd` files.
- Reworked package documentation, including pkgdown configuration, README installation guidance, and the vignette set.

# ggforestplotR 0.0.0.9000

## Initial development version

- Created `ggforestplot()` for coefficient-driven forest plots from tidy data frames and supported model objects.
- Added `as_forest_data()` and `tidy_forest_model()` helpers for preparing standardized forest-plot data.
- Added the initial package scaffolding, manual pages, tests, and website/vignette infrastructure.
