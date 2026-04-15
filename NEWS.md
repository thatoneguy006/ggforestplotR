# ggforestplotR 0.1.1

## Improvements

- Added `events` column support across `ggforestplot()`, `as_forest_data()`, `add_forest_table()`, and `add_split_table()`.
- Added table header styling controls for font size, font face, and font family in both table helpers.
- Split table composition code into dedicated source files to simplify maintenance of the public API.
- Refined split-table examples and documentation to match the expanded `1:5` numeric column mapping.

# ggforestplotR 0.1.0

## Major changes

- Refined split-table layout logic, including dynamic panel widths from column counts, alignment controls, and cleaner stripe/background handling.
- `add_forest_table()` now allows for custom column ordering via the `columns` argument.
- Converted package documentation to roxygen-generated `NAMESPACE` and `.Rd` files.
- Reworked package documentation, including pkgdown configuration, README installation guidance, and the vignette set.

# ggforestplotR 0.0.0.9000

## Initial development version

- Added grouped section support for forest plots.
- Added `add_forest_table()` and `add_split_table()` for plot-table composition.
- Added various plot and table customization options.
- Added package documentation, tests, and a getting-started vignette.
