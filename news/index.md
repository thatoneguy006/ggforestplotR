# Changelog

## ggforestplotR 0.1.1

### Minor Changes

- Added `events` column support across
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md),
  [`as_forest_data()`](https://thatoneguy006.github.io/ggforestplotR/reference/as_forest_data.md),
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md),
  and
  [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_split_table.md).
  (Thanks [@sritchie73](https://github.com/sritchie73) for the
  suggestion)
- Added table header styling controls for font size, font face, and font
  family in both table helpers. (Thanks
  [@sritchie73](https://github.com/sritchie73) for the suggestion)
- Split table composition code into dedicated source files to simplify
  maintenance of the public API.
- Refined split-table examples and documentation to match the expanded
  numeric column mapping.

## ggforestplotR 0.1.0

CRAN release: 2026-03-25

### Major changes

- Refined split-table layout logic, including dynamic panel widths from
  column counts, alignment controls, and cleaner stripe/background
  handling.
- [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  now allows for custom column ordering via the `columns` argument.
- Converted package documentation to roxygen-generated `NAMESPACE` and
  `.Rd` files.
- Reworked package documentation, including pkgdown configuration,
  README installation guidance, and the vignette set.

## ggforestplotR 0.0.0.9000

### Initial development version

- Added grouped section support for forest plots.
- Added
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  and
  [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_split_table.md)
  for plot-table composition.
- Added various plot and table customization options.
- Added package documentation, tests, and a getting-started vignette.
