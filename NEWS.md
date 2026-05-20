# ggforestplotR 0.2.0

## Major Changes

- Added separate table formatting controls for point estimates, confidence interval bounds, and p-values via `estimate_digits`, `interval_digits`, and `p_digits`.
- Added `ref_line`, `ref_line_value`, and `ref_line_label` to standardize reference-line naming while keeping `zero_line` arguments backward compatible.
- Added `term_labels`, a named-vector relabeling helper for displayed model terms.
- Expanded `add_forest_table(columns = ...)` and split-table column selection to support arbitrary preserved data columns in addition to built-in columns.
- Added `column_labels`, a named-vector helper for relabeling displayed table column headers.
- Added a built-in `ci` table column and `ci_fmt` so users can display point estimates and confidence intervals in separate columns. `conf.low` and `conf.high` are accepted as aliases for `ci`.

## Minor Changes

- Added model-aware default effect labels and scales, including odds ratios for binomial logit models and hazard ratios for Cox models.

# ggforestplotR 0.1.1

## Minor Changes

- Added `events` column support across `ggforestplot()`, `as_forest_data()`, `add_forest_table()`, and `add_split_table()`. (Thanks @sritchie73 for the suggestion)
- Added table header styling controls for font size, font face, and font family in both table helpers. (Thanks @sritchie73 for the suggestion)
- Split table composition code into dedicated source files to simplify maintenance of the public API.
- Refined split-table examples and documentation to match the expanded numeric column mapping.

# ggforestplotR 0.1.0

## Major Changes

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
