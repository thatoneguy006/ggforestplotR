# ggforestplotR 0.3.0

## Breaking Changes

- Removed the deprecated table-display shortcut arguments `show_terms`,
  `show_n`, `show_events`, `show_estimate`, and `show_p` from
  `add_forest_table()` and `add_split_table()`. Use `columns`,
  `left_columns`, and `right_columns` to choose table columns explicitly.
- Numeric table column positions now resolve against the supplied/source data
  columns in their original order, rather than the previous fixed built-in
  table-column order.

## Major Changes

- Added mixed-model support for `tidy_forest_model()` and `ggforestplot()`
  when `broom.mixed` is installed. Mixed-model tidiers are restricted to fixed
  effects so random-effect parameter rows are not plotted as terms.
- Added confidence interval truncation support to `ggforestplot()` via
  `ci_limits`, with optional arrowheads for intervals extending beyond the
  displayed range.
- Added `bind_forest_models()` to stack multiple fitted models into one
  grouped forest-plot data frame.

## Minor Changes

- Relaxed forest-data validation so only the core plot geometry columns
  `term`, `estimate`, `conf.low`, and `conf.high` are required. Optional table
  and grouping columns are validated only when used.

# ggforestplotR 0.2.2

## Minor Changes

- Faceted `ggforestplot()` output now respects factor level order in the
  `facet` column, allowing users to control facet ordering directly from their
  data.
- Fixed faceted `ggforestplot()` output when users add `scale_y_discrete(limits = ...)`, preserving per-facet row filtering while accepting visible term labels.

# ggforestplotR 0.2.1

## Breaking Changes

- Simplified the reference-line API in `ggforestplot()`: `ref_line` now takes the numeric reference value directly, and `NULL` hides the line.
- Replaced the older reference-line arguments with `ref_label`, `ref_linetype`, and `ref_color`.

## Major Changes

- Added `facet` and `facet_strip_position` as clearer names for grouped plot panels. The older `grouping` and `grouping_strip_position` arguments now warn and continue to work.
- Added `linewidth` for confidence interval widths. The older `line_size` argument now warns and continues to work.
- Added deprecation warnings for table-display shortcuts in favor of explicit `columns`, `left_columns`, and `right_columns` selections.

## Minor Changes

- Added `stripe_alpha` to control row stripe transparency across plots, side tables, and split tables.
- Updated table helpers so they inherit custom y-axis ordering and filtering from the trained forest plot scale.
- Updated vignettes and tests to use the clearer faceting and geometry argument names.

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
