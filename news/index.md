# Changelog

## ggforestplotR 0.5.0

### Breaking Changes

- Removed the deprecated `grouping` and `grouping_strip_position`
  arguments from
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md).
  Use `facet` and `facet_strip_position` instead.

### Major Changes

- Added
  [`add_favors()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_favors.md)
  for composing two-sided directional arrows and labels beneath the
  forest-plot panel. The fixed-height footer respects log scales, custom
  limits and expansion, and remains aligned when used after
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  or
  [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_split_table.md).
- Added explicit `subgroup` mappings for mixed hierarchical displays,
  allowing standalone covariates and contiguous categorical blocks to
  share a forest plot. Subgroup p-values can be delegated as “overall”
  or “level” via `p_method` in
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md)
- Added covariance-aware subgroup effects for fitted interaction models.
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md)
  and fitted-model
  [`as_forest_data()`](https://thatoneguy006.github.io/ggforestplotR/reference/as_forest_data.md)
  methods now use `marginaleffects` to derive average slopes or
  comparisons from the original fit without refitting subgroup models or
  displaying raw interaction coefficients. Conservative automatic
  selection supports one continuous-by- factor interaction, while
  explicit selection supports factor comparisons and preserves additive,
  odds-ratio, ratio, and hazard-ratio scales.

### Minor Changes

- Grouped values in
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  and
  [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_split_table.md)
  now use the same vertical dodge as their plotted points, keeping table
  text aligned at different output sizes and with custom `dodge_width`
  values.
- Factor-valued `group` columns now retain their level order in legends,
  vertically dodged estimates, and aligned forest-table values.
- Deprecated `term_header`, `n_header`, `events_header`, and `p_header`
  in
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  and
  [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_split_table.md).
  Relabel displayed headers with the `column_labels` named vector
  instead.

## ggforestplotR 0.4.0

### Major Changes

- Added the `forest_data` S3 class as the package’s stable interchange
  format. It stores validated effect-scale, axis, confidence-level,
  reference-value, source-model, source-package, and source-column
  metadata.
- Converted
  [`as_forest_data()`](https://thatoneguy006.github.io/ggforestplotR/reference/as_forest_data.md)
  into an S3 generic with methods for data frames, existing
  `forest_data` objects, linear and generalized linear models, Cox
  models, and supported mixed-model classes.
- Refactored
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
  to consume the `forest_data` contract instead of determining plotting
  behavior from the original model class.
- Added
  [`forest_metadata()`](https://thatoneguy006.github.io/ggforestplotR/reference/forest_metadata.md)
  for inspecting the semantic and provenance metadata associated with a
  `forest_data` object.

### Minor Changes

- Separated the semantic estimate scale from the plotting-axis
  transformation. Ratio estimates now use `estimate_scale = "ratio"` and
  `axis_transform = "log10"`; unexponentiated log-link coefficients use
  `estimate_scale = "log"` and an identity axis.
- [`bind_forest_models()`](https://thatoneguy006.github.io/ggforestplotR/reference/bind_forest_models.md)
  now returns `forest_data` and validates compatible estimate scales,
  axis transformations, reference values, and confidence levels before
  combining models.
- Fitted model objects and duplicate source data frames are not retained
  in `forest_data`; provenance records model classes, package names, and
  source column mappings instead.
- [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md)
  remains available as a compatibility wrapper around the new
  [`as_forest_data()`](https://thatoneguy006.github.io/ggforestplotR/reference/as_forest_data.md)
  model methods.
- Legends positioned at the top or bottom are now collected at the
  patchwork level, centering them across the complete table-and-plot
  composition.
- Grouped tables now display model or group names in a dedicated,
  multiline column instead of prefixing every value. Include `"group"`
  wherever desired in `columns`, `left_columns`, or `right_columns`, and
  rename it through `column_labels` like any other table column.
- [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  now supports `table_width` and `plot_width` for controlling the
  relative widths of the two composed panels.

## ggforestplotR 0.3.1

CRAN release: 2026-08-04

### Minor Changes

- Fixed unintended behavior when passing custom columns to
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  specific to data-frame/plot inheritance.

## ggforestplotR 0.3.0

CRAN release: 2026-07-07

### Breaking Changes

- Removed the deprecated table-display shortcut arguments `show_terms`,
  `show_n`, `show_events`, `show_estimate`, and `show_p` from
  [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_forest_table.md)
  and
  [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/reference/add_split_table.md).
  Use `columns`, `left_columns`, and `right_columns` to choose table
  columns explicitly.
- Numeric table column positions now resolve against the supplied/source
  data columns in their original order, rather than the previous fixed
  built-in table-column order.

### Major Changes

- Added mixed-model support for
  [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/reference/tidy_forest_model.md)
  and
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
  when `broom.mixed` is installed. Mixed-model tidiers are restricted to
  fixed effects so random-effect parameter rows are not plotted as
  terms.
- Added confidence interval truncation support to
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
  via `ci_limits`, with optional arrowheads for intervals extending
  beyond the displayed range.
- Added
  [`bind_forest_models()`](https://thatoneguy006.github.io/ggforestplotR/reference/bind_forest_models.md)
  to stack multiple fitted models into one grouped forest-plot data
  frame.

### Minor Changes

- Relaxed forest-data validation so only the core plot geometry columns
  `term`, `estimate`, `conf.low`, and `conf.high` are required. Optional
  table and grouping columns are validated only when used.

## ggforestplotR 0.2.2

CRAN release: 2026-06-05

### Minor Changes

- Faceted
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
  output now respects factor level order in the `facet` column, allowing
  users to control facet ordering directly from their data.
- Fixed faceted
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md)
  output when users add `scale_y_discrete(limits = ...)`, preserving
  per-facet row filtering while accepting visible term labels.

## ggforestplotR 0.2.1

CRAN release: 2026-05-30

### Breaking Changes

- Simplified the reference-line API in
  [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/reference/ggforestplot.md):
  `ref_line` now takes the numeric reference value directly, and `NULL`
  hides the line.
- Replaced the older reference-line arguments with `ref_label`,
  `ref_linetype`, and `ref_color`.

### Major Changes

- Added `facet` and `facet_strip_position` as clearer names for grouped
  plot panels. The older `grouping` and `grouping_strip_position`
  arguments now warn and continue to work.
- Added `linewidth` for confidence interval widths. The older
  `line_size` argument now warns and continues to work.
- Added deprecation warnings for table-display shortcuts in favor of
  explicit `columns`, `left_columns`, and `right_columns` selections.

### Minor Changes

- Added `stripe_alpha` to control row stripe transparency across plots,
  side tables, and split tables.
- Updated table helpers so they inherit custom y-axis ordering and
  filtering from the trained forest plot scale.
- Updated vignettes and tests to use the clearer faceting and geometry
  argument names.

## ggforestplotR 0.2.0

CRAN release: 2026-05-20

### Major Changes

- Added separate table formatting controls for point estimates,
  confidence interval bounds, and p-values via `estimate_digits`,
  `interval_digits`, and `p_digits`.
- Added `ref_line`, `ref_line_value`, and `ref_line_label` to
  standardize reference-line naming while keeping `zero_line` arguments
  backward compatible.
- Added `term_labels`, a named-vector relabeling helper for displayed
  model terms.
- Expanded `add_forest_table(columns = ...)` and split-table column
  selection to support arbitrary preserved data columns in addition to
  built-in columns.
- Added `column_labels`, a named-vector helper for relabeling displayed
  table column headers.
- Added a built-in `ci` table column and `ci_fmt` so users can display
  point estimates and confidence intervals in separate columns.
  `conf.low` and `conf.high` are accepted as aliases for `ci`.

### Minor Changes

- Added model-aware default effect labels and scales, including odds
  ratios for binomial logit models and hazard ratios for Cox models.

## ggforestplotR 0.1.1

CRAN release: 2026-04-22

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

### Major Changes

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
