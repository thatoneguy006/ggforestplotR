# Change specification review

Work started from `92f395e`, which contains `dev` plus the existing point-size and
grouped-subgroup fixes, on `codex/validated-change-spec`. The maintainer initially
deferred Phase 4, then approved its implementation, version 0.6.0, and a push to
`dev`. No investigation-only API changes were implemented. Fetched `origin/dev`
at `19e8da0` had the same file contents as the starting local revision; its history
was merged before the Phase 4 changes, retaining the validated files.

## Independent findings

| Item | Finding |
| --- | --- |
| 1 | Reproduced original stripe mutation for both x and y scales; fixed by cloning layer environments before data replacement. |
| 2 | Confirmed the conditional factor-level reversal and the reversed original README figures; regression tests and regenerated figures now show data order from top to bottom. Explicit user y-scale limits retain ggplot2 order. |
| 3 | Reproduced retention of `cond`, `zi`, and `disp` rows by the old filter; the conditional-only unit and glmmTMB integration tests pass. |
| 4 | Confirmed bound groups were character rather than ordered factors. Legends now follow supplied labels; the ggplot2 4.x regression compares break values without its `pos` attribute. |
| 5 | Reproduced a silently discarded `exponentate` typo. Unknown model-method arguments now error. |
| 6 | Reproduced quasibinomial metadata reporting additive `Beta`; the family checks now match binomial models. |
| 7 | Confirmed the data-frame conversion never exponentiates values, despite the previous plotting documentation. Documentation and the positivity error now explain the distinction. |
| 8 | Reproduced missing-`term` errors for broom-style data frames. Defaults now match the plotting function. |
| 9 | Reproduced the hard-coded `Group` title for a `cohort` mapping. Titles now use the same helper as table headers. |
| 10 | Reproduced the source-column metadata error when selecting canonical columns. The supplied fix also deleted a required metadata key when no sources survived; corrected it to preserve the key with a null value using list assignment. Removed source mappings are pruned. |
| 11 | Reproduced `1/3` displaying as `0.333333333333333`; numeric extra columns now use the requested estimate precision. |
| 12 | Confirmed estimates and summary rows use the same plot geometry; documented this without changing appearance. |
| 13 | Both redundant hline arguments existed. Independently reproduced the unknown-`inherit.aes` warning with isolated ggplot2 3.4.4; it does not occur with 4.0.3. The package's regression tests pass on both combinations after removing the arguments. |
| 14 | Independently reproduced `mean(6) - mean(4)` legend labels with marginaleffects 0.18.0. Added normalization and direct tests for both known formats. |
| 15 | With unbalanced sleepstudy clusters, default slopes were about 11.44655 and 6.07360 versus fixed slopes 11.57954 and 6.41649. `re.form = NA` matched the fixed slopes. Verified that marginaleffects forwards `level = 0` for nlme models, also matching fixed slopes. Added balanced, unbalanced, and nlme regression tests. |
| 16 | Confirmed unversioned Imports and missing glmmTMB Suggests/compatibility jobs. Dependency-floor investigation and CI details are recorded below. |
| 17 | Confirmed table measurement reaches `grid::convertWidth()` without an active device guard. A temporary null PDF now encloses text measurement and is closed on exit. |
| 18 | Confirmed plot-data construction always supplies `.display_identity` and `display_label`; removed the unreachable fallback paths and retained the panel-key helpers still in use. |
| 19 | Reproduced `tidy.numeric` deprecation followed by an unrelated result error. Unsupported atomic, null, and function inputs now fail directly. |
| 20 | Confirmed identical subgroup validation before and after sorting; retained the validation after sorting. |
| 21 | Confirmed the implementation uses each side's own width, contradicting the shared-maximum comments. Updated only those comments. |
| 22 | Confirmed decorative section characters and abbreviated vignette booleans. The tracked CRAN-SUBMISSION described 0.3.1, but the pre-existing local edit described 0.5.0; preserved that local content in `.tmp-lib/CRAN-SUBMISSION.original` before the requested deletion. |

## Phase 4: approved API consistency changes

Implemented `ref_colour` and `separator_linewidth` in `ggforestplot()`,
`grid_linewidth` in `add_forest_table()`, and `facet` in the data-frame
`as_forest_data()` method. Each replacement retains the old positional slot.
The former name moves to the end of its signature with a null default, warns
with class `ggforestplotR_deprecated_argument`, and forwards its value. Supplying
both old and new names errors. Tests compare identical built plot data, table
adder objects, or forest-data objects, and check the positional slots.

The spec incorrectly assumed `add_split_table()` exposes `grid_line_size`.
It has no grid-line controls. The maintainer explicitly chose to rename only the
existing `add_forest_table()` argument, so no split-table grid API was invented.
Internal data columns and metadata keys, including `grouping`, are retained.
Updated the internal caller, existing tests, vignette examples, documentation,
NEWS, and DESCRIPTION version to 0.6.0.

## Investigation: scope scale handling to forest plots

Installed ggplot2 4.0.3 exports `class_ggplot`, `class_scale`, and the S7
`update_ggplot()` generic. A separate prototype successfully constructed a subclass
of `class_ggplot`, registered a method for `list(class_scale, forest_subclass)`,
added a scale, and built both the subclass plot and an ordinary ggplot. The method
ran only for the subclass. This is a supported route to scope scale-addition
behavior to this package's plot objects; it would require an explicit S7 strategy
and either a ggplot2 4.x dependency floor or an older-version fallback.

The exported `ggplot_build()` remains an S3 generic in 4.0.3. A prototype build
method for the custom subclass ran once and delegated successfully using
`NextMethod()`. Although the current web documentation mentions `build_ggplot()`,
that name is not exported in installed 4.0.3; it should not be treated as a public
hook. These are migration options, not changes made by this work.

References: [ggplot2 extension generic](https://ggplot2.tidyverse.org/reference/update_ggplot.html),
[class definitions](https://ggplot2.tidyverse.org/reference/class_definitions.html),
[build documentation](https://ggplot2.tidyverse.org/reference/ggplot_build.html).

## Investigation: absolute table widths

Installed patchwork 1.3.2 accepted
`wrap_plots(p, p, widths = grid::unit(c(1, 1), c("in", "null")))` and successfully
built a gtable. Its layout retained one inch for the table and one `null` unit for
the forest panel. This agrees with the
[patchwork layout guide](https://patchwork.data-imaginist.com/articles/guides/layout.html).

Currently all widths are numeric relative weights. If the device width doubles,
the table panel approximately doubles too, while its text retains its point size.
Under a mixed-unit design the table panel would retain its measured width in
inches, and the forest panel would fill the remaining space. Layout widths apply
to panel regions; margins, guides, and axis text also consume device space.

Such a change needs explicit API decisions: either interpret numeric
`table_width` values as inches (a breaking semantic change), or retain numeric
relative weights and accept explicit `grid::unit()` values for absolute widths.
With absolute tables, numeric `plot_width` would become a weight among the
remaining `null` panels; with one forest panel it would no longer control the
table-to-forest ratio. Split tables would use their separately measured absolute
widths. Small devices would need defined overflow/minimum-panel behavior. No width
semantics changed in this implementation.

The split helper currently replaces each spec's content width with its relative
`left_width` or `right_width`; the defaults depend on column count. An absolute
redesign would need to preserve measured widths separately instead of treating
those relative defaults as inches.

## Validation

R 4.6.0 was used with ggplot2 4.0.3, patchwork 1.3.2, rlang 1.3.0,
marginaleffects 0.32.0, and glmmTMB 1.1.14. Every numbered item received its own
commit in order after a full `devtools::test()` run. Roxygen documentation was
regenerated when the relevant source documentation or function defaults changed.

The minimum marginaleffects release was investigated in a separate workspace
library. Versions 0.18.0, 0.19.0, 0.25.0, 0.27.0, and 0.28.0 reproduced the
mixed-model predictor error; 0.29.0 passed the complete subgroup test file,
including the new population-level lmer and nlme regressions. The preceding
release 0.28.0 failed, so the declared floor is 0.29.0. The new slope tests follow
the existing suite's suppression of marginaleffects' expected fixed-effect
uncertainty warning from omnibus hypothesis calculations; package runtime warning
behavior was not changed.

The local testthat/pkgload combination did not load helper fixtures into the
development test environment automatically. The temporary development runner
explicitly sources the existing helper files, then calls `devtools::test()` with
that environment. No package test helpers were removed or changed to accommodate
this tooling issue. Startup warnings that locally installed dependencies were
built under R 4.6.1 are outside the test assertion results.

The requested ordinary-ggplot guard is included. A fresh-script device check
returned null device 1 with no output files after composing both table types;
before the fix it returned device 2 and created `Rplots.pdf`. The supplied
in-suite device regression may skip when previous graphics tests already have a
device open; the separate fresh process covers that condition directly.

The CI YAML parses and includes seven matrix configurations, including
`oldest-deps` and `ggplot2-dev`. Remote GitHub Actions jobs were not run by this
local task. Final suite, build, and CRAN-check results follow below.

### Final 0.6.0 results

- Full development suite: **914 passed assertions, 0 failures, 0 errors,
  0 test warnings**, and the one device-state skip described above.
- Focused regression and integration coverage across ten test files also passed
  with ggplot2 3.4.4, patchwork 1.2.0, and marginaleffects 0.29.0, including the
  new deprecation aliases. This run also had only the device-state skip.
- The independent fresh-process device test passed with no files created and no
  device left open.
- `R CMD build .` successfully built `ggforestplotR_0.6.0.tar.gz`, including
  vignettes.
- `R CMD check --as-cran ggforestplotR_0.6.0.tar.gz` completed with **Status: OK:
  0 errors, 0 warnings, 0 notes**, including installed-package tests, rebuilt
  vignette outputs, and the PDF and HTML manuals.
- The earlier 0.5.0.9000 check had only the development-version large-components
  NOTE. The subsequently authorized 0.6.0 bump resolved it without disabling
  any CRAN checks.
