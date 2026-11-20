# Package index

## Plot & Table Construction

Main plotting functions for forest plot creation and composition.

- [`ggforestplot()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/ggforestplot.md)
  : Draw a ggplot2 forest plot
- [`forest_theme()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/forest_theme.md)
  : Style a forest plot and its tables
- [`add_forest_table()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/add_forest_table.md)
  : Add a summary table to a forest plot
- [`add_split_table()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/add_split_table.md)
  : Add split tables around a forest plot
- [`add_favors()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/add_favors.md)
  : Add directional favors labels beneath a forest plot

## Data Preparation

Helpers for standardizing coefficient data and tidying model outputs.

- [`as_forest_data()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data.md)
  : Standardize coefficient data for forest plots
- [`forest_metadata()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/forest_metadata.md)
  : Inspect forest-data metadata
- [`tidy_forest_model()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/tidy_forest_model.md)
  : Tidy a model object for forest plotting
- [`bind_forest_models()`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/bind_forest_models.md)
  : Bind multiple model summaries for a grouped forest plot

## Post-Estimation and Summary Tables

Methods for results that already contain estimates and confidence
intervals.

- [`as_forest_data(`*`<comparisons>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_marginaleffects.md)
  [`as_forest_data(`*`<predictions>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_marginaleffects.md)
  [`as_forest_data(`*`<slopes>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_marginaleffects.md)
  [`as_forest_data(`*`<hypotheses>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_marginaleffects.md)
  : Convert marginaleffects results to forest data
- [`as_forest_data(`*`<emmGrid>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_emmeans.md)
  [`as_forest_data(`*`<summary_emm>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_emmeans.md)
  [`as_forest_data(`*`<emm_list>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_emmeans.md)
  : Convert emmeans results to forest data
- [`as_forest_data(`*`<gtsummary>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_gtsummary.md)
  : Convert gtsummary regression tables to forest data
- [`as_forest_data(`*`<parameters_model>`*`)`](https://thatoneguy006.github.io/ggforestplotR/dev/reference/as_forest_data_parameters.md)
  : Convert parameters tables to forest data
