#' Convert parameters tables to forest data
#'
#' Standardizes coefficient tables from [parameters::model_parameters()]
#' (`parameters_model` objects) into `forest_data`. This brings forest plots
#' to the many model classes supported by `parameters`, including Bayesian,
#' robust, and standardized estimates.
#'
#' @details
#' Estimates, intervals, and p-values are used exactly as `parameters`
#' reported them, including any `exponentiate`, `standardize`, or `vcov`
#' choices. Only fixed effects from the conditional component are kept, and
#' the intercept is dropped unless `intercept = TRUE`. Row labels use the
#' formatted parameter names from `parameters` (for example `"cyl [6]"`).
#' Multivariate models with a `Response` column are faceted by response.
#'
#' The effect scale follows the table: exponentiated tables are ratios on a
#' log10 axis, log-link coefficients such as log-odds are on the log scale,
#' and other coefficients are additive. The effect label is the coefficient
#' name reported by `parameters`, such as `"Odds Ratio"`.
#'
#' @inheritParams as_forest_data_marginaleffects
#' @param data A `parameters_model` object from
#'   [parameters::model_parameters()].
#' @param term_labels Optional named vector used to relabel parameters. Names
#'   may be raw parameter names (such as `"cyl6"`) or formatted labels.
#' @param intercept Logical; whether to keep the intercept.
#' @param pretty_names Logical; if `TRUE`, use the formatted parameter names
#'   from `parameters`. If `FALSE`, use the raw parameter names.
#'
#' @return A `forest_data` object. The original columns, such as `SE` and
#'   `df_error`, are retained for table helpers.
#' @seealso [as_forest_data()], [ggforestplot()]
#' @name as_forest_data_parameters
#'
#' @examples
#' if (requireNamespace("parameters", quietly = TRUE)) {
#'   fit <- glm(am ~ hp + wt + factor(cyl), data = mtcars, family = binomial())
#'   params <- parameters::model_parameters(fit, exponentiate = TRUE)
#'   ggforestplot(params)
#' }
NULL

#' @rdname as_forest_data_parameters
#' @export
as_forest_data.parameters_model <- function(data,
                                            ...,
                                            term_labels = NULL,
                                            exponentiate = NULL,
                                            conf.level = NULL,
                                            intercept = FALSE,
                                            pretty_names = TRUE,
                                            sort_terms = c("none", "descending", "ascending")) {
  rlang::check_dots_empty()
  sort_terms <- match.arg(sort_terms)
  for (arg in c("intercept", "pretty_names")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      abort_forest(
        sprintf("`%s` must be `TRUE` or `FALSE`.", arg),
        class = "ggforestplotR_invalid_data"
      )
    }
  }

  coefficient_name <- attr(data, "coefficient_name", exact = TRUE)
  exponentiated <- attr(data, "exponentiate", exact = TRUE)
  labels <- attr(data, "pretty_names", exact = TRUE)
  stored_level <- attr(data, "ci", exact = TRUE)
  table <- as.data.frame(data, stringsAsFactors = FALSE)
  attributes(table) <- attributes(table)[c("names", "row.names")]
  class(table) <- "data.frame"

  estimate_candidates <- unique(c(
    "Coefficient", "Std_Coefficient", "Median", "Mean", "MAP",
    if (is.character(coefficient_name)) coefficient_name
  ))
  estimate_column <- intersect(estimate_candidates, names(table))
  check_result_columns(
    table,
    c("Parameter", "CI_low", "CI_high"),
    "`parameters`",
    hint = "Use `model_parameters()` with confidence intervals (`ci` not `NULL`)."
  )
  if (length(estimate_column) == 0L) {
    abort_forest(
      "The `parameters` table has no recognized coefficient column.",
      class = "ggforestplotR_invalid_data"
    )
  }
  estimate_column <- estimate_column[[1L]]

  keep <- rep(TRUE, nrow(table))
  if ("Effects" %in% names(table)) {
    keep <- keep & table$Effects %in% "fixed"
  }
  if ("Component" %in% names(table) && any(table$Component %in% "conditional")) {
    keep <- keep & table$Component %in% "conditional"
  }
  if (!isTRUE(intercept)) {
    keep <- keep & !table$Parameter %in% c("(Intercept)", "Intercept", "b_Intercept")
  }
  table <- table[keep, , drop = FALSE]
  if (nrow(table) == 0L) {
    abort_forest(
      "The `parameters` table has no fixed-effect rows to plot.",
      class = "ggforestplotR_invalid_data"
    )
  }

  display <- as.character(table$Parameter)
  if (isTRUE(pretty_names) && is.character(labels) && !is.null(names(labels))) {
    matched <- match(display, names(labels))
    display[!is.na(matched)] <- unname(labels[matched[!is.na(matched)]])
  }
  if (!is.null(term_labels)) {
    display <- apply_term_labels(as.character(table$Parameter), display, term_labels)
    display <- apply_term_labels(display, display, term_labels)
  }

  reserved <- c("estimate", "conf.low", "conf.high", "p.value")
  clashing <- intersect(setdiff(names(table), c(estimate_column, "CI_low", "CI_high", "p")), reserved)
  if (length(clashing) > 0L) {
    names(table)[match(clashing, names(table))] <- paste0(clashing, ".parameters")
  }
  names(table)[match(estimate_column, names(table))] <- "estimate"
  names(table)[match(c("CI_low", "CI_high"), names(table))] <- c("conf.low", "conf.high")
  if ("p" %in% names(table)) {
    names(table)[match("p", names(table))] <- "p.value"
  }
  table$Parameter_label <- display

  label <- if (is.character(coefficient_name) && length(coefficient_name) == 1L) {
    coefficient_name
  } else {
    "Coefficient"
  }
  scale <- if (isTRUE(exponentiated) ||
    (!isFALSE(exponentiated) && is.character(exponentiated))) {
    result_scale("ratio", label)
  } else if (grepl("^Log-", label)) {
    result_scale("log", label)
  } else {
    result_scale("identity", label)
  }
  if (isTRUE(attr(data, "linear_model", exact = TRUE)) && is.character(exponentiated)) {
    scale <- result_scale("identity", label)
  }

  facet <- if ("Response" %in% names(table) &&
    length(unique(table$Response)) > 1L) "Response" else NULL

  result_table_as_forest_data(
    table,
    id_columns = "Parameter_label",
    scale = scale,
    source = "`parameters`",
    label = "Parameter_label",
    facet = facet,
    exponentiate = exponentiate,
    conf.level = resolve_result_conf_level(
      conf.level,
      stored = stored_level,
      source = "`parameters`"
    ),
    sort_terms = sort_terms,
    source_model = c(
      "parameters_model",
      attr(data, "model_class", exact = TRUE)
    ),
    source_package = "parameters"
  )
}
