#' Convert emmeans results to forest data
#'
#' Standardizes estimated marginal means and contrasts from `emmeans` into
#' `forest_data`. [emmeans::emmeans()], [pairs()][emmeans::pairs.emmGrid()],
#' and [emmeans::contrast()] all return `emmGrid` objects, so one method
#' covers means, pairwise comparisons, and custom contrasts. Summaries from
#' [summary()][emmeans::summary.emmGrid()] and [confint()][emmeans::confint.emmGrid()]
#' (`summary_emm`) and `emm_list` results, such as
#' `emmeans(fit, pairwise ~ treatment)`, are supported too.
#'
#' @details
#' `emmGrid` objects are summarized with confidence intervals at `conf.level`
#' and with `adjust` and `type` passed to `summary()`. Contrasts also include
#' their tests; means do not, because a test against zero is rarely
#' meaningful. `summary_emm` objects are used exactly as computed, so
#' `conf.level` can only confirm the stored level.
#'
#' Row labels come from the primary variables, such as the factor levels or
#' the `contrast` column. When `by` variables vary they are added to the label
#' unless they are mapped to `group`, `facet`, or `subgroup`, for example
#' `facet = "sex"`.
#'
#' The effect scale follows the estimate column chosen by `emmeans`:
#'
#' * Contrasts (`estimate`) are additive with a reference line at 0. On a log
#'   or logit link they are log ratios or log odds ratios, and
#'   `exponentiate = TRUE` converts them to ratios.
#' * Back-transformed contrasts (`ratio` and `odds.ratio`) are ratios on a
#'   log10 axis with a reference line at 1.
#' * Marginal means (`emmean`, `response`, `rate`, and similar) have no null
#'   value, so no reference line is drawn. Probabilities (`prob`) use the
#'   probability scale.
#'
#' Intervals and p-values reflect any multiplicity adjustment applied by
#' `emmeans`, such as the Tukey adjustment used by `pairs()`.
#'
#' @inheritParams as_forest_data_marginaleffects
#' @param data An `emmGrid`, `summary_emm`, or `emm_list` object from
#'   `emmeans`.
#' @param conf.level For `emmGrid` objects, the confidence level passed to
#'   `summary()`. `NULL` uses the `emmeans` default. For `summary_emm`
#'   objects, `NULL` uses the stored level and a number must match it.
#' @param type Optional prediction type passed to `summary()`, such as
#'   `"response"` to back-transform means or obtain ratio contrasts.
#' @param adjust Optional multiplicity adjustment passed to `summary()`.
#' @param p_method Subgroup p-value placement when `subgroup` is mapped.
#'   Defaults to `"level"` because `emmeans` reports one test per row.
#' @param which For `emm_list` objects, the element to convert, by position
#'   or name. Defaults to the first element, matching `emmeans` methods.
#'
#' @return A `forest_data` object. The original grid and `by` columns are
#'   retained for table helpers.
#' @seealso [as_forest_data()], [ggforestplot()]
#' @name as_forest_data_emmeans
#'
#' @examples
#' if (requireNamespace("emmeans", quietly = TRUE)) {
#'   warp <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#'   # Marginal means for each tension level, faceted by wool
#'   means <- emmeans::emmeans(warp, ~ tension | wool)
#'   ggforestplot(means, facet = "wool")
#'
#'   # Tukey-adjusted pairwise differences
#'   ggforestplot(pairs(means), facet = "wool")
#'
#'   # Odds ratios from a logistic model
#'   fit <- glm(vs ~ factor(gear) + mpg, data = mtcars, family = binomial())
#'   gear_pairs <- pairs(emmeans::emmeans(fit, ~ gear), adjust = "none")
#'   as_forest_data(gear_pairs, exponentiate = TRUE)
#' }
NULL

#' @rdname as_forest_data_emmeans
#' @export
as_forest_data.emmGrid <- function(data,
                                   ...,
                                   label = NULL,
                                   group = NULL,
                                   facet = NULL,
                                   subgroup = NULL,
                                   term_labels = NULL,
                                   exponentiate = NULL,
                                   estimate_scale = NULL,
                                   effect_label = NULL,
                                   reference_value = NULL,
                                   conf.level = NULL,
                                   type = NULL,
                                   adjust = NULL,
                                   sort_terms = c("none", "descending", "ascending"),
                                   p_method = c("level", "overall")) {
  rlang::check_dots_empty()
  check_result_package("emmeans", "`emmGrid`")

  if (!is.null(conf.level)) {
    resolve_result_conf_level(conf.level, source = "`emmeans`")
  }

  est_type <- tryCatch(data@misc$estType, error = function(error) NULL)
  is_prediction <- is.character(est_type) && length(est_type) == 1L &&
    identical(est_type, "prediction")
  summary_args <- list(infer = c(TRUE, !is_prediction))
  summary_args$level <- conf.level
  summary_args$type <- type
  summary_args$adjust <- adjust
  summary_args <- summary_args[!vapply(summary_args, is.null, logical(1))]

  summarized <- tryCatch(
    rlang::exec(summary, data, !!!summary_args),
    error = function(error) {
      abort_forest(
        "Could not summarize the `emmGrid` object with `emmeans`.",
        class = "ggforestplotR_invalid_model",
        parent = error
      )
    }
  )

  emmeans_as_forest_data(
    summarized,
    label = label,
    group = group,
    facet = facet,
    subgroup = subgroup,
    term_labels = term_labels,
    exponentiate = exponentiate,
    estimate_scale = estimate_scale,
    effect_label = effect_label,
    reference_value = reference_value,
    reference_missing = missing(reference_value),
    conf.level = NULL,
    sort_terms = match.arg(sort_terms),
    p_method = match.arg(p_method),
    source_model = class(data)
  )
}

#' @rdname as_forest_data_emmeans
#' @export
as_forest_data.summary_emm <- function(data,
                                       ...,
                                       label = NULL,
                                       group = NULL,
                                       facet = NULL,
                                       subgroup = NULL,
                                       term_labels = NULL,
                                       exponentiate = NULL,
                                       estimate_scale = NULL,
                                       effect_label = NULL,
                                       reference_value = NULL,
                                       conf.level = NULL,
                                       sort_terms = c("none", "descending", "ascending"),
                                       p_method = c("level", "overall")) {
  rlang::check_dots_empty()
  emmeans_as_forest_data(
    data,
    label = label,
    group = group,
    facet = facet,
    subgroup = subgroup,
    term_labels = term_labels,
    exponentiate = exponentiate,
    estimate_scale = estimate_scale,
    effect_label = effect_label,
    reference_value = reference_value,
    reference_missing = missing(reference_value),
    conf.level = conf.level,
    sort_terms = match.arg(sort_terms),
    p_method = match.arg(p_method),
    source_model = class(data)
  )
}

#' @rdname as_forest_data_emmeans
#' @export
as_forest_data.emm_list <- function(data, ..., which = 1L) {
  valid_which <- (is.numeric(which) && length(which) == 1L && !is.na(which) &&
    which >= 1L && which <= length(data) && which == round(which)) ||
    (is.character(which) && length(which) == 1L && which %in% names(data))

  if (!valid_which) {
    available <- names(data)
    if (is.null(available)) {
      available <- as.character(seq_along(data))
    }
    abort_forest(
      sprintf(
        "`which` must identify one element of the `emm_list`: %s.",
        paste0("`", available, "`", collapse = ", ")
      ),
      class = "ggforestplotR_invalid_data"
    )
  }

  as_forest_data(data[[which]], ...)
}

emmeans_conf_level <- function(x) {
  messages <- attr(x, "mesg", exact = TRUE)
  if (!is.character(messages)) {
    return(NA_real_)
  }

  matched <- regmatches(
    messages,
    regexpr("(?<=Confidence level used: )[0-9.]+", messages, perl = TRUE)
  )
  if (length(matched) == 0L) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(matched[[1L]]))
  if (is.na(value) || value <= 0 || value >= 1) NA_real_ else value
}

emmeans_scale <- function(table, estimate_name, link_name = NULL) {
  link_name <- if (is.character(link_name) && length(link_name) >= 1L) {
    link_name[[1L]]
  } else {
    NA_character_
  }
  log_link <- link_name %in% c("log", "logit", "log odds ratio")
  odds_link <- link_name %in% c("logit", "log odds ratio")

  switch(
    estimate_name,
    estimate = if (log_link) {
      result_scale("log", if (odds_link) "log(OR)" else "log(Ratio)")
    } else {
      result_scale("identity", "Estimate")
    },
    odds.ratio = result_scale("ratio", "OR"),
    ratio = result_scale("ratio", "Ratio"),
    prob = {
      values <- c(table$estimate, table$conf.low, table$conf.high)
      values <- values[is.finite(values)]
      if (length(values) > 0L && all(values >= 0 & values <= 1)) {
        result_scale("probability", "Probability", reference_value = NULL)
      } else {
        result_scale("identity", "Probability", reference_value = NULL)
      }
    },
    emmean = result_scale(
      "identity",
      if (is.na(link_name)) "EMM" else sprintf("EMM (%s scale)", link_name),
      reference_value = NULL
    ),
    response = result_scale("identity", "Response", reference_value = NULL),
    rate = result_scale("identity", "Rate", reference_value = NULL),
    result_scale("identity", estimate_name, reference_value = NULL)
  )
}

emmeans_as_forest_data <- function(x,
                                   label = NULL,
                                   group = NULL,
                                   facet = NULL,
                                   subgroup = NULL,
                                   term_labels = NULL,
                                   exponentiate = NULL,
                                   estimate_scale = NULL,
                                   effect_label = NULL,
                                   reference_value = NULL,
                                   reference_missing = TRUE,
                                   conf.level = NULL,
                                   sort_terms = "none",
                                   p_method = "level",
                                   source_model = NULL,
                                   call = rlang::caller_env()) {
  estimate_name <- attr(x, "estName", exact = TRUE)
  interval_names <- attr(x, "clNames", exact = TRUE)
  table <- as.data.frame(x, stringsAsFactors = FALSE)
  attributes(table) <- attributes(table)[c("names", "row.names")]
  class(table) <- "data.frame"

  if (!is.character(estimate_name) || length(estimate_name) != 1L ||
    !estimate_name %in% names(table)) {
    abort_forest(
      "The `emmeans` summary does not identify its estimate column.",
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }
  if (!is.character(interval_names) || length(interval_names) != 2L ||
    !all(interval_names %in% names(table))) {
    abort_forest(
      paste0(
        "The `emmeans` summary has no confidence intervals. Use `confint()` ",
        "or `summary(infer = TRUE)` instead of `test()`."
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  reserved <- c("estimate", "conf.low", "conf.high", "p.value")
  keep <- setdiff(names(table), c(estimate_name, interval_names, "p.value"))
  clashing <- intersect(keep, reserved)
  if (length(clashing) > 0L) {
    names(table)[match(clashing, names(table))] <- paste0(clashing, ".emm")
  }
  names(table)[match(estimate_name, names(table))] <- "estimate"
  names(table)[match(interval_names, names(table))] <- c("conf.low", "conf.high")

  primary <- attr(x, "pri.vars", exact = TRUE)
  by <- attr(x, "by.vars", exact = TRUE)
  id_columns <- intersect(c(primary, by), names(table))
  if (length(id_columns) == 0L) {
    id_columns <- setdiff(
      names(table),
      c(reserved, "SE", "df", "t.ratio", "z.ratio", "null")
    )
  }

  conf.level <- resolve_result_conf_level(
    conf.level,
    stored = emmeans_conf_level(x),
    source = "`emmeans`",
    call = call
  )

  result_table_as_forest_data(
    table,
    id_columns = id_columns,
    bare_columns = "contrast",
    scale = emmeans_scale(table, estimate_name, attr(x, "linkname", exact = TRUE)),
    source = "`emmeans`",
    label = label,
    group = group,
    facet = facet,
    subgroup = subgroup,
    term_labels = term_labels,
    exponentiate = exponentiate,
    estimate_scale = estimate_scale,
    effect_label = effect_label,
    reference_value = reference_value,
    reference_missing = reference_missing,
    conf.level = conf.level,
    sort_terms = sort_terms,
    p_method = p_method,
    source_model = source_model,
    source_package = "emmeans",
    call = call
  )
}
