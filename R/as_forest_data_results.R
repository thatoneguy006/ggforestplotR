# Shared helpers for post-estimation and summary-table inputs
#
# Objects from marginaleffects, emmeans, gtsummary, and parameters already
# contain estimates and confidence intervals. These helpers turn their
# result tables into `forest_data` through the data-frame method, so all
# validation, sorting, subgroup, and metadata rules stay in one place.

forest_result_classes <- function() {
  c(
    "comparisons", "predictions", "slopes", "hypotheses",
    "emmGrid", "summary_emm", "emm_list",
    "gtsummary",
    "parameters_model"
  )
}

is_forest_result <- function(x) {
  inherits(x, forest_result_classes())
}

check_result_package <- function(package, input) {
  if (!requireNamespace(package, quietly = TRUE)) {
    abort_forest(
      sprintf(
        "The `%s` package is required to convert %s objects.",
        package,
        input
      ),
      class = "ggforestplotR_invalid_model",
      call = rlang::caller_env()
    )
  }

  invisible(TRUE)
}

check_optional_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!is.null(x) && (!is.logical(x) || length(x) != 1L || is.na(x))) {
    abort_forest(
      sprintf("`%s` must be `NULL`, `TRUE`, or `FALSE`.", arg),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  invisible(x)
}

# Scales -----------------------------------------------------------------------

result_scale <- function(estimate_scale,
                         effect_label,
                         reference_value = default_reference_value(estimate_scale)) {
  list(
    estimate_scale = estimate_scale,
    effect_label = effect_label,
    reference_value = reference_value
  )
}

unlog_effect_label <- function(label) {
  if (grepl("^log\\(.+\\)$", label)) {
    sub("^log\\((.+)\\)$", "\\1", label)
  } else {
    "Ratio"
  }
}

log_effect_label <- function(label) {
  if (grepl("^log\\(.+\\)$", label)) label else sprintf("log(%s)", label)
}

# `exponentiate = TRUE` converts log-scale results to ratios and
# `exponentiate = FALSE` converts ratios back to the log scale. `NULL` keeps
# the scale reported by the source object. Rows without geometry keep `NA`.
apply_result_exponentiate <- function(table,
                                      scale,
                                      exponentiate,
                                      call = rlang::caller_env()) {
  check_optional_flag(exponentiate, "exponentiate", call = call)
  columns <- c("estimate", "conf.low", "conf.high")

  if (is.null(exponentiate)) {
    return(list(table = table, scale = scale))
  }

  if (isTRUE(exponentiate)) {
    if (identical(scale$estimate_scale, "ratio")) {
      return(list(table = table, scale = scale))
    }
    if (!identical(scale$estimate_scale, "log")) {
      abort_forest(
        sprintf(
          paste0(
            "`exponentiate = TRUE` requires log-scale estimates, but these ",
            "estimates are on the \"%s\" scale. Request ratios from the ",
            "source package or set `estimate_scale` if the scale was not ",
            "detected correctly."
          ),
          scale$estimate_scale
        ),
        class = "ggforestplotR_invalid_data",
        call = call
      )
    }

    table[columns] <- lapply(table[columns], exp)
    scale <- result_scale("ratio", unlog_effect_label(scale$effect_label))
    return(list(table = table, scale = scale))
  }

  if (identical(scale$estimate_scale, "ratio")) {
    values <- unlist(table[columns], use.names = FALSE)
    if (any(!is.na(values) & values <= 0)) {
      abort_forest(
        "`exponentiate = FALSE` cannot log-transform non-positive ratios.",
        class = "ggforestplotR_invalid_data",
        call = call
      )
    }
    table[columns] <- lapply(table[columns], log)
    scale <- result_scale("log", log_effect_label(scale$effect_label))
  }

  list(table = table, scale = scale)
}

# User overrides are applied after scale detection. A new `estimate_scale`
# resets the default label and reference value unless those are also supplied.
override_result_scale <- function(scale,
                                  estimate_scale = NULL,
                                  call = rlang::caller_env()) {
  if (is.null(estimate_scale)) {
    return(scale)
  }

  if (!is.character(estimate_scale) || length(estimate_scale) != 1L ||
    is.na(estimate_scale) || !estimate_scale %in% forest_estimate_scales()) {
    abort_forest(
      sprintf(
        "`estimate_scale` must be one of: %s.",
        paste(sprintf('"%s"', forest_estimate_scales()), collapse = ", ")
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  result_scale(estimate_scale, default_effect_label(estimate_scale))
}

resolve_result_conf_level <- function(conf.level,
                                      stored = NA_real_,
                                      source,
                                      call = rlang::caller_env()) {
  stored <- if (is.null(stored) || length(stored) != 1L ||
    !is.numeric(stored) || is.na(stored)) {
    NA_real_
  } else {
    as.numeric(stored)
  }

  if (is.null(conf.level)) {
    return(stored)
  }

  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
    is.na(conf.level) || conf.level <= 0 || conf.level >= 1) {
    abort_forest(
      "`conf.level` must be `NULL` or a single number strictly between 0 and 1.",
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  if (!is.na(stored) && !isTRUE(all.equal(conf.level, stored))) {
    abort_forest(
      sprintf(
        paste0(
          "`conf.level = %s` does not match the %s confidence level stored ",
          "in the %s object. Recompute the intervals in %s instead."
        ),
        format(conf.level),
        format(stored),
        source,
        source
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  conf.level
}

# Row labels -------------------------------------------------------------------

result_text_values <- function(values) {
  values <- if (is.factor(values)) as.character(values) else values
  values <- if (is.numeric(values)) {
    format(values, trim = TRUE, digits = 4)
  } else {
    as.character(values)
  }
  values[is.na(values)] <- ""
  values
}

# Compose readable row labels from the identifier columns that vary. A single
# varying column contributes bare values. With several, "bare" columns such as
# `term` and `contrast` are joined by ": " and other columns are shown as
# `name = value`.
compose_result_labels <- function(table,
                                  id_columns,
                                  bare_columns = character(),
                                  term_labels = NULL,
                                  fallback = "Estimate") {
  id_columns <- unique(intersect(id_columns, names(table)))
  n_rows <- nrow(table)

  if (length(id_columns) == 0L) {
    return(rep(fallback, n_rows))
  }

  values <- lapply(id_columns, function(column) {
    text <- result_text_values(table[[column]])
    apply_term_labels(text, text, term_labels)
  })
  names(values) <- id_columns

  informative <- id_columns[vapply(values, function(value) {
    any(nzchar(value))
  }, logical(1))]
  varying <- informative[vapply(values[informative], function(value) {
    length(unique(value)) > 1L
  }, logical(1))]
  if (length(varying) == 0L) {
    varying <- utils::head(informative, 1L)
  }
  if (length(varying) == 0L) {
    return(rep(fallback, n_rows))
  }

  if (length(varying) == 1L) {
    labels <- values[[varying]]
  } else {
    bare <- intersect(varying, bare_columns)
    named <- setdiff(varying, bare_columns)

    labels <- vapply(seq_len(n_rows), function(row) {
      bare_values <- vapply(bare, function(column) {
        values[[column]][[row]]
      }, character(1))
      bare_values <- bare_values[nzchar(bare_values)]
      named_values <- vapply(named, function(column) {
        value <- values[[column]][[row]]
        if (nzchar(value)) paste0(column, " = ", value) else ""
      }, character(1))
      named_values <- named_values[nzchar(named_values)]
      parts <- c(
        if (length(bare_values) > 0L) paste(bare_values, collapse = ": "),
        if (length(named_values) > 0L) paste(named_values, collapse = ", ")
      )
      paste(parts, collapse = ", ")
    }, character(1))
  }

  labels[!nzchar(labels)] <- fallback
  apply_term_labels(labels, labels, term_labels)
}

# Rows that share a label within the same group and facet would be drawn on top
# of each other. Add a row identifier so every displayed row stays distinct.
deduplicate_result_labels <- function(labels, keys = NULL, row_ids = NULL) {
  key <- if (is.null(keys)) labels else paste(keys, labels, sep = "\r")
  duplicated_key <- key %in% key[duplicated(key)]

  if (!any(duplicated_key)) {
    return(labels)
  }

  if (is.null(row_ids)) {
    row_ids <- seq_along(labels)
  }
  labels[duplicated_key] <- paste0(
    labels[duplicated_key],
    " [",
    result_text_values(row_ids[duplicated_key]),
    "]"
  )
  labels
}

# Keep the first appearance of each subgroup and gather its rows into one
# contiguous block. Standalone rows keep their own position.
contiguous_subgroup_order <- function(subgroup, facet = NULL) {
  subgroup <- as.character(subgroup)
  present <- !is.na(subgroup) & nzchar(subgroup)
  facet <- if (is.null(facet)) rep("", length(subgroup)) else as.character(facet)
  key <- ifelse(
    present,
    paste(facet, subgroup, sep = "\r"),
    paste0("\r..row..", seq_along(subgroup))
  )
  order(match(key, unique(key)), seq_along(key))
}

drop_incomplete_result_rows <- function(table,
                                        source,
                                        labels = NULL,
                                        geometry = NULL,
                                        call = rlang::caller_env()) {
  if (is.null(geometry)) {
    geometry <- rep(TRUE, nrow(table))
  }
  complete <- is.finite(table$estimate) & is.finite(table$conf.low) &
    is.finite(table$conf.high)
  drop <- geometry & !complete

  if (any(drop)) {
    affected <- if (is.null(labels)) as.character(which(drop)) else labels[drop]
    affected <- unique(as.character(affected))
    cli::cli_warn(
      c(
        "Dropped {sum(drop)} row{?s} from the {source} object without a finite estimate and confidence interval.",
        "i" = "Affected row{?s}: {.val {affected}}."
      ),
      class = "ggforestplotR_dropped_rows",
      call = call
    )
    table <- table[!drop, , drop = FALSE]
    geometry <- geometry[!drop]
  }

  if (nrow(table) == 0L || !any(geometry)) {
    abort_forest(
      sprintf(
        "The %s object has no rows with finite estimates and confidence intervals.",
        source
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  rownames(table) <- NULL
  table
}

check_result_columns <- function(table, columns, source, hint = NULL,
                                 call = rlang::caller_env()) {
  missing <- setdiff(columns, names(table))

  if (length(missing) > 0L) {
    abort_forest(
      sprintf(
        "The %s object is missing required column(s): %s.%s",
        source,
        paste0("`", missing, "`", collapse = ", "),
        if (is.null(hint)) "" else paste0(" ", hint)
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  invisible(table)
}

check_result_mapping <- function(table, column, arg, reserved = character(),
                                 call = rlang::caller_env()) {
  if (is.null(column)) {
    return(NULL)
  }

  if (!is.character(column) || length(column) != 1L || is.na(column)) {
    abort_forest(
      sprintf("`%s` must be `NULL` or a single column name (string).", arg),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  if (!column %in% names(table) || column %in% reserved) {
    available <- setdiff(names(table), reserved)
    abort_forest(
      sprintf(
        "Column `%s` was not found. Available identifier columns: %s.",
        column,
        if (length(available) == 0L) "none" else paste0("`", available, "`", collapse = ", ")
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  column
}

# Build forest data from a result table whose estimate, interval, and optional
# p-value columns have already been renamed to the canonical names.
result_table_as_forest_data <- function(table,
                                        id_columns,
                                        bare_columns = character(),
                                        scale,
                                        source,
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
                                        conf.level = NA_real_,
                                        sort_terms = c("none", "descending", "ascending"),
                                        p_method = c("level", "overall"),
                                        row_id = NULL,
                                        source_model = NULL,
                                        source_package = NULL,
                                        ratio_hint = NULL,
                                        call = rlang::caller_env()) {
  sort_terms <- match.arg(sort_terms)
  p_method <- match.arg(p_method)
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  attributes(table) <- attributes(table)[c("names", "row.names", "class")]
  class(table) <- "data.frame"

  stat_columns <- c("estimate", "conf.low", "conf.high", "p.value")
  label <- check_result_mapping(table, label, "label", stat_columns, call = call)
  group <- check_result_mapping(table, group, "group", stat_columns, call = call)
  facet <- check_result_mapping(table, facet, "facet", stat_columns, call = call)
  subgroup <- check_result_mapping(table, subgroup, "subgroup", stat_columns, call = call)

  mapped <- c(group, facet, subgroup)
  if (anyDuplicated(mapped)) {
    abort_forest(
      "`group`, `facet`, and `subgroup` must map different columns.",
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  for (column in c(group, facet, subgroup)) {
    if (!is.factor(table[[column]])) {
      values <- table[[column]]
      table[[column]] <- factor(
        result_text_values(values),
        levels = unique(result_text_values(values))
      )
    }
    table[[column]] <- apply_factor_labels(table[[column]], term_labels)
  }

  labels <- if (is.null(label)) {
    compose_result_labels(
      table,
      id_columns = setdiff(id_columns, mapped),
      bare_columns = bare_columns,
      term_labels = term_labels
    )
  } else {
    text <- result_text_values(table[[label]])
    apply_term_labels(text, text, term_labels)
  }

  row_keys <- if (length(mapped) == 0L) {
    NULL
  } else {
    do.call(
      paste,
      c(lapply(mapped, function(column) as.character(table[[column]])), list(sep = "\r"))
    )
  }
  labels <- deduplicate_result_labels(
    labels,
    keys = row_keys,
    row_ids = if (!is.null(row_id) && row_id %in% names(table)) table[[row_id]] else NULL
  )

  label_column <- "label"
  while (label_column %in% names(table)) {
    label_column <- paste0(".", label_column)
  }
  table[[label_column]] <- labels
  table <- drop_incomplete_result_rows(
    table,
    source,
    labels = labels,
    call = call
  )

  if (!is.null(subgroup) && identical(sort_terms, "none")) {
    row_order <- contiguous_subgroup_order(
      table[[subgroup]],
      if (is.null(facet)) NULL else table[[facet]]
    )
    table <- table[row_order, , drop = FALSE]
  }

  scale <- override_result_scale(scale, estimate_scale, call = call)
  converted <- apply_result_exponentiate(table, scale, exponentiate, call = call)
  table <- converted$table
  scale <- converted$scale
  if (!is.null(effect_label)) {
    scale$effect_label <- effect_label
  }
  if (!isTRUE(reference_missing)) {
    scale$reference_value <- reference_value
  }
  check_positive_ratios(table, scale, ratio_hint, call = call)

  as_forest_data.data.frame(
    table,
    term = label_column,
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high",
    label = label_column,
    group = group,
    facet = facet,
    subgroup = subgroup,
    p.value = if ("p.value" %in% names(table)) "p.value" else NULL,
    estimate_scale = scale$estimate_scale,
    effect_label = scale$effect_label,
    reference_value = scale$reference_value,
    conf.level = conf.level,
    source_model = source_model,
    source_package = source_package,
    sort_terms = sort_terms,
    p_method = p_method
  )
}

# Wald intervals computed on the ratio scale can cross zero, which a log axis
# cannot show. Point to a log-scale alternative instead of a generic error.
check_positive_ratios <- function(table, scale, hint = NULL,
                                  call = rlang::caller_env()) {
  if (!identical(scale$estimate_scale, "ratio")) {
    return(invisible(table))
  }

  values <- unlist(table[c("estimate", "conf.low", "conf.high")], use.names = FALSE)
  if (any(!is.na(values) & values <= 0)) {
    abort_forest(
      paste0(
        "Ratio estimates and intervals must be strictly positive for a log ",
        "axis, but some interval bounds are zero or negative.",
        if (is.null(hint)) "" else paste0(" ", hint)
      ),
      class = "ggforestplotR_invalid_data",
      call = call
    )
  }

  invisible(table)
}

apply_factor_labels <- function(values, term_labels = NULL) {
  if (is.null(term_labels) || !is.factor(values)) {
    return(values)
  }

  old_levels <- levels(values)
  new_levels <- apply_term_labels(old_levels, old_levels, term_labels)
  if (anyDuplicated(new_levels)) {
    return(values)
  }
  levels(values) <- new_levels
  values
}

strip_markdown <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }
  x <- as.character(x[[1L]])
  x <- gsub("\\*\\*|__", "", x)
  x <- gsub("<[^>]+>", "", x)
  trimws(x)
}
