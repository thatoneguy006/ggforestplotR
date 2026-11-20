#' Convert gtsummary regression tables to forest data
#'
#' Standardizes `gtsummary` regression tables into `forest_data`, keeping the
#' row structure of the table. One method covers
#' [gtsummary::tbl_regression()], [gtsummary::tbl_uvregression()],
#' [gtsummary::tbl_merge()], [gtsummary::tbl_stack()], and other `gtsummary`
#' tables built from them, such as [gtsummary::tbl_strata()], because they all
#' share the `gtsummary` class and `table_body` layout.
#'
#' @details
#' Estimates, intervals, p-values, labels, and counts are read from the
#' table's `table_body`, so they match the printed table:
#'
#' * Categorical variables become subgroup blocks: the variable label is the
#'   header, levels are indented beneath it, and reference levels are shown as
#'   reference rows without plot geometry.
#' * Continuous variables, single-row (`show_single_row`) variables, and
#'   interaction terms with one row become standalone estimates.
#' * Merged tables ([gtsummary::tbl_merge()]) become grouped estimates that
#'   share rows. The spanning headers, such as `"**Crude**"`, become the
#'   `"Model"` group labels.
#' * Stacked tables with `group_header` and multinomial outcome groups become
#'   facets.
#' * Per-row observation and event counts (`n_obs`, `n_event`) are mapped to
#'   `n` and `events` when present.
#'
#' With the default `p_method = NULL`, p-values from
#' [gtsummary::add_global_p()] are shown on the subgroup header
#' (`"overall"`); otherwise each level keeps its own p-value (`"level"`).
#' In `"overall"` mode, categorical variables without a global p-value show
#' no p-value.
#'
#' The effect scale follows the table's `exponentiate` setting and
#' coefficient type: exponentiated tables are ratios on a log10 axis, while
#' unexponentiated logistic, Poisson, and Cox tables are on the log scale. The
#' effect label is the table's estimate column header, such as `"OR"` or
#' `"HR"`.
#'
#' @param data A `gtsummary` regression table, such as a `tbl_regression`,
#'   `tbl_uvregression`, `tbl_merge`, or `tbl_stack` object.
#' @param ... Not used; must be empty.
#' @param term_labels Optional named vector used to relabel variables (by
#'   variable name, for example `c(age = "Age (years)")`) or levels (by the
#'   displayed level label).
#' @param exponentiate `NULL` keeps the table's scale. `TRUE` converts
#'   log-scale estimates to ratios and `FALSE` converts ratios to the log
#'   scale.
#' @param conf.level `NULL` uses the confidence level used to build the
#'   table. A number must match it.
#' @param sort_terms Must be `"none"` because the table's row structure is
#'   preserved. Retained so `ggforestplot()` and [bind_forest_models()] can
#'   pass it consistently.
#' @param p_method `NULL` chooses `"overall"` when the table has global
#'   p-values and `"level"` otherwise. Supply `"overall"` or `"level"` to
#'   override.
#'
#' @return A `forest_data` object with `subgroup`, `row_type`, `n`, `events`,
#'   and `p.value` columns derived from the table. For merged tables, `group`
#'   identifies the source table.
#' @seealso [as_forest_data()], [ggforestplot()], [add_forest_table()]
#' @name as_forest_data_gtsummary
#'
#' @examples
#' if (requireNamespace("gtsummary", quietly = TRUE) &&
#'   requireNamespace("broom.helpers", quietly = TRUE)) {
#'   trial <- gtsummary::trial
#'   fit <- glm(response ~ age + grade + trt, data = trial, family = binomial())
#'   tbl <- gtsummary::tbl_regression(fit, exponentiate = TRUE)
#'
#'   ggforestplot(tbl) +
#'     add_forest_table(columns = c("term", "n", "estimate", "ci", "p"))
#'
#'   # Crude and adjusted estimates side by side
#'   crude <- gtsummary::tbl_uvregression(
#'     trial[c("response", "age", "grade", "trt")],
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   )
#'   merged <- gtsummary::tbl_merge(
#'     list(crude, tbl),
#'     tab_spanner = c("**Crude**", "**Adjusted**")
#'   )
#'   ggforestplot(merged)
#' }
NULL

#' @rdname as_forest_data_gtsummary
#' @export
as_forest_data.gtsummary <- function(data,
                                     ...,
                                     term_labels = NULL,
                                     exponentiate = NULL,
                                     conf.level = NULL,
                                     sort_terms = c("none", "descending", "ascending"),
                                     p_method = NULL) {
  rlang::check_dots_empty()
  sort_terms <- match.arg(sort_terms)
  if (!identical(sort_terms, "none")) {
    abort_forest(
      "`sort_terms` must be \"none\" for `gtsummary` tables so the table's row order is preserved.",
      class = "ggforestplotR_invalid_data"
    )
  }
  if (!is.null(p_method)) {
    p_method <- match.arg(p_method, c("overall", "level"))
  }

  table_body <- data$table_body
  if (!is.data.frame(table_body)) {
    abort_forest(
      "The `gtsummary` object has no `table_body`.",
      class = "ggforestplotR_invalid_data"
    )
  }
  table_body <- as.data.frame(table_body, stringsAsFactors = FALSE)

  suffixes <- gtsummary_estimate_suffixes(table_body)
  if (length(suffixes) == 0L) {
    abort_forest(
      paste0(
        "The `gtsummary` table has no `estimate` column. `as_forest_data()` ",
        "supports regression tables such as `tbl_regression()`, ",
        "`tbl_uvregression()`, and merged or stacked versions of them."
      ),
      class = "ggforestplotR_invalid_data"
    )
  }
  for (suffix in suffixes) {
    check_result_columns(
      table_body,
      paste0(c("conf.low", "conf.high"), suffix),
      "`gtsummary`",
      hint = "Build the table with `conf.int = TRUE`."
    )
  }

  sets <- gtsummary_estimate_sets(data, table_body, suffixes)
  conf_levels <- unique(stats::na.omit(vapply(sets, `[[`, numeric(1), "conf_level")))
  if (length(conf_levels) > 1L) {
    abort_forest(
      "All merged `gtsummary` tables must use the same confidence level.",
      class = "ggforestplotR_invalid_data"
    )
  }
  conf.level <- resolve_result_conf_level(
    conf.level,
    stored = if (length(conf_levels) == 0L) NA_real_ else conf_levels[[1L]],
    source = "`gtsummary`"
  )

  scales <- lapply(sets, `[[`, "scale")
  scale_names <- unique(vapply(scales, `[[`, character(1), "estimate_scale"))
  if (length(scale_names) > 1L) {
    abort_forest(
      paste0(
        "All merged `gtsummary` tables must report estimates on the same ",
        "scale. Use the same `exponentiate` setting when building them."
      ),
      class = "ggforestplotR_invalid_data"
    )
  }
  labels <- unique(vapply(scales, `[[`, character(1), "effect_label"))
  scale <- scales[[1L]]
  if (length(labels) > 1L) {
    scale$effect_label <- if (identical(scale$estimate_scale, "ratio")) {
      "Ratio"
    } else {
      default_effect_label(scale$estimate_scale)
    }
  }

  long <- gtsummary_long_rows(table_body, sets, term_labels)
  has_global_p <- isTRUE(attr(long, "has_global_p"))
  if (is.null(p_method)) {
    p_method <- if (has_global_p) "overall" else "level"
  }
  if (identical(p_method, "overall")) {
    in_block <- !is.na(long$subgroup)
    long$p.value[in_block] <- long$global_p[in_block]
  }
  long$global_p <- NULL

  converted <- apply_result_exponentiate(long, scale, exponentiate)
  long <- converted$table
  scale <- converted$scale

  has_group <- length(sets) > 1L
  has_facet <- "facet" %in% names(long) && any(!is.na(long$facet))
  has_n <- any(!is.na(long$n))
  has_events <- any(!is.na(long$events))
  if (!has_n) long$n <- NULL
  if (!has_events) long$events <- NULL
  if (!has_group) long$Model <- NULL
  if (!has_facet) long$facet <- NULL

  as_forest_data.data.frame(
    long,
    term = "term",
    estimate = "estimate",
    conf.low = "conf.low",
    conf.high = "conf.high",
    label = "label",
    row_type = "row_type",
    group = if (has_group) "Model" else NULL,
    facet = if (has_facet) "facet" else NULL,
    subgroup = "subgroup",
    n = if (has_n) "n" else NULL,
    events = if (has_events) "events" else NULL,
    p.value = "p.value",
    estimate_scale = scale$estimate_scale,
    effect_label = scale$effect_label,
    reference_value = scale$reference_value,
    conf.level = conf.level,
    source_model = class(data),
    source_package = "gtsummary",
    sort_terms = "none",
    p_method = p_method
  )
}

gtsummary_estimate_suffixes <- function(table_body) {
  columns <- grep("^estimate(_[0-9]+)?$", names(table_body), value = TRUE)
  suffixes <- sub("^estimate", "", columns)
  index <- suppressWarnings(as.integer(sub("^_", "", suffixes)))
  index[!nzchar(suffixes)] <- 0L
  suffixes[order(index)]
}

gtsummary_column <- function(table_body, name, suffix, default = NA) {
  suffixed <- paste0(name, suffix)
  if (suffixed %in% names(table_body)) {
    return(table_body[[suffixed]])
  }
  if (name %in% names(table_body)) {
    return(table_body[[name]])
  }
  rep(default, nrow(table_body))
}

gtsummary_header_label <- function(x, column) {
  header <- x$table_styling$header
  if (!is.data.frame(header) || !all(c("column", "label") %in% names(header))) {
    return(NA_character_)
  }
  label <- header$label[header$column == column]
  if (length(label) == 0L) NA_character_ else strip_markdown(utils::tail(label, 1L))
}

gtsummary_spanning_header <- function(x, column) {
  spanning <- x$table_styling$spanning_header
  if (is.data.frame(spanning) &&
    all(c("column", "spanning_header") %in% names(spanning))) {
    keep <- spanning$column == column
    if ("remove" %in% names(spanning)) {
      keep <- keep & !spanning$remove %in% TRUE
    }
    if ("level" %in% names(spanning)) {
      keep <- keep & spanning$level %in% 1L
    }
    values <- spanning$spanning_header[keep]
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) > 0L) {
      return(strip_markdown(utils::tail(values, 1L)))
    }
  }

  header <- x$table_styling$header
  if (is.data.frame(header) && "spanning_header" %in% names(header)) {
    values <- header$spanning_header[header$column == column]
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) > 0L) {
      return(strip_markdown(utils::tail(values, 1L)))
    }
  }

  NA_character_
}

# Find the inputs used to build a table. Merged and stacked tables keep their
# source tables in `tbls`.
gtsummary_inputs <- function(x) {
  if (is.list(x$inputs) && !is.null(x$inputs$exponentiate)) {
    return(list(x$inputs))
  }
  if (is.list(x$tbls)) {
    return(unlist(lapply(x$tbls, gtsummary_inputs), recursive = FALSE))
  }
  list()
}

gtsummary_input_value <- function(inputs, name) {
  values <- unique(unlist(lapply(inputs, function(input) {
    value <- input[[name]]
    if (is.logical(value) || is.numeric(value)) value[[1L]] else NULL
  })))
  if (length(values) == 1L) values else NULL
}

# Inputs for the `index`-th estimate set. Merged tables number their source
# tables; stacked tables hold merged or plain tables that share that numbering.
gtsummary_set_inputs <- function(x, index) {
  if (inherits(x, "tbl_merge") && is.list(x$tbls) && length(x$tbls) >= index) {
    return(gtsummary_inputs(x$tbls[[index]]))
  }
  if (is.list(x$tbls) && !(is.list(x$inputs) && !is.null(x$inputs$exponentiate))) {
    return(unlist(lapply(x$tbls, gtsummary_set_inputs, index = index), recursive = FALSE))
  }
  gtsummary_inputs(x)
}

gtsummary_estimate_sets <- function(x, table_body, suffixes) {
  merged <- !identical(suffixes, "")

  lapply(seq_along(suffixes), function(index) {
    suffix <- suffixes[[index]]
    inputs <- if (merged) {
      gtsummary_set_inputs(x, as.integer(sub("^_", "", suffix)))
    } else {
      gtsummary_inputs(x)
    }
    exponentiated <- gtsummary_input_value(inputs, "exponentiate")
    conf_level <- gtsummary_input_value(inputs, "conf.level")
    if (is.null(conf_level)) {
      header <- gtsummary_header_label(x, paste0("conf.low", suffix))
      percent <- suppressWarnings(as.numeric(sub("^([0-9.]+)%.*$", "\\1", header)))
      conf_level <- if (is.na(percent)) NA_real_ else percent / 100
    }

    coefficient_type <- unique(stats::na.omit(as.character(
      gtsummary_column(table_body, "coefficients_type", suffix)
    )))
    coefficient_label <- unique(stats::na.omit(as.character(
      gtsummary_column(table_body, "coefficients_label", suffix)
    )))
    header_label <- gtsummary_header_label(x, paste0("estimate", suffix))
    effect_label <- if (!is.na(header_label) && nzchar(header_label)) {
      header_label
    } else if (length(coefficient_label) == 1L) {
      coefficient_label
    } else {
      NA_character_
    }

    log_type <- any(coefficient_type %in% c(
      "logistic", "poisson", "relative_risk", "prop_hazard"
    )) || (length(coefficient_label) == 1L && grepl("^log\\(", coefficient_label))

    scale <- if (isTRUE(exponentiated)) {
      result_scale("ratio", if (is.na(effect_label)) "Ratio" else effect_label)
    } else if (log_type) {
      result_scale("log", if (is.na(effect_label)) "log(Ratio)" else effect_label)
    } else {
      result_scale("identity", if (is.na(effect_label)) "Beta" else effect_label)
    }

    group <- gtsummary_spanning_header(x, paste0("estimate", suffix))
    if (is.na(group)) {
      group <- if (merged) paste("Table", sub("^_", "", suffix)) else NA_character_
    }

    list(
      suffix = suffix,
      group = group,
      scale = scale,
      conf_level = if (is.null(conf_level)) NA_real_ else as.numeric(conf_level)
    )
  })
}

gtsummary_relabel <- function(labels, variables, term_labels) {
  if (is.null(term_labels)) {
    return(labels)
  }
  by_variable <- apply_term_labels(variables, labels, term_labels)
  changed <- by_variable != labels
  labels[changed] <- by_variable[changed]
  labels[!changed] <- apply_term_labels(labels[!changed], labels[!changed], term_labels)
  labels
}

gtsummary_long_rows <- function(table_body, sets, term_labels = NULL) {
  n_rows <- nrow(table_body)
  variable <- as.character(gtsummary_column(table_body, "variable", ""))
  row_type <- as.character(gtsummary_column(table_body, "row_type", "", "label"))
  label <- as.character(gtsummary_column(table_body, "label", "", NA_character_))
  var_label <- as.character(gtsummary_column(table_body, "var_label", "", NA_character_))
  var_label[is.na(var_label)] <- variable[is.na(var_label)]
  label[is.na(label)] <- var_label[is.na(label)]
  facet <- if ("groupname_col" %in% names(table_body)) {
    strip_markdown_vector(table_body$groupname_col)
  } else {
    rep(NA_character_, n_rows)
  }

  header_flags <- lapply(sets, function(set) {
    gtsummary_column(table_body, "header_row", set$suffix)
  })
  is_header <- Reduce(`|`, lapply(header_flags, function(flag) flag %in% TRUE))
  is_header <- is_header & row_type == "label"
  is_level <- row_type == "level"

  # A level belongs to the header row of its variable within the same facet.
  block_key <- paste(facet, variable, sep = "\r")
  header_block <- unique(block_key[is_header])
  in_block <- is_level & block_key %in% header_block
  variable_title <- gtsummary_relabel(var_label, variable, term_labels)
  row_label <- ifelse(
    is_level,
    gtsummary_relabel(label, rep("", n_rows), term_labels),
    gtsummary_relabel(label, variable, term_labels)
  )

  global_p <- lapply(sets, function(set) {
    p <- suppressWarnings(as.numeric(gtsummary_column(table_body, "p.value", set$suffix)))
    p_by_block <- stats::setNames(p[is_header], block_key[is_header])
    unname(p_by_block[block_key])
  })
  has_global_p <- any(vapply(global_p, function(p) any(!is.na(p)), logical(1)))

  pieces <- vector("list", n_rows * length(sets))
  index <- 0L
  multi <- length(sets) > 1L
  for (row in seq_len(n_rows)) {
    if (is_header[[row]] || !row_type[[row]] %in% c("label", "level")) {
      next
    }
    for (set_index in seq_along(sets)) {
      set <- sets[[set_index]]
      value <- function(name, default = NA) {
        gtsummary_column(table_body, name, set$suffix, default)[[row]]
      }
      estimate <- suppressWarnings(as.numeric(value("estimate")))
      conf_low <- suppressWarnings(as.numeric(value("conf.low")))
      conf_high <- suppressWarnings(as.numeric(value("conf.high")))
      reference <- isTRUE(as.logical(value("reference_row")))

      if (!reference && is.na(estimate) && is.na(conf_low) && is.na(conf_high)) {
        # Not estimated in this table, such as a term absent from one model.
        next
      }

      n <- value("n_obs")
      if (is.na(n)) n <- value("N_obs")
      events <- value("n_event")
      if (is.na(events)) events <- value("N_event")

      index <- index + 1L
      pieces[[index]] <- data.frame(
        term = paste(facet[[row]], variable[[row]], label[[row]], sep = "::"),
        label = row_label[[row]],
        estimate = if (reference) NA_real_ else estimate,
        conf.low = if (reference) NA_real_ else conf_low,
        conf.high = if (reference) NA_real_ else conf_high,
        row_type = if (reference) "reference" else "estimate",
        subgroup = if (in_block[[row]]) variable_title[[row]] else NA_character_,
        facet = facet[[row]],
        Model = if (multi) set$group else NA_character_,
        n = suppressWarnings(as.numeric(n)),
        events = suppressWarnings(as.numeric(events)),
        p.value = if (reference) NA_real_ else suppressWarnings(as.numeric(value("p.value"))),
        global_p = global_p[[set_index]][[row]],
        variable = variable[[row]],
        stringsAsFactors = FALSE
      )
    }
  }

  long <- do.call(rbind, pieces[seq_len(index)])
  if (is.null(long) || nrow(long) == 0L) {
    abort_forest(
      "The `gtsummary` table has no estimate rows to plot.",
      class = "ggforestplotR_invalid_data"
    )
  }

  geometry <- long$row_type == "estimate"
  long <- drop_incomplete_result_rows(
    long,
    "`gtsummary`",
    labels = long$label,
    geometry = geometry
  )

  if (multi) {
    group_levels <- unique(vapply(sets, `[[`, character(1), "group"))
    long$Model <- factor(long$Model, levels = group_levels)
  }

  # Keep blocks contiguous within each facet. Stacked tables that repeat a
  # variable without `group_header` cannot be shown as one block.
  facet_key <- ifelse(is.na(long$facet), "", long$facet)
  block <- ifelse(is.na(long$subgroup), NA_character_, paste(facet_key, long$subgroup, sep = "\r"))
  runs <- rle(ifelse(is.na(block), paste0("..row..", seq_along(block)), block))
  repeated <- unique(runs$values[duplicated(runs$values) & !startsWith(runs$values, "..row..")])
  if (length(repeated) > 0L) {
    abort_forest(
      paste0(
        "The `gtsummary` table repeats categorical variable(s) in separate ",
        "blocks: ",
        paste(unique(sub("^.*\r", "", repeated)), collapse = ", "),
        ". For stacked tables, supply `group_header` in `tbl_stack()` so ",
        "each table becomes a facet."
      ),
      class = "ggforestplotR_invalid_data"
    )
  }

  attr(long, "has_global_p") <- has_global_p
  rownames(long) <- NULL
  long
}

strip_markdown_vector <- function(x) {
  x <- as.character(x)
  out <- gsub("\\*\\*|__", "", x)
  out <- trimws(gsub("<[^>]+>", "", out))
  out[is.na(x)] <- NA_character_
  out
}
