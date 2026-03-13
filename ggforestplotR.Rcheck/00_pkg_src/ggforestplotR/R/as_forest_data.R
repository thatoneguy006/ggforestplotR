as_forest_data <- function(data,
                           term,
                           estimate,
                           conf.low,
                           conf.high,
                           label = term,
                           group = NULL,
                           grouping = NULL,
                           n = NULL,
                           p.value = NULL,
                           exponentiate = FALSE,
                           sort_terms = c("none", "descending", "ascending")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  sort_terms <- match.arg(sort_terms)

  cols <- list(
    term = resolve_column(data, term, "term"),
    estimate = resolve_column(data, estimate, "estimate"),
    conf.low = resolve_column(data, conf.low, "conf.low"),
    conf.high = resolve_column(data, conf.high, "conf.high"),
    label = resolve_column(data, label, "label", required = FALSE),
    group = resolve_column(data, group, "group", required = FALSE),
    grouping = resolve_column(data, grouping, "grouping", required = FALSE),
    n = resolve_column(data, n, "n", required = FALSE),
    p.value = resolve_column(data, p.value, "p.value", required = FALSE)
  )

  out <- data.frame(
    term = as.character(data[[cols$term]]),
    estimate = as.numeric(data[[cols$estimate]]),
    conf.low = as.numeric(data[[cols$conf.low]]),
    conf.high = as.numeric(data[[cols$conf.high]]),
    stringsAsFactors = FALSE
  )

  out$label <- if (is.null(cols$label)) {
    out$term
  } else {
    as.character(data[[cols$label]])
  }

  out$group <- if (is.null(cols$group)) {
    NA_character_
  } else {
    as.character(data[[cols$group]])
  }

  out$grouping <- if (is.null(cols$grouping)) {
    NA_character_
  } else {
    as.character(data[[cols$grouping]])
  }

  out$n <- if (is.null(cols$n)) {
    NA_character_
  } else {
    as.character(data[[cols$n]])
  }

  out$p.value <- if (is.null(cols$p.value)) {
    NA_real_
  } else {
    as.numeric(data[[cols$p.value]])
  }

  validate_forest_data(out, exponentiate = exponentiate)

  if (sort_terms == "descending") {
    if (any(!is.na(out$grouping))) {
      out <- out[order(out$grouping, out$estimate, decreasing = c(FALSE, TRUE)), , drop = FALSE]
    } else {
      out <- out[order(out$estimate, decreasing = TRUE), , drop = FALSE]
    }
  } else if (sort_terms == "ascending") {
    if (any(!is.na(out$grouping))) {
      out <- out[order(out$grouping, out$estimate, decreasing = c(FALSE, FALSE)), , drop = FALSE]
    } else {
      out <- out[order(out$estimate, decreasing = FALSE), , drop = FALSE]
    }
  }

  rownames(out) <- NULL

  out
}