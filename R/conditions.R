abort_forest <- function(..., class, call = rlang::caller_env(), parent = NULL) {
  # Interpolate the message as a value so literal template braces stay intact.
  message <- paste0(...)
  cli::cli_abort(
    "{message}",
    class = c(class, "ggforestplotR_error"),
    call = call,
    parent = parent
  )
}

# Argument deprecations

warn_deprecated_argument <- function(arg, replacement) {
  rlang::warn(
    sprintf("`%s` is deprecated; use %s instead.", arg, replacement),
    class = "ggforestplotR_deprecated_argument"
  )
}

warn_deprecated_table_headers <- function(supplied) {
  replacements <- c(
    term_header = "`column_labels = c(term = \"...\")`",
    n_header = "`column_labels = c(n = \"...\")`",
    events_header = "`column_labels = c(events = \"...\")`",
    p_header = "`column_labels = c(p = \"...\")`"
  )

  for (arg in names(supplied)[supplied]) {
    warn_deprecated_argument(arg, replacements[[arg]])
  }

  invisible(NULL)
}
