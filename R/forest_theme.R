#' Style a forest plot and its tables
#'
#' Apply a consistent plot theme and table typography before composing tables.
#' Explicit arguments to [add_forest_table()] or [add_split_table()] override
#' theme settings. Reference and row-header properties inherit body properties
#' when unspecified. Column headers inherit the body family and colour.
#'
#' @param base_size Base plot text size in points.
#' @param base_family Base font family for the plot and tables.
#' @param text_size Body text size in millimeters. Defaults to `base_size`
#'   converted to millimeters.
#' @param text_fontface Body font face: `"plain"`, `"bold"`, `"italic"`,
#'   or `"bold.italic"` (numeric codes 1 through 4 are also accepted).
#' @param text_family Body font family. Defaults to `base_family`.
#' @param text_colour Body text colour.
#' @param header_text_size Column-header size in points.
#' @param header_fontface Column-header font face.
#' @param header_family Column-header font family.
#' @param header_colour Column-header text colour.
#' @param reference_text_size,reference_fontface,reference_family,reference_colour
#'   Overrides for every table cell in reference rows. Size is in millimeters.
#' @param row_header_text_size,row_header_fontface,row_header_family,row_header_colour
#'   Overrides for section and generated subgroup header rows (not column
#'   headers). Size is in millimeters.
#' @param ... Additional named settings passed to [ggplot2::theme()].
#' @return A forest-theme add-on for `ggforestplot() + forest_theme()`.
#'   Apply it before table helpers and `add_favors()`. This is not a standalone
#'   ggplot2 theme and cannot be used with `theme_set()`.
#' @export
#' @examples
#' coefs <- data.frame(
#'   term = c("Age", "BMI"), estimate = c(0.3, -0.2),
#'   conf.low = c(0.1, -0.4), conf.high = c(0.5, 0)
#' )
#' ggforestplot(coefs) +
#'   forest_theme(base_family = "serif", reference_fontface = "italic") +
#'   add_forest_table(header_colour = "navy")
forest_theme <- function(base_size = 11, base_family = "",
                         text_size = base_size / (72.27 / 25.4),
                         text_fontface = "plain", text_family = base_family,
                         text_colour = "black", header_text_size = base_size,
                         header_fontface = "bold", header_family = NULL,
                         header_colour = NULL,
                         reference_text_size = NULL, reference_fontface = NULL,
                         reference_family = NULL, reference_colour = NULL,
                         row_header_text_size = NULL, row_header_fontface = NULL,
                         row_header_family = NULL, row_header_colour = NULL,
                         ...) {
  validate_forest_text_setting(base_size, "base_size")
  validate_forest_text_setting(base_family, "base_family")
  settings <- forest_text_settings(
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    text_fontface = text_fontface,
    text_family = text_family,
    text_colour = text_colour,
    header_colour = header_colour,
    reference_text_size = reference_text_size,
    reference_fontface = reference_fontface,
    reference_family = reference_family,
    reference_colour = reference_colour,
    row_header_text_size = row_header_text_size,
    row_header_fontface = row_header_fontface,
    row_header_family = row_header_family,
    row_header_colour = row_header_colour
  )
  structure(list(
    theme = ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        strip.placement = "outside") +
      ggplot2::theme(...),
    text = settings
  ), class = "ggforestplot_theme")
}

#' @export
#' @keywords internal
ggplot_add.ggforestplot_theme <- function(object, plot, ...) {
  if (inherits(plot, "patchwork") || is.null(plot$ggforestplotR_state)) {
    cli::cli_abort(
      "Add {.fn forest_theme} to a forest plot before composing tables or favors.",
      class = c("ggforestplotR_invalid_theme_target", "ggforestplotR_error")
    )
  }
  plot <- plot + object$theme
  plot$ggforestplotR_state$table_text_style <- object$text
  plot
}

# Shared table text styles

# Construct unresolved options explicitly; NULL means inherit at composition time.
forest_text_settings <- function(
  text_size = NULL,
  header_text_size = NULL,
  header_fontface = NULL,
  header_family = NULL,
  text_fontface = NULL,
  text_family = NULL,
  text_colour = NULL,
  header_colour = NULL,
  reference_text_size = NULL,
  reference_fontface = NULL,
  reference_family = NULL,
  reference_colour = NULL,
  row_header_text_size = NULL,
  row_header_fontface = NULL,
  row_header_family = NULL,
  row_header_colour = NULL,
  error_call = rlang::caller_env()) {
  settings <- list(
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    text_fontface = text_fontface,
    text_family = text_family,
    text_colour = text_colour,
    header_colour = header_colour,
    reference_text_size = reference_text_size,
    reference_fontface = reference_fontface,
    reference_family = reference_family,
    reference_colour = reference_colour,
    row_header_text_size = row_header_text_size,
    row_header_fontface = row_header_fontface,
    row_header_family = row_header_family,
    row_header_colour = row_header_colour
  )
  validate_forest_text_settings(settings, error_call = error_call)
  settings
}

validate_forest_text_setting <- function(value, name,
                                         error_call = rlang::caller_env()) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  scalar <- is.atomic(value) && length(value) == 1L && !anyNA(value)
  if (grepl("size$", name)) {
    valid <- scalar && is.numeric(value) && is.finite(value) && value > 0
    requirement <- "a positive finite number"
  } else if (grepl("fontface$", name)) {
    valid <- scalar && (is.character(value) || is.numeric(value)) &&
      value %in% c("plain", "bold", "italic", "bold.italic", 1:4)
    requirement <- 'one of "plain", "bold", "italic", "bold.italic", or 1:4'
  } else {
    valid <- scalar && is.character(value)
    requirement <- "a single non-missing string"
    if (valid && grepl("colour$", name)) {
      valid <- tryCatch({
        grDevices::col2rgb(value)
        TRUE
      }, error = function(cnd) FALSE)
      requirement <- "a valid colour string"
    }
  }
  if (!valid) {
    cli::cli_abort(
      "{.arg {name}} must be {requirement}.",
      class = c("ggforestplotR_invalid_text_style", "ggforestplotR_error"),
      call = error_call
    )
  }
  invisible(value)
}

validate_forest_text_settings <- function(settings,
                                          error_call = rlang::caller_env()) {
  if (is.null(settings)) {
    return(invisible(settings))
  }
  valid_names <- names(formals(forest_text_settings))
  valid_names <- setdiff(valid_names, "error_call")
  if (!is.list(settings) || (length(settings) > 0L &&
    (is.null(names(settings)) || anyNA(names(settings)) ||
      any(!nzchar(names(settings))) || anyDuplicated(names(settings)) ||
      any(!names(settings) %in% valid_names)))) {
    cli::cli_abort(
      "Text settings must be a uniquely named list of supported style options.",
      class = c("ggforestplotR_invalid_text_style", "ggforestplotR_error"),
      call = error_call
    )
  }
  for (name in names(settings)) {
    validate_forest_text_setting(
      settings[[name]], name, error_call = error_call
    )
  }
  invisible(settings)
}

resolve_forest_text_style <- function(inherited = NULL, overrides = list()) {
  validate_forest_text_settings(inherited)
  validate_forest_text_settings(overrides)
  style <- list(
    text_size = 3.2, text_fontface = "plain",
    text_family = "", text_colour = "black",
    header_text_size = 11, header_fontface = "bold"
  )
  for (settings in list(inherited, overrides)) {
    for (name in names(settings)) {
      if (!is.null(settings[[name]])) {
        style[[name]] <- settings[[name]]
      }
    }
  }
  for (prefix in c("header", "reference", "row_header")) {
    for (property in c("text_size", "fontface", "family", "colour")) {
      name <- paste(prefix, property, sep = "_")
      if (is.null(style[[name]])) {
        body_name <- if (property == "text_size") {
          "text_size"
        } else {
          paste0("text_", property)
        }
        style[[name]] <- style[[body_name]]
      }
    }
  }
  # Normalize numeric faces so grid and ggplot use identical font metrics.
  for (name in grep("fontface$", names(style), value = TRUE)) {
    value <- as.character(style[[name]])
    if (value %in% as.character(1:4)) {
      style[[name]] <- c("plain", "bold", "italic", "bold.italic")[as.integer(value)]
    }
  }
  style
}

forest_cell_text_style <- function(data, style) {
  prefix <- rep("text", nrow(data))
  prefix[data$row_type == "reference"] <- "reference"
  prefix[data$row_type == "header"] <- "row_header"
  out <- data.frame(size = rep(style$text_size, nrow(data)),
    fontface = rep(style$text_fontface, nrow(data)),
    family = rep(style$text_family, nrow(data)),
    colour = rep(style$text_colour, nrow(data)))
  for (type in c("reference", "row_header")) {
    rows <- prefix == type
    for (property in names(out)) {
      suffix <- if (property == "size") "text_size" else property
      out[rows, property] <- style[[paste(type, suffix, sep = "_")]]
    }
  }
  out
}

measure_styled_table_cells <- function(data, style) {
  cells <- forest_cell_text_style(data, style)
  max(vapply(seq_len(nrow(data)), function(i) {
    measure_max_grob_width(data$text[i], cells$size[i] * (72.27 / 25.4),
      cells$fontface[i], cells$family[i])
  }, numeric(1)), 0)
}
