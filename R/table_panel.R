# --- Grid line data ----------------------------------------------------------

build_table_line_data <- function(stripe_data, has_groupings = FALSE) {
  if (isTRUE(has_groupings)) {
    line_parts <- purrr::map(
      split(stripe_data, stripe_data$grouping_panel, drop = TRUE),
      function(panel_data) {
        boundary_values <- unique(c(
          min(panel_data$ymin),
          panel_data$ymin[panel_data$stripe_id > 1L],
          max(panel_data$ymax)
        ))
        data.frame(
          grouping_panel = panel_data$grouping_panel[1L],
          yintercept = boundary_values,
          stringsAsFactors = FALSE
        )
      }
    )
    purrr::list_rbind(line_parts)
  } else {
    data.frame(
      yintercept = unique(c(
        min(stripe_data$ymin),
        stripe_data$ymin[stripe_data$stripe_id > 1L],
        max(stripe_data$ymax)
      )),
      stringsAsFactors = FALSE
    )
  }
}

# --- Text measurement --------------------------------------------------------

with_measurement_device <- function(code) {
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(NULL)
    device <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(device), add = TRUE)
  }
  force(code)
}

#' Ground-truth width measurement via grid graphics.  Handles multi-line
#' strings by splitting on newlines and returning the widest line.
#' @keywords internal
#' @noRd
measure_max_grob_width <- function(text,
                                   fontsize_pt,
                                   fontface = "plain",
                                   fontfamily = "") {
  text <- as.character(text)
  text[is.na(text)] <- ""

  if (length(text) == 0L) return(0)

  widths <- vapply(text, function(value) {
    lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
    if (length(lines) == 0L) return(0)

    max(vapply(lines, function(line) {
      if (!nzchar(line)) return(0)
      grid::convertWidth(
        grid::grobWidth(
          grid::textGrob(
            line,
            gp = grid::gpar(
              fontsize = fontsize_pt,
              fontface = fontface,
              fontfamily = fontfamily
            )
          )
        ),
        "inches",
        valueOnly = TRUE
      )
    }, numeric(1)), 0)
  }, numeric(1))

  max(widths, 0)
}

#' Measure the displayed text width (in inches) for each column, taking the
#' max of the header and all cell values.
#' @keywords internal
#' @noRd
measure_table_text_widths <- function(table_spec,
                                      text_size = 3.2,
                                      header_text_size = 11,
                                      header_fontface = "bold",
                                      header_family = "") {
  with_measurement_device({
    text_size_pt <- text_size * (72.27 / 25.4)

    stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
      key <- table_spec$column_keys[[i]]
      values <- table_spec$table_data$text[table_spec$table_data$column_key == key]
      max(
        measure_max_grob_width(
          table_spec$headers[[i]],
          fontsize_pt = header_text_size,
          fontface = header_fontface,
          fontfamily = header_family
        ),
        if (is.null(table_spec$text_style)) {
          measure_max_grob_width(values, fontsize_pt = text_size_pt, fontface = "plain")
        } else {
          measure_styled_table_cells(
            table_spec$table_data[table_spec$table_data$column_key == key, , drop = FALSE],
            table_spec$text_style
          )
        }
      )
    }, numeric(1)), table_spec$column_keys)
  })
}

# --- Column width estimation -------------------------------------------------

#' Default per-column base padding.  Uses a known lookup for the four standard
#' keys and a sensible fallback for anything else.
#' @keywords internal
#' @noRd
column_base_padding <- function(column_key) {
  known <- c(term = 0.16, n = 0.10, events = 0.12, estimate = 0.18, ci = 0.14, p = 0.12)
  pad <- unname(known[column_key])
  if (is.na(pad)) 0.14 else pad
}

#' Total column width = measured text width + base padding + alignment padding.
#' @keywords internal
#' @noRd
estimate_split_column_widths <- function(table_spec,
                                         text_size = 3.2,
                                         header_text_size = 11,
                                         header_fontface = "bold",
                                         header_family = "",
                                         alignment = c("left", "center", "right")) {
  alignment <- match.arg(alignment)
  text_widths <- measure_table_text_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )
  alignment_padding <- switch(alignment, left = 0.06, right = 0.06, center = 0.05)

  stats::setNames(vapply(seq_along(table_spec$column_keys), function(i) {
    key <- table_spec$column_keys[[i]]
    unname(text_widths[[key]]) + column_base_padding(key) + alignment_padding
  }, numeric(1)), table_spec$column_keys)
}

# --- Layout engines ----------------------------------------------------------

#' Lay out a table spec for a split forest plot.
#'
#' Computes column positions and a content width.  Does NOT try to account for
#' which side of the plot the table will sit on — that's handled at assembly
#' time by equalising the two table widths.
#' @keywords internal
#' @noRd
layout_split_table_spec <- function(table_spec,
                                    text_size = 3.2,
                                    header_text_size = 11,
                                    header_fontface = "bold",
                                    header_family = "",
                                    alignment = c("left", "right")) {
  alignment <- match.arg(alignment)
  column_widths <- estimate_split_column_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    alignment = alignment
  )
  text_widths <- measure_table_text_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )
  gap <- 0.2

  positions <- if (alignment == "left") {
    starts <- c(0, utils::head(cumsum(column_widths + gap), -1))
    starts + 0.02
  } else {
    cumsum(column_widths + c(rep(gap, length(column_widths) - 1L), 0))
  }

  table_spec$table_data$column_position <- unname(
    positions[match(table_spec$table_data$column_key, table_spec$column_keys)]
  )
  table_spec$alignment <- alignment
  table_spec$positions <- unname(positions)
  table_spec$header_positions <- unname(positions)
  table_spec$estimated_column_widths <- unname(column_widths)
  table_spec$displayed_column_widths <- unname(text_widths)
  table_spec$content_width <- sum(column_widths) +
    gap * max(length(column_widths) - 1L, 0)
  table_spec
}

layout_center_table_spec <- function(table_spec,
                                     text_size = 3.2,
                                     header_text_size = 11,
                                     header_fontface = "bold",
                                     header_family = "") {
  column_widths <- estimate_split_column_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family,
    alignment = "center"
  )
  text_widths <- measure_table_text_widths(
    table_spec,
    text_size = text_size,
    header_text_size = header_text_size,
    header_fontface = header_fontface,
    header_family = header_family
  )
  gap <- 0.55
  left_edges <- cumsum(c(0, utils::head(column_widths + gap, -1)))
  positions <- left_edges + column_widths / 2

  table_spec$table_data$column_position <- unname(
    positions[match(table_spec$table_data$column_key, table_spec$column_keys)]
  )
  table_spec$positions <- unname(positions)
  table_spec$header_positions <- unname(positions)
  table_spec$estimated_column_widths <- unname(column_widths)
  table_spec$displayed_column_widths <- unname(text_widths)
  table_spec$content_width <- sum(column_widths) +
    gap * max(length(column_widths) - 1L, 0)
  table_spec
}

left_align_center_table_columns <- function(table_spec,
                                            columns,
                                            inset = 0.02) {
  columns <- intersect(columns, table_spec$column_keys)
  if (length(columns) == 0L) {
    return(table_spec)
  }

  table_spec$table_data$text_hjust <- 0.5
  for (column in columns) {
    column_index <- match(column, table_spec$column_keys)
    left_position <- table_spec$positions[[column_index]] -
      table_spec$estimated_column_widths[[column_index]] / 2 + inset
    rows <- table_spec$table_data$column_key == column
    table_spec$table_data$column_position[rows] <- left_position
    table_spec$table_data$text_hjust[rows] <- 0
  }

  table_spec
}

compute_table_x_limits <- function(table_spec, pad = 0.03) {
  widths <- if (!is.null(table_spec$displayed_column_widths)) {
    table_spec$displayed_column_widths
  } else {
    table_spec$estimated_column_widths
  }
  positions <- table_spec$positions
  alignment <- if (!is.null(table_spec$alignment)) table_spec$alignment else "center"

  if (alignment == "left") {
    xmin <- min(positions)
    xmax <- max(positions + widths)
  } else if (alignment == "right") {
    xmin <- min(positions - widths)
    xmax <- max(positions)
  } else {
    xmin <- min(positions - widths / 2)
    xmax <- max(positions + widths / 2)
  }

  cell_fields <- c("column_key", "column_position", "text_hjust")
  if (all(cell_fields %in% names(table_spec$table_data))) {
    column_index <- match(
      table_spec$table_data$column_key,
      table_spec$column_keys
    )
    cell_widths <- widths[column_index]
    cell_positions <- table_spec$table_data$column_position
    cell_hjust <- table_spec$table_data$text_hjust
    valid_cells <- is.finite(cell_widths) & is.finite(cell_positions) &
      is.finite(cell_hjust)

    if (any(valid_cells)) {
      cell_left <- cell_positions - cell_hjust * cell_widths
      cell_right <- cell_positions + (1 - cell_hjust) * cell_widths
      xmin <- min(xmin, cell_left[valid_cells])
      xmax <- max(xmax, cell_right[valid_cells])
    }
  }

  c(xmin - pad, xmax + pad)
}

split_table_width_multiplier <- function(n_columns) {
  n_columns <- as.integer(n_columns[[1]])

  if (is.na(n_columns) || n_columns <= 0L) {
    return(0)
  }

  if (n_columns == 1L) {
    return(0.5)
  }

  1 + (n_columns - 2L) / 3
}

validate_composition_width <- function(value, arg) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
    !is.finite(value) || value <= 0) {
    abort_forest(
      sprintf("`%s` must be a single positive number.", arg),
      class = "ggforestplotR_invalid_table"
    )
  }

  invisible(value)
}

# Table panel rendering

#' Build a ggplot2 table panel for one side of a split forest plot.
#'
#' Uses symmetric expansion and uniform margins. Each table panel receives its
#' own measured content width; the forest plot receives `plot_width`.
#' @keywords internal
#' @noRd
build_forest_table_plot <- function(table_spec,
                                    stripe_data,
                                    has_groupings = FALSE,
                                    facet_strip_position = c("left", "right"),
                                    table_position = c("left", "right"),
                                    striped_rows = FALSE,
                                    stripe_fill = "grey95",
                                    stripe_colour = NA,
                                    stripe_alpha = 1,
                                    text_size = 3.2,
                                    grid_lines = FALSE,
                                    grid_line_colour = "black",
                                    grid_line_size = 0.3,
                                    grid_line_linetype = 1,
                                    x_expand = ggplot2::expansion(mult = 0.05),
                                    plot_margin = ggplot2::margin(5.5, 4, 5.5, 4),
                                    text_hjust = 0.5,
                                    header_hjust = 0.5,
                                    header_text_size = 11,
                                    header_fontface = "bold",
                                    header_family = NULL,
                                    dodge_width = 0.6) {
  facet_strip_position <- match.arg(facet_strip_position)
  table_position <- match.arg(table_position)

  if (all(is.na(table_spec$positions))) {
    table_spec <- layout_center_table_spec(table_spec, text_size = text_size)
  }

  p <- ggplot2::ggplot(
    table_spec$table_data,
    ggplot2::aes(x = .data$column_position, y = .data$row_key, label = .data$text)
  )

  x_limits <- compute_table_x_limits(table_spec)

  if (isTRUE(striped_rows)) {
    p <- p + ggplot2::geom_rect(
      data = stripe_data[stripe_data$fill_key == "stripe", , drop = FALSE],
      mapping = ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = stripe_fill,
      colour = stripe_colour,
      alpha = stripe_alpha
    )
  }

  if (isTRUE(grid_lines)) {
    line_data <- build_table_line_data(stripe_data, has_groupings = has_groupings)

    if (nrow(line_data) > 0L) {
      p <- p + ggplot2::geom_hline(
        data = line_data,
        mapping = ggplot2::aes(yintercept = .data$yintercept),
        colour = grid_line_colour,
        linewidth = grid_line_size,
        linetype = grid_line_linetype
      )
    }
  }

  text_data <- expand_grouped_table_text(
    table_spec$table_data,
    group_levels = table_spec$group_levels
  )
  text_style <- table_spec$text_style
  if (is.null(text_style)) {
    text_style <- resolve_forest_text_style(overrides = list(
      text_size = text_size, header_text_size = header_text_size,
      header_fontface = header_fontface, header_family = header_family
    ))
  }
  cells <- forest_cell_text_style(text_data, text_style)
  text_data$.text_size <- cells$size
  text_data$.text_fontface <- cells$fontface
  text_data$.text_family <- cells$family
  text_data$.text_colour <- cells$colour
  centered_text <- text_data[!text_data$dodge_text, , drop = FALSE]
  grouped_text <- text_data[text_data$dodge_text, , drop = FALSE]
  text_layers <- list()

  if (nrow(grouped_text) > 0L) {
    # Train every row before separate text layers can reorder the y scale.
    p <- p + ggplot2::geom_blank(
      data = table_spec$table_data,
      mapping = ggplot2::aes(y = .data$row_key),
      inherit.aes = FALSE
    )
  }

  if (nrow(centered_text) > 0L) {
    text_layers[[length(text_layers) + 1L]] <- forest_table_text_layer(
      centered_text, text_hjust = text_hjust
    )
  }
  if (nrow(grouped_text) > 0L) {
    text_layers[[length(text_layers) + 1L]] <- forest_table_text_layer(
      grouped_text, text_hjust = text_hjust, dodge_width = dodge_width
    )
  }

  p <- p +
    text_layers +
    ggplot2::scale_x_continuous(
      breaks = table_spec$header_positions,
      labels = table_spec$headers,
      position = "top",
      expand = x_expand,
      limits = x_limits
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_y_discrete(labels = function(x) rep("", length(x)), drop = TRUE) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(
        face = header_fontface,
        family = header_family,
        size = header_text_size,
        colour = text_style$header_colour,
        hjust = header_hjust,
        margin = ggplot2::margin(b = 0)
      ),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_blank(),
      strip.placement = "outside",
      plot.margin = plot_margin
    )

  if (isTRUE(has_groupings)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(grouping_panel),
      ncol = 1, scales = "free_y",
      strip.position = facet_strip_position,
      drop = FALSE
    )
  }

  p
}

# Build independent layers with all mappings declared before construction.
forest_table_text_layer <- function(data, text_hjust = 0.5, dodge_width = NULL) {
  mapping <- ggplot2::aes(
    size = .data$.text_size,
    fontface = .data$.text_fontface,
    family = .data$.text_family,
    colour = .data$.text_colour,
    group = .data$row_key
  )
  args <- list()
  if ("text_hjust" %in% names(data)) {
    mapping$hjust <- ggplot2::aes(hjust = .data$text_hjust)$hjust
  } else {
    args$hjust <- text_hjust
  }
  position <- "identity"
  if (!is.null(dodge_width)) {
    mapping$group <- ggplot2::aes(group = .data$text_group)$group
    position <- ggplot2::position_dodge(width = dodge_width)
  }
  rlang::exec(
    ggplot2::geom_text,
    data = data,
    mapping = mapping,
    position = position,
    lineheight = 0.95,
    show.legend = FALSE,
    !!!args
  )
}
