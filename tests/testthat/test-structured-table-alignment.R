vignette_structured_rows <- function() {
  data.frame(
    term = c("Female", "Male", "Clinical", "Age", "", "Overall"),
    estimate = c(NA, 1.42, NA, 1.03, NA, 1.18),
    conf.low = c(NA, 1.10, NA, 1.01, NA, 1.04),
    conf.high = c(NA, 1.84, NA, 1.05, NA, 1.34),
    subgroup = c("Sex", "Sex", NA, NA, NA, NA),
    row_type = c("reference", "estimate", "header", "estimate", "spacer",
                 "summary")
  )
}

expect_structured_panel_alignment <- function(composition, expected_rows) {
  panels <- attr(composition, "ggforestplotR_composition")$panels
  forest <- Filter(function(p) !is.null(p$ggforestplotR_state), panels)[[1L]]
  forest_build <- ggplot2::ggplot_build(forest)
  forest_y <- forest_build$layout$panel_params[[1L]]$y
  expect_length(forest_y$get_limits(), expected_rows)
  point_index <- which(vapply(
    forest$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)
  ))
  points <- forest_build$data[[point_index]]

  for (table in Filter(function(p) is.null(p$ggforestplotR_state), panels)) {
    built <- ggplot2::ggplot_build(table)
    expect_equal(built$layout$panel_params[[1L]]$y$get_limits(),
                 forest_y$get_limits())
    expect_equal(built$layout$panel_params[[1L]]$y.range,
                 forest_build$layout$panel_params[[1L]]$y.range)
    text_indices <- which(vapply(
      table$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1)
    ))
    text <- do.call(rbind, built$data[text_indices])
    estimates <- text[text$label %in% c("1.42", "1.03", "1.18"), ]
    if (nrow(estimates) > 0L) {
      matched <- match(round(10^points$x, 2), as.numeric(estimates$label))
      expect_false(anyNA(matched))
      expect_equal(as.numeric(points$y), as.numeric(estimates$y[matched]))
    }
  }
}

test_that("structured tables retain blank spacer rows from the vignette", {
  raw <- vignette_structured_rows()
  repeated <- rbind(raw[1:5, ], raw[5:6, ])
  for (data in list(raw, repeated)) {
    for (striped in c(FALSE, TRUE)) {
      plot <- ggforestplot(
        data, subgroup = "subgroup", row_type = "row_type",
        exponentiate = TRUE, striped_rows = striped
      )
      expect_structured_panel_alignment(
        plot + add_split_table(
          left_columns = "term", right_columns = c("estimate", "ci"),
          reference_text = "Reference"
        ), nrow(data) + 1L
      )
      for (side in c("left", "right")) {
        expect_structured_panel_alignment(
          plot + add_forest_table(
            columns = c("term", "estimate", "ci"), position = side
          ), nrow(data) + 1L
        )
      }
    }
  }
})

test_that("explicit y limits can still exclude blank spacer rows", {
  plot <- suppressMessages(
    ggforestplot(
      vignette_structured_rows(), subgroup = "subgroup",
      row_type = "row_type", exponentiate = TRUE
    ) + ggplot2::scale_y_discrete(limits = function(x) x[nzchar(x)])
  )
  expect_structured_panel_alignment(
    plot + add_split_table(
      left_columns = "term", right_columns = c("estimate", "ci")
    ), 6L
  )
})
