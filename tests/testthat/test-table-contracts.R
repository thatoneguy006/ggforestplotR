make_contract_data <- function() {
  data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.30, -0.20, 0.40),
    conf.low = c(0.10, -0.40, 0.20),
    conf.high = c(0.50, 0.00, 0.60),
    sample_size = c(120, 115, 98),
    p_value = c(0.012, 0.031, 0.004)
  )
}

test_that("add_forest_table returns a two-panel patchwork", {
  p <- ggforestplot(make_contract_data(), n = "sample_size", p.value = "p_value")

  out <- add_forest_table(
    p,
    position = "left",
    show_n = TRUE,
    show_p = TRUE,
    estimate_label = "Beta"
  )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 2L)
  expect_length(out$patches$plots, 1L)
  expect_s3_class(out$patches$plots[[1]], "ggplot")
})

test_that("add_forest_table supports ggplot add syntax as a terminal step", {
  out <- ggforestplot(make_contract_data(), n = "sample_size", p.value = "p_value") +
    ggplot2::labs(title = "Contract") +
    add_forest_table(position = "right", show_n = TRUE, show_p = TRUE)

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 2L)
  expect_length(out$patches$plots, 1L)
})

test_that("add_split_table returns left plot right panels in order", {
  p <- ggforestplot(make_contract_data(), n = "sample_size", p.value = "p_value")

  out <- add_split_table(
    p,
    left_columns = c("term", "n"),
    right_columns = c("estimate", "p"),
    estimate_label = "HR"
  )

  expect_s3_class(out, "patchwork")
  expect_s3_class(out, "ggplot")
  expect_length(out$patches$layout$widths, 3L)
  expect_length(out$patches$plots, 2L)
  expect_s3_class(out$patches$plots[[2]], "ggplot")
})

test_that("add_split_table ggplot add syntax preserves a forest-plot center panel", {
  out <- ggforestplot(make_contract_data(), n = "sample_size", p.value = "p_value") +
    add_split_table(left_columns = c("term", "n"), right_columns = c("estimate", "p"))

  center_plot <- out$patches$plots[[2]]

  expect_s3_class(out, "patchwork")
  expect_s3_class(center_plot, "ggplot")
  expect_true(!is.null(center_plot$ggforestplotR_state))
})
