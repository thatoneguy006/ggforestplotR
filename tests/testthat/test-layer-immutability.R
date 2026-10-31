

test_that("adding an x scale does not modify the original plot", {
  coefs <- data.frame(term = c("Age", "BMI"), estimate = c(1.1, 0.9),
                      conf.low = c(1.02, 0.8), conf.high = c(1.2, 1.01))
  p <- ggforestplot(coefs, exponentiate = TRUE, striped_rows = TRUE)
  i <- p$ggforestplotR_state$stripe_layer_index
  before <- p$layers[[i]]$data
  p2 <- suppressMessages(p + ggplot2::scale_x_log10(limits = c(0.5, 4)))
  expect_identical(p$layers[[i]]$data, before)
  expect_equal(unique(p2$layers[[i]]$data$xmin), 0.5)
  expect_equal(unique(p2$layers[[i]]$data$xmax), 4)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_no_error(ggplot2::ggplot_build(p2))
})

test_that("adding y limits does not drop stripes from the original plot", {
  s <- data.frame(term = c("Sex", "Male", "Female"),
                  estimate = c(NA, 1.2, NA), conf.low = c(NA, 1.0, NA),
                  conf.high = c(NA, 1.5, NA),
                  row_type = c("header", "estimate", "reference"))
  q <- ggforestplot(s, row_type = "row_type", exponentiate = TRUE,
                    striped_rows = TRUE)
  i <- q$ggforestplotR_state$stripe_layer_index
  before <- q$layers[[i]]$data
  q2 <- suppressMessages(q + ggplot2::scale_y_discrete(limits = c("Male", "Female")))
  expect_identical(q$layers[[i]]$data, before)
})

test_that("composing a table does not modify the original plot", {
  coefs <- data.frame(term = c("Age", "BMI", "Treatment"),
                      estimate = c(0.1, -0.08, 0.34), conf.low = c(0.02, -0.16, 0.12),
                      conf.high = c(0.18, 0, 0.56))
  p <- ggforestplot(coefs, striped_rows = TRUE, separate_groups = "term",
                    separate_lines = TRUE)
  layers_before <- lapply(p$layers, function(layer) layer$data)
  invisible(p + add_forest_table())
  expect_identical(lapply(p$layers, function(layer) layer$data), layers_before)
})
