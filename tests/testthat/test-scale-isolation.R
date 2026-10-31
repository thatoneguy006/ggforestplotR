test_that("ordinary ggplots are unaffected by the scale methods", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, factor(cyl))) + ggplot2::geom_point()
  limited <- p + ggplot2::scale_y_discrete(limits = c("8", "4"))
  expect_equal(ggplot2::layer_scales(limited)$y$get_limits(), c("8", "4"))
  logged <- p + ggplot2::scale_x_log10()
  expect_no_error(ggplot2::ggplot_build(logged))
})
