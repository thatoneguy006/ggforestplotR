test_that("ggforestplot returns a ggplot object for tidy input", {
  raw <- data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0)
  )

  p <- ggforestplot(raw)

  expect_s3_class(p, "ggplot")
})

test_that("ggforestplot enforces positive values for exponentiated plots", {
  raw <- data.frame(
    term = "Treatment",
    estimate = 1.2,
    conf.low = 0.9,
    conf.high = 1.6
  )

  expect_s3_class(ggforestplot(raw, exponentiate = TRUE), "ggplot")
})
