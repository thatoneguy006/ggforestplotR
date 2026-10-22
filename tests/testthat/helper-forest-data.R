make_simple_forest_data <- function(...) {
  data.frame(
    term = c("Age", "BMI"),
    estimate = c(0.3, -0.2),
    conf.low = c(0.1, -0.4),
    conf.high = c(0.5, 0.0),
    ...
  )
}

make_table_forest_data <- function() {
  data.frame(
    term = c("Age", "BMI", "Treatment"),
    estimate = c(0.3, -0.2, 0.4),
    conf.low = c(0.1, -0.4, 0.2),
    conf.high = c(0.5, 0.0, 0.6),
    sample_size = c(120, 115, 98),
    event_count = c(42, 39, 31),
    p_value = c(0.012, 0.031, 0.004)
  )
}

make_mixed_subgroup_data <- function() {
  data.frame(
    term = c("Age", "White", "Black", "BMI", "Female", "Male"),
    subgroup_name = c(NA, "Race", "Race", "", "Sex", "Sex"),
    estimate = c(1.03, 1.01, 0.89, 0.97, 0.96, 0.98),
    conf.low = c(1.01, 0.95, 0.80, 0.94, 0.89, 0.92),
    conf.high = c(1.05, 1.07, 0.99, 1.00, 1.04, 1.06),
    sample_size = c(500, 310, 190, 500, 280, 220),
    event_count = c(120, 80, 40, 120, 68, 52),
    p_value = c(0.002, 0.04, 0.04, 0.06, 0.31, 0.31),
    note = c("linear", "level", "level", "linear", "level", "level"),
    stringsAsFactors = FALSE
  )
}

make_grouped_subgroup_data <- function() {
  raw <- make_mixed_subgroup_data()
  raw <- raw[rep(seq_len(nrow(raw)), each = 2L), ]
  raw$model <- factor(
    rep(c("Base", "Adjusted"), 6L),
    levels = c("Adjusted", "Base")
  )
  raw$estimate <- seq_len(nrow(raw)) / 10
  raw$conf.low <- raw$estimate - 0.04
  raw$conf.high <- raw$estimate + 0.04
  raw$p_value <- raw$p_value + rep(c(0, 0.01), 6L)
  raw
}
