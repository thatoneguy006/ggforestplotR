structured_rows <- function() {
  data.frame(
    term = c("Section", "Female", "Male", NA, NA, "Overall"),
    estimate = c(NA, NA, 1.42, NA, NA, 1.18),
    conf.low = c(NA, NA, 1.10, NA, NA, 1.04),
    conf.high = c(NA, NA, 1.84, NA, NA, 1.34),
    row_type = c("header", "reference", "estimate", "spacer", "spacer", "summary"),
    p.value = c(0.03, NA, 0.008, NA, NA, 0.01),
    n = c(NA, 60, 55, NA, NA, 115),
    extra = c("Head", "Ref", "Male", NA, NA, "All")
  )
}

make_structured_plot <- function(raw = structured_rows(), ...) {
  ggforestplot(raw, row_type = "row_type", p.value = "p.value",
               n = "n", ref_line = NULL, ...)
}

as_structured_data <- function(raw, ...) {
  as_forest_data(raw, term = "term", estimate = "estimate",
                 conf.low = "conf.low", conf.high = "conf.high", ...)
}

table_cell <- function(spec, key, type) {
  cells <- spec$table_data
  cells$text[cells$column_key == key & cells$row_type == type]
}
