forest_snapshot_variant <- function() {
  # ggplot2 3.x and patchwork 1.2 use different clipping and legend layouts.
  if (utils::packageVersion("ggplot2") < "4.0.0") "ggplot2-3" else NULL
}

snapshot_split_table <- function(...) {
  # Column widths are measured on the active graphics device when the table
  # is built. Use the same PDF metrics in CI and in interactive sessions.
  grDevices::pdf(NULL, family = "Helvetica")
  device <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(device), add = TRUE)
  add_split_table(...)
}
