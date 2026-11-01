forest_snapshot_variant <- function() {
  # ggplot2 3.x and patchwork 1.2 use different clipping and legend layouts.
  if (utils::packageVersion("ggplot2") < "4.0.0") "ggplot2-3" else NULL
}
