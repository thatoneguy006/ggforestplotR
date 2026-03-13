\name{ggforestplot}
\alias{ggforestplot}
\title{Draw a ggplot2 forest plot}
\usage{
ggforestplot(
  data,
  term = "term",
  estimate = "estimate",
  conf.low = "conf.low",
  conf.high = "conf.high",
  label = term,
  group = NULL,
  grouping = NULL,
  n = NULL,
  p.value = NULL,
  exponentiate = FALSE,
  sort_terms = c("none", "descending", "ascending"),
  point_size = 2.3,
  point_shape = 19,
  line_size = 0.5,
  staple_width = 0,
  dodge_width = 0.6,
  striped_rows = FALSE,
  stripe_fill = "grey95",
  stripe_colour = NA,
  table_position = c("none", "left", "right"),
  table_show_terms = TRUE,
  table_show_n = !is.null(n),
  table_show_estimate = TRUE,
  table_term_header = "Term",
  table_n_header = "N",
  table_estimate_label = "Estimate",
  table_digits = 2,
  table_text_size = 3.2,
  zero_line = TRUE,
  zero_line_linetype = 2,
  zero_line_colour = "grey60",
  xlab = NULL,
  ylab = NULL,
  title = NULL
)
}
\arguments{
  \item{data}{Either a tidy coefficient data frame or a model object supported by \code{broom::tidy()}.}
  \item{term, estimate, conf.low, conf.high, label, group, grouping, n, p.value}{Column names used when \code{data} is a data frame.}
  \item{exponentiate}{Logical; if TRUE, draw the estimate axis on a log scale with a null line at 1.}
  \item{sort_terms}{How to sort rows: "none", "descending", or "ascending".}
  \item{point_size}{Point size for coefficient markers.}
  \item{point_shape}{Shape used for coefficient markers.}
  \item{line_size}{Line width for confidence intervals.}
  \item{staple_width}{Width of the terminal staples on confidence interval lines.}
  \item{dodge_width}{Horizontal dodging used for grouped estimates.}
  \item{striped_rows}{Logical; if TRUE, shade alternating rows.}
  \item{stripe_fill}{Fill color used for shaded rows.}
  \item{stripe_colour}{Border color for shaded rows.}
  \item{table_position}{Where to attach the summary table: no table, left, or right.}
  \item{table_show_terms}{Logical; if TRUE, include a term column in the attached table.}
  \item{table_show_n}{Logical; if TRUE, include an N column in the attached table.}
  \item{table_show_estimate}{Logical; if TRUE, include a formatted estimate plus 95\% CI column in the attached table.}
  \item{table_term_header}{Header text for the term column.}
  \item{table_n_header}{Header text for the N column.}
  \item{table_estimate_label}{Label used for the estimate table header, for example \code{"HR"} or \code{"Beta"}.}
  \item{table_digits}{Number of digits used when formatting estimates in the attached table.}
  \item{table_text_size}{Text size used inside the attached table.}
  \item{zero_line}{Logical; if TRUE, draw a null reference line.}
  \item{zero_line_linetype}{Line type for the null reference line.}
  \item{zero_line_colour}{Color for the null reference line.}
  \item{xlab, ylab, title}{Optional plot labels.}
}
\value{
A \code{ggplot} object when \code{table_position = "none"}; otherwise a combined grob containing the plot and attached table.
}
\description{
Builds a forest plot from standardized coefficient data or directly from a fitted model, with optional grouped sections, striped rows, point-shape and staple controls, and a side summary table.
}