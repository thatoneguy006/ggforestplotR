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
  grouping_strip_position = c("left", "right"),
  separator_group = NULL,
  n = NULL,
  p.value = NULL,
  exponentiate = FALSE,
  sort_terms = c("none", "descending", "ascending"),
  point_size = 2.3,
  point_shape = 19,
  line_size = 0.5,
  staple_width = 0.2,
  dodge_width = 0.6,
  separator_lines = FALSE,
  separator_line_linetype = 2,
  separator_line_colour = "black",
  separator_line_size = 0.4,
  striped_rows = FALSE,
  stripe_fill = "grey95",
  stripe_colour = NA,
  zero_line = TRUE,
  zero_line_linetype = 2,
  zero_line_colour = "grey60"
)
}
\arguments{
  \item{data}{Either a tidy coefficient data frame or a model object supported by \code{broom::tidy()}.}
  \item{term, estimate, conf.low, conf.high, label, group, grouping, grouping_strip_position, separator_group, n, p.value}{Column names used when \code{data} is a data frame.}
  \item{exponentiate}{Logical; if TRUE, draw the estimate axis on a log scale with a null line at 1.}
  \item{sort_terms}{How to sort rows: "none", "descending", or "ascending".}
  \item{point_size}{Point size for coefficient markers.}
  \item{point_shape}{Shape used for coefficient markers.}
  \item{line_size}{Line width for confidence intervals.}
  \item{staple_width}{Width of the terminal staples on confidence interval lines.}
  \item{dodge_width}{Horizontal dodging used for grouped estimates.}
  \item{separator_lines}{Logical; if TRUE, draw dashed separator lines around each labeled block identified by \code{separator_group}.}
  \item{separator_line_linetype}{Line type used for separator lines.}
  \item{separator_line_colour}{Colour used for separator lines.}
  \item{separator_line_size}{Line width used for separator lines.}
  \item{striped_rows}{Logical; if TRUE, shade alternating rows.}
  \item{stripe_fill}{Fill color used for shaded rows.}
  \item{stripe_colour}{Border color for shaded rows.}
  \item{zero_line}{Logical; if TRUE, draw a null reference line.}
  \item{zero_line_linetype}{Line type for the null reference line.}
  \item{zero_line_colour}{Color for the null reference line.}
}
\value{A \code{ggplot} object. Use standard \code{ggplot2} functions such as \code{labs()} for plot labels, and \code{add_forest_table()} to compose a side table after styling the plot.}
\description{Builds a forest plot from standardized coefficient data or directly from a fitted model, with optional grouped sections, striped rows, separator lines for labeled variable blocks, and point-shape and staple controls.}
