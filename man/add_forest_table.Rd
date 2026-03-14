\name{add_forest_table}
\alias{add_forest_table}
\title{Add a Side Table to a Forest Plot}
\usage{
add_forest_table(
  plot = NULL,
  position = c("left", "right"),
  show_terms = TRUE,
  show_n = NULL,
  show_estimate = TRUE,
  show_p = FALSE,
  term_header = "Term",
  n_header = "N",
  estimate_label = "Estimate",
  p_header = "P-value",
  digits = NULL,
  text_size = NULL,
  striped_rows = NULL,
  stripe_fill = NULL,
  stripe_colour = NULL,
  grid_lines = FALSE,
  grid_line_colour = "black",
  grid_line_size = 0.3,
  grid_line_linetype = 1
)
}
\arguments{
  \item{plot}{A plot created by \code{ggforestplot()}. Leave as \code{NULL} to use \code{+ add_forest_table(...)} syntax.}
  \item{position}{Whether to place the table on the left or right of the forest plot.}
  \item{show_terms}{Whether to show the term column in the table.}
  \item{show_n}{Whether to show the \code{N} column. Defaults to \code{TRUE} when the underlying plot data include an \code{n} column.}
  \item{show_estimate}{Whether to show the formatted estimate and confidence interval column.}
  \item{show_p}{Whether to show the p-value column to the right of the estimate column.}
  \item{term_header}{Header text for the term column.}
  \item{n_header}{Header text for the \code{N} column.}
  \item{estimate_label}{Header label for the estimate column.}
  \item{p_header}{Header text for the p-value column.}
  \item{digits}{Number of digits used when formatting estimates and p-values. Defaults to \code{2}.}
  \item{text_size}{Text size for table contents. Defaults to \code{3.2}.}
  \item{striped_rows}{Whether to draw alternating row stripes behind the table. Defaults to the stripe setting used in \code{ggforestplot()}.}
  \item{stripe_fill}{Fill colour used for striped rows. Defaults to the stripe fill used in \code{ggforestplot()}.}
  \item{stripe_colour}{Outline colour for striped rows. Defaults to the stripe outline used in \code{ggforestplot()}.}
  \item{grid_lines}{Whether to draw black horizontal grid lines in the table.}
  \item{grid_line_colour}{Colour used for the table grid lines.}
  \item{grid_line_size}{Line width used for the table grid lines.}
  \item{grid_line_linetype}{Line type used for the table grid lines.}
}
\value{A patchwork-composed plot containing the forest plot and side table, or a ggplot add-on object when \code{plot = NULL}.}
\description{Compose a side table onto a forest plot after the plot has been styled.}
\examples{
coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.3, -0.2, 0.4),
  conf.low = c(0.1, -0.4, 0.2),
  conf.high = c(0.5, 0.0, 0.6),
  sample_size = c(120, 115, 98),
  p_value = c(0.012, 0.031, 0.004)
)

p <- ggforestplot(coefs, n = "sample_size", p.value = "p_value")
add_forest_table(p, position = "left", show_n = TRUE, show_p = TRUE, estimate_label = "Beta")

ggforestplot(coefs, n = "sample_size", p.value = "p_value") +
  add_forest_table(position = "right", show_n = TRUE, show_p = TRUE, estimate_label = "Beta")
}
