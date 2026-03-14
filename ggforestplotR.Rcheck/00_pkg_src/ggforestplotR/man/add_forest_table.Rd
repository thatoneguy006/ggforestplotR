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
  term_header = "Term",
  n_header = "N",
  estimate_label = "Estimate",
  digits = NULL,
  text_size = NULL,
  striped_rows = NULL,
  stripe_fill = NULL,
  stripe_colour = NULL
)
}
\arguments{
  \item{plot}{A plot created by \code{ggforestplot()}. Leave as \code{NULL} to use \code{+ add_forest_table(...)} syntax.}
  \item{position}{Whether to place the table on the left or right of the forest plot.}
  \item{show_terms}{Whether to show the term column in the table.}
  \item{show_n}{Whether to show the \code{N} column. Defaults to the setting used in \code{ggforestplot()}.}
  \item{show_estimate}{Whether to show the formatted estimate and confidence interval column.}
  \item{term_header}{Header text for the term column.}
  \item{n_header}{Header text for the \code{N} column.}
  \item{estimate_label}{Header label for the estimate column.}
  \item{digits}{Number of digits used when formatting estimates. Defaults to the setting used in \code{ggforestplot()}.}
  \item{text_size}{Text size for table contents. Defaults to the setting used in \code{ggforestplot()}.}
  \item{striped_rows}{Whether to draw alternating row stripes behind the table. Defaults to the setting used in \code{ggforestplot()}.}
  \item{stripe_fill}{Fill colour used for striped rows. Defaults to the setting used in \code{ggforestplot()}.}
  \item{stripe_colour}{Outline colour for striped rows. Defaults to the setting used in \code{ggforestplot()}.}
}
\value{A patchwork-composed plot containing the forest plot and side table, or a ggplot add-on object when \code{plot = NULL}.}
\description{Compose a side table onto a forest plot after the plot has been styled.}
\examples{
coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.3, -0.2, 0.4),
  conf.low = c(0.1, -0.4, 0.2),
  conf.high = c(0.5, 0.0, 0.6),
  sample_size = c(120, 115, 98)
)

p <- ggforestplot(coefs, n = "sample_size")
add_forest_table(p, position = "left", show_n = TRUE, estimate_label = "Beta")

ggforestplot(coefs, n = "sample_size") +
  add_forest_table(position = "right", show_n = TRUE, estimate_label = "Beta")
}