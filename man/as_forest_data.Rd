\name{as_forest_data}
\alias{as_forest_data}
\title{Standardize coefficient data for forest plots}
\usage{
as_forest_data(
  data,
  term,
  estimate,
  conf.low,
  conf.high,
  label = term,
  group = NULL,
  grouping = NULL,
  n = NULL,
  p.value = NULL,
  exponentiate = FALSE,
  sort_terms = c("none", "descending", "ascending")
)
}
\arguments{
  \item{data}{A data frame containing coefficient estimates and intervals.}
  \item{term}{Column name holding the model term identifier.}
  \item{estimate}{Column name holding the point estimate.}
  \item{conf.low}{Column name holding the lower confidence bound.}
  \item{conf.high}{Column name holding the upper confidence bound.}
  \item{label}{Optional column name used for the displayed row label.}
  \item{group}{Optional column name used for color-grouping multiple estimates per row.}
  \item{grouping}{Optional column name used to split rows into grouped plot sections.}
  \item{n}{Optional column name holding sample sizes or other N labels for attached tables.}
  \item{p.value}{Optional column name holding p-values.}
  \item{exponentiate}{Logical; if TRUE, require positive values for estimates and intervals.}
  \item{sort_terms}{How to sort rows: "none", "descending", or "ascending".}
}
\value{
A standardized data frame with columns ready for \code{ggforestplot()}.
}
\description{
Standardizes a coefficient table into the internal forest-plot data structure.
}