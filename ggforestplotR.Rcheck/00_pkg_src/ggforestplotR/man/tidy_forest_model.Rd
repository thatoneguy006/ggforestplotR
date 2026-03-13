\name{tidy_forest_model}
\alias{tidy_forest_model}
\title{Tidy a model object for forest plotting}
\usage{
tidy_forest_model(
  model,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  intercept = FALSE,
  sort_terms = c("none", "descending", "ascending")
)
}
\arguments{
  \item{model}{A fitted model object supported by \code{broom::tidy()}.}
  \item{conf.int}{Logical; if TRUE, request confidence intervals from \code{broom::tidy()}.}
  \item{conf.level}{Confidence level for intervals.}
  \item{exponentiate}{Logical; passed through to \code{broom::tidy()}.}
  \item{intercept}{Logical; if FALSE, drop the intercept term.}
  \item{sort_terms}{How to sort rows: "none", "descending", or "ascending".}
}
\value{
A standardized coefficient data frame ready for \code{ggforestplot()}.
}
\description{
Uses \code{broom::tidy()} to convert a fitted model into forest-plot data.
}