% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_poly.R
\name{predict_poly_het}
\alias{predict_poly_het}
\title{Predict poly for model where interactions were created using i()}
\usage{
predict_poly_het(
  m,
  df,
  het.var,
  xvar = "temp",
  min = 0,
  max = 35,
  ref = 20,
  ci_level = 95,
  step.length = 1,
  coefs = NULL,
  id.col = NULL
)
}
\arguments{
\item{id.col}{}
}
\description{
Predict poly for model where interactions were created using i()
}
