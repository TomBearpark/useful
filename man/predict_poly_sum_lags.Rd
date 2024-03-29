% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_poly_sum_lags.R
\name{predict_poly_sum_lags}
\alias{predict_poly_sum_lags}
\title{Get the response function from a polynomial FE regression, with SEs}
\usage{
predict_poly_sum_lags(
  m,
  var,
  min,
  max,
  ref,
  ivar_tag = "",
  ci_level = 95,
  step.length = 1,
  coefs = NULL,
  id.col = NULL,
  xvar_name = "temp",
  include_checks = FALSE,
  divider = NULL
)
}
\arguments{
\item{m}{An output of a fixest regression}

\item{var}{The string identifier for thevariable we want to get a RF for.
For this to work, you need
to make sure that the polynomial terms are consistently named, and include
this string. They need to be named "l{lag}_{var}_p{poly}" at the moment}

\item{min}{The minimum value we want to predic the RF for}

\item{max}{THe max value we want to predict the RF for}

\item{ref}{The reference value, all other}

\item{ci_level}{The confidence interval}

\item{step.length}{How far apart should we predict each value}

\item{coefs}{Use this if you want to manually specify the coefs, rather
than automatically guess them}

\item{id.col}{can specify something to put into an extra column: useful for
facetting in later plots}

\item{xvar_name}{what do you want the prediction variable column name to be}

\item{include_checks}{turn on some potentially handy name checks?}

\item{divider}{numeric constant to rescale outputs}
}
\value{
a tibble
}
\description{
Get the response function from a polynomial FE regression, with SEs
}
\examples{
library(fixest); library(dplyr)
data <- tibble(y = rnorm(10), tavg = rnorm(10), tavg2 = tavg^2)
m <- feols(fml = y ~ tavg + tavg2, data = data)
predict_poly(m, "tavg", 10, 30, 20)

}
