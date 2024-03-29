% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivariate_models.R
\name{analyze_x_posterior}
\alias{analyze_x_posterior}
\title{Analyze an input density by calculating the mean and confidence intervals}
\usage{
analyze_x_posterior(xv, fv, xknown = NA)
}
\arguments{
\item{xv}{A vector of evenly spaced ages at which the density, fv,
is calculated}

\item{fv}{A vector of densities}

\item{xknown}{Known age [optional]}
}
\value{
A list containing the results of the analysis
}
\description{
\code{analyze_x_posterior} analyzes an input density given by the input pair
of vectors xv and fv by calculating the mean and confidence intervals for a
handful of quantiles (0.001, 0.025, 0.5, 0.975, and 0.999). xv is assumed to
be evenly spaced. Optionally, a known age (xknown) can be input, for which
the density is estimated.
}
