% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate_models.R
\name{get_approx_x_range_cont}
\alias{get_approx_x_range_cont}
\title{Get the approximate range of x-values given a single, known response value
for a continuous variable}
\usage{
get_approx_x_range_cont(w, th_w, mod_spec)
}
\arguments{
\item{w}{The response (a single value, not a vector)}

\item{th_w}{The parameter vector}

\item{mod_spec}{The model specification}
}
\value{
An approximate range for x
}
\description{
In order to sample efficiently from the posterior density (and, ultimately,
to calculate the mean age and confidence intervals), it is helpful to have an
approximate estimate for the range of x-values that may be spanned by the
posterior density. get_approx_x_range_cont provides a rough estimate of that
range using only the likelihood (that is, it does not use the prior, which is
usually far flatter than the likelihood; the range itself is also an estimate
for the range implied by the likelihood).
}
