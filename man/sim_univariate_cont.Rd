% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate_models.R
\name{sim_univariate_cont}
\alias{sim_univariate_cont}
\title{Create simulated data for a univariate continuous model. For x, either the
number of samples (N) and a parameter vector (th_x) must be given, or the
full vector (x) must be given.}
\usage{
sim_univariate_cont(th_w, modSpec, N = NA, th_x = NA, x = NA)
}
\arguments{
\item{th_w}{The continuous parameter vector [c,kappa]}

\item{modSpec}{The model specification}

\item{N}{The number of samples to simulate}

\item{th_x}{The parameterization for x}

\item{x}{The vector of independent variables}
}
\value{
A list object of simulated data containing x and w
}
\description{
Create simulated data for a univariate continuous model. For x, either the
number of samples (N) and a parameter vector (th_x) must be given, or the
full vector (x) must be given.
}
