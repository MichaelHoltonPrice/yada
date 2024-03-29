% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate_models.R
\name{calc_bin_prob}
\alias{calc_bin_prob}
\title{Calculate bin probabilities for a univariate ordinal dataset}
\usage{
calc_bin_prob(x, v, bin_bounds = NA)
}
\arguments{
\item{x}{The vector of independent variables}

\item{v}{The vector of responses (m-values)}

\item{bin_bounds}{(Optional) The bounds to use for binning. If not given, 20
evenly spaced bins on the interval min(x) to max(x) are used.}
}
\value{
A list with the bin centers, bin counts, and bin proportions
}
\description{
x is the independent variable and v is the response variable for univariate
ordinal data. Bin the data by x, and calculate the proportion of
observations falling into each ordinal category for each bin.

The input bin_bounds is optional. If it is not provided, 20 evenly spaced
bins on the interval min(x) to max(x) are used.

The output is a list containing three variables:

bin_centers -- The x-values of the bin centers
bin_counts -- A matrix with dimenesions num_bins by num_cat giving the
  counts of the ordinal variable (v) for each bin and category
bin_prop -- A matrix with dimenesions num_bins by num_cat giving the
  probabilities of the ordinal variable (v) for each bin and category (a
  normalized version of bin_counts)
}
