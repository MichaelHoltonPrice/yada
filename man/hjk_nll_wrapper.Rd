% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivariate_models.R
\name{hjk_nll_wrapper}
\alias{hjk_nll_wrapper}
\title{A wrapper for calculating the negative log-likehood in fit_multivariate}
\usage{
hjk_nll_wrapper(
  param,
  th_y_bar0,
  th_y_bar_scale,
  calc_data,
  tf_cat_vect,
  save_file = NA
)
}
\arguments{
\item{param}{The normalized parameter vector}

\item{th_y_bar0}{The offset to use for the normalized parameter vector}

\item{th_y_bar_scale}{The scaling to use for the normalized parameter vector}

\item{calc_data}{The calculation data that support rapid calculation of the
negative log-likelihood}

\item{tf_cat_vect}{The transform category vector so that the optimization
can be uncsontrained}

\item{save_file}{(default: NA, none used) A save file to capture
optimization progress}
}
\value{
The negative log-likelihood
}
\description{
This wrapper function for doing the multivariate negative log-likelihood
calculation accomplishes three things. First, if the negative log-likelihood
evaluates to NA it replaces the value with Inf. Second, it rescales the
normalized, input parameter vector (param) to account for the offset
(th_y_bar0) and scale (th_y_bar_scale); in particular, the rescaling is
param = th_y_bar0 + param*th_y_bar_scale. Third (optionally), it saves to
file progress information on the optimization (notably, the best fit found
so far). This progress information can be used resume interupted
optimizations.
}
