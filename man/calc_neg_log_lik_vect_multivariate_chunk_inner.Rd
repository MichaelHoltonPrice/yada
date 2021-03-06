% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivariate_models.R
\name{calc_neg_log_lik_vect_multivariate_chunk_inner}
\alias{calc_neg_log_lik_vect_multivariate_chunk_inner}
\title{A helper function called by calc_neg_log_lik_vect_multivariate_chunk_outer
that calculates the negative log-likelihood for a set of observations using
a conventional for loop.}
\usage{
calc_neg_log_lik_vect_multivariate_chunk_inner(th_y, calcData)
}
\arguments{
\item{th_y}{The parameter vector}

\item{calcData}{Data needed for a speedy negative log-likelihood calculation}
}
\value{
The vector of negative log-likelihoods
}
\description{
A helper function called by calc_neg_log_lik_vect_multivariate_chunk_outer
that calculates the negative log-likelihood for a set of observations using
a conventional for loop.
}
