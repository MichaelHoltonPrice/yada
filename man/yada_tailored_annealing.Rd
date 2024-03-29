% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yada_mcmc.R
\name{yada_tailored_annealing}
\alias{yada_tailored_annealing}
\title{Do a tailored optimization using simulated annealing}
\usage{
yada_tailored_annealing(neg_log_lik, theta0, ...)
}
\arguments{
\item{neg_log_lik}{The function for the negative log-likelihood}

\item{theta0}{The initial value of the parameter vector}

\item{...}{Variables required by neg_log_lik}
}
\value{
A list containing eta_best (the best value of the negative
  log-likelihood encountered), theta_best (the corresponding best parameter
  vector), and eta_vect (the full vector of negative log-likelihoods for the
  sampled parameter vectors)
}
\description{
A tailored optimization using simulated annealing that works well for
univariate models. The annealing can be used by itself for optimization or
to initialize another optimization algorithm. While this optimization via
annealing usually requires more computations of the negative log-likelihood
function compared to other algorithms (e.g., directly calling
stats::optim with method='BFGS'), experience has shown that other algorithms
do not always find the minimum of the negative log-likelihood. Put slightly
differently, this tailored algorithm trades off number of computations for
improved robustness, which is likely almost always a trade-off the user
desires. The simulated annealing is adaptive. In particular, with each
change in temperature (and prior to annealing at the first temperature)
sampling is separately done to set the standard deviation of the (normal)
proposal distribution to achieve an acceptance ratio of about 0.23.
}
