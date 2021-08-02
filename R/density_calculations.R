#' @title
#' Calculate the probability density function (PDF) matrix for a Weibull mixture
#' model
#'
#' @description
#' Given the input vector x and Weibull mixture parameters theta, calculate the
#' probability density function (PDF) matrix, which has dimensions length(x) by
#' num_mix, where num_mix = length(theta)/2. The mixture proportions are not
#' accounted for in the calculation.
#'
#' @param x Locations at which to evaluate probability density function
#' @param theta The value of shape and scale parameters with the ordering
#'   (sh_1,sc_1,sh_2,sc_2,...)
#'
#' @return The PDF matrix with dimensions length(x) by length(theta)/2
#'
#' @export
calc_weib_mix_density_matrix <- function(x, theta) {
  num_obs <- length(x) # Number of observations
  num_mix <- length(theta) / 2 # Number of mixtures
  # For x, the shape parameter, and scale parameter create a long vector of
  # length num_obs*num_mix for vectorized input to dweibull
  x_vect <- rep(x, num_mix)
  shape_vect <- as.vector(t(matrix(rep(theta[seq(1, length(theta), by = 2)],
                                       length(x)),
                                   nrow = num_mix)))
  scale_vect <- as.vector(t(matrix(rep(theta[seq(2, length(theta), by = 2)],
                                       length(x)),
                                   nrow = num_mix)))
  # Calculate the probability density and transform to a matrix with dimensions num_obs x num_mix
  return(matrix(dweibull(x_vect, shape_vect, scale_vect), ncol = num_mix))
}

#' @title Caculate the density for a Weibull mixture model
#'
#' @description
#' Given the input vector x and Weibull mixture proportions and parameters z and
#' theta, calculate the probability density function for the input vector x.
#'
#' @param x Locations at which to evaluate probability density function
#' @param z Vector of mixture proportions
#' @param theta The value of shape and scale parameters with the ordering
#'   (sh1_,sc_1,sh_2,sc_2,...)
#'
#' @return The PDF vector for the input vector x
#'
#' @export
calc_weib_mix_density <- function(x, z, theta) {
  density_matrix <- calc_weib_mix_density_matrix(x, theta)
  dens <- rowSums(t(t(density_matrix) * z))
  return(dens)
}

#' @title
#' Calculate integration weights assuming the midpoint rule
#'
#' @description
#' x is a vector of locations where a function to be integrated is evaluated
#' (the function values, f, are not input). Let x_n be the locations of
#' integration and f_n the corresponding function values for g=1, 2, ... N.
#' Assuming trapezoidal integration at the midpoints between elements of xu,
#' the weights to use for integration are dx_n = (x_(n+1) - x_(n-1)) / 2,
#' where the conventions x_0 = x_1 and x_(N+1) = tau_N are used.
#'
#' @param x A vector of locations where the function is sampled, possibly
#'   irregularly
#'
#' @return A vector of integration weights the same length as x
#'
#' @export
calc_trapez_weights <- function(x) {
  N <- length(x)
  weight_vect <- rep(NA, length(x))
  ind_cent <- 2:(N - 1)
  weight_vect[ind_cent] <- (x[ind_cent + 1] - x[ind_cent - 1]) / 2
  weight_vect[1] <- (x[2] - x[1]) / 2
  weight_vect[N] <- (x[N] - x[N - 1]) / 2
  return(weight_vect)
}
