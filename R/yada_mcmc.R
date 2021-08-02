#' @title
#' Sample a likelihood function at a given temperature using an input negative
#' log-likelihood
#'
#' @description
#' Given an input negative log-likelihood and temperature, do MCMC sampling of
#' the parameter vector. Let eta be the value of the negative log-likelihood.
#' The function that is sampled is exp(-eta/temp), where temp is the
#' temperature; that is, the likelihood^(1/temp) is sampled. The proposal
#' is made using an independent normal draw for each variable in the parameter
#' vector, theta, with the standard deviation of the proposal distribution set
#' by the scalar parameter prop_scale (the same scale is used for all variables).
#'
#' The output is a list with starting and sampling information ,and includes the
#' following named elements:
#'
#' \itemize{
#'   \item{\code{theta0}}
#'     {The initial value of the parameter vector}
#'   \item{\code{eta0}}
#'     {The initial value of the negative log-likelihood}
#'   \item{\code{temp}}
#'     {The temperature}
#'   \item{\code{prop_scale}}
#'     {The scale (standard deviation) for the normal proposal distribution}
#'   \item{\code{theta_best}}
#'     {The best parameter vector found (lowest negative log-likelihood)}
#'   \item{\code{eta_best}}
#'     {The best (lowest) negative log-likelihood encountered}
#'   \item{\code{accept_vect}}
#'     {A boolean vector indicating which samples were accepted}
#'   \item{\code{eta_vect}}
#'     {A vector of negative log-likelihood values (for kept samples)}
#'   \item{\code{eta_vect}}
#'     {A matrix of sampled parameter vectors, size num_samp by length(theta)}
#' }
#'
#' @param neg_log_lik The function for the negative log-likelihood
#' @param theta0 The initial value of the parameter vector
#' @param num_samp The number of samples to make
#' @param temp The temperature
#' @param prop_scale A scalar value for the standard deviation of the proposal
#'   distribution
#' @param ... Variables required by neg_log_lik
#'
#' @return A list containing starting and sampling information (see description)
#'
#' @export
yada_sample_mcmc <- function(neg_log_lik,theta0,num_samp,temp,prop_scale,...) {
  if(length(prop_scale) != 1) {
    stop('prop_scale should be a scalar')
  }

  eta0 <- neg_log_lik(theta0,...)
  if(!is.finite(eta0)) {
    stop('The negative log-likelihood is not finite for the input initialization vector theta0')
  }

  theta <- theta0
  eta   <- eta0

  theta_best <- theta
  eta_best   <- eta

  accept_vect <- rep(NA,num_samp)
  eta_vect    <- rep(NA,num_samp)
  theta_mat   <- matrix(NA,num_samp,length(theta))
  for(n in 1:num_samp) {
    theta_prop <- theta + rnorm(length(theta))*prop_scale
    eta_prop <- neg_log_lik(theta_prop,...)

    if(!is.finite(eta_prop)) {
      accept <- F
    } else {
      alpha <- min(1,exp(-(eta_prop-eta)/temp))
      accept <- runif(1) < alpha
    }

    accept_vect[n] <- accept
    if(!accept) {
      theta_prop <- theta
      eta_prop   <- eta
    } else {
      if(eta_prop < eta_best) {
        theta_best <- theta_prop
        eta_best   <- eta_prop
      }
    }
    eta_vect[n] <- eta_prop
    theta_mat[n,] <- theta_prop

    # Get ready for next sample
    theta <- theta_prop
    eta   <- eta_prop
  }
  output <- list(theta0=theta0,
                 eta0=eta0,
                 temp=temp,
                 prop_scale=prop_scale,
                 theta_best=theta_best,
                 eta_best=eta_best,
                 accept_vect=accept_vect,
                 eta_vect=eta_vect,
                 theta_mat=theta_mat)
  return(output)
}

#' @title Do a tailored optimization using simulated annealing
#'
#' @description
#' A tailored optimization using simulated annealing that works well for
#' univariate models. The annealing can be used by itself for optimization or
#' to initialize another optimization algorithm. While this optimization via
#' annealing usually requires more computations of the negative log-likelihood
#' function compared to other algorithms (e.g., directly calling
#' stats::optim with method='BFGS'), experience has shown that other algorithms
#' do not always find the minimum of the negative log-likelihood. Put slightly
#' differently, this tailored algorithm trades off number of computations for
#' improved robustness, which is likely almost always a trade-off the user
#' desires. The simulated annealing is adaptive. In particular, with each
#' change in temperature (and prior to annealing at the first temperature)
#' sampling is separately done to set the standard deviation of the (normal)
#' proposal distribution to achieve an acceptance ratio of about 0.23.
#'
#' @param neg_log_lik The function for the negative log-likelihood
#' @param theta0 The initial value of the parameter vector
#' @param ... Variables required by neg_log_lik
#'
#' @return A list containing eta_best (the best value of the negative
#'   log-likelihood encountered), theta_best (the corresponding best parameter
#'   vector), and eta_vect (the full vector of negative log-likelihoods for the
#'   sampled parameter vectors)
#'
#' @export
yada_tailored_annealing <- function(neg_log_lik,theta0,...) {
  eta0 <- neg_log_lik(theta0,...)
  if(!is.finite(eta0)) {
    stop('The negative log-likelihood is not finite for the input initialization vector theta0')
  }

  N <- length(theta0)
  scale0 <- 1e-6 # The baseline scale to start with
  big_rescale <- 10
  sml_rescale <- 2
  max_temp <- 1e+2
  min_temp <- 1e-1
  temp_ratio <- 0.75
  # final temp likely slightly smaller than min_temp
  num_temp <- 1 + ceiling(log(min_temp/max_temp)/log(temp_ratio))
  temp_vect <- max_temp*temp_ratio^(0:(num_temp-1))
  
  num_samp_titrate <- 100 # For finding the scale
  num_samp <- 5000 # For sampling at a temperature

  # The target acceptance ratio, motivated by Gelman et al. 1996 -- Efficient
  # Metropolis Jumping Rules
  alpha_target <- 0.23

  theta <- theta0
  prop_scale <- scale0
  sampList <- list()
  theta_best <- theta0
  eta_best <- eta0
  eta_vect <- c()
  for(nt in 1:length(temp_vect)) {
    temp <- temp_vect[nt]
    # First, "hone in" on a scaling of the proposal distribution that achieves
    # an acceptance probability near alpha_target
    done <- F
    counter <- 0
    while(!done) {
      samp <- yada_sample_mcmc(neg_log_lik,
                               theta,
                               num_samp_titrate,
                               temp,
                               prop_scale,...)
      if(samp$eta_best < eta_best) {
        eta_best   <- samp$eta_best
        theta_best <- samp$theta_best
      }
      alpha <- mean(samp$accept_vect)
      if(alpha == 1) {
        prop_scale <- prop_scale*big_rescale
      } else if(alpha == 0) {
        prop_scale <- prop_scale/big_rescale
      } else {
        noise <- sqrt(alpha*(1-alpha)/num_samp_titrate)
        if(abs(alpha-alpha_target) >= noise) {
          done <- T
        } else {
          if(alpha >= alpha_target) {
            prop_scale <- prop_scale*sml_rescale
          } else {
            prop_scale <- prop_scale/sml_rescale
          }
        }
      }
    }
    counter <- counter + 1
    if(counter == 100) {
      done <- T
    }
    samp <- yada_sample_mcmc(neg_log_lik,theta,num_samp,temp,prop_scale,...)
    if(samp$eta_best < eta_best) {
      eta_best   <- samp$eta_best
      theta_best <- samp$theta_best
    }
    theta_mat   <- matrix(NA,num_samp,length(theta))
    theta <- samp$theta_mat[nrow(samp$theta_mat),]
    eta_vect <- c(eta_vect,samp$eta_vect)
  }

  return(list(eta_best=eta_best,theta_best=theta_best,eta_vect=eta_vect))
}
