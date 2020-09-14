#' Given an input negative log-likelihood and temperature, do MCMC sampling of
#' the parameter vector.
#'
#' @param neg_log_lik The function for the negative log-likelihood
#' @param theta0 The initial value of the parameter vector
#' @param numSamp The number of samples to make
#' @param temp The temperature
#' @param propScale A scalar value for the standard deviation of the proposal distribution
#' @param ... Variables required by neg_log_lik
#' @return A list with sampling information
#' @export
yada_sample_mcmc <- function(neg_log_lik,theta0,numSamp,temp,propScale,...) {
  if(length(propScale) != 1) {
    stop('propScale should be a scalar')
  }

  eta0 <- neg_log_lik(theta0,...)
  if(!is.finite(eta0)) {
    stop('The negative log-likelihood is not finite for the input initialization vector theta0')
  }

  theta <- theta0
  eta   <- eta0

  theta_best <- theta
  eta_best   <- eta

  acceptVect <- rep(NA,numSamp)
  etaVect    <- rep(NA,numSamp)
  thetaMat   <- matrix(NA,numSamp,length(theta))
  for(n in 1:numSamp) {
    theta_prop <- theta + rnorm(length(theta))*propScale
    eta_prop <- neg_log_lik(theta_prop,...)

    if(!is.finite(eta_prop)) {
      accept <- F
    } else {
      alpha <- min(1,exp(-(eta_prop-eta)/temp))
      accept <- runif(1) < alpha
    }

    acceptVect[n] <- accept
    if(!accept) {
      theta_prop <- theta
      eta_prop   <- eta
    } else {
      if(eta_prop < eta_best) {
        theta_best <- theta_prop
        eta_best   <- eta_prop
      }
    }
    etaVect[n] <- eta_prop
    thetaMat[n,] <- theta_prop

    # Get ready for next sample
    theta <- theta_prop
    eta   <- eta_prop
  }
  output <- list(theta0=theta0,eta0=eta0,temp=temp,propScale=propScale,theta_best=theta_best,eta_best=eta_best,acceptVect=acceptVect,etaVect=etaVect,thetaMat=thetaMat)
  return(output)
}

#' A tailored optimization that works well for univariate models. While this
#' tailored optimzation usually rrequires more computations of the negative
#' log-likelihood function compared to other algorithms (e.g., directly calling
#' stats::optim with method='BFGS'), experience has shown that other algorithms
#' do not always find the minimum of the negative log-likelihood. Put slightly
#' differently, this tailored algorithm trades off number of computations for
#' improved robustness, which is likely almost always a trade-off the user
#' desires. The optimization consists of two steps. First, simulated annealing
#' is used, with an initial step at each temperature to set the standard
#' deviation of the (normal) proposal distribution to achieve an acceptance
#' ratio of about 0.23. Second, stats::optim is called using method='BFGS' to
#' refine the solution found by the simulated annealing.
#'
#' @param neg_log_lik The function for the negative log-likelihood
#' @param theta0 The initial value of the parameter vector
#' @param ... Variables required by neg_log_lik
#' @return A list with sampling information and optimization results
#' @export
yada_tailored_optim <- function(neg_log_lik,theta0,...) {
  eta0 <- neg_log_lik(theta0,...)
  if(!is.finite(eta0)) {
    stop('The negative log-likelihood is not finite for the input initialization vector theta0')
  }

  N <- length(theta0)
  scale0 <- 1e-6 # The baseline scale to start with
  bigRescale <- 10
  smlRescale <- 2
  tempMax <- 1e2
  tempMin <- 1e0
  tempRatio <- 0.95
  numTemp <- 1 + ceiling(log(tempMin/tempMax)/log(tempRatio)) # final temp likely slightly smaller than tempMin
  tempVect <- tempMax*tempRatio^(0:(numTemp-1))
  
  numSampTitrate <- 100 # For finding the scale
  numSamp <- 1000 # For sampling at a temperature

  # The target acceptance ratio
  alphaTarget <- 0.23 # motivated by Gelman et al. 1996 -- Efficient Metropolis Jumping Rules

  theta <- theta0
  propScale <- scale0
  sampList <- list()
  theta_best <- theta0
  eta_best <- eta0
  etaVect <- c()
  for(nt in 1:length(tempVect)) {
    temp <- tempVect[nt]
    # First, "hone in" on a scaling of the proposal distribution that achieves
    # an acceptance probability near alphaTarget
    done <- F
    counter <- 0
    while(!done) {
      samp <- yada_sample_mcmc(neg_log_lik,theta,numSampTitrate,temp,propScale,...)
      if(samp$eta_best < eta_best) {
        eta_best   <- samp$eta_best
        theta_best <- samp$theta_best
      }
      alpha <- mean(samp$acceptVect)
      if(alpha == 1) {
        propScale <- propScale*bigRescale
      } else if(alpha == 0) {
        propScale <- propScale/bigRescale
      } else {
        noise <- sqrt(alpha*(1-alpha)/numSampTitrate)
        if(abs(alpha-alphaTarget) >= noise) {
          done <- T
        } else {
          if(alpha >= alphaTarget) {
            propScale <- propScale*smlRescale
          } else {
            propScale <- propScale/smlRescale
          }
        }
      }
    }
    counter <- counter + 1
    if(counter == 100) {
      done <- T
    }
    samp <- yada_sample_mcmc(neg_log_lik,theta,numSamp,temp,propScale,...)
    if(samp$eta_best < eta_best) {
      eta_best   <- samp$eta_best
      theta_best <- samp$theta_best
    }
    thetaMat   <- matrix(NA,numSamp,length(theta))
    theta <- samp$thetaMat[nrow(samp$thetaMat),]
    etaVect <- c(etaVect,samp$etaVect)
  }

  # Refine with a call to optim
  optimControl <- list(reltol=1e-12,maxit=100000,ndeps=rep(1e-8,length(theta_best)))
  fitBFGS <- optim(theta_best,neg_log_lik,method='BFGS',control=optimControl,...)
  
  return(list(eta_best_mcmc=eta_best,theta_best_mcmc=theta_best,etaVect=etaVect,fitBFGS=fitBFGS))
}
