###' Calculate the negative log-likelihood, eta_w, for a univariate continuous
###' model.
###'
###' @param x The independent variable
###' @param w The continuous response
###' @param th_w The parameter vector
###' @param modSpec The model specification
###' @return The negative log-likelihood, eta_w
###calc_cont_neg_log_lik <- function(x,w,th_w,modSpec) {
#
#  kappa <- th_w[get_var_index('alpha',modSpec,i=1)]
#  cc    <- th_w[get_var_index,'a',i=1] # use cc instead of c since is defined for base R
#  return(noiseSpec_str)
#}

#' Get the number of parameters for named variables of a univariate, continuous
#' model
#'
#' @param varName The variable name (either c or kappa)
#' @param modSpec The model specification
#' @return The number of parameters
#' @export
get_num_var_univariate_cont <- function(varName,modSpec) {
  if(varName == 'c') {
    if(modSpec$meanSpec != 'powLaw' && modSpec$meanSpec != 3) {
     stop(paste0('Unrecognized meanSpec for a continuous variable, ',modSpec$meanSpec))
    }
    return(get_num_var_mean(modSpec$meanSpec))
  } else if(varName == 'kappa') {
    return(get_num_var_noise(modSpec$noiseSpec))
  } else {
    stop(paste0('Unrecognized variable name for a continuous variable, ',varName))
  }
}

#' Get the number of parameters for named variables of a univariate, ordinal 
#' model
#'
#' @param varName The variable name (b, tau, or beta)
#' @param modSpec The model specification
#' @return The number of parameters
#' @export
get_num_var_univariate_ord <- function(varName,modSpec) {
  if(varName == 'b') {
    if( !(modSpec$meanSpec %in% c('powLawOrd','logOrd','linOrd')) && !(modSpec$meanSpec %in% c(0,1,2))) {
     stop(paste0('Unrecognized meanSpec for an ordinal variable, ',modSpec$meanSpec))
    }
    return(get_num_var_mean(modSpec$meanSpec))
  } else if(varName == 'tau') {
    return(modSpec$M)
  } else if(varName == 'beta') {
    return(get_num_var_noise(modSpec$noiseSpec))
  } else {
    stop(paste0('Unrecognized variable name for an ordinal variable, ',varName))
  }
}

#' Get the variable indices for a univariate, continuous model given the
#' variable name
#'
#' @param varName The variable name (either c or kappa)
#' @param modSpec The model specification
#' @return The variable indices
#' @export
get_var_index_univariate_cont <- function(varName,modSpec) {
  if(varName == 'c') {
    numPreceding = 0
  } else if(varName == 'kappa') {
    numPreceding = get_num_var_univariate_cont('c',modSpec)
  } else {
    stop(paste0('Unrecognized variable name for a continuous variable, ',varName))
  }
  return(numPreceding + 1:get_num_var_univariate_cont(varName,modSpec))
}

#' Get the variable indices for a univariate, ordinal model given the
#' variable name
#'
#' @param varName The variable name (b, tau, or beta)
#' @param modSpec The model specification
#' @return The variable indices
#' @export
get_var_index_univariate_ord <- function(varName,modSpec) {
  if(varName == 'b') {
    numPreceding = 0
  } else if(varName == 'tau') {
    numPreceding = get_num_var_univariate_ord('b',modSpec)
  } else if(varName == 'beta') {
    numPreceding = get_num_var_univariate_ord('b',modSpec) + get_num_var_univariate_ord('tau',modSpec)
  } else {
    stop(paste0('Unrecognized variable name for an ordinal variable, ',varName))
  }
  numParam = get_num_var_univariate_ord(varName,modSpec)
  if(numParam == 0) {
    return(c())
  }
  return(numPreceding + 1:get_num_var_univariate_ord(varName,modSpec))
}

#' Calcuate the noise, psi, for a univariate continuous model by extracting beta
#' from th_w and calling calc_noise.
#'
#' @param x The vector of independent variables
#' @param th_w The continuous parameter vector [c,kappa]
#' @param modSpec The model specification
#' @return The noise, psi(x,kappa), which is the same length as x
#' @export
calc_noise_univariate_cont <- function(x,th_w,modSpec) {
  kappa <- th_w[get_var_index_univariate_cont('kappa',modSpec)]
  psi <- calc_noise(x,modSpec$noiseSpec,kappa)
  return(psi)
}

#' Calcuate the noise, gamma, for a univariate ordinal model by extracting beta
#' from th_v and calling calc_noise.
#'
#' @param x The vector of independent variables
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param modSpec The model specification
#' @return The noise, gamma(x,beta), which is the same length as x
#' @export
calc_noise_univariate_ord <- function(x,th_v,modSpec) {
  beta <- th_v[get_var_index_univariate_ord('beta',modSpec)]
  gamma <- calc_noise(x,modSpec$noiseSpec,beta)
  return(gamma)
}

#' Calcuate the mean, h, for a univariate continuous model by extracting c from
#' th_w and calling calc_mean.
#'
#' @param x The vector of independent variables
#' @param th_w The continuous parameter vector [c,kappa]
#' @param modSpec The model specification
#' @return The mean, h(x,c), which is the same length as x
#' @export
calc_mean_univariate_cont <- function(x,th_w,modSpec) {
  c <- th_w[get_var_index_univariate_cont('c',modSpec)]
  h <- calc_mean(x,modSpec$meanSpec,c)
  return(h)
}

#' Calcuate the mean, g, for a univariate ordinal model by extracting b from th_v
#' and calling calc_mean.
#'
#' @param x The vector of independent variables
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param modSpec The model specification
#' @return The mean, g(x,b), which is the same length as x
#' @export
calc_mean_univariate_ord <- function(x,th_v,modSpec) {
  b <- th_v[get_var_index_univariate_ord('b',modSpec)]
  g <- calc_mean(x,modSpec$meanSpec,b)
  return(g)
}

#' Calcuate the vector of negative log-likelihoods for an ordinal model
#'
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param x The vector of independent variables
#' @param v The vector of ordinal responses
#' @param modSpec The model specification
#' @param tfCatVect (Optional) The transformation category vector to transform th_v to a constrained representation
#' @return The vector of negative log-likelihoods, which is the same length as x
#' @export
calc_neg_log_lik_vect_ord <- function(th_v,x,v,modSpec,tfCatVect=NA) {
  # th_v has ordering [b,tau,beta]
  # eta_v is the negative log-likelihood
  # For optimization, make th_v the first input

  # Get the number of observations and check that x and v are the same length
  N <- length(x)
  if(N != length(v)) {
    stop('Input vectors x and v do not match in length')
  }

  if(!all(is.na(tfCatVect))) {
    th_v <- param_unconstr2constr(th_v,tfCatVect)
  }

  g     <- calc_mean_univariate_ord (x,th_v,modSpec)
  gamma <- calc_noise_univariate_ord(x,th_v,modSpec)

  tau <- th_v[get_var_index_univariate_ord('tau',modSpec)]
  # Initialize vector of negative log-likelihoods (eta_v)
  eta_v <- rep(NA,N)

  # Loop over m values to calculate eta_v
  for(m in 0:modSpec$M) { 
    indm <- v == m
    if(sum(indm) > 0 ) {
      if(m==0) {
        Phi_lo <- 0
      } else {
        Phi_lo <- pnorm( (tau[m]- g[indm])/gamma[indm] )
      }

      if(m==modSpec$M) {
        Phi_hi <- 1
      } else {
        Phi_hi <- pnorm( (tau[m+1]- g[indm])/gamma[indm] )
      }
      eta_v[indm] <- -log(Phi_hi - Phi_lo)
    }
  }
  return(eta_v)
}

#' Calcuate the negative log-likelihood for an ordinal model
#'
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param x The vector of independent variables
#' @param v The vector of ordinal responses
#' @param modSpec The model specification
#' @param tfCatVect (Optional) The transformation category vector to transform th_v to a constrained representation
#' @return The negative log-likelihood
#' @export
calc_neg_log_lik_ord <- function(th_v,x,v,modSpec,tfCatVect=NA) {
  return(sum(calc_neg_log_lik_vect_ord(th_v,x,v,modSpec,tfCatVect)))
}


#' Calcuate the vector of negative log-likelihoods for a continuous model
#'
#' @param th_v The continuous parameter vector [c,kappa]
#' @param x The vector of independent variables
#' @param w The vector of continuous responses
#' @param modSpec The model specification
#' @param tfCatVect (Optional) The transformation category vector to transform th_w to a constrained representation
#' @return The vector of negative log-likelihoods, which is the same length as x
#' @export
calc_neg_log_lik_vect_cont <- function(th_w,x,w,modSpec,tfCatVect=NA) {
  # Get the number of observations and check that x and w are the same length
  N <- length(x)
  if(N != length(w)) {
    stop('Input vectors x and w do not match in length')
  }

  if(!all(is.na(tfCatVect))) {
    th_w <- param_unconstr2constr(th_w,tfCatVect)
  }

  h     <- calc_mean_univariate_cont (x,th_w,modSpec)
  psi   <- calc_noise_univariate_cont(x,th_w,modSpec)
  eta_w <- -dnorm(w,h,psi,log=T)
  return(eta_w)
}

#' Calcuate the negative log-likelihood for a continuous model
#' @param th_w The continuous parameter vector [c,kappa]
#' @param x The vector of independent variables
#' @param w The vector of continuous responses
#' @param modSpec The model specification
#' @param tfCatVect (Optional) The transformation category vector to transform th_w to a constrained representation
#' @return The negative log-likelihood
#' @export
calc_neg_log_lik_cont <- function(th_w,x,w,modSpec,tfCatVect=NA) {
  return(sum(calc_neg_log_lik_vect_cont(th_w,x,w,modSpec,tfCatVect)))
}

#' Create simulated data for a univariate continuous model. For x, either the
#' number of samples (N) and a parameter vector (th_x) must be given, or the
#' full vector (x) must be given.
#' @param th_w The continuous parameter vector [c,kappa]
#' @param modSpec The model specification
#' @param N The number of samples to simulate
#' @param th_x The parameterization for x
#' @param x The vector of independent variables
#' @return A list object of simulated data containing x and w
#' @export
sim_univariate_cont <- function(th_w,modSpec,N=NA,th_x=NA,x=NA) {
  # N is the number of simulated observations
  # th_x parameterizes x. Currently, only a uniform distribution is supported
  # th_w parameterizes w (given x)

  have_x_model  <- !all(is.na(th_x)) && !is.na(N)
  have_x_direct <- !all(is.na(x))

  if(have_x_model + have_x_direct != 1) {
    stop('Either (1) th_x and N should be input or (2) x should be input')
  }

  if(have_x_model) {
    if(th_x$fitType == 'uniform') {
      x <- runif(N,th_x$xmin,th_x$xmax)
    } else {
      stop('Only uniform currently supported')
    }
  } else {
    N <- length(x)
  }

  h     <- calc_mean_univariate_cont (x,th_w,modSpec)
  psi   <- calc_noise_univariate_cont(x,th_w,modSpec)

  w <- h + rnorm(N)*psi
  
  return(list(x=x,w=w))
}

th_w_constr2unconstr <- function(th_w,modSpec) {
  if(modSpec$meanSpec != 'powLaw') {
    stop('For a continuous variable, the meanSpec must be powLaw')
  }

}


#' For a given model specification of a univariate continuous variable, return a
#' vector that gives the categories used by param_constr2uncontr and
#' param_constr2unconstr.
#'
#' @param modSpec The model specification
#' @return A vector of categories
#' @export
get_univariate_cont_transform_categories <- function(modSpec) {
  return(c(get_mean_transform_categories(modSpec$meanSpec),get_noise_transform_categories(modSpec$noiseSpec)))
}


#' For a given model specification of a univariate ordinal variable, return a
#' vector that gives the categories used by param_constr2uncontr and
#' param_constr2unconstr.
#'
#' @param modSpec The model specification
#' @return A vector of categories
#' @export
get_univariate_ord_transform_categories <- function(modSpec) {
  catVectMean  <- get_mean_transform_categories( modSpec$meanSpec)
  catVectTau   <- c(0,rep(3,modSpec$M-1))
  catVectNoise <- get_noise_transform_categories(modSpec$noiseSpec)
  return(c(catVectMean,catVectTau,catVectNoise))
}

#' Fit a continuous model
#' @param x The vector of independent variables
#' @param w The vector of continuous responses
#' @param modSpec The model specification
#' @param reqConv Whether to require convergence of the optimization
#' @return The continuous parameter vector, th_w
#' @export
fit_univariate_cont <- function(x,w,modSpec,reqConv=T) {

  if(modSpec$meanSpec != 'powLaw') {
    stop('For a continuous variable, the meanSpec must be powLaw')
  }

  # Initialize parameters
  c1 <- 1 # initialize linear in x
  c2 <- diff(range(w))/diff(range(x))
  c3 <- min(w)
  baseScale <- diff(range(w))/2

#  th_w0 <- c(a0,r0,b0,s0)
#  if(haveKappa) {
#    kappa0 <- 0.0001
#    th_w0 <- c(th_w0,kappa0)
#  }

  if(modSpec$noiseSpec == 'const') {
    kappa <- c(baseScale)
  } else if(modSpec$noiseSpec == 'lin_pos_int') {
    kappa <- c(baseScale,0.0001)
  } else if(modSpec$noiseSpec == 'lin_pos_int') {
    kappa <- c(baseScale,baseScale*2,0)
  }
  th_w0 <- c(c1,c2,c3,kappa)

  th_w_bar0 <- theta_y_constr2unconstr(th_w0,modSpec)
  optimControl <- list(reltol=1e-12,maxit=100000,ndeps=rep(1e-8,length(th_w_bar0)))
  fit <- optim(th_w_bar0,powLawNegLogLik,method='BFGS',control=optimControl,x=x,w=w,noiseSpec=noiseSpec,transformVar=T)
  if(reqConv && (fit$convergence != 0)) {
    stop(paste0('fit did not converge. convergence code = ',fit$convergence))
  }

  th_w <- theta_y_unconstr2constr(fit$par,modSpec)

  return(th_w)
}


