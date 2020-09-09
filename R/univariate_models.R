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
#calc_neg_log_lik_vect_ord <- function(th_v,x,v,modSpec) {
#  # TODO: Implement variable transformations
#  # th_v has ordering [b,tau,beta]
#  # eta_v is the negative log-likelihood
#  # For optimization, make th_v the first input
#  tau <- th_v[get_var_index_univariate_ord('tau',modSpec)]
#
#  M <- modSpec$M
#
#  rho <- th_v[1]           # rho
#  tau <- th_v[2:(M+1)]     # tau_1 ... tau_M
#  s   <- th_v[M+2]         # s
#
#  if(haveKappa) {
#    if(noiseSpec %in% c('linear','linear_pos_int')) {
#      kappa <- th_v[M+3]     # kappa
#    } else if(noiseSpec == 'hyperb') {
#      kappa1 <- th_v[M+3]    # kappa1 (y0)
#      kappa2 <- th_v[M+4]    # kappa1 (sigma0)
#    } else {
#      stop(paste0('Unrecognized noiseSpec with kappa, ',noiseSpec))
#    }
#  }
#
#  # Initialize vector of negative log-likelihoods (eta_v)
#  N <- length(x)
#  if(N != length(v)) {
#    stop('Input vectors x and v do not match in length')
#  }
#  eta_v <- rep(NA,N)
# 
#  for(m in 0:M) { 
#    ind <- v == m
#    if(sum(ind) > 0 ) {
#      xm <- x[ind]
#      if(noiseSpec == 'const') {
#        sig <- s 
#      } else if(noiseSpec == 'linear') {
#        sig <- s*xm + kappa
#      } else if(noiseSpec == 'linear_pos_int') {
#        sig <- s*(1+kappa*xm)
#      } else if(noiseSpec == 'linear_no_int') {
#        sig <- s*xm
#      } else if(noiseSpec == 'hyperb') {
#        sig <- (s/2)*(sqrt((xm+kappa1/s)^2 + 4*kappa2*(kappa2 - kappa1)/s^2) + xm + kappa1/s)
#      } else {
#        stop(paste('Unrecognized noiseSpec,',noiseSpec))
#      }
#
#      if(m==0) {
#        Phi_lo <- 0
#      } else {
#        Phi_lo <- pnorm( (tau[m]- xm^rho)/sig )
#      }
#
#      if(m==M) {
#        Phi_hi <- 1
#      } else {
#        Phi_hi <- pnorm( (tau[m+1]- xm^rho)/sig )
#      }
#
#      eta_v[ind] <- - log(Phi_hi - Phi_lo)
#    }
#  }
#
#  return(eta_v)
#}

