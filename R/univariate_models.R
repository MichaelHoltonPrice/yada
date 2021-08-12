#' @title
#' Get the number of parameters for named variables of a univariate, ordinal 
#' model
#'
#' @description
#' Get the number of parameters for named variables of a univariate, ordinal
#' model (b, tau, or beta)
#'
#' @param var_name The variable name (b, tau, or beta)
#' @param mod_spec The model specification
#'
#' @return The number of parameters
#'
#' @export
get_num_var_univariate_ord <- function(var_name,mod_spec) {
  if(var_name == 'b') {
    if( !(mod_spec$mean_spec %in% c('pow_law_ord','log_ord','lin_ord')) &&
      !(mod_spec$mean_spec %in% c(0,1,2))) {
     stop(paste0('Unrecognized mean_spec for an ordinal variable, ',
                 mod_spec$mean_spec))
    }
    return(get_num_var_mean(mod_spec$mean_spec))
  } else if(var_name == 'tau') {
    return(mod_spec$M)
  } else if(var_name == 'beta') {
    return(get_num_var_noise(mod_spec$noise_spec))
  } else {
    stop(paste0('Unrecognized variable name for an ordinal variable, ',
                var_name))
  }
}

#' @title
#' Get the number of parameters for named variables of a univariate, continuous
#' model
#'
#' @description
#' Get the number of parameters for named variables of a univariate, continuous
#' model (either c or kappa)
#'
#' @param var_name The variable name (either c or kappa)
#' @param mod_spec The model specification
#'
#' @return The number of parameters
#'
#' @export
get_num_var_univariate_cont <- function(var_name,mod_spec) {
  if(var_name == 'c') {
    if(mod_spec$mean_spec != 'pow_law' && mod_spec$mean_spec != 3) {
     stop(paste0('Unrecognized mean_spec for a continuous variable, ',
                 mod_spec$mean_spec))
    }
    return(get_num_var_mean(mod_spec$mean_spec))
  } else if(var_name == 'kappa') {
    return(get_num_var_noise(mod_spec$noise_spec))
  } else {
    stop(paste0('Unrecognized variable name for a continuous variable, ',
                var_name))
  }
}

#' @title
#' Get the variable indices for a univariate, ordinal model given the variable
#' name
#'
#' @description
#' Get the variable indices for a univariate, ordinal model given the variable
#' name (b, tau, or beta)
#'
#' @param var_name The variable name (b, tau, or beta)
#' @param mod_spec The model specification
#'
#' @return The variable indices
#'
#' @export
get_var_index_univariate_ord <- function(var_name,mod_spec) {
  if(var_name == 'b') {
    num_preceding = 0
  } else if(var_name == 'tau') {
    num_preceding = get_num_var_univariate_ord('b',mod_spec)
  } else if(var_name == 'beta') {
    num_preceding = get_num_var_univariate_ord('b',mod_spec) +
      get_num_var_univariate_ord('tau',mod_spec)
  } else {
    stop(paste0('Unrecognized variable name for an ordinal variable, ',var_name))
  }
  num_param = get_num_var_univariate_ord(var_name,mod_spec)
  if(num_param == 0) {
    return(c())
  }
  return(num_preceding + 1:get_num_var_univariate_ord(var_name,mod_spec))
}

#' @title
#' Get the variable indices for a univariate, continuous model given the
#' variable name
#'
#' @description
#' Get the variable indices for a univariate, continuous model given the
#' variable name (either c or kappa)
#'
#' @param var_name The variable name (either c or kappa)
#' @param mod_spec The model specification
#'
#' @return The variable indices
#'
#' @export
get_var_index_univariate_cont <- function(var_name,mod_spec) {
  if(var_name == 'c') {
    num_preceding = 0
  } else if(var_name == 'kappa') {
    num_preceding = get_num_var_univariate_cont('c',mod_spec)
  } else {
    stop(paste0('Unrecognized variable name for a continuous variable, ',
                var_name))
  }
  return(num_preceding + 1:get_num_var_univariate_cont(var_name,mod_spec))
}

#' @title Calculate the noise for a univariate ordinal model
#'
#' @description
#' Calculate the noise, gamma, for a univariate ordinal model by extracting beta
#' from th_v and calling calc_noise.
#'
#' @param x The vector of independent variables
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param mod_spec The model specification
#'
#' @return The noise, gamma(x,beta), which is the same length as x
#'
#' @export
calc_noise_univariate_ord <- function(x,th_v,mod_spec) {
  beta <- th_v[get_var_index_univariate_ord('beta',mod_spec)]
  gamma <- calc_noise(x,mod_spec$noise_spec,beta)
  return(gamma)
}

#' @title Calculate the noise for a univariate continuous model
#'
#' @description
#' Calculate the noise, psi, for a univariate continuous model by extracting
#' beta from th_w and calling calc_noise.
#'
#' @param x The vector of independent variables
#' @param th_w The continuous parameter vector [c,kappa]
#' @param mod_spec The model specification
#'
#' @return The noise, psi(x,kappa), which is the same length as x
#'
#' @export
calc_noise_univariate_cont <- function(x,th_w,mod_spec) {
  kappa <- th_w[get_var_index_univariate_cont('kappa',mod_spec)]
  psi <- calc_noise(x,mod_spec$noise_spec,kappa)
  return(psi)
}

#' @title Calculate the mean for a univariate ordinal model
#'
#' @description
#' Calcualte the mean, g, for a univariate ordinal model by extracting b from
#' th_v and calling calc_mean.
#'
#' @param x The vector of independent variables
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param mod_spec The model specification
#'
#' @return The mean, g(x,b), which is the same length as x
#'
#' @export
calc_mean_univariate_ord <- function(x,th_v,mod_spec) {
  b <- th_v[get_var_index_univariate_ord('b',mod_spec)]
  g <- calc_mean(x,mod_spec$mean_spec,b)
  return(g)
}

#' @title Calculate the mean for a univariate continuous model
#'
#' @description
#' Calculate the mean, h, for a univariate continuous model by extracting c from
#' th_w and calling calc_mean.
#'
#' @param x The vector of independent variables
#' @param th_w The continuous parameter vector [c,kappa]
#' @param mod_spec The model specification
#'
#' @return The mean, h(x,c), which is the same length as x
#'
#' @export
calc_mean_univariate_cont <- function(x,th_w,mod_spec) {
  c <- th_w[get_var_index_univariate_cont('c',mod_spec)]
  h <- calc_mean(x,mod_spec$mean_spec,c)
  return(h)
}

#' @title
#' Calculate the vector of negative log-likelihoods for an ordinal model
#'
#' @description
#' Calculate the vector of negative log-likelihoods for an ordinal model
#'
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param x The vector of independent variables
#' @param v The vector of ordinal responses
#' @param mod_spec The model specification
#' @param tf_cat_vect (Optional) The transformation category vector to transform
#'   th_v to a constrained representation
#'
#' @return The vector of negative log-likelihoods, which is the same length as x
#'
#' @export
calc_neg_log_lik_vect_ord <- function(th_v,x,v,mod_spec,tf_cat_vect=NA) {
  # th_v has ordering [b,tau,beta]
  # eta_v is the negative log-likelihood
  # For optimization, make th_v the first input

  # Get the number of observations and check that x and v are the same length
  N <- length(x)
  if(N != length(v)) {
    stop('Input vectors x and v do not match in length')
  }

  if(!all(is.na(tf_cat_vect))) {
    th_v <- param_unconstr_to_constr(th_v,tf_cat_vect)
  }

  g     <- calc_mean_univariate_ord (x,th_v,mod_spec)
  gamma <- calc_noise_univariate_ord(x,th_v,mod_spec)

  tau <- th_v[get_var_index_univariate_ord('tau',mod_spec)]
  # Initialize vector of negative log-likelihoods (eta_v)
  eta_v <- rep(NA,N)

  # Loop over m values to calculate eta_v
  for(m in 0:mod_spec$M) {
    indm <- v == m
    if(sum(indm) > 0 ) {
      if(m==0) {
        Phi_lo <- 0
      } else {
        Phi_lo <- pnorm( (tau[m]- g[indm])/gamma[indm] )
      }

      if(m==mod_spec$M) {
        Phi_hi <- 1
      } else {
        Phi_hi <- pnorm( (tau[m+1]- g[indm])/gamma[indm] )
      }
      eta_v[indm] <- -log(Phi_hi - Phi_lo)
    }
  }

  # Handle x=0 with m=0 if the mean_spec is log_ord
  if(mod_spec$mean_spec == 'log_ord') {
    ind <- (x == 0) & (v == 0)
    if(sum(ind) > 0) {
      eta_v[ind] <- 0
    }
  }
  return(eta_v)
}

#' @title
#' Calculate the negative log-likelihood for an ordinal model
#'
#' @description
#' Calculate the negative log-likelihood for an ordinal model
#'
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param x The vector of independent variables
#' @param v The vector of ordinal responses
#' @param mod_spec The model specification
#' @param tf_cat_vect (Optional) The transformation category vector to transform
#'   th_v to a constrained representation
#'
#' @return The negative log-likelihood
#'
#' @export
calc_neg_log_lik_ord <- function(th_v,x,v,mod_spec,tf_cat_vect=NA) {
  return(sum(calc_neg_log_lik_vect_ord(th_v,x,v,mod_spec,tf_cat_vect)))
}

#' @title
#' Calculate the vector of negative log-likelihoods for a continuous model
#'
#' @description
#' Calculate the vector of negative log-likelihoods for a continuous model
#'
#' @param th_v The continuous parameter vector [c,kappa]
#' @param x The vector of independent variables
#' @param w The vector of continuous responses
#' @param mod_spec The model specification
#' @param tf_cat_vect (Optional) The transformation category vector to transform
#'   th_w to a constrained representation
#'
#' @return The vector of negative log-likelihoods, which is the same length as x
#'
#' @export
calc_neg_log_lik_vect_cont <- function(th_w,x,w,mod_spec,tf_cat_vect=NA) {
  # Get the number of observations and check that x and w are the same length
  N <- length(x)
  if(N != length(w)) {
    stop('Input vectors x and w do not match in length')
  }

  if(!all(is.na(tf_cat_vect))) {
    th_w <- param_unconstr_to_constr(th_w,tf_cat_vect)
  }

  h     <- calc_mean_univariate_cont (x,th_w,mod_spec)
  psi   <- calc_noise_univariate_cont(x,th_w,mod_spec)
  eta_w <- -dnorm(w,h,psi,log=T)

  return(eta_w)
}

#' @title
#' Calculate the negative log-likelihood for a continuous model
#'
#' @description
#' Calculate the negative log-likelihood for a continuous model
#'
#' @param th_w The continuous parameter vector [c,kappa]
#' @param x The vector of independent variables
#' @param w The vector of continuous responses
#' @param mod_spec The model specification
#' @param tf_cat_vect (Optional) The transformation category vector to transform
#'   th_w to a constrained representation
#'
#' @return The negative log-likelihood
#'
#' @export
calc_neg_log_lik_cont <- function(th_w,x,w,mod_spec,tf_cat_vect=NA) {
  return(sum(calc_neg_log_lik_vect_cont(th_w,x,w,mod_spec,tf_cat_vect)))
}

#' @title Simulate a univariate ordinal model
#'
#' @description
#' Create simulated data for a univariate ordinal model. For x, either the
#' number of samples (N) and a parameter vector (th_x) must be given, or the
#' full vector (x) must be given.
#'
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param mod_spec The model specification
#' @param N The number of samples to simulate
#' @param th_x The parameterization for x
#' @param x The vector of independent variables
#'
#' @return A list object of simulated data containing x, v, and v*
#'
#' @export
sim_univariate_ord <- function(th_v,mod_spec,N=NA,th_x=NA,x=NA) {
  # N is the number of simulated observations
  # th_x parameterizes x. Currently, only a uniform distribution is supported
  # th_w parameterizes w (given x)

  have_x_model  <- !all(is.na(th_x)) && !is.na(N)
  have_x_direct <- !all(is.na(x))

  if(have_x_model + have_x_direct != 1) {
    stop('Either (1) th_x and N should be input or (2) x should be input')
  }

  if(have_x_model) {
    if(th_x$fit_type == 'uniform') {
      x <- runif(N,th_x$xmin,th_x$xmax)
    } else {
      stop('Only uniform currently supported')
    }
  } else {
    N <- length(x)
  }

  g     <- calc_mean_univariate_ord (x,th_v,mod_spec)
  gamma <- calc_noise_univariate_ord(x,th_v,mod_spec)

  vstar <- g + rnorm(N)*gamma

  tau <- th_v[get_var_index_univariate_ord('tau',mod_spec)]
  v <- rep(NA,N)
  for(n in 1:N) {
    v[n] <- as.numeric(cut(vstar[n],c(-Inf,tau,Inf))) - 1 # The ordinal observation
  }
  
  return(list(x=x,v=v,vstar=vstar))
}

#' @title Simulate a univariate ordinal model
#'
#' @description
#' Create simulated data for a univariate continuous model. For x, either the
#' number of samples (N) and a parameter vector (th_x) must be given, or the
#' full vector (x) must be given.
#'
#' @param th_w The continuous parameter vector [c,kappa]
#' @param mod_spec The model specification
#' @param N The number of samples to simulate
#' @param th_x The parameterization for x
#' @param x The vector of independent variables
#'
#' @return A list object of simulated data containing x and w
#'
#' @export
sim_univariate_cont <- function(th_w,mod_spec,N=NA,th_x=NA,x=NA) {
  # N is the number of simulated observations
  # th_x parameterizes x. Currently, only a uniform distribution is supported
  # th_w parameterizes w (given x)

  have_x_model  <- !all(is.na(th_x)) && !is.na(N)
  have_x_direct <- !all(is.na(x))

  if(have_x_model + have_x_direct != 1) {
    stop('Either (1) th_x and N should be input or (2) x should be input')
  }

  if(have_x_model) {
    if(th_x$fit_type == 'uniform') {
      x <- runif(N,th_x$xmin,th_x$xmax)
    } else {
      stop('Only uniform currently supported')
    }
  } else {
    N <- length(x)
  }

  h     <- calc_mean_univariate_cont (x,th_w,mod_spec)
  psi   <- calc_noise_univariate_cont(x,th_w,mod_spec)

  w <- h + rnorm(N)*psi

  return(list(x=x,w=w))
}

#' @title
#' Get the tranform categories for a univariate ordinal model
#'
#' @description
#' For a given model specification of a univariate ordinal variable, return a
#' vector that gives the categories used by param_constr2uncontr and
#' param_constr_to_unconstr.
#'
#' @param mod_spec The model specification
#'
#' @return A vector of categories
#'
#' @export
get_univariate_ord_transform_categories <- function(mod_spec) {
  catVectMean  <- get_mean_transform_categories( mod_spec$mean_spec)
  catVectTau   <- c(0,rep(3,mod_spec$M-1))
  catVectNoise <- get_noise_transform_categories(mod_spec$noise_spec)
  return(c(catVectMean,catVectTau,catVectNoise))
}

#' @title
#' Get the tranform categories for a univariate continuous model
#'
#' @description
#' For a given model specification of a univariate continuous variable, return a
#' vector that gives the categories used by param_constr2uncontr and
#' param_constr_to_unconstr.
#'
#' @param mod_spec The model specification
#'
#' @return A vector of categories
#'
#' @export
get_univariate_cont_transform_categories <- function(mod_spec) {
  return(c(get_mean_transform_categories(mod_spec$mean_spec),
           get_noise_transform_categories(mod_spec$noise_spec)))
}

#' @title Initialize a univariate ordinal model
#'
#' @description
#' Initialize the parameter vector for a univariate ordinal model
#'
#' @param x The vector of independent variables
#' @param v The vector of ordinal responses
#' @param mod_spec The model specification
#' @param anneal Whether or not to refine the initialization by annealing
#' @param anneal_seed An optional seed to make annealing reproducible
#'   (default: NA, not used)
#'
#' @return An initialization for the ordinal parameter vector, th_v0
#'
#' @export
init_univariate_ord <- function(x,v,mod_spec,anneal=T,anneal_seed=NA) {
  if(mod_spec$mean_spec == 'pow_law_ord') {
    gmin <- min(x)
    gmax <- max(x)
    b0 <- 1
  } else if(mod_spec$mean_spec == 'log_ord') {
    gmin <- min(log(x[x!=0]))
    gmax <- max(log(x[x!=0]))
    b0 <- c()
  } else if(mod_spec$mean_spec == 'lin_ord') {
    gmin <- min(x)
    gmax <- max(x)
    b0 <- c()
  } else {
    stop(paste0('Unrecognized mean_spec, ',mod_spec$mean_spec))
  }
  base_scale <- gmax - gmin
  tau0 <- gmin + base_scale / (mod_spec$M+1) * (1:mod_spec$M)

  if(mod_spec$noise_spec == 'const') {
    beta0 <- base_scale
  } else if(mod_spec$noise_spec == 'lin_pos_int') {
    beta0 <- c(base_scale,0.001)
  } else {
    stop(paste0('Unrecognized noise_spec, ',mod_spec$noise_spec))
  }

  th_v0 <- c(b0,tau0,beta0)

  if(anneal) {
    if (!is.na(anneal_seed)) {
      set.seed(anneal_seed)
    }
    tf_cat_vect <- get_univariate_ord_transform_categories(mod_spec)
    th_v_bar0 <- param_constr_to_unconstr(th_v0,tf_cat_vect)
    anneal_output <- yada_tailored_annealing(calc_neg_log_lik_ord,
                                             th_v_bar0,
                                             x=x,
                                             v=v,
                                             mod_spec=mod_spec,
                                             tf_cat_vect=tf_cat_vect)
    th_v0 <- param_unconstr_to_constr(anneal_output$theta_best,tf_cat_vect)
  }
  return(th_v0)
}

#' @title Fit a univariate ordinal model
#'
#' @description
#' Fit a univariate ordinal model
#'
#' @param x The vector of independent variables
#' @param v The vector of ordinal responses
#' @param mod_spec The model specification
#' @param anneal Whether or not to refine the initialization by annealing
#' @param req_conv Whether to require convergence of the optimization
#' @param anneal_seed An optional seed to pass to init_univariate_ord to make
#'   annealing reproducible (default: NA, not used)
#'
#' @return The ordinal parameter vector, th_v
#'
#' @export
fit_univariate_ord <- function(x,v,mod_spec,
                               anneal=T,req_conv=T,anneal_seed=NA) {
  # Handle possible missing observations without throwing an error or warning
  ind_bad <- is.na(x) | is.na(v)
  x <- x[!ind_bad]
  v <- v[!ind_bad]
  th_v0 <- init_univariate_ord(x,v,mod_spec,
                               anneal=anneal,anneal_seed=anneal_seed)

  tf_cat_vect <- get_univariate_ord_transform_categories(mod_spec)
  th_v_bar0 <- param_constr_to_unconstr(th_v0,tf_cat_vect)


  optim_control <- list(reltol=1e-12,maxit=100000,ndeps=rep(1e-8,length(th_v_bar0)))
  fit_BFGS <- optim(th_v_bar0,
                    calc_neg_log_lik_ord,
                    method='BFGS',
                    control=optim_control,
                    x=x,
                    v=v,
                    mod_spec=mod_spec,
                    tf_cat_vect=tf_cat_vect)

  if(req_conv && (fit_BFGS$convergence != 0)) {
    stop(paste0('fit did not converge. convergence code = ',
                fit_BFGS$convergence))
  }

  th_v <- param_unconstr_to_constr(fit_BFGS$par,tf_cat_vect)
  return(th_v)
}

#' @title Fit a univariate continuous model
#'
#' @description
#' Fit a univariate continuous model
#'
#' @param x The vector of independent variables
#' @param w The vector of continuous responses
#' @param mod_spec The model specification
#' @param req_conv Whether to require convergence of the optimization
#'
#' @return The continuous parameter vector, th_w
#'
#' @export
fit_univariate_cont <- function(x,w,mod_spec,req_conv=T) {
  # Handle possible missing observations without throwing an error or warning
  ind_bad <- is.na(x) | is.na(w)
  x <- x[!ind_bad]
  w <- w[!ind_bad]

  if(mod_spec$mean_spec != 'pow_law') {
    stop('For a continuous variable, the mean_spec must be pow_law')
  }

  # Initialize parameters
  c1 <- 1 # initialize linear in x
  c2 <- diff(range(w))/diff(range(x))
  c3 <- min(w)
  base_scale <- diff(range(w))/2

  if(mod_spec$noise_spec == 'const') {
    kappa <- c(base_scale)
  } else if(mod_spec$noise_spec == 'lin_pos_int') {
    kappa <- c(base_scale,0.0001)
  } else {
    stop(paste0('Unrecognized noise_spec, ',mod_spec$noise_spec))
  }

  th_w0 <- c(c1,c2,c3,kappa)
  tf_cat_vect <- get_univariate_cont_transform_categories(mod_spec)
  th_w_bar0 <- param_constr_to_unconstr(th_w0,tf_cat_vect)

  optim_control <- list(reltol=1e-12,
                        maxit=100000,
                        ndeps=rep(1e-8,length(th_w_bar0)))
  fit <- optim(th_w_bar0,
               calc_neg_log_lik_cont,
               method='BFGS',
               control=optim_control,
               x=x,
               w=w,
               mod_spec=mod_spec,
               tf_cat_vect=tf_cat_vect)
  if(req_conv && (fit$convergence != 0)) {
    stop(paste0('fit did not converge. convergence code = ',fit$convergence))
  }

  th_w <- param_unconstr_to_constr(fit$par,tf_cat_vect)

  return(th_w)
}

#' @title
#' Calculate the likelihood for a univariate ordinal model as a function of x
#'
#' @description
#' Calculate the probability of measuring the response m as a function of x for
#' a univariate ordinal model
#'
#' @param x The vector of independent variables
#' @param th_v The ordinal parameter vector [b,tau,beta]
#' @param m The ordinal response value
#' @param mod_spec The model specification
#' @return The vector for probabilities, which has the same length as x
#' @export
calc_q <- function(x,th_v,m,mod_spec) {
  return(exp(-calc_neg_log_lik_vect_ord(th_v,x,rep(m,length(x)),mod_spec)))
}

#' @title Calculate bin probabilities for a univariate ordinal dataset
#'
#' @description
#' x is the independent variable and v is the response variable for univariate
#' ordinal data. Bin the data by x, and calculate the proportion of
#' observations falling into each ordinal category for each bin.
#'
#' The input bin_bounds is optional. If it is not provided, 20 evenly spaced
#' bins on the interval min(x) to max(x) are used.
#'
#' The output is a list containing three variables:
#'
#' bin_centers -- The x-values of the bin centers
#' bin_counts -- A matrix with dimenesions num_bins by num_cat giving the
#'   counts of the ordinal variable (v) for each bin and category
#' bin_prop -- A matrix with dimenesions num_bins by num_cat giving the
#'   probabilities of the ordinal variable (v) for each bin and category (a
#'   normalized version of bin_counts)
#'
#' @param x The vector of independent variables
#' @param v The vector of responses (m-values)
#' @param bin_bounds (Optional) The bounds to use for binning. If not given, 20
#'   evenly spaced bins on the interval min(x) to max(x) are used.
#'
#' @return A list with the bin centers, bin counts, and bin proportions
#'
#' @export
calc_bin_prob <- function(x, v, bin_bounds=NA) {
  if(all(is.na(bin_bounds))) {
    bin_bounds <- seq(min(x), max(x), len=21)
  }
  num_bins <- length(bin_bounds) - 1
  
  num_cat <- length(unique(v)) # number of categories
  bin_counts <- matrix(NA, num_bins, num_cat)
  bin_centers <- (bin_bounds[1:num_bins] + bin_bounds[2:(num_bins+1)])/2

  for(b in 1:num_bins) {
    if(b < num_bins) {
      ind_b <- bin_bounds[b] <= x & x <  bin_bounds[b+1]
    } else {
      ind_b <- bin_bounds[b] <= x & x <= bin_bounds[b+1]
    }
    xb <- x[ind_b]
    vb <- v[ind_b]
    for(m in 0:(num_cat-1)) {
      bin_counts[b, m+1] <- sum(vb == m)
    }
  }

  bin_prop <- matrix(NA, num_bins, num_cat) # bin proportions
  for(b in 1:num_bins) {
    bin_prop[b,] <- bin_counts[b,] / sum(bin_counts[b,])
  }

  return(list(bin_centers=bin_centers,
              bin_counts=bin_counts,
              bin_rop=bin_prop))
}

#' @title
#' Get the approximate range of x-values given a single, known response value
#' for an ordinal variable
#'
#' @description
#' In order to sample efficiently from the posterior density (and, ultimately,
#' to calculate the mean age and confidence intervals), it is helpful to have an
#' approximate estimate for the range of x-values that may be spanned by the
#' posterior density. get_approx_x_range_ord provides a rough estimate of that
#' range using only the likelihood (that is, it does not use the prior, which is
#' usually far flatter than the likelihood; the range itself is also an estimate
#' for the range implied by the likelihood).
#'
#' @param v The response (a single value, not a vector)
#' @param th_v The parameter vector
#' @param mod_spec The model specification
#'
#' @return An approximate range for x
#'
#' @export
get_approx_x_range_ord <- function(v,th_v,mod_spec) {

  if (length(v) != 1) {
    stop("v should be a single value, not a vector")
  }

  mean_spec <- mod_spec$mean_spec
  if(is.numeric(mean_spec)) {
    mean_spec <- mean_spec_int_to_str(mean_spec)
  }

  if (! mean_spec %in% c("pow_law_ord","log_ord","lin_ord")) {
    stop(paste0('Unrecognized mean_spec for an ordinal variable, ',
                mod_spec$mean_spec))
  }

  b   <- th_v[get_var_index_univariate_ord("b",mod_spec)]
  tau <- th_v[get_var_index_univariate_ord("tau",mod_spec)]
  M <- mod_spec$M
  if (v == 0) {
    xlo <- 0
    if (mod_spec$mean_spec == "pow_law_ord") {
      xhi <- tau[1]^(1/b)
    } else if (mod_spec$mean_spec == "log_ord") {
      xhi <- exp(tau[1])
    } else {
      # lin_ord
      xhi <- tau[1]
    }
  } else if(v == M) {
    if (mod_spec$mean_spec == "pow_law_ord") {
      xlo <- tau[M]^(1/b)
    } else if(mod_spec$mean_spec == "log_ord") {
      xlo <- exp(tau[M])
    } else {
      # lin_ord
      xlo <- tau[M]
    }

    if (M == 1) {
      xprev <- 0
    } else {
      if (mod_spec$mean_spec == "pow_law_ord") {
        xprev <- tau[M-1]^(1/b)
      } else if(mod_spec$mean_spec == "log_ord") {
        xprev <- exp(tau[M-1])
      } else {
        # lin_ord
        xprev <- tau[M-1]
      }
    }
    xhi <- xlo + (xlo - xprev)
  } else {
    # v is neither 0 nor M
    if (mod_spec$mean_spec == "pow_law_ord") {
      xlo <- tau[v  ]^(1/b)
      xhi <- tau[v+1]^(1/b)
    } else if (mod_spec$mean_spec == "log_ord") {
      xlo <- exp(tau[v  ])
      xhi <- exp(tau[v+1])
    } else {
      # lin_ord
      xlo <- tau[v  ]
      xhi <- tau[v+1]
    }
  }
  return(c(xlo,xhi))
}

#' @title
#' Get the approximate range of x-values given a single, known response value
#' for a continuous variable
#'
#' @description
#' In order to sample efficiently from the posterior density (and, ultimately,
#' to calculate the mean age and confidence intervals), it is helpful to have an
#' approximate estimate for the range of x-values that may be spanned by the
#' posterior density. get_approx_x_range_cont provides a rough estimate of that
#' range using only the likelihood (that is, it does not use the prior, which is
#' usually far flatter than the likelihood; the range itself is also an estimate
#' for the range implied by the likelihood).
#'
#' @param w The response (a single value, not a vector)
#' @param th_w The parameter vector
#' @param mod_spec The model specification
#'
#' @return An approximate range for x
#'
#' @export
get_approx_x_range_cont <- function(w,th_w,mod_spec) {

  if (length(w) != 1) {
    stop("w should be a single value, not a vector")
  }

  mean_spec <- mod_spec$mean_spec
  if(is.numeric(mean_spec)) {
    mean_spec <- mean_spec_int_to_str(mean_spec)
  }

  if (! mean_spec %in% c("pow_law")) {
    stop(paste0('Unrecognized mean_spec for a continuous variable, ',
                mod_spec$mean_spec))
  }

  ind_c    <- get_var_index_univariate_cont("c",mod_spec)
  ind_kappa <- get_var_index_univariate_cont("kappa",mod_spec)
  c1 <- th_w[ind_c[1]]
  c2 <- th_w[ind_c[2]]
  c3 <- th_w[ind_c[3]]

  if (w-c3 <= 0) {
    xmiddle <- 0
  } else {
    xmiddle <- ((w-c3)/c2)^(1/c1)
  }

  psi <- calc_noise(xmiddle,mod_spec$noise_spec,th_w[ind_kappa])

  xlo <- xmiddle - psi
  xhi <- xmiddle + psi

  if (xlo < 0) {
    xlo <- 0
  }

  return(c(xlo,xhi))
}

#' @title Calculate ordinal model confidence intervals
#' 
#' @description Wrapper function that takes an ordinal model and outputs the 
#' point estimate and 95% and 99% confidence intervals for each ordinal stage.
#' To allow reproducibility, an optional random number input seed seed can be
#' provided. The input seed should either be (1) NA [the default], (2) a single
#' integer, or (3) a vector of length M+1, where M is the number of ordinal
#' categories. If no seed is provided, one is drawn and treated as if it (a
#' single integer) was input. If a single integer is provided, it is used to
#' generate a vector of integers with  length M+1. The vector is used to
#' provide seeds for initializing the seed for each call to calc_x_posterior.
#'
#' @param ord_model List containing the parameter vector for y [th_y] and model
#' specification [mod_spec] for an ordinal univariate model (likely the 
#' output of `solve_ord_problem`)
#' @param th_x Parameterization for prior on x
#' @param input_seed An optional seed that can be used to make results
#'   reproducible. The input_seed must be either (1) NA / not provided (the
#'   default), (2) a single integer, or (3) a vector with length M+1 (see
#'   Description for further details).
#' 
#' @export
#'
calc_ci_ord <- function(ord_model, th_x, input_seed=NA) {
  # Handle the random number seeds
  if (length(input_seed) == 1) {
    if (is.na(input_seed)) {
      base_seed <- sample.int(1000000,1)
    } else {
      base_seed <- input_seed
    }
    set.seed(base_seed)
    seed_vect <- sample.int(1000000,ord_model$mod_spec$M+1)
  } else {
    if (length(input_seed) != ord_model$mod_spec$M + 1) {
      stop("If input_seed is a vector, it must have length M+1")
    }
    seed_vect <- input_seed
  }

  # Initialize matrix (M+1 x 6)
  ci_mat <- matrix(nrow=(ord_model$mod_spec$M+1), ncol=6)
  
#  # Add cdep_spec to univariate mod_spec?
#  ord_model$mod_spec$cdep_spec <- 'indep'

  # Run through each ordinal stage, calculating the posterior density,
  # point estimate, 95%, and 99% confidence intervals and populating ci_mat
  for(m in 0:ord_model$mod_spec$M) {
    x_post <- calc_x_posterior(y=m, th_x, ord_model$th_y,
                               ord_model$mod_spec, seed=seed_vect[m+1])
    xcalc <- x_post$x
    x_analyze <- analyze_x_posterior(xcalc, x_post$density)

    ci_mat[m+1,1] <- m  # ordinal stage
    ci_mat[m+1,2] <- round(x_analyze$xmean, 2)  # point estimate
    ci_mat[m+1,3] <- round(x_analyze$xlo, 2)  # lower 95
    ci_mat[m+1,4] <- round(x_analyze$xhi, 2)  # upper 95
    ci_mat[m+1,5] <- round(x_analyze$xlolo, 2) # lower 99
    ci_mat[m+1,6] <- round(x_analyze$xhihi, 2) # upper 99
  }

  # Format ci_mat as final data frame
  ci_df <- as.data.frame(ci_mat)
  colnames(ci_df) <- c("ord_stage", "point_est", "CI95_lower",
                       "CI95_upper", "CI99_lower", "CI99_upper")
  
  return(ci_df)
}
