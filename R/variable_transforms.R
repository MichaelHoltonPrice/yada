#' Transform a vector from a constrained to an unconstrained representation.
#' The categories are:
#' Category	Description
#' 0		Unconstrained
#' 1		Positive
#' 2		-1 to 1
#' 3		Positive difference
#' 
#' @param param The vector of constrained parameters
#' @param tf_cat_vect The vector of transformation categories
#' @return The vector of unconstrained parameters, param_bar
#' @export
param_constr_to_unconstr <- function(param,tf_cat_vect) {
  ind_other <- (tf_cat_vect < 0) & (3 < tf_cat_vect)
  if(any(ind_other)) {
    stop('Categories must be between 0 and 3')
  }

  param_bar <- rep(NA,length(param))
  for(categ in 0:3) {
    ind <- which(tf_cat_vect == categ)
    if(sum(ind) > 0) {
      if(categ == 0) {
        param_bar[ind] <- param[ind]
      } else if(categ == 1) {
        param_bar[ind] <- log(param[ind])
      } else if(categ == 2) {
        param_bar[ind] <- gtools::logit((param[ind]+1)/2)
      } else {
        # categ = 3
        param_bar[ind] <- log(param[ind] - param[ind-1])
      }
    }
  }
  return(param_bar)
}

#' Transform a vector from an unconstrained to a constrained representation.
#' The categories are:
#' Category	Description
#' 0		Unconstrained
#' 1		Positive
#' 2		-1 to 1
#' 3		Positive difference
#' 
#' @param param_bar The vector of unconstrained parameters
#' @param tf_cat_vect The vector of transformation categories
#' @return The vector of constrained parameters, param
#' @export
param_unconstr_to_constr <- function(param_bar,tf_cat_vect) {
  ind_other <- (tf_cat_vect < 0) & (3 < tf_cat_vect)
  if(any(ind_other)) {
    stop('Categories must be between 0 and 3')
  }

  param <- rep(NA,length(param_bar))
  for(categ in 0:3) {
    ind <- which(tf_cat_vect == categ)
    if(sum(ind) > 0) {
      if(categ == 0) {
        param[ind] <- param_bar[ind]
      } else if(categ == 1) {
        param[ind] <- exp(param_bar[ind])
      } else if(categ == 2) {
        param[ind] <- -1 + 2/(1+exp(-param_bar[ind]))
      } else {
        # categ = 3
        for(n in ind) {
          param[n] <- param[n-1] + exp(param_bar[n])
        }
      }
    }
  }
  return(param)
}

#' Extract the parameter vector for a univariate fit from a larger parameter
#' vector
#'
#' @param th_y The full parameter vector
#' @param mod_spec The full model specification
#' @param j The index of the ordinal variable to extract (default: NA)
#' @param k The index of the ordinal variable to extract (default: NA)
#'
#' @return The univariate parameter vector
#' @export
extract_univariate_param_vect <- function(th_y,mod_spec,j=NA,k=NA) {

  if (is.na(j) && is.na(k)) {
    stop("Either j or k must be specified")
  }

  if (!is.na(j) && !is.na(k)) {
    stop("Both of j and k should not be specified")
  }

  if (!is.na(j)) {
    ind_b    <- get_var_index_multivariate("a",mod_spec,j=j)
    ind_tau  <- get_var_index_multivariate("tau",mod_spec,j=j)
    ind_beta <- get_var_index_multivariate("alpha",mod_spec,j=j)
    return (th_y[c(ind_b,ind_tau,ind_beta)])
  } else {
    ind_c     <- get_var_index_multivariate("a",mod_spec,k=k)
    ind_kappa <- get_var_index_multivariate("alpha",mod_spec,k=k)
    return (th_y[c(ind_c,ind_kappa)])
  }
}
