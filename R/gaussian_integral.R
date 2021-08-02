#' @title
#' Calculate the conditional multivariate Gaussian integral over a
#' rectangular domain
#'
#' @description
#' Calculate the multiviarate Gaussian integral over a rectangular domain where
#' some of the variables are known, and thus conditioned on. In particular,
#' there are two types of variables, dependent and given, and the integral is
#' over dependent variables. The known variables are provided as a vector ygiv,
#' with length K. The bounds of the integral over dependent variables are lo and
#' hi, each of which have length J. The multivariate Gaussian density is
#' specified by a mean vector, mean_vect, of length J+K, and a covariance
#' matrix, cov_mat, with dimensions (J+K) by (J+K). Edge case, such as J=0 and
#' K>0, for which which no integral is needed, are handled.
#'
#' @param mean_vect The vector of means [length J+K]
#' @param cov_mat The covariance matrix [dimension (J+K) by (J+K)]
#' @param lo The lower limit of integration for the dependent variables [length
#'   J]
#' @param hi The upper limit of integration for the dependent variables [length
#'   J]
#' @param y_giv The known values of the conditioned variables [length K]
#' @param log Whether to return the logarithm of the integral (default FALSE)
#'
#' @return The value of the integral
#'
#' @import mvtnorm
#' @import condMVNorm
#'
#' @export
calc_conditional_gaussian_integral <- function(mean_vect,
                                               cov_mat,
                                               lo=c(),
                                               hi=c(),
                                               y_giv=c(),
                                               log=FALSE) {
  J <- length(lo)
  if(J != length(hi)) {
    stop('lo and hi must have the same length')
  }
  K <- length(y_giv)

  if( (J == 0) && (K == 0)) {
    stop('J and K cannot both be zero')
  }

  # If J is 0, no integral is needed
  if(J == 0) {
    if( K == 1 ) {
      # Have exactly one given variable. Call dnorm
      if(log==T) {
        return(dnorm(y_giv,mean=mean_vect,sd=sqrt(cov_mat),log=T))
      } else {
        return(dnorm(y_giv,mean=mean_vect,sd=sqrt(cov_mat),log=F))
      }
    } else {
      # Have more than one given variable. Call dmvnorm
      if(log==T) {
        return(dmvnorm(y_giv,mean=mean_vect,sigma=cov_mat,log=T))
      } else {
        return(dmvnorm(y_giv,mean=mean_vect,sigma=cov_mat,log=F))
      }
    }
  }

  # If K is 0, no conditioning is needed
  if(K == 0) {
    if( J == 1 ) {
      # Have exactly one dependent variable. Call pnorm
      return_val <-
        pnorm(hi,
              mean=mean_vect,
              sd=sqrt(cov_mat)) - pnorm(lo,mean=mean_vect,sd=sqrt(cov_mat))
    } else {
      # Have more than one dependent variable. Call pmvnorm
      # The integral:
      return_val <- pmvnorm(lower=lo,
                                    upper=hi,
                                    mean=mean_vect,
                                    sigma=cov_mat)
      return_val <- as.numeric(return_val)
    }

    if(log==T) {
      return(log(return_val))
    } else {
      return(return_val)
    }
  }

  # If this point is reach, there is at least on each of giv and dep
  # Conditioned integral needed
  ind_dep <- 1:J
  ind_giv <- J + (1:K)
  # Do the conditioning
  condNorm <- condMVN(mean=mean_vect,
                      sigma=cov_mat,
                      dependent=ind_dep,
                      given=ind_giv,
                      X.given=y_giv,
                      check.sigma=F)
  # Do the integral
  p <- pmvnorm(lower=lo,
               upper=hi,
               mean=condNorm$condMean,
               sigma=condNorm$condVar)
  cond_int_value <- as.numeric(p) # The value of the conditioned integral


  # Calculate the (point) contribution of the conditioning
  if(K == 1) {
    # There is only one conditioned variable. Call dnorm
    if(log==T) {
      cond_pnt_value <- dnorm(y_giv,
                              mean=mean_vect[ind_giv],
                              sd=sqrt(cov_mat[ind_giv,ind_giv]),log=T)
    } else {
      cond_pnt_value <- dnorm(y_giv,
                              mean=mean_vect[ind_giv],
                              sd=sqrt(cov_mat[ind_giv,ind_giv]),
                              log=F)
    }
  } else {
    # There is more than one conditioned variable. Call mvtnorm::dmvnorm
    if(log==T) {
      cond_pnt_value <- dmvnorm(y_giv,
                                mean=mean_vect[ind_giv],
                                sigma=cov_mat[ind_giv,ind_giv],
                                log=T)
    } else {
      cond_pnt_value <- dmvnorm(y_giv,
                                mean=mean_vect[ind_giv],
                                sigma=cov_mat[ind_giv, ind_giv],
                                log=F)
    }
  }
  if(log==T) {
    return(log(cond_int_value) + cond_pnt_value)
  } else {
    return(cond_int_value*cond_pnt_value)
  }
}