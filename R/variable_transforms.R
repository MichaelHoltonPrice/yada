#' Transform a vector from a constrained to an unconstrained representation.
#' The categories and bounds are:
#' category	 min	max
#' 0		-Inf	Inf
#' 1		   0	Inf
#' 2		  -1	 +1
#' 
#' @param param The vector of constrained parameters
#' @param transfMat The matrix specifying the transform
#' @return The vector unconstrained parameters, param_bar
#' @export
param_constr2unconstr <- function(param,transfMat) {
  cat_vect <- transfMat[,1]
  ind_other <- (cat_vect < 0) & (2 < cat_vect)
  if(any(ind_other)) {
    stop('Categories must be between 0 and 2')
  }

  param_bar <- rep(NA,length(param))
  for(categ in 0:2) {
    ind <- cat_vect == categ
    if(sum(ind) > 0) {
      if(categ == 0) {
        param_bar[ind] <- param[transfMat[ind,2]]
      } else if(categ == 1) {
        param_bar[ind] <- log(param[transfMat[ind,2]])
      } else {
        # categ = 2
        param_bar[ind] <- gtools::logit((param[transfMat[ind,2]]+1)/2)
      }
    }
  }
  return(param_bar)
}

#' Transform a vector from an unconstrained to a unconstrained representation.
#' The categories and bounds are:
#' category	 min	max
#' 0		-Inf	Inf
#' 1		   0	Inf
#' 2		  -1	 +1
#' 
#' @param param_bar The vector of unconstrained parameters
#' @param transfMat The matrix specifying the transform
#' @return The vector of constrained parameters, param
#' @export
param_unconstr2constr <- function(param_bar,transfMat) {
  cat_vect <- transfMat[,1]
  ind_other <- (cat_vect < 0) & (2 < cat_vect)
  if(any(ind_other)) {
    stop('Categories must be between 0 and 2')
  }

  param <- rep(NA,length(param_bar))
  for(categ in 0:2) {
    ind <- cat_vect == categ
    if(sum(ind) > 0) {
      if(categ == 0) {
        param[ind] <- param_bar[transfMat[ind,2]]
      } else if(categ == 1) {
        param[ind] <- exp(param_bar[transfMat[ind,2]])
      } else {
        # categ = 2
        param[ind] <- -1 + 2/(1+exp(-param_bar[transfMat[ind,2]]))
      }
    }
  }
  return(param)
}
