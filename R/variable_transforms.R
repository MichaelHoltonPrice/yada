#' Transform a vector from a constrained to an unconstrained representation.
#' The categories are:
#' Category	Description
#' 0		Unconstrained
#' 1		Positive
#' 2		-1 to 1
#' 3		Positive difference
#' 
#' @param param The vector of constrained parameters
#' @param tfCatVect The vector of transformation categories
#' @return The vector of unconstrained parameters, param_bar
#' @export
param_constr2unconstr <- function(param,tfCatVect) {
  ind_other <- (tfCatVect < 0) & (3 < tfCatVect)
  if(any(ind_other)) {
    stop('Categories must be between 0 and 3')
  }

  param_bar <- rep(NA,length(param))
  for(categ in 0:3) {
    ind <- which(tfCatVect == categ)
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
#' @param tfCatVect The vector of transformation categories
#' @return The vector of constrained parameters, param
#' @export
param_unconstr2constr <- function(param_bar,tfCatVect) {
  ind_other <- (tfCatVect < 0) & (3 < tfCatVect)
  if(any(ind_other)) {
    stop('Categories must be between 0 and 3')
  }

  param <- rep(NA,length(param_bar))
  for(categ in 0:3) {
    ind <- which(tfCatVect == categ)
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
