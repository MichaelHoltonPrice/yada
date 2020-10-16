#' Calculate the multiviarate Gaussian integral over a rectangular domain where
#' some of the variables are known, and thus conditioned on. In particular,
#' there are two types of variables, dependent and given, and the integral is
#' over dependent variables. The known variables are provided as a vector ygiv,
#' with length K. The bounds of the integral over dependent variables are lo and
#' hi, each of which have length J. The multivariate Gaussian density is
#' specified by a mean vector, meanVect, of length J+K, and a covariance
#' matrix, covMat, with dimensions (J+K) by (J+K). Edge case, such as J=0 and
#' K>0, for which which no integral is needed, are handled.
#'
#' @param meanVect The vector of means [length J+K]
#' @param covMat The covariance matrix [dimension (J+K) by (J+K)]
#' @param lo The lower limit of integration for the dependent variables [length J]
#' @param hi The upper limit of integration for the dependent variables [length J]
#' @param y_giv The known values of the conditioned variables [length K]
#' @param log Whether to return the logarithm of the integral (default FALSE)
#'
#' @return The value of the integral
#' @export
calc_conditional_gaussian_integral <- function(meanVect,covMat,lo=c(),hi=c(),y_giv=c(),log=F) {
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
        return(dnorm(y_giv,mean=meanVect,sd=sqrt(covMat),log=T))
      } else {
        return(dnorm(y_giv,mean=meanVect,sd=sqrt(covMat),log=F))
      }
    } else {
      # Have more than one given variable. Call dmvnorm
      if(log==T) {
        return(mvtnorm::dmvnorm(y_giv,mean=meanVect,sigma=covMat,log=T))
      } else {
        return(mvtnorm::dmvnorm(y_giv,mean=meanVect,sigma=covMat,log=F))
      }
    }
  }

  # If K is 0, no conditioning is needed
  if(K == 0) {
    if( J == 1 ) {
      # Have exactly one dependent variable. Call pnorm
      returnVal <- pnorm(hi,mean=meanVect,sd=sqrt(covMat)) - pnorm(lo,mean=meanVect,sd=sqrt(covMat))
    } else {
      # Have more than one dependent variable. Call pmvnorm
      returnVal <- mvtnorm::pmvnorm(lower=lo, upper=hi,mean=meanVect,sigma=covMat) # The integral
      returnVal <- as.numeric(returnVal)
    }

    if(log==T) {
      return(log(returnVal))
    } else {
      return(returnVal)
    }
  }

  # If this point is reach, there is at least on each of giv and dep
  # Conditioned integral needed
  ind_dep <- 1:J
  ind_giv <- J + (1:K)
  # Do the conditioning
  condNorm <- condMVNorm::condMVN(mean=meanVect,sigma=covMat, dependent=ind_dep, given=ind_giv,X.given=y_giv,check.sigma=F)
  # Do the integral
  p <- mvtnorm::pmvnorm(lower=lo, upper=hi,mean=condNorm$condMean,sigma=condNorm$condVar)
  condIntValue <- as.numeric(p) # The value of the conditioned integral


  # Calculate the (point) contribution of the conditioning
  if(K == 1) {
    if(log==T) {
      condPntValue <- dnorm(y_giv,mean=meanVect[ind_giv],sd=sqrt(covMat[ind_giv,ind_giv]),log=T)
    } else {
      condPntValue <- dnorm(y_giv,mean=meanVect[ind_giv],sd=sqrt(covMat[ind_giv,ind_giv]),log=F)
    }
  } else {
    if(log==T) {
      condPntValue <- mvtnorm::dmvnorm(y_giv,mean=meanVect[ind_giv],sigma=covMat[ind_giv,ind_giv],log=T)
    } else {
      condPntValue <- mvtnorm::dmvnorm(y_giv,mean=meanVect[ind_giv],sigma=covMat[ind_giv,ind_giv],log=F)
    }
  }
  if(log==T) {
    return(log(condIntValue) + condPntValue)
  } else {
    return(condIntValue*condPntValue)
  }
}

#' @export
calc_conditional_gaussian_integral_approx <- function(meanVect,covMat,lo=c(),hi=c(),y_giv=c(),log=F,N=100,fixZero=F) {
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
        return(dnorm(y_giv,mean=meanVect,sd=sqrt(covMat),log=T))
      } else {
        return(dnorm(y_giv,mean=meanVect,sd=sqrt(covMat),log=F))
      }
    } else {
      # Have more than one given variable. Call dmvnorm
      if(log==T) {
        return(mvtnorm::dmvnorm(y_giv,mean=meanVect,sigma=covMat,log=T))
      } else {
        return(mvtnorm::dmvnorm(y_giv,mean=meanVect,sigma=covMat,log=F))
      }
    }
  }

  # If K is 0, no conditioning is needed
  if(K == 0) {
    if( J == 1 ) {
      # Have exactly one dependent variable. Call pnorm
      returnVal <- pnorm(hi,mean=meanVect,sd=sqrt(covMat)) - pnorm(lo,mean=meanVect,sd=sqrt(covMat))
    } else {
      # Have more than one dependent variable. Call pmvnorm
      #returnVal <- mvtnorm::pmvnorm(lower=lo, upper=hi,mean=meanVect,sigma=covMat) # The integral
      #returnVal <- as.numeric(returnVal)
      X <- mvtnorm::rmvnorm(N, mean = meanVect, sigma = covMat,method="chol")
      p <- apply(X,1,function(x){all((lo < x) & (x < hi))})
      p <- sum(p)/N
      if(fixZero) {
        if(p == 0) {
          returnVal <- p
        } else {
          returnVal <- mvtnorm::pmvnorm(lower=lo, upper=hi,mean=meanVect,sigma=covMat) # The integral
          returnVal <- as.numeric(returnVal)
        }
      } else {
        returnVal <- p
      }
    }

    if(log==T) {
      return(log(returnVal))
    } else {
      return(returnVal)
    }
  }

  # If this point is reach, there is at least on each of giv and dep
  # Conditioned integral needed
  ind_dep <- 1:J
  ind_giv <- J + (1:K)
  # Do the conditioning
  condNorm <- condMVNorm::condMVN(mean=meanVect,sigma=covMat, dependent=ind_dep, given=ind_giv,X.given=y_giv,check.sigma=F)
  # Approximate the integral
#  p <- mvtnorm::pmvnorm(lower=lo, upper=hi,mean=condNorm$condMean,sigma=condNorm$condVar)
  X <- mvtnorm::rmvnorm(N, mean = condNorm$condMean, sigma = condNorm$condVar,method="chol")

  p <- apply(X,1,function(x){all((lo < x) & (x < hi))})


#  p <- 0
#  for(n in 1:N) { # should vectorize
#    # each row a sample?
#    if( all((lo < X[n,]) & (X[n,] < hi)) ) {
#      p <- p + 1
#    }
#  }
  p <- sum(p)/N

  if(fixZero) {
    if(p == 0) {
      p <- mvtnorm::pmvnorm(lower=lo, upper=hi,mean=condNorm$condMean,sigma=condNorm$condVar)
    }
  }
  #rmvnorm(N, mean = condNorm$condMean, sigma = condNorm$condVar),method=c("eigen", "svd", "chol")
  condIntValue <- as.numeric(p) # The value of the conditioned integral


  # Calculate the (point) contribution of the conditioning
  if(K == 1) {
    if(log==T) {
      condPntValue <- dnorm(y_giv,mean=meanVect[ind_giv],sd=sqrt(covMat[ind_giv,ind_giv]),log=T)
    } else {
      condPntValue <- dnorm(y_giv,mean=meanVect[ind_giv],sd=sqrt(covMat[ind_giv,ind_giv]),log=F)
    }
  } else {
    if(log==T) {
      condPntValue <- mvtnorm::dmvnorm(y_giv,mean=meanVect[ind_giv],sigma=covMat[ind_giv,ind_giv],log=T)
    } else {
      condPntValue <- mvtnorm::dmvnorm(y_giv,mean=meanVect[ind_giv],sigma=covMat[ind_giv,ind_giv],log=F)
    }
  }
  if(log==T) {
    return(log(condIntValue) + condPntValue)
  } else {
    return(condIntValue*condPntValue)
  }
}
