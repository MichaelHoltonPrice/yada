#' Get the number of parameters for named variables of a multivariate model
#' model
#'
#' @param varName The variable name (a, tau, alpha, z)
#' @param modSpec The model specification
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @param preceding Whether to return the number of precceding variables
#' @return The number of parameters
#' @export
get_num_var_multivariate <- function(varName,modSpec,j=NA,k=NA,i=NA,preceding=F) {
  
  if(preceding) {
    #variables <- c('a','tau','alpha','z')
    # First, handle the case where none of j, k, and i are specified
    if(is.na(j) && is.na(k) && is.na(i)) {
      if(varName == 'a') {
        return(0)
      } else {
        variables <- c('a','tau','alpha')
        ind <- which(variables == varName) # index in variables
        return(get_num_var_multivariate(variables[ind-1],modSpec,preceding=T) + get_num_var_multivariate(variables[ind-1],modSpec,preceding=F))
      }
    }

    # Next, handle the cases one of j, k, and i are specified
    if(!is.na(j) && is.na(k) && is.na(i)) {
      return(get_num_var_multivariate(varName,modSpec,i=j,preceding=T))
    } else if(is.na(j) && !is.na(k) && is.na(i)) {
      return(get_num_var_multivariate(varName,modSpec,i=get_J(modSpec)+k,preceding=T))
    } else if(is.na(j) && is.na(k) && !is.na(i)) {
      if(i == 1) {
        # This is the base case, which is identical to i not being input
        return(get_num_var_multivariate(varName,modSpec,preceding=T))
      } else {
        return(get_num_var_multivariate(varName,modSpec,i=i-1,preceding=T) + get_num_var_multivariate(varName,modSpec,i=i-1,preceding=F))
      }
    } else {
      stop('Only one of j, k, or i should be specified')
    }
  }

  if(varName %in% c('a','alpha')) {
    if(!is.na(j)) {
      # Number of variables for a particular ordinal variable
      if(!is.na(k)) {
        stop('If varName is a or alpha and j is specified, k should not be specified')
      }
      if(!is.na(i)) {
        stop('If varName is a or alpha and j is specified, i should not be specified')
      }
      J <- get_J(modSpec)
      if(j > J) {
        stop(paste0('j = ',j,' is greater than the number of ordinal variables J = ',J))
      }
      if(varName == 'a') {
        return(get_num_var_mean(modSpec$meanSpec[j]))
      } else {
        # alpha
        return(get_num_var_noise(modSpec$noiseSpec[j]))
      }
    } else if(!is.na(k)) {
      # Number of variables for a particular continuous variable
      if(!is.na(i)) {
        stop('If varName is a or alpha and k is specified, i should not be specified')
      }
      K <- get_K(modSpec)
      if(k > K) {
        stop(paste0('k = ',k,' is greater than the number of continuous variables K = ',K))
      }
      J <- get_J(modSpec)
      if(varName == 'a') {
        return(get_num_var_mean(modSpec$meanSpec[J+k]))
      } else {
        # alpha
        return(get_num_var_noise(modSpec$noiseSpec[J+k]))
      }
    } else if(!is.na(i)) {
      # Number of variables for a variable generally
      J <- get_J(modSpec)
      K <- get_K(modSpec)
      if(i > J+K) {
        stop(paste0('i = ',i,' is greater than the number of variables J+K = ',J+K))
      }
      return(get_num_var_mean(modSpec$meanSpec[]))
    } else { # j, k, and i are NA
      J <- get_J(modSpec)
      K <- get_K(modSpec)
      if(J+K == 0) {
        stop('This model specification contains no variables')
      }
      numVar <- 0
      for(i in 1:(J+K)) {
        if(varName == 'a') {
          numVar <- numVar + get_num_var_mean(modSpec$meanSpec[i])
        } else {
          # alpha
          numVar <- numVar + get_num_var_noise(modSpec$noiseSpec[i])
        }
      }
      return(numVar)
    }
  } else if(varName == 'tau') {
    if(!is.na(k)) {
      stop('If varName is tau, k should not be specified')
    }

    if(!is.na(j)) {
      # Number of variables for a particular ordinal variable
      if(!is.na(i)) {
        stop('If varName is tau and j is specified, i should not be specified')
      }
      J <- get_J(modSpec)
      if(j > J) {
        stop(paste0('j = ',j,' is greater than the number of ordinal variables J = ',J))
      }
      return(modSpec$M[j])
    } else if(!is.na(i)) {
      # Number of variables for a variable generally
      J <- get_J(modSpec)
      if(i > J) {
        stop(paste0('i = ',i,' is greater than the number of ordinal variables J = ',J))
      }
      return(get_num_var_multivariate('tau',modSpec,j=i))
    } else { # j, k, and i are NA
      J <- get_J(modSpec)
      if(J == 0) {
        stop('This model specification contains no ordinal variables')
      }
      numVar <- 0
      for(j in 1:J) {
        numVar <- numVar + get_num_var_multivariate('tau',modSpec,j=j)
      }
      return(numVar)
    }
  }
}

#' Get the number of ordinal variables given a model specification. If J is not
#' a field in modSpec, 0 is returned.
#'
#' @param modSpec The model specification
#' @return The number of ordinal variables, J
#' @export
get_J <- function(modSpec) {
  # A helper function to get J, which is assumed 0 if J is not a field in
  # modSpec.
  if('J' %in% names(modSpec)) {
    return(modSpec$J)
  } else {
    return(0)
  }
}

#' Get the number of continuous variables given a model specification. If K is
#' not a field in modSpec, 0 is returned.
#'
#' @param modSpec The model specification
#' @return The number of continuous variables, K
#' @export
get_K <- function(modSpec) {
  if('K' %in% names(modSpec)) {
    return(modSpec$K)
  } else {
    return(0)
  }
}
