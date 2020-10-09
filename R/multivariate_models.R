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
    # First, handle the case where none of j, k, and i are specified
    if(is.na(j) && is.na(k) && is.na(i)) {
      if(varName == 'a') {
        return(0)
      } else {
        variables <- c('a','tau','alpha','z')
        ind <- which(variables == varName) # index in variables
        return(get_num_var_multivariate(variables[ind-1],modSpec,preceding=T) + get_num_var_multivariate(variables[ind-1],modSpec,preceding=F))
      }
    }

    # Next, handle the case where one of j, k, and i are specified
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
      if(varName == 'a') {
        return(get_num_var_mean(modSpec$meanSpec[i]))
      } else {
        # alpha
        return(get_num_var_noise(modSpec$noiseSpec[i]))
      }
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
        return(0)
      }
      numVar <- 0
      for(j in 1:J) {
        numVar <- numVar + get_num_var_multivariate('tau',modSpec,j=j)
      }
      return(numVar)
    }
  } else if(varName == 'z') {
    get_z_length(modSpec)
  } else {
    stop(paste('Unrecognized variable',varName))
  }
}


#' Return the number of correlation terms (length of z)
#'
#' @param modSpec The model specification
#' @return The number of correlation terms
#' @export
get_z_length <- function(modSpec) {
  if( !('cdepSpec' %in% names(modSpec)) ) {
    return(0)
  }
  if(tolower(modSpec$cdepSpec == 'indep')) {
    return(0)
  }
  groupSizes <- as.vector(table(modSpec$cdepGroups))
  numGroups  <- length(as.vector(table(modSpec$cdepGroups)))
  return(sum(groupSizes > 1) + choose(numGroups,2))
}



#' Get the indices in the full parameter vector, th_y, of a named variable.
#' Optionally, the variable index (j, k, or i) can be specified, or (for the
#' correlation, z, the variable indices i1 and i2. The overall index, i, equals
#' j for ordinal variables and J + k for continuous variables, where J is the
#' number of ordinal variables. If no index is specified, the full range of
#' indices for the input variable is returned. If the variable index (or
#' indices) is specified, the relevant subrange is returned. The following
#' table lists the five valid input patterns, where 1 indicates the index
#' variable is specified and a 0 indicates it is not (and is thus equal to the
#' default value, NA).
#' j   k   i  i1  i2
#' 0   0   0   0   0    no index
#' 1   0   0   0   0    j specified
#' 0   1   0   0   0    k specified
#' 0   0   1   0   0    i specified
#' 0   0   0   1   1    i1 and i2 specified

#'
#' @param varName The variable name (a, tau, alpha, z)
#' @param modSpec The model specification
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @param i1 The overall index for the first variable of a pair
#' @param i2 The overall index for the second variable of a pair
#' @return The indices in the full parameter vector, th_y
#' @export
get_var_index_multivariate <- function(varName,modSpec,j=NA,k=NA,i=NA,i1=NA,i2=NA) {

  J <- get_J(modSpec)
  K <- get_K(modSpec)

  # Handle z specially before handling other variables
  if(varName == 'z') {
    if(modSpec$cdepSpec != 'dep') {
      stop('z requested but model is not conditionally dependent')
    }

    offset <- get_num_var_multivariate('z',modSpec,preceding=T)
    if(is.na(i1) && is.na(i2)) {
      # Then return all the indices
      return(offset + 1:get_num_var_multivariate('z',modSpec))
    }

    if(i1 == i2) {
      stop('i1 should not equal i2')
    }

    # If i1 and i2 are members of the same group, this is an intragroup
    # correlation that is stored in the beginning of z.
    g1 <- modSpec$cdepGroups[i1]
    g2 <- modSpec$cdepGroups[i2]
    if(is.na(g1) || is.na(g2)) {
      stop('Correlation requested for a variable with no correlations')
    }
    if(g1 == g2) {
      return(offset + g1)
    }

    # If i1 and i2 are members of different groups, this is an intergroup
    # correlation that is stored after the intragroup correlations.
    offset <- offset + sum(as.vector(table(modSpec$cdepGroups)) > 1)
    numGroups <- length(unique(modSpec$cdepGroups)[!is.na(unique(modSpec$cdepGroups))])
    if(g1 < g2) {
      index <- elemToIndex(c(g1-1,g2-1),numGroups) + 1
    } else {
      index <- elemToIndex(c(g2-1,g1-1),numGroups) + 1
    }
    return(offset+index)
  } # end code for varName == 'z'

  # Do some error checking

  # The following are valid input patterns:
  #
  # j   k   i  i1  i2
  # 0   0   0   0   0    no index
  # 1   0   0   0   0    j specified
  # 0   1   0   0   0    k specified
  # 0   0   1   0   0    i specified
  # 0   0   0   1   1    i1 and i2 specified

  inputPattern <- !is.na(c(j,k,i,i1,i2))

  if(!all(inputPattern == c(F,F,F,F,F)) &&
     !all(inputPattern == c(T,F,F,F,F)) &&
     !all(inputPattern == c(F,T,F,F,F)) &&
     !all(inputPattern == c(F,F,T,F,F)) &&
     !all(inputPattern == c(F,F,F,T,T)) ) { # this case should in fact have already been handled
    stop('Unsupported input pattern for index variables. See yada documentation')
  }

  if(!is.na(j)) {
    if(!(varName %in% c('a','tau','alpha')) ) {
      stop('Unsupported variable for j being specified')
    }
    if(j < 1 || J < j) {
      stop('j is not between 1 and J')
    }
  }

  if(!is.na(k)) {
    if(!(varName %in% c('a','alpha')) ) {
      stop('Unsupported variable for k being specified')
    }
    if(k < 1 || K < k) {
      stop('k is not between 1 and K')
    }
  }

  if(!is.na(i)) {
    if(!(varName %in% c('a','tau','alpha')) ) {
      stop('Unsupported variable for i being specified')
    }
    if(i < 1 || (J+K) < i) {
      stop('i is not between 1 and J+K')
    }
  }

  # Number of variables
  N <- get_num_var_multivariate(varName,modSpec,j=j,k=k,i=i)
  if(N == 0) {
    return(c())
  } else {
    offset <- get_num_var_multivariate(varName,modSpec,j=j,k=k,i=i,preceding=T)
    return(offset + (1:N))
  }

  stop('This point should never be reached')
}



#' For a multivariate model, get the indices in the full parameter vector
#' (th_y) correspdoning to a given univariate model. The correlation term is
#' ignored for conditionally dependent models. Exactly one of j, k, or i must
#' be specified (see the help for get_var_index_multivariate for more on j, k
#' and i).
#'
#' @param modSpec The model specification
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @export
get_univariate_indices <- function(modSpec,j=NA,k=NA,i=NA) {

  # Do error checking on pattern of inputs
  inputPattern <- !is.na(c(j,k,i))
  if(!all(inputPattern == c(T,F,F)) &&
     !all(inputPattern == c(F,T,F)) &&
     !all(inputPattern == c(F,F,T)) ) {
    stop('Unsupported input pattern for index variables. See yada documentation')
  }

  # If i is is specified, determine whether i corresponds to an ordinal or
  # continuous variable, and (re)call get_univariate_indices, specifying j or
  # k.
  if(!is.na(i)) {
    J <- get_J(modSpec)
    if(i <= J) {
      return(get_univariate_indices(modSpec,j=i))
    } else {
      return(get_univariate_indices(modSpec,k=i-J))
    }
  }

  # If this point is reach, either j or k is specified (but not both)
  if(!is.na(j)) {
    ind <- c(get_var_index_multivariate('a',modSpec,j=j),
             get_var_index_multivariate('tau',modSpec,j=j),
             get_var_index_multivariate('alpha',modSpec,j=j))
  } else { # k is specified
    ind <- c(get_var_index_multivariate('a',modSpec,k=k),
             get_var_index_multivariate('alpha',modSpec,k=k))
  }
  return(ind)
}
