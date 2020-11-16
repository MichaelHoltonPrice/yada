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

#' Return True for conditionally dependent models and False for conditionally
#' independent models. For the special case of a univariate model, False is
#' returned. Aside from this, the model is conditionally indepenendent if
#' (a) modSpec$cdepSpec is 'indep' or (b) modSpec$cdepSpec is 'dep', but cdepGroups is all NA.
#'
#' @param modSpec The model specification
#' @return Whether or not the model is conditionally dependent
#' @export
is_cdep <- function(modSpec) {

  J <- get_J(modSpec)
  K <- get_K(modSpec)
  if(J + K == 1) {
    return(F)
  }

  if(tolower(modSpec$cdepSpec) == 'indep') {
    return(F)
  } else if(tolower(modSpec$cdepSpec) == 'dep') {
    return(!all(is.na(modSpec$cdepGroups)))
  } else {
    stop(paste('Unsupported cdepSpec,',modSpec$cdepSpec))
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
    if(!is_cdep(modSpec)) {
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

#' Build the mapping between inputs and indices needed by the function
#' get_var_index_multivariate_fast.
#'
#' @param modSpec The model specification
#' @return The mapping
#' @export
get_var_index_multivariate_mapping <- function(modSpec) {
  # The following are valid input patterns:
  #
  # j   k   i  i1  i2
  # 0   0   0   0   0    no index
  # 1   0   0   0   0    j specified
  # 0   1   0   0   0    k specified
  # 0   0   1   0   0    i specified
  # 0   0   0   1   1    i1 and i2 specified

  J <- get_J(modSpec)
  K <- get_K(modSpec)

  # First pattern, no index
  # For each variable, create a vector giving the index range
  no_index       <- list(a=range(get_var_index_multivariate('a',modSpec)))
  if(J > 0) {
    no_index$tau <- range(get_var_index_multivariate('tau',modSpec))
  } else {
    no_index$tau <- NULL
  }
  no_index$alpha <- range(get_var_index_multivariate('alpha',modSpec))
  if(is_cdep(modSpec)) {
    no_index$z     <- range(get_var_index_multivariate('z',modSpec))
  }
  mapping <- list(no_index=no_index)

  # Second pattern, j index
  # Handled using the mapping for the fourth pattern, i index

  # Third pattern, k index
  # Handled using the mapping for the fourth pattern, i index

  # Fourth pattern, i index
  # For each variable, create a matrix giving the index range for i
  # [Not applicable for z]
  a_mat     <- matrix(NA,J+K,2)
  if(J > 0) {
    tau_mat   <- matrix(NA,J,2)
  } else {
    tau_mat <- NULL
  }
  alpha_mat <- matrix(NA,J+K,2)
  for(i in 1:(J+K)) {
    a_ind <- get_var_index_multivariate('a',modSpec,i=i)
    if(length(a_ind) > 0) {
      a_mat[i,1] <- min(a_ind)
      a_mat[i,2] <- max(a_ind)
    }

    if( (J > 0) && (i <= J) ) {
      tau_ind <- get_var_index_multivariate('tau',modSpec,i=i)
      if(length(tau_ind) > 0) {
        tau_mat[i,1] <- min(tau_ind)
        tau_mat[i,2] <- max(tau_ind)
      }
    }

    alpha_ind <- get_var_index_multivariate('alpha',modSpec,i=i)
    if(length(alpha_ind) > 0) {
      alpha_mat[i,1] <- min(alpha_ind)
      alpha_mat[i,2] <- max(alpha_ind)
    }
  }
  i_index <- list(a=a_mat,tau=tau_mat,alpha=alpha_mat)

  if(!is_cdep(modSpec)) {
    return(list(no_index=no_index,i_index=i_index,modSpec=modSpec))
  }

  # Fifth pattern, i1 and i2 indices
  # Iterate over unique pairs to create a vector of length choose(J+K,2) of
  # index values.
  # [Not applicable for a, tau, or alpha]
  i1_i2_index <- rep(NA,choose(J+K,2))

  counter <- 0 # using a counter is probably clearer than using combinadic indexing
  for(i1 in 1:(J+K-1)) {
    for(i2 in (i1+1):(J+K)) {
      counter <- counter + 1
      if(!is.na(modSpec$cdepGroups[i1]) && !is.na(modSpec$cdepGroups[i2])) {
        i1_i2_index[counter] <- get_var_index_multivariate('z',modSpec,i1=i1,i2=i2)
      }
    }
  }

  mapping <- list(no_index=no_index,i_index=i_index,i1_i2_index=i1_i2_index,modSpec=modSpec)
  return(mapping)
}

#' Accomplishes the same task as get_var_index_multivariate, but using a
#' pre-built mapping to increase the speed of the lookup.
#'
#' @param varName The variable name (a, tau, alpha, z)
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @param i1 The overall index for the first variable of a pair
#' @param i2 The overall index for the second variable of a pair
#' @return The indices in the full parameter vector, th_y
#' @export
get_var_index_multivariate_fast <- function(varName,mapping,j=NA,k=NA,i=NA,i1=NA,i2=NA) {


  # The following are valid input patterns:
  #
  # j   k   i  i1  i2
  # 0   0   0   0   0    no index
  # 1   0   0   0   0    j specified
  # 0   1   0   0   0    k specified
  # 0   0   1   0   0    i specified
  # 0   0   0   1   1    i1 and i2 specified

  inputPattern <- !is.na(c(j,k,i,i1,i2))

  J <- get_J(mapping$modSpec)
  K <- get_K(mapping$modSpec)
  if(all(inputPattern == c(F,F,F,F,F))) {
    # no index
    varRange <- mapping$no_index[[varName]]
    return(varRange[1]:varRange[2])
  } else if(all(inputPattern == c(T,F,F,F,F))) {
    # j index
    if((j < 1) || (J < j)) {
      stop('j should be between 1 and J')
    } else {
      get_var_index_multivariate_fast(varName,mapping,i=j)
    }
  } else if(all(inputPattern == c(F,T,F,F,F))) {
    # k index
    if((k < 1) || (K < k)) {
      stop('k should be between 1 and K')
    } else {
      get_var_index_multivariate_fast(varName,mapping,i=J+k)
    }
  } else if(all(inputPattern == c(F,F,T,F,F))) {
    # i index
    if(varName == 'a') {
      lo <- mapping$i_index$a[i,1]
      hi <- mapping$i_index$a[i,2]
    } else if(varName == 'tau') {
      lo <- mapping$i_index$tau[i,1]
      hi <- mapping$i_index$tau[i,2]
    } else if(varName == 'alpha') {
      lo <- mapping$i_index$alpha[i,1]
      hi <- mapping$i_index$alpha[i,2]
  } else if(all(inputPattern == c(F,F,F,T,T))) {
    # i1 and i2 indices specified
    } else {
      stop(paste0('Unsupported variable, ', varName, ' for i being specified'))
    }
    if(is.na(lo)) {
      return(c())
    } else {
      return(lo:hi)
    }
  } else if(all(inputPattern == c(F,F,F,T,T))) {
    # i1 and i2 indices
    if(varName != 'z') {
      stop('varName must be z if i1 and i2 are specified')
    }

    if(i1 == i2) {
      stop('i1 should not equal i2')
    }

    if(is.na(mapping$modSpec$cdepGroups[i1]) || is.na(mapping$modSpec$cdepGroups[i2])) {
      stop('Correlation requested for a variable with no correlations')
    }

    if(i1 < i2) {
      combIndex <- elemToIndex(c(i1-1,i2-1),J+K) + 1
    } else {
      combIndex <- elemToIndex(c(i2-1,i1-1),J+K) + 1
    }
    return(mapping$i1_i2_index[combIndex])
  } else {
    stop('Unsupported input pattern for index variables. See yada documentation')
  }
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


#calc_neg_log_lik_vect_multivariate_core <- function(th_y,x,Y,modSpecList,mappingList) {
#  '%dopar%' <- foreach::'%dopar%'
#  negLogLikVect <- foreach::foreach(n=1:ncol(Y), .combine=cbind) %dopar% {
#    negLogLik <- calc_neg_log_lik_multivariate_core(th_y,x[n],Y[,n],modSpecList[[n]],mappingList[[n]])
#  }
#
#  return(as.vector(negLogLikVect))
#}

#' For a single observation with response vector y0 and model specification
#' modSpec0, modify modSpec0 to account for missing observations in y0, which
#' are indicated with NA. In addition, create additional variables to support
#' calculation of the negative log-likelihood for the observation. (a) mapping:
#' a list of index mappings for the reduced data observation (see
#' get_var_index_multivariate_mapping and get_var_index_multivariate_fast).
#' (b) mapping0: the mapping for the unreduced problem. (c) ind: a vector that
#' subsets the full parameter vector to account for the reduction. (d) keep:
#' a boolean vector indicating which variables were kept in the reduction.
#'
#' @param y0 The unreduced response vector
#' @param modSpec The unreduced model specification
#' @return The list with y, modSpec, mapping, mapping0, ind, and keep
#' @export
remove_missing_variables <- function(y0,modSpec0) {

  J0 <- yada::get_J(modSpec0)
  K0 <- yada::get_K(modSpec0)
  keep <- !is.na(y0)

  if(J0 == 0) {
    J = 0
  } else {
    J <- sum(keep[1:J0])
  }

  if(K0 == 0) {
    K = 0
  } else {
    K <- sum(keep[J0+(1:K0)])
  }

  y <- y0[keep]
  modSpec <- list(meanSpec=modSpec0$meanSpec[keep])
  modSpec$noiseSpec <- modSpec0$noiseSpec[keep]
  modSpec$J <- J
  modSpec$K <- K
  if(J > 0) {
    modSpec$M <- modSpec0$M[keep[1:J0]]
  }
  modSpec$cdepSpec <- modSpec0$cdepSpec

  if(modSpec$cdepSpec == 'dep') {
  groups0         <- modSpec0$cdepGroups
  groups0_reduced <- modSpec0$cdepGroups[keep]

  # number of groups before removing variables
  Ng0 <- length(unique(groups0[!is.na(groups0)]))

  # number of groups after removing variables
  Ng <- length(unique(groups0_reduced[!is.na(groups0_reduced)]))

  uniqueGroups0_reduced <- sort(unique(groups0_reduced[!is.na(groups0_reduced)]))
  new2old_group <- rep(NA,Ng)

  groups <- rep(NA,Ng)
  for(i in 1:length(groups0_reduced)) {
     if(!is.na(groups0_reduced[i])) {
       groups[i] <- which(groups0_reduced[i] == uniqueGroups0_reduced)
     } else {
       groups[i] <- NA
     }
  }

  new2old_group <- uniqueGroups0_reduced
  modSpec$cdepGroups <- groups
  }

  mapping0 <- get_var_index_multivariate_mapping(modSpec0)
  mapping  <- get_var_index_multivariate_mapping(modSpec)

  # Create index subsetting of the new th_y from the old th_y0
  ind <- c()
  if(J0 > 0) {
    for(j0 in 1:J0) {
      if(keep[j0]) {
        # a
        ind <- c(ind,get_var_index_multivariate_fast('a',mapping0,j=j0))
        # tau
        ind <- c(ind,get_var_index_multivariate_fast('tau',mapping0,j=j0))
        # alpha
        ind <- c(ind,get_var_index_multivariate_fast('alpha',mapping0,j=j0))
      }
    }
  }

  if(K0 > 0) {
    for(k0 in 1:K0) {
      if(keep[J0+k0]) {
        # a
        ind <- c(ind,get_var_index_multivariate_fast('a',mapping0,k=k0))
        # alpha
        ind <- c(ind,get_var_index_multivariate_fast('alpha',mapping0,k=k0))
      }
    }
  }

  if(modSpec$cdepSpec == 'dep') {
  # Add non-singleton groups
  nonSingGroups0 <- get_non_singleton_groups(groups0)
  nonSingGroups  <- get_non_singleton_groups(groups)
  numBefore <- get_num_var_multivariate('z',modSpec0,preceding=T)
  if(length(nonSingGroups) > 0) {
    for(g in nonSingGroups) {
      g0 <- new2old_group[g]
      ind <- c(ind,numBefore + which(g0 == nonSingGroups0))
    }
  }

  # Add cross-group correlations
  for(g1 in 1:(Ng-1)) {
    for(g2 in (g1+1):Ng) {
      g1_0 <- new2old_group[g1]
      g2_0 <- new2old_group[g2]
      ind <- c(ind,numBefore + length(nonSingGroups0) + elemToIndex(c(g1_0,g2_0)-1,Ng0) + 1)
    }
  }
  }
  ind <- sort(ind)

  return(list(y=y,modSpec=modSpec,mapping=mapping,mapping0=mapping0,ind=ind,keep=keep))
}

#' Prepare for the negative log-likelihood calculations by creating indexing
#' objects to speed up execution.
#'
#' @param x The independent variable
#' @param Y The matrix of responses
#' @param modSpec The model specification
#' @export
prep_for_neg_log_lik_multivariate <- function(x,Y,modSpec) {
  N <- length(x)
  if(N != ncol(Y)) {
    stop('length of x should equal the number of columns in Y')
  }

  J <- yada::get_J(modSpec)
  K <- yada::get_K(modSpec)
  if(nrow(Y) != J+K) {
    stop('J+K should equal the number of rows in Y')
  }

  if(any(is.na(x))) {
    stop('x should not contain missing values')
  }

  if(any(colSums(is.na(Y)) == (J+K))) {
    stop('Y should not contain observations with all missing values')
  }

  # Handle special cases where the meanSpec is logOrd and x is 0
  Y0 <- Y
  ind_log <- which(modSpec$meanSpec == 'logOrd')
  if(length(ind_log) > 0) {
    for(j in ind_log) {
      yj <- Y[j,]
      xj <- x
      indj <- which(!is.na(yj))
      xj <- xj[indj]
      yj <- yj[indj]
      if(any((xj == 0) & (yj > 0))) {
        stop(paste0('For variable j=',j,', cases exist where meanSpec is logOrd, x=0, and m>0'))
      }
      matches <- which((xj == 0) & (yj == 0))
      if(length(matches) > 0) {
        Y[j,indj[matches]] <- NA
      }
    }
  }

  # Call remove_missing_variables to populate the following lists:
  calcData <- list()
  for(n in 1:N) {
    # Remove missing variables
    remap <- remove_missing_variables(Y[,n],modSpec)
    calcData_n <- list(x=x[n],y=remap$y,modSpec=remap$modSpec,mapping=remap$mapping,mapping0=remap$mapping0,ind=remap$ind,keep=remap$keep)
    calcData[[n]] <- calcData_n
  }

  return(calcData)
}

#' @export
get_z_full_fast <- function(th_y,mapping,asMatrix=F) {

  J <- yada::get_J(mapping$modSpec)
  K <- yada::get_K(mapping$modSpec)

  z_full <- rep(0,choose(J+K,2))
  B <- which(!is.na(mapping$i1_i2_index))
  ind_z <- mapping$i1_i2_index[B]
  z_full[B] <- th_y[ind_z]

  if(!asMatrix) {
    return(z_full)
  }

  zMat <- diag(J+K)
  zMat[upper.tri(zMat)] <- z_full
  zMat <- t(zMat)
  zMat[upper.tri(zMat)] <- z_full
  zMat <- t(zMat)

  return(zMat)
}

#' @export
calc_neg_log_lik_vect_multivariate <- function(th_y,calcData,tfCatVect=NA) {
  if(!all(is.na(tfCatVect))) {
    th_y <- param_unconstr2constr(th_y,tfCatVect)
  }

  N <- length(calcData)

  negLogLikVect <- foreach(n=1:N,.combine=cbind,.packages=c('yada','ksoptim')) %dopar% {
    negLogLik_n <- calc_neg_log_lik_scalar_multivariate(th_y,calcData[[n]])
  }
  return(as.vector(negLogLikVect))
}

#' @export
calc_neg_log_lik_scalar_multivariate <- function(th_y,calcData_n) {
  prepData_n <- calc_cond_gauss_int_inputs(th_y[calcData_n$ind],calcData_n$x,calcData_n$y,calcData_n$mapping)
  negLogLik_n <- -calc_conditional_gaussian_integral(prepData_n$meanVect,prepData_n$covMat,prepData_n$lo,prepData_n$hi,prepData_n$y_giv,log=T)
  return(negLogLik_n)
}

#' @export
calc_neg_log_lik_vect_multivariate_chunk_outer <- function(th_y,calcData,tfCatVect=NA,numChunks=round(length(calcData)/10)) {
  if(!all(is.na(tfCatVect))) {
    th_y <- param_unconstr2constr(th_y,tfCatVect)
  }

  N <- length(calcData)
  folds <- nestfs::create.folds(numChunks,N)

  negLogLikList <- foreach(k=1:numChunks,.combine=cbind,.packages=c('yada','ksoptim')) %dopar% {
    negLogLikVect <- calc_neg_log_lik_vect_multivariate_chunk_inner(th_y,calcData[folds[[k]]])
  }

  negLogLikVect <- rep(NA,N)
  for(k in 1:numChunks) {
    negLogLikVect[folds[[k]]] <- negLogLikList[[k]]
  }
  
  return(negLogLikVect)
}

#' @export
calc_neg_log_lik_vect_multivariate_chunk_inner <- function(th_y,calcData) {

  N <- length(calcData)
  negLogLikVect <- rep(NA,N)
  
  for(n in 1:N) {
    negLogLikVect[n] <- calc_neg_log_lik_scalar_multivariate(th_y,calcData[[n]])
  }
  return(negLogLikVect)
}

#' @export
calc_neg_log_lik_multivariate <- function(th_y,calcData,tfCatVect=NA) {
  negLogLikVect <- calc_neg_log_lik_vect_multivariate(th_y,calcData,tfCatVect)
  return(sum(negLogLikVect))
}

#' For a given model specification of a multivariate model, return a vector that
#' gives the categories used by param_constr2uncontr and param_constr2unconstr.
#'
#' @param modSpec The model specification
#' @return A vector of categories
#' @export
get_multivariate_transform_categories <- function(modSpec) {

  # Initialize the category vectors for each a, tau, and alpha
  a_cat     <- c()
  tau_cat   <- c()
  alpha_cat <- c()

  # Iterate over ordinal variables to populate a_cat, tau_cat, and alpha_cat
  J <- get_J(modSpec)
  if(J > 0) {
    for(j in 1:J) {
      modSpec_j <- list(J=1)
      modSpec_j$meanSpec <- modSpec$meanSpec[j]
      modSpec_j$noiseSpec <- modSpec$noiseSpec[j]
      modSpec_j$M <- modSpec$M[j]
      a_cat     <- c(a_cat    ,get_mean_transform_categories( modSpec_j$meanSpec))
      tau_cat   <- c(tau_cat  ,c(0,rep(3,modSpec_j$M-1)))
      alpha_cat <- c(alpha_cat,get_noise_transform_categories(modSpec_j$noiseSpec))
    }
  }

  # Iterate over continuous variables to populate a_cat and alpha_cat
  K <- get_K(modSpec)
  if(K > 0) {
    for(k in 1:K) {
      modSpec_k <- list(K=1)
      modSpec_k$meanSpec <- modSpec$meanSpec[J+k]
      modSpec_k$noiseSpec <- modSpec$noiseSpec[J+k]
      a_cat     <- c(a_cat    ,get_mean_transform_categories( modSpec_k$meanSpec))
      alpha_cat <- c(alpha_cat,get_noise_transform_categories(modSpec_k$noiseSpec))
    }
  }

  tfCatVect <- c(a_cat,tau_cat,alpha_cat)

  # If necessary, add categories for z
  if(is_cdep(modSpec)) {
    tfCatVect <- c(tfCatVect,rep(2,get_z_length(modSpec)))
  }
  return(tfCatVect)
}

#' Renumber the groups in the input vector group0 so that the group numbering
#' is sequential integers (this is typically done after subsetting an original
#' group vector).
#'
#' @param groups0 The starting group vector, probably with gaps in it
#' @return The updated group vector
#' @export
renumber_groups <- function(groups0) {
  uniqueGroups <- sort(unique(groups0[!is.na(groups0)]))
  groups <- rep(NA,length(groups0))
  for(g in 1:length(groups0)) {
     if(!is.na(groups0[g])) {
       groups[g] <- which(groups0[g] == uniqueGroups)
     }
  }
  return(groups)
}

#' groupVect is a vector assigning each of its elements to a unique group.
#' Return the non-singleton groups in groupVect -- that is, the groups with more
#' than one member
#'
#' For example:
#' groupVect <- c(1,2,NA,4,1,3,3)
#' print(get_non_singleton_groups(groupVect))
#' [1] 1 3
#'
#' @param groupVect The vector of group assignments
#' @return A vector of non-singlteon groups
#' @export
get_non_singleton_groups <- function(groupVect) {

  counts <- table(groupVect)
  return(sort(as.numeric(names(counts))[counts > 1]))
}

#' For a single obersvation, calculate the inputs to the conditional Gaussian
#' integral, which are meanVect, coMat, lo, and hi. These are returned as a
#' list.
#'
#' @param th_y The parameter vector
#' @param x The independent variable (a scalar)
#' @param y The response variables (a vector)
#' @param mapping The mapping for fast extraction of indices (see get_var_index_multivariate_mapping)
#' @return A list with the mean vector (meanVect), covariance matrix (covMat), low limit of integration (lo), high limit of integration (hi), and given/conditioned response variables (y_giv).
#' @export
calc_cond_gauss_int_inputs <- function(th_y,x,y,mapping) {
  J <- get_J(mapping$modSpec) # number of ordinal variables
  K <- get_K(mapping$modSpec) # number of continuous variables

  if( (J == 0) && (K == 0)) {
    stop('J and K cannot both be zero')
  }

  # Calculate meanVect, noiseVect, lo, and hi using for loops over the ordinal
  # and continuous variables.
  meanVect  <- rep(NA,J+K)
  noiseVect <- rep(NA,J+K)
  if(J > 0) {
    lo <- rep(-Inf,J)
    hi <- rep( Inf,J)
    for(j in 1:J) {
      tau_j     <- th_y[get_var_index_multivariate_fast('tau'  ,mapping,j=j)]
      if(mapping$modSpec$meanSpec[j] == 'powLawOrd') {
        a_j       <- th_y[get_var_index_multivariate_fast('a'    ,mapping,j=j)]
        meanVect[j] <- x^a_j
      } else if(mapping$modSpec$meanSpec[j] == 'logOrd') {
        meanVect[j] <- log(x)
      } else if(mapping$modSpec$meanSpec[j] == 'linOrd') {
        meanVect[j] <- x
      } else {
        stop(paste0('Unsupported meanSpec = ',mapping$modSpec$meanSpec[j]))
      }

      alpha_j   <- th_y[get_var_index_multivariate_fast('alpha',mapping,j=j)]
      if(mapping$modSpec$noiseSpec[j] == 'const') {
        noiseVect[j] <- alpha_j
      } else if(mapping$modSpec$noiseSpec[j] == 'lin_pos_int') {
        noiseVect[j] <- alpha_j[1]*(1 + x*alpha_j[2])
      } else {
        stop(paste0('Unsupported noiseSpec = ',mapping$modSpec$noiseSpec[j]))
      }

      tau_j <- th_y[get_var_index_multivariate_fast('tau',mapping,j=j)]
      if(y[j] > 0) {
        lo[j] <- tau_j[y[j]]
      }
      if(y[j] < mapping$modSpec$M[j]) {
        hi[j] <- tau_j[y[j]+1]
      }
    }
  } else {
    lo <- c()
    hi <- c()
  }

  if(K > 0) {
    for(k in 1:K) {
      a_k       <- th_y[get_var_index_multivariate_fast('a'    ,mapping,k=k)]
      alpha_k   <- th_y[get_var_index_multivariate_fast('alpha',mapping,k=k)]

      if(mapping$modSpec$meanSpec[J+k] == 'powLaw') {
        meanVect[J+k] <- a_k[2]*x^a_k[1] + a_k[3]
      } else {
        stop(paste0('Unsupported meanSpec = ',mapping$modSpec$meanSpec[J+k]))
      }

      if(mapping$modSpec$noiseSpec[J+k] == 'const') {
        noiseVect[J+k] <- alpha_k
      } else if(mapping$modSpec$noiseSpec[J+k] == 'lin_pos_int') {
        noiseVect[J+k] <- alpha_k[1]*(1 + x*alpha_k[2])
      } else {
        stop(paste0('Unsupported noiseSpec = ',mapping$modSpec$noiseSpec[J+k]))
      }
    }
    y_giv <- y[J + (1:K)]
  } else {
    y_giv <- c()
  }

  # Calculate the covariance matrix (covMat)
  z_full <- rep(0,choose(J+K,2))
  B <- which(!is.na(mapping$i1_i2_index))
  ind_z <- mapping$i1_i2_index[B]
  z_full[B] <- th_y[ind_z]

  zMat <- diag(J+K)
  zMat[upper.tri(zMat)] <- z_full
  zMat <- t(zMat)
  zMat[upper.tri(zMat)] <- z_full
  zMat <- t(zMat)
  covMat <- as.matrix(noiseVect) %*% base::t(as.matrix(noiseVect))
  covMat <- covMat * zMat
  return(list(meanVect=meanVect,covMat=covMat,lo=lo,hi=hi,y_giv=y_giv))
}

#' Calculate the posterior probability p(x|y,theta_x,theta_y) for the mixed
#' cumulative probit model at the points in xcalc given the response vector
#' y. xcalc is assumed to be evenly spaced and the calculation is for a single
#' observation.
#'
#' @param xcalc A vector of ages at which to calculate the posterior probability
#' @param y The response vector for a single observation
#' @param th_x Parameterization for prior on x
#' @param th_y Parameterization for likelihood
#' @param modSpec The model specification
#' @return A vector of posterior probabilities
#' @export
calc_x_posterior <- function(xcalc,y,th_x,th_y,modSpec) {
  # Assume xcalc is evenly spaced
  dx <- xcalc[2] - xcalc[1] 
  if(!all.equal(diff(xcalc),rep(dx,length(xcalc)-1))) {
    stop('xcalc must be evenly spaced')
  }
  p_xy <- calc_joint(xcalc,y,th_x,th_y,modSpec)
  p_x <- p_xy / sum(p_xy) / dx
  return(p_x)
}

#' Calculate the joint density probability p(x,y|th_x,th_y) for the mixed
#' cumulative probit model at the points in xcalc. xcalc is assumed to be evenly
#' spaced.
#'
#' @param xcalc A vector of ages at which to calculate the posterior probability
#' @param y The response vector for a single observation
#' @param th_x Parameterization for prior on x
#' @param th_y Parameterization for likelihood (conditional on x)
#' @param modSpec The model specification
#' @return A vector of joint probabilities
#' @export
calc_joint <- function(xcalc,y,th_x,th_y,modSpec) {
  Y <- matrix(y,nrow=length(y),ncol=length(xcalc))
  calcData <- prep_for_neg_log_lik_multivariate(xcalc,Y,modSpec)
  logLikVect <- -calc_neg_log_lik_vect_multivariate(th_y,calcData) # the vector of log-likelihoods

  logPriorVect <- log(calc_x_density(xcalc,th_x))

  logJointVect <- logLikVect + logPriorVect

  fv <- exp(logJointVect)
  fv[!is.finite(fv)] <- 0
  return(fv)
}

#' Calculate the density at x given a parameterization for the density of
#' theta_x. Currently, the exponential distribution, Weibull mixtures, and
#' uniform distribution are supported.
#'
#' @param x A vector of ages at which to calculate the density
#' @param th_x A list with the fit type and parameter vector
#' @export
calc_x_density <- function(x,th_x) {
  if(tolower(th_x$fitType) == 'exponential') { 
    return(dexp(x,th_x$fit))
  } else if (tolower(th_x$fitType) == 'offsetweibmix') { 
    return(calcPdfWeibMix(x+th_x$weibOffset,th_x$fit$lambda,c(rbind(th_x$fit$shape,th_x$fit$scale))))
  } else if (tolower(th_x$fitType) == 'uniform') { 
    f <- rep(1,length(x))
    ind0 <- x < th_x$xmin | th_x$xmax < x
    f[ind0] <- 0
    return(f/(th_x$xmax-th_x$xmin))
  } else {
    stop(paste('Unsupported fit type:',th_x$fitType))
  }
}

#' Given the input vector x and Weibull mixture parameters theta, calculate the
#' probability density function (PDF) matrix, which has dimensions length(x) by
#' numMix, where numMix = length(theta)/2. The mixture proportions are not
#' accounted for in the calculation.
#'
#' @param x Locations at which to evaluate probability density function
#' @param theta The value of shape and scale parameters with the ordering [sh1,sc1,sh2,sc2,...]
#' @return The PDF matrix with dimensions length(x) by length(theta)/2
#' @export
calcPdfMatrixWeibMix <- function(x, theta) {
  numObs <- length(x) # Number of observations
  numMix <- length(theta) / 2 # Number of mixtures
  # For x, the shape parameter, and scale parameter create a long vector of
  # length numObs*numMix for vectorized input to dweibull
  x_vect <- rep(x, numMix)
  shape_vect <- as.vector(t(matrix(rep(theta[seq(1, length(theta), by = 2)], length(x)), nrow = numMix)))
  scale_vect <- as.vector(t(matrix(rep(theta[seq(2, length(theta), by = 2)], length(x)), nrow = numMix)))
  # Calculate the probability density and transform to a matrix with dimensions numObs x numMix
  return(matrix(dweibull(x_vect, shape_vect, scale_vect), ncol = numMix))
}

#' Given the input vector x and Weibull mixture proportions and parameters z and
#' theta, calculate the probability density function for the input vector x.
#'
#' @param x Locations at which to evaluate probability density function
#' @param z Vector of mixture proportions
#' @param theta The value of shape and scale parameters with the ordering [sh1,sc1,sh2,sc2,...]
#' @return The PDF vector for the input vector x
#' @export
calcPdfWeibMix <- function(x, z, theta) {
  pdfMatrix <- calcPdfMatrixWeibMix(x, theta)
  pdf <- rowSums(t(t(pdfMatrix) * z))
  return(pdf)
}
