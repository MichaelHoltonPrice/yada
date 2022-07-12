#' @title Get J for a yada model specification
#'
#' @description
#' Get the number of ordinal variables, J, given a model specification. If J is
#' not a field in mod_spec, 0 is returned.
#'
#' @param mod_spec The model specification
#'
#' @return The number of ordinal variables, J
#'
#' @export
get_J <- function(mod_spec) {
  # A helper function to get J, which is assumed 0 if J is not a field in
  # mod_spec.
  if('J' %in% names(mod_spec)) {
    return(mod_spec$J)
  } else {
    return(0)
  }
}

#' @title Get K for a yada model specification
#'
#' @description
#' Get the number of continuous variables, K, given a model specification. If K
#' is not a field in mod_spec, 0 is returned.
#'
#' @param mod_spec The model specification
#'
#' @return The number of continuous variables, K
#'
#' @export
get_K <- function(mod_spec) {
  if('K' %in% names(mod_spec)) {
    return(mod_spec$K)
  } else {
    return(0)
  }
}

#' @title Get the number of correlation terms for a yada model specification
#'
#' @description
#' Return the number of correlation terms (length of z)
#'
#' @param mod_spec The model specification
#'
#' @return The number of correlation terms
#'
#' @export
get_z_length <- function(mod_spec) {
  if( !('cdep_spec' %in% names(mod_spec)) ) {
    return(0)
  }
  if(tolower(mod_spec$cdep_spec == 'indep')) {
    return(0)
  }
  group_sizes <- as.vector(table(mod_spec$cdep_groups))
  num_groups  <- length(as.vector(table(mod_spec$cdep_groups)))
  return(sum(group_sizes > 1) + choose(num_groups,2))
}

#' @title Is this model conditionally dependent?
#'
#' @description
#' Return True for conditionally dependent models and False for conditionally
#' independent models. For the special case of a univariate model, False is
#' returned. Aside from this, the model is conditionally indepenendent if
#' (a) mod_spec$cdep_spec is 'indep' or (b) mod_spec$cdep_spec is 'dep', but
#' cdep_groups is all NA.
#'
#' @param mod_spec The model specification
#'
#' @return Whether or not the model is conditionally dependent
#'
#' @export
is_cdep <- function(mod_spec) {

  J <- get_J(mod_spec)
  K <- get_K(mod_spec)
  if(J + K == 1) {
    return(F)
  }

  if(tolower(mod_spec$cdep_spec) == 'indep') {
    return(F)
  } else if(tolower(mod_spec$cdep_spec) == 'dep') {
    return(!all(is.na(mod_spec$cdep_groups)))
  } else {
    stop(paste('Unsupported cdep_spec,',mod_spec$cdep_spec))
  }
}

#' @title
#' Get the number of parameters for named variables of a multivariate model
#'
#' @description
#' Get the number of variables for named variables of a multivariate yada model
#' specification. This can be done either for specific ordinal or continuous
#' variables (e.g., j=1 or k=1), including using i (i=j for ordinal variables
#' and i=J+k for continuous variables). For example, to get the number of
#' variables that specify the mean for the second ordinal variable use:
#'
#' get_num_var_multivariate("a",mod_spec,j=2)
#'
#' As another example, to get the number of correlation terms (equivalent to
#' directly calling get_z_length) use:
#'
#' get_num_var_multivariate("z",mod_spec)
#'
#' Either none of j, k, and i should be specified, or exactly one of them
#' should. Errors are thrown for pertinent mis-uses. For example, tau only
#' applies to ordinal variables, so an informative error message is given if
#' var_name is 'tau' and k is input.
#'
#' The optional input [preceding] indicates whether to give instead the number
#' of variables in the parameter vector that preced the specified input. It is
#' primarily used to recursively obtain the result for when preceding is TRUE,
#' the normal usage.
#'
#' @param var_name The variable name (a, tau, alpha, z)
#' @param mod_spec The model specification
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @param preceding Whether to return the number of precceding variables
#' @return The number of parameters
#' @export
get_num_var_multivariate <- function(var_name,
                                     mod_spec,
                                     j=NA,
                                     k=NA,
                                     i=NA,
                                     preceding=F) {
  if(preceding) {
    # First, handle the case where none of j, k, and i are specified
    if(is.na(j) && is.na(k) && is.na(i)) {
      if(var_name == 'a') {
        return(0)
      } else {
        variables <- c('a','tau','alpha','z')
        ind <- which(variables == var_name) # index in variables
        return(get_num_var_multivariate(variables[ind-1],
                                        mod_spec,
                                        preceding=T) +
                 get_num_var_multivariate(variables[ind-1],
                                          mod_spec,
                                          preceding=F))
      }
    }

    # Next, handle the case where one of j, k, and i are specified
    if(!is.na(j) && is.na(k) && is.na(i)) {
      return(get_num_var_multivariate(var_name,mod_spec,i=j,preceding=T))
    } else if(is.na(j) && !is.na(k) && is.na(i)) {
      return(get_num_var_multivariate(var_name,
                                      mod_spec,
                                      i=get_J(mod_spec)+k,
                                      preceding=T))
    } else if(is.na(j) && is.na(k) && !is.na(i)) {
      if(i == 1) {
        # This is the base case, which is identical to i not being input
        return(get_num_var_multivariate(var_name,mod_spec,preceding=T))
      } else {
        return(get_num_var_multivariate(var_name,mod_spec,i=i-1,preceding=T) +
                 get_num_var_multivariate(var_name,mod_spec,i=i-1,preceding=F))
      }
    } else {
      stop('Only one of j, k, or i should be specified')
    }
  }

  if(var_name %in% c('a','alpha')) {
    if(!is.na(j)) {
      # Number of variables for a particular ordinal variable
      if(!is.na(k)) {
        stop(paste0('If var_name is a or alpha and j is specified, k should ',
                    'not be specified'))
      }
      if(!is.na(i)) {
        stop(paste0('If var_name is a or alpha and j is specified, i should ',
                    'not be specified'))
      }
      J <- get_J(mod_spec)
      if(j > J) {
        stop(paste0('j = ',j,' is greater than the number of ordinal ',
                    'variables J = ',J))
      }
      if(var_name == 'a') {
        return(get_num_var_mean(mod_spec$mean_spec[j]))
      } else {
        # alpha
        return(get_num_var_noise(mod_spec$noise_spec[j]))
      }
    } else if(!is.na(k)) {
      # Number of variables for a particular continuous variable
      if(!is.na(i)) {
        stop(paste0('If var_name is a or alpha and k is specified, i should ',
                    'not be specified'))
      }
      K <- get_K(mod_spec)
      if(k > K) {
        stop(paste0('k = ',k,' is greater than the number of continuous ',
                    'variables K = ',K))
      }
      J <- get_J(mod_spec)
      if(var_name == 'a') {
        return(get_num_var_mean(mod_spec$mean_spec[J+k]))
      } else {
        # alpha
        return(get_num_var_noise(mod_spec$noise_spec[J+k]))
      }
    } else if(!is.na(i)) {
      # Number of variables for a variable generally
      J <- get_J(mod_spec)
      K <- get_K(mod_spec)
      if(i > J+K) {
        stop(paste0('i = ',i,' is greater than the number of variables ',
                    'J+K = ',J+K))
      }
      if(var_name == 'a') {
        return(get_num_var_mean(mod_spec$mean_spec[i]))
      } else {
        # alpha
        return(get_num_var_noise(mod_spec$noise_spec[i]))
      }
    } else { # j, k, and i are NA
      J <- get_J(mod_spec)
      K <- get_K(mod_spec)
      if(J+K == 0) {
        stop('This model specification contains no variables')
      }
      num_var <- 0
      for(i in 1:(J+K)) {
        if(var_name == 'a') {
          num_var <- num_var + get_num_var_mean(mod_spec$mean_spec[i])
        } else {
          # alpha
          num_var <- num_var + get_num_var_noise(mod_spec$noise_spec[i])
        }
      }
      return(num_var)
    }
  } else if(var_name == 'tau') {
    if(!is.na(k)) {
      stop('If var_name is tau, k should not be specified')
    }

    if(!is.na(j)) {
      # Number of variables for a particular ordinal variable
      if(!is.na(i)) {
        stop('If var_name is tau and j is specified, i should not be specified')
      }
      J <- get_J(mod_spec)
      if(j > J) {
        stop(paste0('j = ',j,' is greater than the number of ordinal ',
                    'variables J = ',J))
      }
      return(mod_spec$M[j])
    } else if(!is.na(i)) {
      # Number of variables for a variable generally
      J <- get_J(mod_spec)
      if(i > J) {
        stop(paste0('i = ',i,' is greater than the number of ordinal ',
                    'variables J = ',J))
      }
      return(get_num_var_multivariate('tau',mod_spec,j=i))
    } else { # j, k, and i are NA
      J <- get_J(mod_spec)
      if(J == 0) {
        return(0)
      }
      num_var <- 0
      for(j in 1:J) {
        num_var <- num_var + get_num_var_multivariate('tau',mod_spec,j=j)
      }
      return(num_var)
    }
  } else if(var_name == 'z') {
    get_z_length(mod_spec)
  } else {
    stop(paste('Unrecognized variable',var_name))
  }
}

#' @title
#' Get the indices in the parameter vector th_y of named variables
#'
#' @description
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
#' @param var_name The variable name (a, tau, alpha, z)
#' @param mod_spec The model specification
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @param i1 The overall index for the first variable of a pair
#' @param i2 The overall index for the second variable of a pair
#' @return The indices in the full parameter vector, th_y
#' @export
get_var_index_multivariate <- function(var_name,
                                       mod_spec,
                                       j=NA,
                                       k=NA,
                                       i=NA,
                                       i1=NA,
                                       i2=NA) {

  J <- get_J(mod_spec)
  K <- get_K(mod_spec)

  # Handle z specially before handling other variables
  if(var_name == 'z') {
    if(!is_cdep(mod_spec)) {
      stop('z requested but model is not conditionally dependent')
    }

    offset <- get_num_var_multivariate('z',mod_spec,preceding=T)
    if(is.na(i1) && is.na(i2)) {
      # Then return all the indices
      return(offset + 1:get_num_var_multivariate('z',mod_spec))
    }

    if(i1 == i2) {
      stop('i1 should not equal i2')
    }

    # If i1 and i2 are members of the same group, this is an intragroup
    # correlation that is stored in the beginning of z.
    g1 <- mod_spec$cdep_groups[i1]
    g2 <- mod_spec$cdep_groups[i2]
    if(is.na(g1) || is.na(g2)) {
      stop('Correlation requested for a variable with no correlations')
    }
    if(g1 == g2) {
      return(offset + g1)
    }

    # If i1 and i2 are members of different groups, this is an intergroup
    # correlation that is stored after the intragroup correlations.
    offset <- offset + sum(as.vector(table(mod_spec$cdep_groups)) > 1)
    num_groups <-
      length(unique(mod_spec$cdep_groups)[!is.na(unique(mod_spec$cdep_groups))])
    if(g1 < g2) {
      index <- elem_to_index(c(g1-1,g2-1),num_groups) + 1
    } else {
      index <- elem_to_index(c(g2-1,g1-1),num_groups) + 1
    }
    return(offset+index)
  } # end code for var_name == 'z'

  # Do some error checking

  # The following are valid input patterns:
  #
  # j   k   i  i1  i2
  # 0   0   0   0   0    no index
  # 1   0   0   0   0    j specified
  # 0   1   0   0   0    k specified
  # 0   0   1   0   0    i specified
  # 0   0   0   1   1    i1 and i2 specified

  input_pattern <- !is.na(c(j,k,i,i1,i2))

  if(!all(input_pattern == c(F,F,F,F,F)) &&
     !all(input_pattern == c(T,F,F,F,F)) &&
     !all(input_pattern == c(F,T,F,F,F)) &&
     !all(input_pattern == c(F,F,T,F,F)) &&
     !all(input_pattern == c(F,F,F,T,T)) ) { # this case should in fact have
                                            # already been handled
    stop(paste0('Unsupported input pattern for index variables. See yada ',
                'documentation'))
  }

  if(!is.na(j)) {
    if(!(var_name %in% c('a','tau','alpha')) ) {
      stop('Unsupported variable for j being specified')
    }
    if(j < 1 || J < j) {
      stop('j is not between 1 and J')
    }
  }

  if(!is.na(k)) {
    if(!(var_name %in% c('a','alpha')) ) {
      stop('Unsupported variable for k being specified')
    }
    if(k < 1 || K < k) {
      stop('k is not between 1 and K')
    }
  }

  if(!is.na(i)) {
    if(!(var_name %in% c('a','tau','alpha')) ) {
      stop('Unsupported variable for i being specified')
    }
    if(i < 1 || (J+K) < i) {
      stop('i is not between 1 and J+K')
    }
  }

  # Number of variables
  N <- get_num_var_multivariate(var_name,mod_spec,j=j,k=k,i=i)
  if(N == 0) {
    return(c())
  } else {
    offset <- get_num_var_multivariate(var_name,mod_spec,j=j,k=k,i=i,preceding=T)
    return(offset + (1:N))
  }

  stop('This point should never be reached')
}

#' @title
#' A helper function to build the mapping for [get_var_index_multivariate_fast]
#'
#' @description
#' Build the mapping between inputs and indices needed by the function
#' get_var_index_multivariate_fast.
#'
#' @param mod_spec The model specification
#'
#' @return The mapping
#'
#' @export
get_var_index_multivariate_mapping <- function(mod_spec) {
  # The following are valid input patterns:
  #
  # j   k   i  i1  i2
  # 0   0   0   0   0    no index
  # 1   0   0   0   0    j specified
  # 0   1   0   0   0    k specified
  # 0   0   1   0   0    i specified
  # 0   0   0   1   1    i1 and i2 specified

  J <- get_J(mod_spec)
  K <- get_K(mod_spec)

  # First pattern, no index
  # For each variable, create a vector giving the index range
  a <- get_var_index_multivariate('a',mod_spec)

  if (length(a) == 0) {
    no_index       <- list(a=c(NULL,NULL))
  } else {
    no_index       <- list(a=range(get_var_index_multivariate('a',
                                                              mod_spec)))
  }
  if(J > 0) {
    no_index$tau <- range(get_var_index_multivariate('tau',mod_spec))
  } else {
    no_index$tau <- NULL
  }
  no_index$alpha <- range(get_var_index_multivariate('alpha',mod_spec))
  if(is_cdep(mod_spec)) {
    no_index$z     <- range(get_var_index_multivariate('z',mod_spec))
  }

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
    a_ind <- get_var_index_multivariate('a',mod_spec,i=i)
    if(length(a_ind) > 0) {
      a_mat[i,1] <- min(a_ind)
      a_mat[i,2] <- max(a_ind)
    }

    if( (J > 0) && (i <= J) ) {
      tau_ind <- get_var_index_multivariate('tau',mod_spec,i=i)
      if(length(tau_ind) > 0) {
        tau_mat[i,1] <- min(tau_ind)
        tau_mat[i,2] <- max(tau_ind)
      }
    }

    alpha_ind <- get_var_index_multivariate('alpha',mod_spec,i=i)
    if(length(alpha_ind) > 0) {
      alpha_mat[i,1] <- min(alpha_ind)
      alpha_mat[i,2] <- max(alpha_ind)
    }
  }
  i_index <- list(a=a_mat,tau=tau_mat,alpha=alpha_mat)

  if(!is_cdep(mod_spec)) {
    return(list(no_index=no_index,i_index=i_index,mod_spec=mod_spec))
  }

  # Fifth pattern, i1 and i2 indices
  # Iterate over unique pairs to create a vector of length choose(J+K,2) of
  # index values.
  # [Not applicable for a, tau, or alpha]
  i1_i2_index <- rep(NA,choose(J+K,2))

  # using a counter is probably clearer than using combinadic indexing
  counter <- 0
  for(i1 in 1:(J+K-1)) {
    for(i2 in (i1+1):(J+K)) {
      counter <- counter + 1
      if(!is.na(mod_spec$cdep_groups[i1]) && !is.na(mod_spec$cdep_groups[i2])) {
        i1_i2_index[counter] <- get_var_index_multivariate('z',
                                                           mod_spec,
                                                           i1=i1,
                                                           i2=i2)
      }
    }
  }

  mapping <- list(no_index=no_index,
                  i_index=i_index,
                  i1_i2_index=i1_i2_index,
                  mod_spec=mod_spec)
  return(mapping)
}

#' @title
#' Quickly get the indices in the parameter vector th_y of named variables
#'
#' @description
#' Accomplishes the same task as [get_var_index_multivariate], but using a
#' pre-built mapping to increase the speed of the lookup.
#'
#' @param var_name The variable name (a, tau, alpha, z)
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @param i1 The overall index for the first variable of a pair
#' @param i2 The overall index for the second variable of a pair
#' @return The indices in the full parameter vector, th_y
#' @export
get_var_index_multivariate_fast <- function(var_name,
                                            mapping,
                                            j=NA,
                                            k=NA,
                                            i=NA,
                                            i1=NA,
                                            i2=NA) {


  # The following are valid input patterns:
  #
  # j   k   i  i1  i2
  # 0   0   0   0   0    no index
  # 1   0   0   0   0    j specified
  # 0   1   0   0   0    k specified
  # 0   0   1   0   0    i specified
  # 0   0   0   1   1    i1 and i2 specified

  input_pattern <- !is.na(c(j,k,i,i1,i2))

  J <- get_J(mapping$mod_spec)
  K <- get_K(mapping$mod_spec)
  if(all(input_pattern == c(F,F,F,F,F))) {
    # no index
    varRange <- mapping$no_index[[var_name]]
    return(varRange[1]:varRange[2])
  } else if(all(input_pattern == c(T,F,F,F,F))) {
    # j index
    if((j < 1) || (J < j)) {
      stop('j should be between 1 and J')
    } else {
      get_var_index_multivariate_fast(var_name,mapping,i=j)
    }
  } else if(all(input_pattern == c(F,T,F,F,F))) {
    # k index
    if((k < 1) || (K < k)) {
      stop('k should be between 1 and K')
    } else {
      get_var_index_multivariate_fast(var_name,mapping,i=J+k)
    }
  } else if(all(input_pattern == c(F,F,T,F,F))) {
    # i index
    if(var_name == 'a') {
      lo <- mapping$i_index$a[i,1]
      hi <- mapping$i_index$a[i,2]
    } else if(var_name == 'tau') {
      lo <- mapping$i_index$tau[i,1]
      hi <- mapping$i_index$tau[i,2]
    } else if(var_name == 'alpha') {
      lo <- mapping$i_index$alpha[i,1]
      hi <- mapping$i_index$alpha[i,2]
  } else if(all(input_pattern == c(F,F,F,T,T))) {
    # i1 and i2 indices specified
    } else {
      stop(paste0('Unsupported variable, ', var_name, ' for i being specified'))
    }
    if(is.na(lo)) {
      return(c())
    } else {
      return(lo:hi)
    }
  } else if(all(input_pattern == c(F,F,F,T,T))) {
    # i1 and i2 indices
    if(var_name != 'z') {
      stop('var_name must be z if i1 and i2 are specified')
    }

    if(i1 == i2) {
      stop('i1 should not equal i2')
    }

    if(is.na(mapping$mod_spec$cdep_groups[i1]) ||
       is.na(mapping$mod_spec$cdep_groups[i2])) {
      stop('Correlation requested for a variable with no correlations')
    }

    if(i1 < i2) {
      comb_index <- elem_to_index(c(i1-1,i2-1),J+K) + 1
    } else {
      comb_index <- elem_to_index(c(i2-1,i1-1),J+K) + 1
    }
    return(mapping$i1_i2_index[comb_index])
  } else {
    stop(paste0('Unsupported input pattern for index variables. See yada ',
                'documentation'))
  }
}

#' For a multivariate model, get the indices in the full parameter vector
#' (th_y) correspdoning to a given univariate model. The correlation term is
#' ignored for conditionally dependent models. Exactly one of j, k, or i must
#' be specified (see the help for get_var_index_multivariate for more on j, k
#' and i).
#'
#' @param mod_spec The model specification
#' @param j The ordinal index
#' @param k The continuous index
#' @param i The overall index
#' @export
get_univariate_indices <- function(mod_spec,j=NA,k=NA,i=NA) {

  # Do error checking on pattern of inputs
  input_pattern <- !is.na(c(j,k,i))
  if(!all(input_pattern == c(T,F,F)) &&
     !all(input_pattern == c(F,T,F)) &&
     !all(input_pattern == c(F,F,T)) ) {
    stop(paste0('Unsupported input pattern for index variables. See yada ',
                'documentation'))
  }

  # If i is is specified, determine whether i corresponds to an ordinal or
  # continuous variable, and (re)call get_univariate_indices, specifying j or
  # k.
  if(!is.na(i)) {
    J <- get_J(mod_spec)
    if(i <= J) {
      return(get_univariate_indices(mod_spec,j=i))
    } else {
      return(get_univariate_indices(mod_spec,k=i-J))
    }
  }

  # If this point is reach, either j or k is specified (but not both)
  if(!is.na(j)) {
    ind <- c(get_var_index_multivariate('a',mod_spec,j=j),
             get_var_index_multivariate('tau',mod_spec,j=j),
             get_var_index_multivariate('alpha',mod_spec,j=j))
  } else { # k is specified
    ind <- c(get_var_index_multivariate('a',mod_spec,k=k),
             get_var_index_multivariate('alpha',mod_spec,k=k))
  }
  return(ind)
}

#' @title
#' Return the categories used by [param_constr2uncontr] and
#' [param_constr_to_unconstr] for a multivariate yada model specification
#'
#' @description
#' For the input model specification of a multivariate model, return a vector
#' that gives the categories used by [param_constr2uncontr] and
#' [param_constr_to_unconstr].
#'
#' @param mod_spec The model specification
#'
#' @return A vector of categories
#'
#' @export
get_multivariate_transform_categories <- function(mod_spec) {

  # Initialize the category vectors for each a, tau, and alpha
  a_cat     <- c()
  tau_cat   <- c()
  alpha_cat <- c()

  # Iterate over ordinal variables to populate a_cat, tau_cat, and alpha_cat
  J <- get_J(mod_spec)
  if(J > 0) {
    for(j in 1:J) {
      mod_spec_j <- list(J=1)
      mod_spec_j$mean_spec <- mod_spec$mean_spec[j]
      mod_spec_j$noise_spec <- mod_spec$noise_spec[j]
      mod_spec_j$M <- mod_spec$M[j]
      a_cat     <- c(a_cat,
                     get_mean_transform_categories( mod_spec_j$mean_spec))
      tau_cat   <- c(tau_cat ,c(0,rep(3,mod_spec_j$M-1)))
      alpha_cat <- c(alpha_cat,
                     get_noise_transform_categories(mod_spec_j$noise_spec))
    }
  }

  # Iterate over continuous variables to populate a_cat and alpha_cat
  K <- get_K(mod_spec)
  if(K > 0) {
    for(k in 1:K) {
      mod_spec_k <- list(K=1)
      mod_spec_k$mean_spec <- mod_spec$mean_spec[J+k]
      mod_spec_k$noise_spec <- mod_spec$noise_spec[J+k]
      a_cat     <- c(a_cat,
                     get_mean_transform_categories( mod_spec_k$mean_spec))
      alpha_cat <- c(alpha_cat,
                     get_noise_transform_categories(mod_spec_k$noise_spec))
    }
  }

  tf_cat_vect <- c(a_cat,tau_cat,alpha_cat)

  # If necessary, add categories for z
  if(is_cdep(mod_spec)) {
    tf_cat_vect <- c(tf_cat_vect,rep(2,get_z_length(mod_spec)))
  }
  return(tf_cat_vect)
}

#' @title
#' For a single observation, remove missing variables and perform a number of
#' related tasks (see Description)
#'
#' @description
#' For a single observation with response vector y0 and model specification
#' mod_spec0, modify mod_spec0 to account for missing observations in y0, which
#' are indicated with NA. In addition, create additional variables to support
#' calculation of the negative log-likelihood for the observation. (a) mapping:
#' a list of index mappings for the reduced data observation (see
#' get_var_index_multivariate_mapping and get_var_index_multivariate_fast).
#' (b) mapping0: the mapping for the unreduced problem. (c) ind: a vector that
#' subsets the full parameter vector to account for the reduction. (d) keep:
#' a boolean vector indicating which variables were kept in the reduction.
#'
#' @param y0 The unreduced response vector
#' @param mod_spec The unreduced model specification
#'
#' @return The list with y, mod_spec, mapping, mapping0, ind, and keep
#'
#' @export
remove_missing_variables <- function(y0,mod_spec0) {

  J0 <- get_J(mod_spec0)
  K0 <- get_K(mod_spec0)
  keep <- !is.na(y0)  # non-NA variables

  # Find J and K for current individual
  if(J0 == 0) {
    J = 0
  } else if (J0 == 1 & sum(keep[1]) == 0) {
    J = 1
  } else {
    J <- sum(keep[1:J0])
  }

  if(K0 == 0) {
    K = 0
  } else {
    K <- sum(keep[J0+(1:K0)])
  }

  y <- y0[keep]  # keep only non-NA values

  # Initialize mod_spec
  mod_spec <- mod_spec0
  mod_spec$J <- J
  mod_spec$K <- K
  if (J > 0) {
    mod_spec$M <- mod_spec0$M[keep[1:J0]]
  } else {
    mod_spec$M <- NULL
  }
  mod_spec$mean_spec <- mod_spec0$mean_spec[keep]
  mod_spec$noise_spec <- mod_spec0$noise_spec[keep]
  if (J+K == 1) {
    mod_spec$cdep_spec <- 'indep'
    mod_spec$cdep_groups <- NULL
  }


  if(mod_spec$cdep_spec == 'dep') {
    groups0         <- mod_spec0$cdep_groups
    groups0_reduced <- mod_spec0$cdep_groups[keep]

    # number of groups before removing variables
    Ng0 <- length(unique(groups0[!is.na(groups0)]))

    # number of groups after removing variables
    Ng <- length(unique(groups0_reduced[!is.na(groups0_reduced)]))

    uniqu_groups0_reduced <-
      sort(unique(groups0_reduced[!is.na(groups0_reduced)]))

    groups <- rep(NA,Ng)
    for(i in 1:length(groups0_reduced)) {
       if(!is.na(groups0_reduced[i])) {
         groups[i] <- which(groups0_reduced[i] == uniqu_groups0_reduced)
       } else {
         groups[i] <- NA
       }
    }

    new_to_old_group <- uniqu_groups0_reduced
    mod_spec$cdep_groups <- groups
  }

  mapping0 <- get_var_index_multivariate_mapping(mod_spec0)
  mapping  <- get_var_index_multivariate_mapping(mod_spec)

  # Create index subsetting of the new th_y from the old th_y0
  ind <- c()
  if(J0 > 0) {
    for(j0 in 1:J0) {
      if(keep[j0]) {
        # a
        ind <- c(ind,
                 get_var_index_multivariate_fast('a',mapping0,j=j0))
        # tau
        ind <- c(ind,
                 get_var_index_multivariate_fast('tau',mapping0,j=j0))
        # alpha
        ind <- c(ind,
                 get_var_index_multivariate_fast('alpha',mapping0,j=j0))
      }
    }
  }

  if(K0 > 0) {
    for(k0 in 1:K0) {
      if(keep[J0+k0]) {
        # a
        ind <- c(ind,
                 get_var_index_multivariate_fast('a',mapping0,k=k0))
        # alpha
        ind <- c(ind,
                 get_var_index_multivariate_fast('alpha',mapping0,k=k0))
      }
    }
  }

  if(mod_spec$cdep_spec == 'dep') {
    # Add non-singleton groups
    non_sing_groups0 <- get_non_singleton_groups(groups0)
    non_sing_groups  <- get_non_singleton_groups(groups)
    num_before <- get_num_var_multivariate('z',mod_spec0,preceding=T)
    if(length(non_sing_groups) > 0) {
      for(g in non_sing_groups) {
        g0 <- new_to_old_group[g]
        ind <- c(ind,num_before + which(g0 == non_sing_groups0))
      }
    }

    # Add cross-group correlations
    if(Ng > 1) {
      for(g1 in 1:(Ng-1)) {
        for(g2 in (g1+1):Ng) {
          g1_0 <- new_to_old_group[g1]
          g2_0 <- new_to_old_group[g2]
          ind <- c(ind,
                   num_before +
                     length(non_sing_groups0) +
                     elem_to_index(c(g1_0,g2_0)-1,Ng0) + 1)
        }
      }
    }
  }
  ind <- sort(ind)

  return(list(y=y,mod_spec=mod_spec,
              mapping=mapping,mapping0=mapping0,ind=ind,keep=keep))
}

#' @title Prepare for the negative log-likelihood calculations
#'
#' @description
#' Prepare for the negative log-likelihood calculations by creating indexing
#' objects to speed up execution.
#'
#' @param x The independent variable
#' @param Y The matrix of responses
#' @param mod_spec The model specification
#' @param remove_log_ord_cases Whether or not to remove log_ord edge cases from
#'   the return list, calc_data. remove_log_ord_cases should be TRUE if
#'   calc_data is being used to calculate a likelihood function and should,
#'   typically, otherwise be FALSE (e.g., if log_ord is being used for
#'   posterior inference).
#' @param no_missing_var (Default: TRUE) Whether or not to require all
#'   variables to have no missing values.
#'
#' @return Data needed for a speedy negative log-likelihood calculation (calc_data)
#'
#' @export
prep_for_neg_log_lik_multivariate <- function(x,
                                              Y,
                                              mod_spec,
                                              remove_log_ord=FALSE,
                                              no_missing_var=TRUE) {
  N <- length(x)
  if(N != ncol(Y)) {
    stop('length of x should equal the number of columns in Y')
  }

  J <- get_J(mod_spec)
  K <- get_K(mod_spec)
  if(nrow(Y) != J+K) {
    stop('J+K should equal the number of rows in Y')
  }

  if(any(is.na(x))) {
    stop('x should not contain missing values')
  }

  if(any(colSums(is.na(Y)) == (J+K))) {
    stop('Y should not contain observations with all missing values')
  }

  if (no_missing_var) {
    if(any(rowSums(is.na(Y)) == ncol(Y))) {
      stop('Y should not contain variables with all missing values')
    }
  }

  # Store the starting inputs
  x0 <- x
  Y0 <- Y

  # Handle special cases where the mean_spec is log_ord and x is 0
  ind_log <- which(mod_spec$mean_spec == 'log_ord')
  if(length(ind_log) > 0) {
    for(j in ind_log) {
      yj <- Y[j,]
      xj <- x
      indj <- which(!is.na(yj))
      xj <- xj[indj]
      yj <- yj[indj]
      if(any((xj == 0) & (yj > 0))) {
        stop(paste0('For variable j=',j,
                    ', cases exist where mean_spec is log_ord, x=0, and m>0'))
      }
      matches <- which((xj == 0) & (yj == 0))
      if(length(matches) > 0) {
        Y[j,indj[matches]] <- NA
      }
    }
  }

  # Call remove_missing_variables to populate the following lists:
  calc_data <- list()
  for(n in 1:N) {
    if (all(is.na(Y[,n]))) {
      calc_data[[n]] <- list(log_ord_edge_case=TRUE)
    } else {
      # Remove missing variables
      remap <- tryCatch({remove_missing_variables(Y[,n],mod_spec)},
        warning=function(w) {
          message(n)
        })

      if (length(remap$y) == 0) {
        stop("This should not happen")
        next
      }
      calc_data_n <- list(x=x[n],y=remap$y,mod_spec=remap$mod_spec,
                         mapping=remap$mapping,mapping0=remap$mapping0,
                         ind=remap$ind,keep=remap$keep,log_ord_edge_case=FALSE)
      calc_data[[n]] <- calc_data_n
    }
  }

  # If necessary, remove log_ord edge cases
  if (remove_log_ord) {
    # remove_log_ord should be TRUE if these data are being used for a maximum
    # likelihood calculation. Otherwise (e.g., if they are being used for
    # posterior inference), keep the NULL objects, which correspond to
    # log_ord special cases.
    ind_to_keep <- unlist(lapply(calc_data,
                                 function(calc_data_n){
                                   !calc_data_n$log_ord_edge_case}))
    calc_data <- calc_data[ind_to_keep]
  }

  return(calc_data)
}

#' @title Quickly extract the correlation vector, z
#'
#' @description
#' Extract the full correlation vector of length choose(J+K,2), accounting for
#' the grouping structure specified by mod_spec$cdep_groups.
#'
#' @param th_y The parameter vector
#' @param mapping The mapping object that supports quick indexing
#' @param asMatrix Whether or not to return a (J+K) by (J+K) matrix of
#'   correlations (default FALSE)
#'
#' @return The full correlation vector (or matrix)
#'
#' @export
get_z_full_fast <- function(th_y,mapping,asMatrix=F) {

  J <- get_J(mapping$mod_spec)
  K <- get_K(mapping$mod_spec)

  z_full <- rep(0,choose(J+K,2))
  B <- which(!is.na(mapping$i1_i2_index))
  ind_z <- mapping$i1_i2_index[B]
  z_full[B] <- th_y[ind_z]

  if(!asMatrix) {
    return(z_full)
  }

  z_mat <- diag(J+K)
  z_mat[lower.tri(z_mat)] <- z_full
  z_mat <- t(z_mat)
  z_mat[lower.tri(z_mat)] <- z_full

  return(z_mat)
}

#' @title
#' Use a parallel for loop to calculate the vector of negative log-likelihoods
#'
#' @description
#' Use a parallel for loop to calculate the vector of negative log-likelihoods.
#'
#' @param th_y The parameter vector
#' @param calc_data Data needed for a speedy negative log-likelihood calculation
#' @param tf_cat_vect A vector to transform th_y from an unconstrained to a
#'   constrained representation (if necessary)
#'
#' @return The vector of negative log-likelihoods
#'
#' @export
calc_neg_log_lik_vect_multivariate <- function(th_y,calc_data,tf_cat_vect=NA) {
  if(!all(is.na(tf_cat_vect))) {
    th_y <- param_unconstr_to_constr(th_y,tf_cat_vect)
  }

  N <- length(calc_data)

  neg_log_lik_vect <- foreach(n=1:N,.combine=cbind,.packages=c('yada')) %dopar% {
    neg_log_lik_n <- calc_neg_log_lik_scalar_multivariate(th_y,calc_data[[n]])
  }
  return(as.vector(neg_log_lik_vect))
}

#' @title
#' Calculate the negative log-likelihood for a single observation
#'
#' @description
#' Calculate the negative log-likelihood for a single observation
#'
#' @param th_y The parameter vector
#' @param calc_data_n Data needed for a speedy negative log-likelihood
#'   calculation (for a single observation, n)
#'
#' @return The negative log-likelihood for the observation
#'
#' @export
calc_neg_log_lik_scalar_multivariate <- function(th_y,calc_data_n) {
  # If calc_data_n$log_ord_edge_case is TRUE, this is a log_ord edge case,
  # for which the likelihood is 1 and the negative log-likelihod is zero. It is
  # incumbent on functions that call these functions to "know" this and, if
  # necessary, handle it appropriately.
  if (calc_data_n$log_ord_edge_case)  {
    return(0)
  }

  prep_data_n <- calc_cond_gauss_int_inputs(th_y[calc_data_n$ind],
                                            calc_data_n$x,
                                            calc_data_n$y,
                                            calc_data_n$mapping)
  neg_log_lik_n <- -calc_conditional_gaussian_integral(prep_data_n$mean_vect,
                                                       prep_data_n$cov_mat,
                                                       prep_data_n$lo,
                                                       prep_data_n$hi,
                                                       prep_data_n$y_giv,
                                                       log=T)
  return(neg_log_lik_n)
}

#' @title
#' The outer function for calculating the negative log-likelihood in chunks
#'
#' @description
#' Use a parallel for loop along with "chunks" of input data to calculate the
#' vector of negative log-likelihoods. For each chunk, a conventional for loop
#' is used for each negative log-likelihood calculation and a parallel for loop
#' is used to iterate across chunks. The output is identical to
#' calc_neg_log_lik_vect_multivariate, but chunking the data can lead to better
#' performance on some syanalysis_names.
#'
#' @param th_y The parameter vector
#' @param calc_data Data needed for a speedy negative log-likelihood calculation
#' @param tf_cat_vect A vector to transform th_y from an unconstrained to a
#'   constrained representation (if necessary)
#' @param num_chunks The number of chunks to use (default:
#'   round(length(calc_data)/10))
#'
#' @return The vector of negative log-likelihoods
#'
#' @import nestfs
#' @export
calc_neg_log_lik_vect_multivariate_chunk_outer <-
  function(th_y,
           calc_data,
           tf_cat_vect=NA,
           num_chunks=round(length(calc_data)/10)) {
  if(!all(is.na(tf_cat_vect))) {
    th_y <- param_unconstr_to_constr(th_y,tf_cat_vect)
  }

  N <- length(calc_data)
  folds <- create.folds(num_chunks,N)

  neg_log_lik_vect <-
    foreach(k=1:num_chunks,.combine=c,.packages=c('yada')) %dopar% {
      calc_neg_log_lik_vect_multivariate_chunk_inner(th_y,calc_data[folds[[k]]])
    }

  # Reorder neg_log_lik_vect
  neg_log_lik_vect_out <- rep(NA,N)
  preceding <- 0
  for(k in 1:num_chunks) {
    fold_length = length(folds[[k]])
    neg_log_lik_vect_out[folds[[k]]] <-
      neg_log_lik_vect[preceding + (1:fold_length)]
    preceding <- preceding + fold_length
  }

  return(neg_log_lik_vect_out)
}

#' @title
#' The inner function for calculating the negative log-likelihood in chunks
#'
#' @description
#' A helper function called by calc_neg_log_lik_vect_multivariate_chunk_outer
#' that calculates the negative log-likelihood for a set of observations using
#' a conventional for loop.
#'
#' @param th_y The parameter vector
#' @param calc_data Data needed for a speedy negative log-likelihood calculation
#'
#' @return The vector of negative log-likelihoods
#'
#' @export
calc_neg_log_lik_vect_multivariate_chunk_inner <- function(th_y,calc_data) {

  N <- length(calc_data)
  neg_log_lik_vect <- rep(NA,N)

  for(n in 1:N) {
    neg_log_lik_vect[n] <-
      calc_neg_log_lik_scalar_multivariate(th_y,calc_data[[n]])
  }
  return(neg_log_lik_vect)
}
#' @title Calculate the overall (summed) negative log-likelihood
#'
#' @description
#' Calculate the overall negative log-likelihood for a set of observations by
#' calling calc_neg_log_lik_vect_multivariate and summing the resulting vector.
#'
#' @param th_y The parameter vector
#' @param calc_data Data needed for a speedy negative log-likelihood calculation
#' @param tf_cat_vect A vector to transform th_y from an unconstrained to a
#'   constrained representation (if necessary)
#'
#' @return The overall negative log-likelihood
#'
#' @export
calc_neg_log_lik_multivariate <- function(th_y,calc_data,tf_cat_vect=NA) {
  neg_log_lik_vect <-
    calc_neg_log_lik_vect_multivariate(th_y,calc_data,tf_cat_vect)
  return(sum(neg_log_lik_vect))
}

#' @title
#' A wrapper for calculating the negative log-likehood in fit_multivariate
#'
#' @description
#' This wrapper function for doing the multivariate negative log-likelihood
#' calculation accomplishes three things. First, if the negative log-likelihood
#' evaluates to NA it replaces the value with Inf. Second, it rescales the
#' normalized, input parameter vector (param) to account for the offset
#' (th_y_bar0) and scale (th_y_bar_scale); in particular, the rescaling is
#' param = th_y_bar0 + param*th_y_bar_scale. Third (optionally), it saves to
#' file progress information on the optimization (notably, the best fit found
#' so far). This progress information can be used resume interupted
#' optimizations.
#'
#' @param param The normalized parameter vector
#' @param th_y_bar0 The offset to use for the normalized parameter vector
#' @param th_y_bar_scale The scaling to use for the normalized parameter vector
#' @param calc_data The calculation data that support rapid calculation of the
#'   negative log-likelihood
#' @param tf_cat_vect The transform category vector so that the optimization
#'   can be uncsontrained
#' @param save_file (default: NA, none used) A save file to capture
#'   optimization progress
#'
#' @return The negative log-likelihood
#' @export
hjk_nll_wrapper <- function(param,
                            th_y_bar0,
                            th_y_bar_scale,
                            calc_data,
                            tf_cat_vect,
                            save_file=NA) {

  # Apply the offset and scale
  th_y_bar <- th_y_bar0 + param*th_y_bar_scale
  # Calculate the negative log-likelihood
  eta <- calc_neg_log_lik_multivariate(th_y_bar,
                                       calc_data,
                                       tf_cat_vect)
  # If the negative log-likelihood is NA, set it to Inf (that is what
  # dfoptim::hjk expects).
  if (is.na(eta)) {
    eta <- Inf
  }

  # If necessary, save progress to file
  if (!is.na(save_file)) {
    if (!file.exists(save_file)) {
      hjk_record <- list(n=1,
                         eta_vect=eta,
                         eta_best=eta,
                         param_best=param,
                         eta0=eta,
                         th_y_bar0=th_y_bar0,
                         th_y_bar_scale=th_y_bar_scale)
      n <- 1
    } else {
      hjk_record <- readRDS(save_file)
      hjk_record$n <- hjk_record$n + 1
      hjk_record$eta_vect <- c(hjk_record$eta_vect, eta)
      if (is.finite(eta)) {
        if (eta < hjk_record$eta_best) {
          hjk_record$eta_best <- eta
          hjk_record$param_best <- param
        }
      }
    }
    saveRDS(hjk_record, save_file)
  }
  return(eta)
}

#' @title
#' Renumber the groups in the input vector group0 so that the group numbering is
#' sequential integers
#'
#' @description
#' Renumber the groups in the input vector group0 so that the group numbering
#' is sequential integers (this is typically done after subsetting an original
#' group vector).
#'
#' @param groups0 The starting group vector, probably with gaps in it
#'
#' @return The updated group vector
#'
#' @export
renumber_groups <- function(groups0) {
  unique_groups <- sort(unique(groups0[!is.na(groups0)]))
  groups <- rep(NA,length(groups0))
  for(g in 1:length(groups0)) {
     if(!is.na(groups0[g])) {
       groups[g] <- which(groups0[g] == unique_groups)
     }
  }
  return(groups)
}

#' @title Get the non-singleton groups for the input group vector
#'
#' @description
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
#'
#' @return A vector of non-singlteon groups
#'
#' @export
get_non_singleton_groups <- function(groupVect) {
  counts <- table(groupVect)
  return(sort(as.numeric(names(counts))[counts > 1]))
}

#' @title Calculate the inputs to the conditional Gaussian integral
#'
#' @description
#' For a single observation, calculate the inputs to the conditional Gaussian
#' integral, which are mean_vect, coMat, lo, and hi. These are returned as a
#' list.
#'
#' @param th_y The parameter vector
#' @param x The independent variable (a scalar)
#' @param y The response variables (a vector)
#' @param mapping The mapping for fast extraction of indices (see
#'   [get_var_index_multivariate_mapping])
#'
#' @return A list with the mean vector (mean_vect), covariance matrix (cov_mat),
#'   low limit of integration (lo), high limit of integration (hi), and
#'   given/conditioned response variables (y_giv).
#'
#' @export
calc_cond_gauss_int_inputs <- function(th_y,x,y,mapping) {
  J <- get_J(mapping$mod_spec) # number of ordinal variables
  K <- get_K(mapping$mod_spec) # number of continuous variables

  if( (J == 0) && (K == 0)) {
    stop('J and K cannot both be zero')
  }

  # Calculate mean_vect, noise_vect, lo, and hi using for loops over the ordinal
  # and continuous variables.
  mean_vect  <- rep(NA,J+K)
  noise_vect <- rep(NA,J+K)
  if(J > 0) {
    lo <- rep(-Inf,J)
    hi <- rep( Inf,J)
    for(j in 1:J) {
      tau_j     <- th_y[get_var_index_multivariate_fast('tau'  ,mapping,j=j)]
      if(mapping$mod_spec$mean_spec[j] == 'pow_law_ord') {
        a_j       <- th_y[get_var_index_multivariate_fast('a'    ,mapping,j=j)]
        mean_vect[j] <- x^a_j
      } else if(mapping$mod_spec$mean_spec[j] == 'log_ord') {
        mean_vect[j] <- log(x)
      } else if(mapping$mod_spec$mean_spec[j] == 'lin_ord') {
        mean_vect[j] <- x
      } else {
        stop(paste0('Unsupported mean_spec = ',mapping$mod_spec$mean_spec[j]))
      }

      alpha_j   <- th_y[get_var_index_multivariate_fast('alpha',mapping,j=j)]
      if(mapping$mod_spec$noise_spec[j] == 'const') {
        noise_vect[j] <- alpha_j
      } else if(mapping$mod_spec$noise_spec[j] == 'lin_pos_int') {
        noise_vect[j] <- alpha_j[1]*(1 + x*alpha_j[2])
      } else {
        stop(paste0('Unsupported noise_spec = ',mapping$mod_spec$noise_spec[j]))
      }

      tau_j <- th_y[get_var_index_multivariate_fast('tau',mapping,j=j)]
      if(y[j] > 0) {
        lo[j] <- tau_j[y[j]]
      }
      if(y[j] < mapping$mod_spec$M[j]) {
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

      if(mapping$mod_spec$mean_spec[J+k] == 'pow_law') {
        mean_vect[J+k] <- a_k[2]*x^a_k[1] + a_k[3]
      } else {
        stop(paste0('Unsupported mean_spec = ',mapping$mod_spec$mean_spec[J+k]))
      }

      if(mapping$mod_spec$noise_spec[J+k] == 'const') {
        noise_vect[J+k] <- alpha_k
      } else if(mapping$mod_spec$noise_spec[J+k] == 'lin_pos_int') {
        noise_vect[J+k] <- alpha_k[1]*(1 + x*alpha_k[2])
      } else {
        stop(paste0('Unsupported noise_spec = ',mapping$mod_spec$noise_spec[J+k]))
      }
    }
    y_giv <- y[J + (1:K)]
  } else {
    y_giv <- c()
  }

  # Calculate the covariance matrix (cov_mat)
  z_full <- rep(0,choose(J+K,2))
  B <- which(!is.na(mapping$i1_i2_index))
  ind_z <- mapping$i1_i2_index[B]
  z_full[B] <- th_y[ind_z]

  z_mat <- diag(J+K)
  z_mat[lower.tri(z_mat)] <- z_full
  z_mat <- t(z_mat)
  z_mat[lower.tri(z_mat)] <- z_full

  cov_mat <- as.matrix(noise_vect) %*% t(as.matrix(noise_vect))
  cov_mat <- cov_mat * z_mat
  return(list(mean_vect=mean_vect,cov_mat=cov_mat,lo=lo,hi=hi,y_giv=y_giv))
}


# TODO: consider adding support for missing variables in sim_multivariate

#'@title
#' Create simulated observations for a multivarate mixed cumulative probit model
#'
#' @description
#' Create simulated observations for a multivariate mixed cumulative probit
#' model. For x, either the number of samples (N) and a parameter vector (th_x)
#' must be given, or the full vector (x) must be given.
#'
#' @param th_y The parameter vector for the y-model to simulate with
#' @param mod_spec The model specification (for the y-model)
#' @param N The number of samples to simulate
#' @param th_x The parameterization for x
#' @param x The vector of independent variables
#'
#' @return A list object of simulated data containing x, Y, and Ystar
#'
#' @export
sim_multivariate <- function(th_y,mod_spec,N=NA,th_x=NA,x=NA) {
  # N is the number of simulated observations
  # th_x parameterizes x. Currently, only a uniform or exponential distribution
  #   is supported
  # th_y parameterizes y (given x)

  have_x_model  <- !all(is.na(th_x)) && !is.na(N)
  have_x_direct <- !all(is.na(x))

  if(have_x_model + have_x_direct != 1) {
    stop('Either (1) th_x and N should be input or (2) x should be input')
  }

  if(have_x_model) {
    if(tolower(th_x$fit_type) == 'exponential') {
      x <- rexp(N,th_x$fit)
    } else if(tolower(th_x$fit_type) == 'uniform') {
      x <- runif(N,th_x$fit[1],th_x$fit[2])
    } else {
      stop('Only uniform currently supported')
    }
  } else {
    N <- length(x)
  }

  J <- get_J(mod_spec)
  K <- get_K(mod_spec)

  # Populate Ystar by iterating over observations
  mapping <- get_var_index_multivariate_mapping(mod_spec)
  Ystar <- matrix(NA,J+K,N)
  for(n in 1:N) {
    if(J > 0) {
      for(j in 1:J) {
        th_v <- th_y[get_univariate_indices(mod_spec,j=j)]
        mod_spec_j <- list(mean_spec=mod_spec$mean_spec[j],
                          noise_spec=mod_spec$noise_spec[j],
                          M=mod_spec$M[j],
                          J=1,
                          K=0)
        Ystar[j,n] <- calc_mean_univariate_ord(x[n],th_v,mod_spec_j)
      }
    }

    if(K > 0) {
      for(k in 1:K) {
        th_w <- th_y[get_univariate_indices(mod_spec,k=k)]
        mod_spec_k <- list(mean_spec=mod_spec$mean_spec[J+k],
                          noise_spec=mod_spec$noise_spec[J+k],
                          J=0,
                          K=1)
        Ystar[J+k,n] <- calc_mean_univariate_cont(x[n],th_w,mod_spec_k)
      }
    }

    # Add noise to the latent vector for observation n
    # The value of y = c(1,1,1,1) is immaterial in the following command because
    # only cov_mat is used
    cgi <- calc_cond_gauss_int_inputs(th_y,x[n],c(1,1,1,1),mapping)
    Ystar[,n] <- Ystar[,n] + MASS::mvrnorm(mu=rep(0,J+K),Sigma=cgi$cov_mat)
  }

  Y <- Ystar
  for(j in 1:J) {
    tau_j <- th_y[get_var_index_multivariate_fast('tau',mapping,j=j)]
    for(n in 1:N) {
      # If the mean_spec is 'log_ord' and x[n] is zero, Ystar is -Inf and the
      # ordinal category, m, must be explicitly set to 0
      if( Ystar[j,n] == -Inf) {
        Y[j,n] <- 0
      } else {
        # The ordinal observation
        Y[j,n] <- as.numeric(cut(Ystar[j,n],c(-Inf,tau_j,Inf))) - 1
      }
    }
  }

  return(list(x=x,Y=Y,Ystar=Ystar))
}

#' @title Fit a multivariate mixed cumulative probit model.
#'
#' @description
#' Do a maximum likelihood fit of  multivariate mixed cumulative probit model
#' using the Hooke-Jeeves (hjk) algorithm. The parameter vector used in the
#' optimization has, roughly, a mean of zero and an expectation that the best
#' actual value is within about +/-1 of the starting value. This is
#' accomplished by subtracting the mean of the conditionally dependent fit and
#' dividing by the standard error of the conditionally dependent fit.
#'
#' @param x The vector of independent variables
#' @param Y The matrix of responses
#' @param mod_spec The model specification
#' @param cindep_model A list specifying the conditionally independent model
#'   (likely the output of build_cindep_model). cindep_model must contain
#'   cindep_model$th_y_bar and cindep_model$th_y_bar_scale.
#' @param save_file String containing the progress file name
#' @param hjk_control A list passed to the function dfoptim::hjk()
#'
#' @return A list object containing  the best-fit parameter vector (th_y), and
#'   the unconstrained best-fit parameter (th_y_bar), the return object from
#'   the optimization (hjk_output), the model specification (mod_spec), and 
#'   a record of how many legs it took for optimization (legs)
#'
#' @export
fit_multivariate <- function(x,Y,mod_spec,
                             cindep_model,
                             prog_file=NA,
                             save_file=NA,
                             hjk_control=list()) {
  
  if (!is.na(prog_file)) {
    if (file.exists(prog_file)) {
      print("prog_file already exists, starting from previous run")
    }
  }
  
  # Create (a) the calculation data that supports rapid calculation of the
  # negative log-likelihood (calc_data) and (b) the transform category vector
  # so that the optimization can be unconstrained (tf_cat_vect).
  calc_data = prep_for_neg_log_lik_multivariate(x,Y,
                                                mod_spec,remove_log_ord=TRUE)
  tf_cat_vect = get_multivariate_transform_categories(mod_spec)
  
  # Initialize vectors if this is the first run
  if (!file.exists(prog_file)) {
    # Extract initialization variables from cindep_model
    th_y_bar0      <- cindep_model$th_y_bar
    th_y_bar_scale <- cindep_model$th_y_bar_se
    
    # number of correlation terms
    Nz <- get_z_length(mod_spec)
    th_y_bar0 <- c(th_y_bar0, rep(0,Nz))
    
    # For the scale of the correlations terms, use the median of the other
    # variables
    th_y_bar_scale <- c(th_y_bar_scale, rep(median(th_y_bar_scale), Nz))
    
    # Set initial parameter vector to 0
    param0 <- rep(0,length(th_y_bar0))
    
    # Set optimization leg to 1, meaning this is the first run
    leg <- 1
    
  } else {
    # Manipulate prog_file to find all previous runs
    n <- 1  # at least one leg already exists
    split_file_list <- strsplit(prog_file,"/|\\.")  # split prog_file
    data_dir <- split_file_list[[1]][1]  # extract data directory
    file_name <- split_file_list[[1]][2]  # extract file name without extension
    prog_file_vec <- list.files(data_dir,file_name)  # all hjk files
    
    # Find the most recent prog_file and import
    if (length(grep("leg",prog_file_vec)) > 0) {
      max_leg <- max(grep("leg",prog_file_vec))
      prog_file0 <- readRDS(paste0(data_dir,"/",file_name,"_leg",max_leg,".rds"))
    } else {
      max_leg <- 1
      prog_file0 <- readRDS(prog_file)
    }
    
    leg <- n + max_leg  # new leg
    
    # Initialize values from previous run
    param0 <- prog_file0$param_best
    th_y_bar0 <- prog_file0$th_y_bar0
    th_y_bar_scale <- prog_file0$th_y_bar_scale
    
    # New save file, adding leg information
    prog_file <- paste0(data_dir,"/",file_name,"_leg",leg,".rds")
  }
  
  
  # Solve the optimization problem
  hjk_output <- dfoptim::hjk(param0,
                             hjk_nll_wrapper,
                             control=hjk_control,
                             th_y_bar0=th_y_bar0,
                             th_y_bar_scale=th_y_bar_scale,
                             calc_data=calc_data,
                             tf_cat_vect=tf_cat_vect,
                             save_file=prog_file)
  
  
  # Extract the fit (changing back to the constrained representation)
  th_y_bar <- th_y_bar0 + hjk_output$par * th_y_bar_scale
  th_y <- param_unconstr_to_constr(th_y_bar, tf_cat_vect)
  
  output <- list(th_y=th_y,
                 th_y_bar=th_y_bar,
                 hjk_output=hjk_output,
                 mod_spec=mod_spec,
                 leg=leg)
  
  # Add removed_vars to list, if applicable
  if ("removed_vars" %in% names(cindep_model)) {
       output <- append(output, 
                        list(removed_vars=cindep_model$removed_vars))
  }
  
  if (!is.na(save_file)) {
    saveRDS(output, save_file)
  }
  
  return(output)
}


#' @title Sample from the posterior density of x
#'
#' @description
#' Do Metropolis-Hastings sampling of the posterior probability
#' p(x|y,theta_x,theta_y) for the mixed cumulative probit model given the
#' response vector y.
#'
#' @param xcalc A vector of ages at which to calculate the posterior probability
#' @param y The response vector for a single observation
#' @param th_x Parameterization for prior on x
#' @param th_y Parameterization for likelihood
#' @param mod_spec The model specification
#' @param seed An optional input seed to make sampling reproducibile
#'   (default: NA, not used)
#'
#' @return A vector of posterior probabilities
#'
#' @export
sample_x_posterior <- function(y,
                               th_x,
                               th_y,
                               mod_spec,
                               num_samp,
                               thinning=1,
                               prop_rescale=.1,
                               seed=NA) {

  # TODO: here and elsewhere consider allowing observations with missing values
  #       for the sampling and posterior inference (but throw a warning)
  if (!is.na(seed)) {
    set.seed(seed)
  }

  # Get an approximate range for x using the individual variable fits.
  xmin_vect <- c()
  xmax_vect <- c()

  J <- get_J(mod_spec)
  K <- get_K(mod_spec)
  if (J > 0) {
    for (j in 1:J) {
      v <- y[j]
      if (!is.na(v)) {
        th_v <- extract_univariate_param_vect(th_y,mod_spec,j=j)
        mod_spec_j <- list(J=1,
                          M=mod_spec$M[j],
                          mean_spec=mod_spec$mean_spec[j],
                          noise_spec=mod_spec$noise_spec[j])
        mod_spec_j$cdep_spec <- "indep"
        xrange_j <- get_approx_x_range_ord(v,th_v,mod_spec_j)
        xmin_vect <- c(xmin_vect,min(xrange_j))
        xmax_vect <- c(xmax_vect,max(xrange_j))
      }
    }
  }

  if (K > 0) {
    for (k in 1:K) {
      w <- y[J + k]
      if (!is.na(w)) {
        th_w <- extract_univariate_param_vect(th_y,mod_spec,k=k)
        mod_spec_k <- list(K=1,
                          mean_spec=mod_spec$mean_spec[J+k],
                          noise_spec=mod_spec$noise_spec[J+k])
        mod_spec_k$cdep_spec <- "indep"
        xrange_k <- get_approx_x_range_cont(w,th_w,mod_spec_k)
        xmin_vect <- c(xmin_vect,min(xrange_k))
        xmax_vect <- c(xmax_vect,max(xrange_k))
      }
    }
  }

  xlo <- median(xmin_vect)
  if(xlo < 0 | is.na(xlo)) {
    xlo <- 0
  }

  xhi <- median(xmax_vect)
  if(is.na(xhi)) {
    xhi <- 0.001
  }
  if(xhi <= xlo) {
    if (xlo == 0) {
      xhi <- 1
    } else {
      xhi <- 2*xhi
    }
  }
  x0 <- (xlo + xhi) / 2
  dx <- (xhi-xlo)*prop_rescale

  x_vect <- rep(NA,num_samp)
  x <- x0
  f <- calc_x_posterior(y,th_x,th_y,mod_spec,x,normalize=F)
  f <- f$density
  for(n in 1:num_samp) {
    x_prop <- x + rnorm(1)*dx

    if (x_prop < 0) {
      # Reject if x is less than zero
      accept <- FALSE
    } else {
      f_prop <- calc_x_posterior(y,th_x,th_y,mod_spec,x_prop,normalize=F)
      f_prop <- f_prop$density
      if(!is.finite(f_prop)) {
        accept <- FALSE
      } else {
        alpha <- min(1,f_prop/f)
        accept <- runif(1) < alpha
      }
    }


    if(accept) {
      x <- x_prop
      f <- f_prop
    }
    x_vect[n] <- x
  }

  if (thinning != 1) {
    x_vect <- x_vect[seq(1,num_samp,thinning)]
  }
  return(x_vect)
}

#' @title Calculate the posterior density of x
#'
#' @description
#' Calculate the posterior probability p(x|y,theta_x,theta_y) for the mixed
#' cumulative probit model at the points in xcalc given the response vector
#' y. The calculation is for a single observation. If the normalization is done,
#' xcalc is assumed to be evenly spaced.
#'
#' @param xcalc A vector of ages at which to calculate the posterior probability
#' @param y The response vector for a single observation
#' @param th_x Parameterization for prior on x
#' @param model The model used to calculate the posterior probability
#' @param xcalc A vector of evenly spaced values at which to calculate the 
#'   posterior probability density (default: c())
#' @param normalize Whether or not to normalize to integrate to 1 (default:
#'   TRUE).
#' @param seed An optional input seed to make sampling reproducibile
#'   (default: NA, not used)
#' @return A vector of posterior probabilities
#' @export
calc_x_posterior <- function(y,th_x,model,
                             xcalc=c(),normalize=T,seed=NA) {
  
  if ( (length(xcalc) > 0) && !is.na(seed) ) {
    stop("A seed should not be provided if xcalc is provided")
  }
  
  # If xcalc is input and normalize is TRUE, xcalc must be evenly spaced
  # TODO: add support for trapezoidal integration
  if ( (length(xcalc) > 0) && normalize) {
    dx <- xcalc[2] - xcalc[1]
    equality_check <- all.equal(diff(xcalc), rep(dx,length(xcalc)-1))
    if (class(equality_check) == "character") {
      # all.equal return a character object if the check fails
      bad_xcalc <- TRUE
    } else {
      # equality_check is logical
      bad_xcalc <- !equality_check
    }
    if (bad_xcalc) {
      stop('xcalc must be evenly spaced if it is input and normalize is TRUE')
    }
  }
  
  # Define the th_y and mod_spec from the given model
  th_y <- model$th_y
  mod_spec <- model$mod_spec
  
  if ( (length(xcalc) > 0) && !is.na(seed) ) {
    stop("A seed should not be provided if xcalc is provided")
  }
  # If xcalc is not provided, determine it adaptively
  if (length(xcalc) == 0) {
    # Sample 1000 times. Sensible sampling parameters are determined adaptively
    # within sample_x_posterior
    x_samp  <- sample_x_posterior(y,
                                  th_x,
                                  th_y,
                                  mod_spec,
                                  1000,
                                  prop_rescale=1,
                                  seed=seed)
    
    # Pad by 25% on either side
    xmin <- min(x_samp)
    xmax <- max(x_samp)
    
    xmin <- xmin - (xmax-xmin)*.25
    xmax <- xmax + (xmax-xmin)*.25
    
    # Ensure that xmin is not negative
    if (xmin < 0) {
      xmin <- 0
    }
    
    # Use 1000 samples for xcalc on the range xmin to xmax
    xcalc <- seq(xmin,xmax,len=1000)
    dx <- (xmax-xmin)/1000
  } else {
    x_samp <- c()
  }
  
  # Handle the possibility that x=0 and m>0 with a log_ord mean_spec by
  # removing the first observation from xcalc if it is zero.
  fix_x <- FALSE
  J <- get_J(mod_spec)
  if (J > 0) {
    for (j in 1:J) {
      if (mod_spec$mean_spec[j] == "log_ord") {
        if (xcalc[1] == 0) {
          if (y[j] > 0 & !is.na(y[j])) {
            fix_x <- TRUE
          }
        }
      }
    }
  }
  
  if(fix_x) {
    p_xy <- calc_joint(xcalc[-1],y,th_x,th_y,mod_spec)
    p_xy <- c(0,p_xy)
  } else {
    p_xy <- calc_joint(xcalc,y,th_x,th_y,mod_spec)
  }
  
  if (normalize) {
    # This point should not be reached unless xcalc is evenly spaced
    p_x <- p_xy / sum(p_xy) / dx
  } else {
    p_x <- p_xy
  }
  return(list(x=xcalc,density=p_x,x_samp=x_samp))
}

#' @title Calculate the joint density probability p(x,y|th_x,th_y)
#'
#' @description
#' Calculate the joint density probability p(x,y|th_x,th_y) for the mixed
#' cumulative probit model at the points in xcalc. xcalc is assumed to be evenly
#' spaced.
#'
#' @param xcalc A vector of ages at which to calculate the posterior probability
#' @param y The response vector for a single observation
#' @param th_x Parameterization for prior on x
#' @param th_y Parameterization for likelihood (conditional on x)
#' @param mod_spec The model specification
#'
#' @return A vector of joint probabilities
#'
#' @export
calc_joint <- function(xcalc,y,th_x,th_y,mod_spec) {
  xcalc0 <- xcalc
  ind_fix <- rep(FALSE, length(xcalc0))
  J <- get_J(mod_spec)
  if (J > 0) {
    for (j in 1:J) {
      if (mod_spec$mean_spec[j] == "log_ord") {
        if (y[j] > 0 & !is.na(y[j])) {
          ind_fix[which(xcalc == 0)] <- TRUE
        }
      }
    }
  }

  xcalc <- xcalc0[!ind_fix]
  Y <- matrix(y,nrow=length(y),ncol=length(xcalc))

  # Since calc_data is being used for posterior inference, do not use
  # remove_log_ord=TRUE (the default is FALSE)
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,Y,mod_spec,no_missing_var=F)
  # the vector of log-likelihoods
  log_lik_vect0 <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data)
  log_lik_vect <- rep(Inf, length(xcalc0))
  log_lik_vect[!ind_fix] <- log_lik_vect0

  log_prior_vect <- log(calc_x_density(xcalc,th_x))

  log_joint_vect <- log_lik_vect + log_prior_vect

  fv <- exp(log_joint_vect)
  fv[!is.finite(fv)] <- 0
  return(fv)
}

#' @title Calculate the density for a model th_x, p(x|th_x)
#'
#' @description
#' Calculate the density at x given a parameterization for the density of
#' th_x. Currently, the exponential distribution, Weibull mixtures, and
#' uniform distribution are supported. If fit_type is 'exponential' or
#' 'uniform', the fit parameter (fit) is a vector, whereas for 'offset_weib_mix'
#' it is a list with the vectors lambda (weighting), shape, and scale.
#'
#' @param x A vector of ages at which to calculate the density
#' @param th_x A list with the fit type (fit_type) and the fit parameter
#'   specification (fit; see details)
#' @export
calc_x_density <- function(x,th_x) {
  if(tolower(th_x$fit_type) == 'exponential') {
    return(dexp(x,th_x$fit))
  } else if (tolower(th_x$fit_type) == 'offset_weib_mix') {
    return(calc_weib_mix_density(x+th_x$weib_offset,
                          th_x$fit$lambda,
                          c(rbind(th_x$fit$shape,
                                  th_x$fit$scale))))
  } else if (tolower(th_x$fit_type) == 'uniform') {
    f <- rep(1,length(x))
    ind0 <- x < th_x$fit[1] | th_x$fit[2] < x
    f[ind0] <- 0
    return(f/(th_x$fit[2]-th_x$fit[1]))
  } else {
    stop(paste('Unsupported fit type:',th_x$fit_type))
  }
}

#' @title
#' Analyze an input density by calculating the mean and credible intervals
#'
#' @description
#' \code{analyze_x_posterior} analyzes an input density given by the input pair
#' of vectors xv and fv by calculating the mean and confidence intervals for a
#' handful of quantiles (0.001, 0.025, 0.5, 0.975, and 0.999) or the 
#' highest density interval (0.95, 0.99). xv is assumed to
#' be evenly spaced. Optionally, a known age (xknown) can be input, for which
#' the density is estimated.
#'
#' @param xv A vector of evenly spaced ages at which the density, fv,
#'   is calculated
#' @param fv A vector of densities
#' @param ci_type Whether the credible interval should be calculated using 
#'   "quantiles" or highest density interval ("hdi"). (default is "hdi")
#' @param xknown Known age [optional]
#' 
#'
#' @return A list containing the results of the analysis
#'
#' @import bayestestR
#' @export
analyze_x_posterior <- function(xv,fv,ci_type="hdi",xknown=NA) {
  if (!(ci_type %in% c('quantiles','hdi'))) {
    stop(paste0("Current selection of ci_type=",ci_type," not supported."))
  }
  
  # TODO: consider adding a check that xv is evenly spaced
  dx <- xv[2] - xv[1]
  # fv is the same as p_x
  fv <- fv / sum(fv) / dx
  Fv <- c(0,dx*cumsum(fv)[1:(length(xv)-1)])
  
  if (ci_type=="quantiles") {
    q <- 0.001
    n <- max(which(Fv <= q))
    xlolo <- xv[n] + dx*(q-Fv[n])/(Fv[n+1]-Fv[n])
    flolo <- fv[n] + (xlolo-xv[n])*(fv[n+1]-fv[n])/dx
    
    q <- 0.025
    n <- max(which(Fv <= q))
    xlo <- xv[n] + dx*(q-Fv[n])/(Fv[n+1]-Fv[n])
    flo <- fv[n] + (xlo-xv[n])*(fv[n+1]-fv[n])/dx
    
    q <- 0.975
    n <- max(which(Fv <= q))
    xhi <- xv[n] + dx*(q-Fv[n])/(Fv[n+1]-Fv[n])
    fhi <- fv[n] + (xhi-xv[n])*(fv[n+1]-fv[n])/dx
    
    q <- 0.999
    n <- max(which(Fv <= q))
    xhihi <- xv[n] + dx*(q-Fv[n])/(Fv[n+1]-Fv[n])
    fhihi <- fv[n] + (xhihi-xv[n])*(fv[n+1]-fv[n])/dx
  } 
  if (ci_type=="hdi") {
    samples <- sample(xv, 10000, replace=T, prob=fv)
    hdi99 <- suppressWarnings(hdi(samples, ci=0.99))
    hdi95 <- suppressWarnings(hdi(samples, ci=0.95))
    
    xlolo <- hdi99$CI_low
    flolo <- fv[which(xv==xlolo)]
    
    xlo <- hdi95$CI_low
    flo <- fv[which(xv==xlo)]
    
    xhi <- hdi95$CI_high
    fhi <- fv[which(xv==xhi)]
    
    xhihi <- hdi95$CI_high
    fhihi <- fv[which(xv==xhihi)]
    
  }
  
  q <- 0.5
  n <- max(which(Fv <= q))
  xmed <- xv[n] + dx*(q-Fv[n])/(Fv[n+1]-Fv[n])
  fmed <- fv[n] + (xmed-xv[n])*(fv[n+1]-fv[n])/dx
  
  xmean <- sum(fv*xv)*dx
  n <- max(which(xv < xmean))
  fmean <- fv[n] + (xmean-xv[n])*(fv[n+1]-fv[n])/dx
  
  n <- which(fv==max(fv))
  xmode <- xv[n]
  fmode <- fv[n] + (xmode-xv[n])*(fv[n+1]-fv[n])/dx
  
  return_list <- list(x=xv,density=fv,dx=dx,
                      xlolo=xlolo,xlo=xlo,xmed=xmed,xhi=xhi,xhihi=xhihi,
                      xmean=xmean,xmode=xmode,
                      flolo=flolo,flo=flo,fmed=fmed,fhi=fhi,fhihi=fhihi,
                      fmean=fmean,fmode=fmode)
  if(!is.na(xknown)) {
    # Calculate the expectation (over the density) of (x-xknown)^2
    expected_sqr_err <- sum(dx*fv * (xv-xknown)^2)
    if(xknown == 0) {
      fknown <- fv[1]
      Fknown <- 0
    } else {
      n <- max(which(xv < xknown))
      fknown <- fv[n] + (fv[n+1] - fv[n]) * (xknown-xv[n]) / dx
      Fknown <- Fv[n] + (Fv[n+1] - Fv[n]) * (xknown-xv[n]) / dx
    }
    return_list$expected_sqr_err <- expected_sqr_err
    return_list$xknown <- xknown
    return_list$fknown <- fknown
    return_list$Fknown <- Fknown
  }
  return(return_list)
}

#' @title
#' Calculate the Kullback-Leibler (KL) divergence of a posterior density from a
#' prior density
#'
#' @description
#' The inputs are an output from \code{calc_x_posterior}, x_post_obj, and a
#' parameterization of the prior, th_x (see \code{calc_x_density}. The output
#' is the divergence of the posterior density from the prior density, which is
#' a measure of the information gained from the update step. The divergence is
#' calculated using a base 2 logarithm, so the units of the output are bits.
#' Any points for which the prior or posterior density is undefined,
#' non-finite, or zero are ignored.
#'
#' @param x_post_obj The result of a call to calc_x_posterior
#' @param th_w The parameterization of the prior
#'
#' @return The Kullback-Leibler (KL) divergence
#'
#' @export
calc_kl_div <- function(x_post_obj,th_x) {
  # Calculate the prior
  fprior <- calc_x_density(x_post_obj$x, th_x)

  # Extract the posterior
  fpost <- x_post_obj$density

  # Remove densities that are NA
  ind_bad <- is.na(fprior) | is.na(fpost)
  fprior <- fprior[!ind_bad]
  fpost  <- fpost [!ind_bad]

  # Remove densities that are not finite
  ind_bad <- !is.finite(fprior) | !is.finite(fpost)
  fprior <- fprior[!ind_bad]
  fpost  <- fpost [!ind_bad]

  # Remove densities that are zero
  ind_bad <- (fprior == 0) | (fpost == 0)
  ind_zero <- (fprior == 0) | (fpost == 0)
  fprior <- fprior[!ind_bad]
  fpost  <- fpost [!ind_bad]

  # TODO: Consider using trapezoidal integration to avoid the need to assume
  #       that points are evenly spaced.
  # Assume that the points are evenly spaced (even after accounting for bad
  # points)
  dx <- x_post_obj$x[2] - x_post_obj$x[1]

  # Calculate the KL-divergence
  kl_div <- sum(fpost*log2(fpost/fprior))*dx
  return(kl_div)
}
