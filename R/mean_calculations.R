#' @title
#' Convert from an integer to string representation of the mean specification
#'
#' @description
#' Given an integer specification of the mean, convert to a string
#' specification. This is done for code readability. In the technical
#' documentation, the variable S^{(f)} gives the mean specifiation, but for
#' readability mean_spec is instead used in the code. An error is thrown for
#' invalid inputs. The valid cases are:
#'
#' int	str
#' 0	pow_law_ord
#' 1	log_ord
#' 2	lin_ord
#' 3	pow_law
#' 
#' @param mean_spec_int The mean specification as an integer
#'
#' @return The mean specification as a string
#'
#' @examples
#' mean_spec_int_to_str(0)
#' mean_spec_int_to_str(1)
#'
#' @export
mean_spec_int_to_str <- function(mean_spec_int) {
  if(length(mean_spec_int) != 1) {
    stop('The input, mean_spec_int, does not have length 1')
  }
  if(!is.numeric(mean_spec_int)) {
    stop('The input, mean_spec_int, is not numeric')
  }
  if(mean_spec_int == 0) {
    mean_spec_str <- 'pow_law_ord'
  } else if(mean_spec_int == 1) {
    mean_spec_str <- 'log_ord'
  } else if(mean_spec_int == 2) {
    mean_spec_str <- 'lin_ord'
  } else if(mean_spec_int == 3) {
    mean_spec_str <- 'pow_law'
  } else {
    stop(paste0('Unrecognized case, mean_spec_int = ',mean_spec_int))
  }
  return(mean_spec_str)
}

#' @title Calculate the mean response for a yada mean specification
#'
#' @description
#' Calculate the mean, f, given x, the parameter vector a, and the mean
#' specification mean_spec.
#' 
#' @param x The independent variable
#' @param mean_spec The mean specification
#' @param a The parameter vector
#'
#' @return f(x,a), the mean (same length as x)
#'
#' @export
calc_mean <- function(x,mean_spec,a=NA) {
  if(is.numeric(mean_spec)) {
    mean_spec <- mean_spec_int_to_str(mean_spec)
  }

  if(mean_spec == 'pow_law_ord') {
    return(x^a[1])
  } else if(mean_spec == 'log_ord') {
    return(log(x))
  } else if(mean_spec == 'lin_ord') {
    return(x)
  } else if(mean_spec == 'pow_law') {
    return(a[2]*x^a[1]+a[3])
  } else {
    stop(paste0('Unrecognized case, mean_spec = ',mean_spec))
  }
}

#' @title Get the number of variables for a yada mean specification
#'
#' @description
#' Return the number of parameters for the mean specification mean_spec.
#' 
#' @param mean_spec The mean specification
#'
#' @return The number of mean parameters
#'
#' @export
get_num_var_mean <- function(mean_spec) {
 if(is.numeric(mean_spec)) {
    mean_spec <- mean_spec_int_to_str(mean_spec)
  }

  if(mean_spec == 'pow_law_ord') {
    return(1)
  } else if(mean_spec == 'log_ord') {
    return(0)
  } else if(mean_spec == 'lin_ord') {
    return(0)
  } else if(mean_spec == 'pow_law') {
    return(3)
  } else {
    stop(paste0('Unrecognized case, mean_spec = ',mean_spec))
  }
}

#' @title
#' Return the transform categories (see param_constr_to_unconstr) for a yada mean
#' specification
#'
#' @description
#' For a given mean specification, return a vector that gives the categories
#' used by param_constr_to_unconstr and param_constr_to_unconstr.
#'
#' @param mean_spec The mean specification
#'
#' @return A vector of categories
#'
#' @export
get_mean_transform_categories <- function(mean_spec) {
  if(is.numeric(mean_spec)) {
    mean_spec <- mean_spec_int_to_str(mean_spec)
  }

  if(mean_spec == 'pow_law_ord') {
    return(1)
  } else if(mean_spec == 'log_ord') {
    return(c())
  } else if(mean_spec == 'lin_ord') {
    return(c())
  } else if(mean_spec == 'pow_law') {
    return(c(1,1,0))
  } else {
    stop(paste0('Unrecognized case, mean_spec = ',mean_spec))
  }
}
