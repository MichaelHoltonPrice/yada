#' @title
#' Convert from an integer to string representation of the noise specification
#'
#' @description
#' Given an integer specification of the noise, convert to a string
#' specification. This is done for code readability. In the technical
#' documentation, the variable S^{(sigma)} gives the noise specifiation, but
#' for readability noise_spec is instead used in the code. An error is thrown
#' for invalid inputs. The valid cases are:
#'
#' int	str
#' 0	const
#' 1	lin_pos_int
#' 
#' @param noise_spec_int The noise specification as an integer
#'
#' @return noise_spec_str The noise specification as a string
#'
#' @examples
#' noise_spec_int_to_str(0)
#' noise_spec_int_to_str(1)
#'
#' @export
noise_spec_int_to_str <- function(noise_spec_int) {
  if(length(noise_spec_int) != 1) {
    stop('The input, noise_spec_int, does not have length 1')
  }
  if(!is.numeric(noise_spec_int)) {
    stop('The input, noise_spec_int, is not numeric')
  }
  if(noise_spec_int == 0) {
    noise_spec_str <- 'const'
  } else if(noise_spec_int == 1) {
    noise_spec_str <- 'lin_pos_int'
  } else {
    stop(paste0('Unrecognized case, noise_spec_int = ',noise_spec_int))
  }
  return(noise_spec_str)
}

#' @title Calculate the noise for a yada noise specification
#'
#' @description
#' Calculate the noise, sigma, given x, the parameter vector alpha, and the
#' noise specification noise_spec.
#' 
#' @param x The independent variable
#' @param noise_spec The noise specification
#' @param alpha The parameter vector
#' @param  The parameter vector
#' @return sigma(x,alpha), the noise (same length as x)
#' @export
calc_noise <- function(x,noise_spec,alpha) {
  if(is.numeric(noise_spec)) {
    noise_spec <- noise_spec_int_to_str(noise_spec)
  }

  if(noise_spec == 'const') {
    return(rep(alpha[1],length(x)))
  } else if(noise_spec == 'lin_pos_int') {
    return(alpha[1]*(1 + x*alpha[2]))
  } else {
    stop(paste0('Unrecognized case, noise_spec = ',noise_spec))
  }
}
#' @title Get the number of variables for a yada noise specification
#'
#' @description
#' Return the number of parameters for the noise specification noise_spec.
#' 
#' @param noise_spec The noise specification
#'
#' @return The number of noise parameters
#'
#' @export
get_num_var_noise <- function(noise_spec) {
 if(is.numeric(noise_spec)) {
    noise_spec <- noise_spec_int_to_str(noise_spec)
  }

  if(noise_spec == 'const') {
    return(1)
  } else if(noise_spec == 'lin_pos_int') {
    return(2)
  } else {
    stop(paste0('Unrecognized case, noise_spec = ',noise_spec))
  }
}
#' @title
#' Return the transform categories (see param_constr_to_unconstr) for a yada noise
#' specification
#'
#' @description
#' For a given noise specification, return a vector that gives the categories
#' used by param_constr2uncontr and param_constr_to_unconstr.
#'
#' @param noise_spec The noise specification
#'
#' @return A vector of categories
#'
#' @export
get_noise_transform_categories <- function(noise_spec) {
  if(is.numeric(noise_spec)) {
    noise_spec <- noise_spec_int_to_str(noise_spec)
  }

  if(noise_spec == 'const') {
    return(1)
  } else if(noise_spec == 'lin_pos_int') {
    return(c(1,1))
  } else {
    stop(paste0('Unrecognized case, noise_spec = ',noise_spec))
  }
}
