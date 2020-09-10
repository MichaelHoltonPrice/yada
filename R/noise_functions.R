#' Given an integer specification of the noise, convert to a string
#' specification. This is done for code readability. In the technical
#' documentation, the variable S^{(sigma)} gives the noise specifiation, but
#' for readability noiseSpec is instead used in the code. An error is thrown for
#' invalid inputs. The valid cases are:
#'
#' int	str
#' 0	const
#' 1	lin_pos_int
#' 2	hyperb
#' 
#' @param noiseSpec_int The noise specification as an integer
#' @return noiseSpec_str The noise specification as a string
#' @examples
#' noiseSpec_int2str(0)
#' noiseSpec_int2str(1)
#' @export
noiseSpec_int2str <- function(noiseSpec_int) {
  if(length(noiseSpec_int) != 1) {
    stop('The input, noiseSpec_int, does not have length 1')
  }
  if(!is.numeric(noiseSpec_int)) {
    stop('The input, noiseSpec_int, is not numeric')
  }
  if(noiseSpec_int == 0) {
    noiseSpec_str <- 'const'
  } else if(noiseSpec_int == 1) {
    noiseSpec_str <- 'lin_pos_int'
  } else if(noiseSpec_int == 2) {
    noiseSpec_str <- 'hyperb'
  } else {
    stop(paste0('Unrecognized case, noiseSpec_int = ',noiseSpec_int))
  }
  return(noiseSpec_str)
}

#' Calculate the noise, sigma, given x, the parameter vector alpha, and the
#' noise specification noiseSpec.
#' 
#' @param x The independent variable
#' @param noiseSpec The noise specification
#' @param alpha The parameter vector
#' @return sigma(x,alpha), the noise (same length as x)
#' @export
calc_noise <- function(x,noiseSpec,alpha) {
  if(is.numeric(noiseSpec)) {
    noiseSpec <- noiseSpec_int2str(noiseSpec)
  }

  if(noiseSpec == 'const') {
    return(rep(alpha[1],length(x)))
  } else if(noiseSpec == 'lin_pos_int') {
    return(alpha[1]*(1 + x*alpha[2]))
  } else if(noiseSpec == 'hyperb') {
    return((alpha[1]/2) * (sqrt((x + alpha[3]/alpha[1])^2 + 4*alpha[2]*(alpha[2]-alpha[3])/(alpha[1]^2)) + x + alpha[3]/alpha[1]))
  } else {
    stop(paste0('Unrecognized case, noiseSpec = ',noiseSpec))
  }
}

#' Return the number of parameters for the noise specification noiseSpec.
#' 
#' @param noiseSpec The noise specification
#' @return The number of noise parameters
#' @export
get_num_var_noise <- function(noiseSpec) {
 if(is.numeric(noiseSpec)) {
    noiseSpec <- noiseSpec_int2str(noiseSpec)
  }

  if(noiseSpec == 'const') {
    return(1)
  } else if(noiseSpec == 'lin_pos_int') {
    return(2)
  } else if(noiseSpec == 'hyperb') {
    return(3)
  } else {
    stop(paste0('Unrecognized case, noiseSpec = ',noiseSpec))
  }
}

#' For a given noise specification, return a vector that gives the categories
#' used by param_constr2uncontr and param_constr2unconstr.
#'
#' @param noiseSpec The noise specification
#' @return A vector of categories
#' @export
get_noise_transform_categories <- function(noiseSpec) {
  if(is.numeric(noiseSpec)) {
    noiseSpec <- noiseSpec_int2str(noiseSpec)
  }

  if(noiseSpec == 'const') {
    return(1)
  } else if(noiseSpec == 'lin_pos_int') {
    return(c(1,1))
  } else if(noiseSpec == 'hyperb') {
    return(c(1,1,0))
  } else {
    stop(paste0('Unrecognized case, noiseSpec = ',noiseSpec))
  }
}
