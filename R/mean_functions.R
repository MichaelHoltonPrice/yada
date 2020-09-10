#' Given an integer specification of the mean, convert to a string
#' specification. This is done for code readability. In the technical
#' documentation, the variable S^{(f)} gives the mean specifiation, but for
#' readability meanSpec is instead used in the code. An error is thrown for
#' invalid inputs. The valid cases are:
#'
#' int	str
#' 0	powLawOrd
#' 1	logwOrd
#' 2	linOrd
#' 3	powLaw
#' 
#' @param meanSpec_int The mean specification as an integer
#' @return meanSpec_str The mean specification as a string
#' @examples
#' meanSpec_int2str(0)
#' meanSpec_int2str(1)
#' @export
meanSpec_int2str <- function(meanSpec_int) {
  if(length(meanSpec_int) != 1) {
    stop('The input, meanSpec_int, does not have length 1')
  }
  if(!is.numeric(meanSpec_int)) {
    stop('The input, meanSpec_int, is not numeric')
  }
  if(meanSpec_int == 0) {
    meanSpec_str <- 'powLawOrd'
  } else if(meanSpec_int == 1) {
    meanSpec_str <- 'logOrd'
  } else if(meanSpec_int == 2) {
    meanSpec_str <- 'linOrd'
  } else if(meanSpec_int == 3) {
    meanSpec_str <- 'powLaw'
  } else {
    stop(paste0('Unrecognized case, meanSpec_int = ',meanSpec_int))
  }
  return(meanSpec_str)
}

#' Calculate the mean, f, given x, the parameter vector a, and the mean
#' specification meanSpec.
#' 
#' @param x The independent variable
#' @param meanSpec The mean specification
#' @param a The parameter vector
#' @return f(x,a), the mean (same length as x)
#' @export
calc_mean <- function(x,meanSpec,a=NA) {
  if(is.numeric(meanSpec)) {
    meanSpec <- meanSpec_int2str(meanSpec)
  }

  if(meanSpec == 'powLawOrd') {
    return(x^a[1])
  } else if(meanSpec == 'logOrd') {
    return(log(x))
  } else if(meanSpec == 'linOrd') {
    return(x)
  } else if(meanSpec == 'powLaw') {
    return(a[2]*x^a[1]+a[3])
  } else {
    stop(paste0('Unrecognized case, meanSpec = ',meanSpec))
  }
}


#' Return the number of parameters for the mean specification meanSpec.
#' 
#' @param meanSpec The mean specification
#' @return The number of mean parameters
#' @export
get_num_var_mean <- function(meanSpec) {
 if(is.numeric(meanSpec)) {
    meanSpec <- meanSpec_int2str(meanSpec)
  }

  if(meanSpec == 'powLawOrd') {
    return(1)
  } else if(meanSpec == 'logOrd') {
    return(0)
  } else if(meanSpec == 'linOrd') {
    return(0)
  } else if(meanSpec == 'powLaw') {
    return(3)
  } else {
    stop(paste0('Unrecognized case, meanSpec = ',meanSpec))
  }
}



#' For a given mean specification, return a vector that gives the categories
#' used by param_constr2uncontr and param_constr2unconstr.
#'
#' @param meanSpec The mean specification
#' @return A vector of categories
#' @export
get_mean_transform_categories <- function(meanSpec) {
  if(is.numeric(meanSpec)) {
    meanSpec <- meanSpec_int2str(meanSpec)
  }

  if(meanSpec == 'powLawOrd') {
    return(1)
  } else if(meanSpec == 'logOrd') {
    return(c())
  } else if(meanSpec == 'linOrd') {
    return(c())
  } else if(meanSpec == 'powLaw') {
    return(c(1,1,0))
  } else {
    stop(paste0('Unrecognized case, meanSpec = ',meanSpec))
  }
}
