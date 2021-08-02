#' @title Calculate the combinadic
#'
#' @description
#' [yada::comb], [yada::dual] [yada::elem], [yada::elem_to_index], and
#' [yada::largest_V] together implement combinadics  as described in the
#' following document (see also details):
#'
#' James McCaffrey -- Generating the mth Lexicographical Element of a
#' Mathematical Combination
#'
#' Indexing begins from 0, not 1.
#'
#' @details
#' Consider a set of N things of which k unique ones are chosen. The number of
#' unique sets of things that can be chosen is choose(N,k). For example, if N=5
#' and k=3 there are choose(5,3) = 10 unique such sets. Adopting lexigraphic
#' ordering starting at 0, the combinadic (comb), dual, and element (elem) are:
#'
#' m   comb	dual   elem
#' 0    210    9    012
#' 1    310    8    013
#' 2    320    7    014
#' 3    321    6    023
#' 4    410    5    024
#' 5    420    4    034
#' 6    421    3    123
#' 7    430    2    124
#' 8    431    1    134
#' 9    432    0    234
#'
#' If the lexical index, m, is 2, the corresponding element with N=5 and k=3 is:
#'
#' e <- elem(2,5,3)
#' print(e)
#' [1] 0 1 4
#'
#' If the element is 421, the corresponding lexical index is:
#'
#' m <- elem_to_index(c(1,2,3),5)
#' print(m)
#' [1] 6
#' 
#' @param m The lexical index of each unique choice (starting from 0)
#' @param N The number of unique things
#' @param k The number of things to choose out of N total things
#' @param c The combinadic (a vector; see details)
#'
#' @return The combinadic (a vector; see details)

#' @export
comb <- function(m,N,k) {
  # The combinadic
  x <- m
  a <- N
  b <- k

  # Use cv instead of c since c is an R function
  cv = rep(NA,k)

  for(ii in 1:k) {
    cv[ii] <- largest_V(a,b,x)
    x <- x - choose(cv[ii],b)
    a <- cv[ii]
    b <- b - 1
  }
  return(cv)
}

###' @param d The dual(see details)
###' @param e The element (a vector; see details)

#' @title Calculate the dual
#'
#' @description
#' For an overview of combinadics and the dual see [yada::comb]. To calculate
#' the dual, the number of elements that can be chosen, num_elements, must be
#' known. It is either input directly or determined from N and k, which are
#' input. In total, therefore, there are two valid inputs patterns for the three
#' optional inputs num_elements, N, and k: specify numElements or specify both N
#' and k. If all three optional inputs are given, N and k are ignored.
#'
#' @param m The lexical index of each unique choice (starting from 0)
#' @param num_elements The number of things that can be chosen (default: NA, not
#'   used)
#' @param N The number of unique things (default: NA, not used)
#' @param k The number of things to choose out of N total things (default: NA,
#'   not used)
#'
#' return The dual, d (see details for [yada::comb]
#' @export
dual <- function(m,num_elements=NA,N=NA,k=NA) {
  # The dual
  if(!is.na(num_elements)) {
    d <- (num_elements-1) - m
    return(d)
  }

  if(!is.na(N) && !is.na(k)) {
    d <- (choose(N,k)-1) - m
    return(d)
  }

  stop('Unrecognized input pattern')
}

#' @title Calculate the element
#'
#' @description
#' For an overview of combinadics and the element see [yada::comb].
#'
#' @param m The lexical index of each unique choice (starting from 0)
#' @param N The number of unique things
#' @param k The number of things to choose out of N total things
#'
#' return The element, e (a vector; see details for [yada::comb]
#'
#' @export
elem <- function(m,N,k) {
  # The element
  x <- dual(m,N=N,k=k)
  
  # Use cv instead of c since c is an R function
  cv <- comb(x,N,k)
  e <- (N-1) - cv
  return(e)
}

#' @title
#' Convert the element (a vector) to the corresponding index (a scalar integer)
#'
#' @description
#' For an overview of combinadics and the element/index see [yada::comb].
#'
#' @param m The element (a vector)
#' @param N The number of unique things
#'
#' return The index, a scalar integer (see details for [yada::comb]
#'
#' @export
elem_to_index <- function(e,N) {
  # Convert the element to an index
  k <- length(e)
  y <- 0
  for(ii in 1:k) {
    y <- y + choose(N-1-e[ii],k-(ii-1));
  }

  index = choose(N,k) - 1 - y
  return(index)
}

# A helper function to calculate the largest V
largest_V <- function(a,b,x) {
  # Calculate the largest V
  v <- a-1
  while(choose(v,b) > x) {
    v <- v - 1
  }
  return(v)
}
