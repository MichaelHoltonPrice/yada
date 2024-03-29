% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivariate_models.R
\name{get_num_var_multivariate}
\alias{get_num_var_multivariate}
\title{Get the number of parameters for named variables of a multivariate model}
\usage{
get_num_var_multivariate(
  var_name,
  mod_spec,
  j = NA,
  k = NA,
  i = NA,
  preceding = F
)
}
\arguments{
\item{var_name}{The variable name (a, tau, alpha, z)}

\item{mod_spec}{The model specification}

\item{j}{The ordinal index}

\item{k}{The continuous index}

\item{i}{The overall index}

\item{preceding}{Whether to return the number of precceding variables}
}
\value{
The number of parameters
}
\description{
Get the number of variables for named variables of a multivariate yada model
specification. This can be done either for specific ordinal or continuous
variables (e.g., j=1 or k=1), including using i (i=j for ordinal variables
and i=J+k for continuous variables). For example, to get the number of
variables that specify the mean for the second ordinal variable use:

get_num_var_multivariate("a",mod_spec,j=2)

As another example, to get the number of correlation terms (equivalent to
directly calling get_z_length) use:

get_num_var_multivariate("z",mod_spec)

Either none of j, k, and i should be specified, or exactly one of them
should. Errors are thrown for pertinent mis-uses. For example, tau only
applies to ordinal variables, so an informative error message is given if
var_name is 'tau' and k is input.

The optional input [preceding] indicates whether to give instead the number
of variables in the parameter vector that preced the specified input. It is
primarily used to recursively obtain the result for when preceding is TRUE,
the normal usage.
}
