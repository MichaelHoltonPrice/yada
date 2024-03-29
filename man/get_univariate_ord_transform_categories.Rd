% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate_models.R
\name{get_univariate_ord_transform_categories}
\alias{get_univariate_ord_transform_categories}
\title{Get the tranform categories for a univariate ordinal model}
\usage{
get_univariate_ord_transform_categories(mod_spec)
}
\arguments{
\item{mod_spec}{The model specification}
}
\value{
A vector of categories
}
\description{
For a given model specification of a univariate ordinal variable, return a
vector that gives the categories used by param_constr2uncontr and
param_constr_to_unconstr.
}
