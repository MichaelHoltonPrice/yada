% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivariate_models.R
\name{is_cdep}
\alias{is_cdep}
\title{Return True for conditionally dependent models and False for conditionally
independent models. For the special case of a univariate model, False is
returned. Aside from this, the model is conditionally indepenendent if
(a) modSpec$cdepSpec is 'indep' or (b) modSpec$cdepSpec is 'dep', but cdepGroups is all NA.}
\usage{
is_cdep(modSpec)
}
\arguments{
\item{modSpec}{The model specification}
}
\value{
Whether or not the model is conditionally dependent
}
\description{
Return True for conditionally dependent models and False for conditionally
independent models. For the special case of a univariate model, False is
returned. Aside from this, the model is conditionally indepenendent if
(a) modSpec$cdepSpec is 'indep' or (b) modSpec$cdepSpec is 'dep', but cdepGroups is all NA.
}
