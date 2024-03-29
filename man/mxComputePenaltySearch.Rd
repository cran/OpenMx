% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxCompute.R
\name{mxComputePenaltySearch}
\alias{mxComputePenaltySearch}
\alias{MxComputePenaltySearch-class}
\title{Regularize parameter estimates}
\usage{
mxComputePenaltySearch(
  plan,
  ...,
  freeSet = NA_character_,
  verbose = 0L,
  fitfunction = "fitfunction",
  approach = "EBIC",
  ebicGamma = 0.5
)
}
\arguments{
\item{plan}{compute plan to optimize the model}

\item{...}{Not used.  Forces remaining arguments to be specified by name.}

\item{freeSet}{names of matrices containing free variables}

\item{verbose}{integer. Level of run-time diagnostic output. Set to zero to disable}

\item{fitfunction}{the name of the deviance function}

\item{approach}{what fit function to use to compare regularized models? Currently only EBIC is available}

\item{ebicGamma}{what Gamma value to use for EBIC? Must be between 0 and 1}
}
\description{
Add a penalty to push some subset of the parameter estimates toward zero.
}
\references{
Jacobucci, R., Grimm, K. J., & McArdle, J. J. (2016).
Regularized structural equation modeling.
<i>Structural equation modeling: a multidisciplinary journal, 23</i>(4), 555-566.
}
