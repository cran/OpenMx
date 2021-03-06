% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxCompute.R
\name{mxComputeHessianQuality}
\alias{mxComputeHessianQuality}
\alias{MxComputeHessianQuality-class}
\title{Compute the quality of the Hessian}
\usage{
mxComputeHessianQuality(freeSet = NA_character_, ..., verbose = 0L)
}
\arguments{
\item{freeSet}{names of matrices containing free variables}

\item{...}{Not used.  Forces remaining arguments to be specified by name.}

\item{verbose}{integer. Level of run-time diagnostic output. Set to zero to disable}
}
\description{
Tests whether the Hessian is positive definite
(model$output$infoDefinite) and, if so, computes the approximate condition
number (model$output$conditionNumber). See Luenberger & Ye (2008)
Second Order Test (p. 190) and Condition Number (p. 239).
}
\details{
The condition number is approximated by \eqn{\mathrm{norm}(H) *
\mathrm{norm}(H^{-1})}{norm(H) * norm(solve(H))} where H is the
Hessian. The norm is either the 1- or infinity-norm (both obtain
the same result due to symmetry).
}
\references{
Luenberger, D. G. & Ye, Y. (2008). Linear and nonlinear programming. Springer.
}
