% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxCompute.R
\name{mxComputeTryHard}
\alias{mxComputeTryHard}
\alias{MxComputeTryHard-class}
\title{Repeatedly attempt a compute plan until successful}
\usage{
mxComputeTryHard(
  plan,
  ...,
  freeSet = NA_character_,
  verbose = 0L,
  location = 1,
  scale = 0.25,
  maxRetries = 3L
)
}
\arguments{
\item{plan}{compute plan to optimize the model}

\item{...}{Not used.  Forces remaining arguments to be specified by name.}

\item{freeSet}{names of matrices containing free variables}

\item{verbose}{integer. Level of run-time diagnostic output. Set to zero to disable}

\item{location}{location of the perturbation distribution}

\item{scale}{scale of the perturbation distribution}

\item{maxRetries}{maximum number of plan evaluations per invocation (including the first evaluation)}
}
\description{
The provided compute plan is run until the status code indicates
success (0 or 1). It gives up after a small number of retries.
}
\details{
Upon failure, start values are randomly perturbed.  Currently only
the uniform distribution is implemented.  The distribution is
parameterized by arguments \code{location} and \code{scale}.  The
location parameter is the distribution's median.  For the uniform
distribution, \code{scale} is the absolute difference between its
median and extrema (i.e., half the width of the rectangle).  Each
start value is multiplied by a random draw and then added to a
random draw from a distribution with the same \code{scale} but
with a median of zero.
}
\references{
Shanno, D. F. (1985). On Broyden-Fletcher-Goldfarb-Shanno method. \emph{Journal of
Optimization Theory and Applications, 46}(1), 87-94.
}
\seealso{
\code{\link{mxTryHard}}
}
