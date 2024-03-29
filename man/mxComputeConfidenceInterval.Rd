% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxCompute.R
\name{mxComputeConfidenceInterval}
\alias{mxComputeConfidenceInterval}
\alias{MxComputeConfidenceInterval-class}
\title{Find likelihood-based confidence intervals}
\usage{
mxComputeConfidenceInterval(
  plan,
  ...,
  freeSet = NA_character_,
  verbose = 0L,
  engine = NULL,
  fitfunction = "fitfunction",
  tolerance = NA_real_,
  constraintType = "none"
)
}
\arguments{
\item{plan}{compute plan to optimize the model}

\item{...}{Not used.  Forces remaining arguments to be specified by name.}

\item{freeSet}{names of matrices containing free variables}

\item{verbose}{integer. Level of run-time diagnostic output. Set to zero to disable}

\item{engine}{\lifecycle{deprecated}}

\item{fitfunction}{the name of the deviance function}

\item{tolerance}{\lifecycle{deprecated}}

\item{constraintType}{one of c('ineq', 'none')}
}
\description{
There are various equivalent ways to pose the optimization
problems required to estimate confidence intervals. Most accurate
solutions are achieved when the problem is posed using non-linear
constraints. However, the available optimizers (CSOLNP, SLSQP, and NPSOL) often have difficulty with non-linear
constraints.
}
\references{
Neale, M. C. & Miller M. B. (1997). The use of likelihood based
confidence intervals in genetic models.  \emph{Behavior Genetics,
27}(2), 113-120.

Pek, J. & Wu, H. (2015). Profile likelihood-based confidence intervals and regions for structural equation models.
\emph{Psychometrika, 80}(4), 1123-1145.

Wu, H. & Neale, M. C. (2012). Adjusted confidence intervals for a
bounded parameter. \emph{Behavior genetics, 42}(6), 886-898.
}
