% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxPenalty.R
\name{mxPenaltyRidge}
\alias{mxPenaltyRidge}
\title{mxPenaltyRidge}
\usage{
mxPenaltyRidge(
  what,
  name,
  lambda = 0,
  lambda.step = 0.01,
  lambda.max = 0.4,
  lambda.min = NA,
  epsilon = 1e-05,
  scale = 1,
  ...,
  hyperparams = c("lambda")
)
}
\arguments{
\item{what}{A character vector of parameters to regularize}

\item{name}{Name of the regularizer object}

\item{lambda}{strength of the penalty to be applied at start (default 0)}

\item{lambda.step}{lambda step during penalty search (default 0.01)}

\item{lambda.max}{when to end the lambda search (default 0.4)}

\item{lambda.min}{minimum lambda value (default lambda)}

\item{epsilon}{how close to zero is zero?}

\item{scale}{a given parameter is divided by \code{scale} before comparison with \code{epsilon}}

\item{...}{Not used.  Forces remaining arguments to be specified by name}

\item{hyperparams}{a character vector of hyperparameter names}
}
\description{
Ridge regression regularization
}
