% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxUnitTesting.R
\name{omxCheckError}
\alias{omxCheckError}
\title{Correct Error Message Function}
\usage{
omxCheckError(expression, message)
}
\arguments{
\item{expression}{an R expression that produces an error}

\item{message}{a character string with the desired error message}
}
\description{
This function tests whether the correct error message is thrown.
}
\details{
Arguments \sQuote{expression} and \sQuote{message} give the expression
that generates the error and the message that is supposed to be generated, respectively.
}
\references{
The OpenMx User's guide can be found at <https://openmx.ssri.psu.edu/documentation>.
}
\seealso{
\code{\link{omxCheckWarning}}
\code{\link{omxCheckWithinPercentError}},
\code{\link{omxCheckIdentical}}, \code{\link{omxCheckSetEquals}},
\code{\link{omxCheckTrue}}, \code{\link{omxCheckEquals}}
}
