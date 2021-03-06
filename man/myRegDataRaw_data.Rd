\name{myRegDataRaw}
\alias{myRegDataRaw}
\docType{data}
\title{
Example regression data with correlated predictors
}

\description{
Data set used in some of OpenMx's examples.
}

\usage{data("myRegDataRaw")}
\format{
  A data frame with 100 observations on the following variables.
  \describe{
    \item{\code{w}}{Predictor variable}
    \item{\code{x}}{Predictor variable}
    \item{\code{y}}{Predictor variable}
    \item{\code{z}}{Outcome variable}
  }
}

\details{
w, x, and y are predictors of z.  x and y are correlated.  Equal to \link{myRegData}.
}

\source{
Simulated.
}

\references{
The OpenMx User's guide can be found at \url{https://openmx.ssri.psu.edu/documentation/}.
}

\examples{
data(myRegData)
data(myRegDataRaw)

all(myRegDataRaw == myRegData)

}
\keyword{datasets}
