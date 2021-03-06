% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxModelDisplay.R
\name{omxQuotes}
\alias{omxQuotes}
\title{omxQuotes}
\usage{
omxQuotes(name)
}
\arguments{
\item{name}{a character vector}
}
\value{
a character string
}
\description{
Quote helper function, often for error messages.
}
\details{
This is a helper function for creating a nicely
put together formatted string.
}
\examples{
omxQuotes(c("Oh", "blah", "dee", "Oh", "blah", "da"))
omxQuotes(c("A", "S", "F"))
omxQuotes("Hello World")
}
