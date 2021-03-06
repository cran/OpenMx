% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxApply.R
\name{imxSfClient}
\alias{imxSfClient}
\title{imxSfClient}
\usage{
imxSfClient()
}
\description{
As of snowfall 1.84, the snowfall supervisor process
stores an internal state information in a variable
named ".sfOption" that is located in the "snowfall" namespace.
The snowfall client processes store internal state
information in a variable named ".sfOption" that is located
in the global namespace.
}
\details{
As long as the previous statement is true, then the current
process is a snowfall client if-and-only-if exists(".sfOption").
}
