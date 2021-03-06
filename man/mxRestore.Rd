% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MxRestore.R
\name{mxRestore}
\alias{mxRestore}
\alias{mxRestoreFromDataFrame}
\title{Restore model state from a checkpoint file}
\usage{
mxRestore(
  model,
  chkpt.directory = mxOption(model, "Checkpoint directory"),
  chkpt.prefix = mxOption(model, "Checkpoint Prefix"),
  line = NULL,
  strict = FALSE
)

mxRestoreFromDataFrame(model, checkpoint, line = NULL)
}
\arguments{
\item{model}{an \link{MxModel} object}

\item{chkpt.directory}{character. Directory where the checkpoint file is located}

\item{chkpt.prefix}{character. Prefix of the checkpoint file}

\item{line}{integer. Which line from the checkpoint file to restore (defaults to the last line)}

\item{strict}{logical. Require that the checkpoint name and model name match}

\item{checkpoint}{a data.frame containing the model state}
}
\value{
Returns an MxModel object with free parameters updated to the last
saved values. When \sQuote{line} is provided, the MxModel is updated
to the values on that line within the checkpoint file.
}
\description{
Restore model state from a checkpoint file
}
\details{
In general, the arguments \sQuote{chkpt.directory} and \sQuote{chkpt.prefix} should be identical to the \code{\link{mxOption}}: \sQuote{Checkpoint Directory} and \sQuote{Checkpoint Prefix} that were specified on the model before execution.

Alternatively, the checkpoint file can be manually loaded as a data.frame in R and passed to \code{\link{mxRestoreFromDataFrame}}.
Use \code{\link{read.table}} with the options \code{header=TRUE, sep="\t", stringsAsFactors=FALSE, check.names=FALSE}.
}
\examples{
library(OpenMx)

# Simulate some data

x=rnorm(1000, mean=0, sd=1)
y= 0.5*x + rnorm(1000, mean=0, sd=1)
tmpFrame <- data.frame(x, y)
tmpNames <- names(tmpFrame)

dir <- tempdir()  # safe place to create files
mxOption(key="Checkpoint Directory", value=dir)

# Create a model that includes an expected covariance matrix,
# an expectation function, a fit function, and an observed covariance matrix

data <- mxData(cov(tmpFrame), type="cov", numObs = 1000)
expCov <- mxMatrix(type="Symm", nrow=2, ncol=2, values=c(.2,.1,.2), free=TRUE, name="expCov")
expFunction <- mxExpectationNormal(covariance="expCov", dimnames=tmpNames)
fitFunction <- mxFitFunctionML()
testModel <- mxModel(model="testModel", expCov, data, expFunction, fitFunction)

#Use mxRun to optimize the free parameters in the expected covariance matrix
modelOut <- mxRun(testModel, checkpoint = TRUE)
modelOut$expCov

#Use mxRestore to load the last checkpoint saved state of the model
modelRestore <- mxRestore(testModel)
modelRestore$expCov
}
\references{
The OpenMx User's guide can be found at \url{https://openmx.ssri.psu.edu/documentation}
}
\seealso{
Other model state: 
\code{\link{mxComputeCheckpoint}()},
\code{\link{mxSave}()}
}
\concept{model state}
