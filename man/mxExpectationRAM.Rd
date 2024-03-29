%
%   Copyright 2007-2021 by the individuals mentioned in the source code history
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%        http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

\name{mxExpectationRAM}
\alias{mxExpectationRAM}
\alias{MxExpectationRAM-class}
\alias{show,MxExpectationRAM-method}
\alias{print,MxExpectationRAM-method}

\title{Create an MxExpectationRAM Object}

\description{
This function creates an MxExpectationRAM object.
}

\usage{
mxExpectationRAM(A="A", S="S", F="F", M = NA, dimnames = NA, thresholds = NA,
                 threshnames = dimnames, ..., between=NULL, verbose=0L,
  .useSparse=NA, expectedCovariance=NULL, expectedMean=NULL,
  discrete = as.character(NA), selectionVector = as.character(NA),
  expectedFullCovariance=NULL, expectedFullMean=NULL)
}


\arguments{
   \item{A}{A character string indicating the name of the 'A' matrix.}
   \item{S}{A character string indicating the name of the 'S' matrix.}
   \item{F}{A character string indicating the name of the 'F' matrix.}
   \item{M}{An optional character string indicating the name of the 'M' matrix.}
   \item{dimnames}{An optional character vector to be assigned to the column names of the 'F' and 'M' matrices.}
   \item{thresholds}{An optional character string indicating the name of the thresholds matrix.}
   \item{threshnames}{An optional character vector to be assigned to the column names of the thresholds matrix.}
   \item{...}{Not used.  Forces remaining arguments to be specified by name.}
   \item{between}{A character vector of matrices that specify cross model relationships.}
   \item{verbose}{integer. Level of runtime diagnostic output.}
   \item{.useSparse}{logical. Whether to use sparse matrices to compute
   the expectation. The default \code{NA} allows the backend to decide.}
   \item{expectedCovariance}{An optional character string indicating the
   name of a matrix for the observed model implied covariance.}
   \item{expectedMean}{An optional character string indicating the name
   of a matrix for the observed model implied mean.}
   \item{discrete}{An optional character string indicating the name of
     the discrete matrix.}
   \item{selectionVector}{An optional character string indicating the name of
     the Pearson selection vector matrix.}
   \item{expectedFullCovariance}{An optional character string indicating the
   name of a matrix for the full model implied covariance. Both latent
   and observed variables are included.}
   \item{expectedFullMean}{An optional character string indicating the name
   of a matrix for the full model implied mean. Both latent
   and observed variables are included.}
}

\details{
Expectation functions define the way that model expectations are calculated. The mxExpectationRAM calculates the expected covariance and means of a given \link{MxData} object given a RAM model. This model is defined by reticular action modeling (McArdle and McDonald, 1984). The 'A', 'S', and 'F' arguments refer to \link{MxMatrix} objects with the associated properties of the A, S, and F matrices in the RAM modeling approach. \emph{Note for advanced users}: these matrices may be replaced by mxAlgebras. Such a model will lack properties (labels, free, bounds) that other functions may be expecting.

The \link{MxMatrix} objects included as arguments may be of any type, but should have the properties described above. The mxExpectationRAM will not return an error for incorrect specification, but incorrect specification will likely lead to estimation problems or errors in the \link{mxRun} function.

The 'A' argument refers to the A or asymmetric matrix in the RAM approach. This matrix consists of all of the asymmetric paths (one-headed arrows) in the model. A free parameter in any row and column describes a regression of the variable represented by that row regressed on the variable represented in that column.

The 'S' argument refers to the S or symmetric matrix in the RAM approach, and as such must be square. This matrix consists of all of the symmetric paths (two-headed arrows) in the model. A free parameter in any row and column describes a covariance between the variable represented by that row and the variable represented by that column. Variances are covariances between any variable at itself, which occur on the diagonal of the specified matrix.

The 'F' argument refers to the F or filter matrix in the RAM approach. If no latent variables are included in the model (i.e., the A and S matrices are of both of the same dimension as the data matrix), then the 'F' should refer to an identity matrix. If latent variables are included (i.e., the A and S matrices are not of the same dimension as the data matrix), then the 'F' argument should consist of a horizontal adhesion of an identity matrix and a matrix of zeros.

The 'M' argument refers to the M or means matrix in the RAM approach.  It is a 1 x n matrix, where n is the number of manifest variables + the number of latent variables. The M matrix must be specified if either the mxData type is \dQuote{cov} or \dQuote{cor} and a means vector is provided, or if the mxData type is \dQuote{raw}.  Otherwise the M matrix is ignored.

The 'dimnames' arguments takes an optional character vector.  If this argument is not a single NA, then this vector be assigned to be the column names of the 'F' matrix and optionally to the 'M' matrix, if the 'M' matrix exists.

mxExpectationRAM evaluates with respect to an \link{MxData} object. The \link{MxData} object need not be referenced in the mxExpectationRAM function, but must be included in the \link{MxModel} object.

To evaluate an mxExpectationRAM object, place it, the \link{mxData}
object which the expected covariance approximates, any referenced
\link{MxAlgebra} and \link{MxMatrix} objects, and optional
\link{MxBounds} and \link{MxConstraint} objects in an \link{MxModel}
object and evaluate it using \link{mxRun}. The results of the optimization can be found in the 'output' slot of the resulting model, and may be obtained using the \link{mxEval} function.
}

\value{
    Returns a new MxExpectationRAM object. mxExpectationRAM objects should be included in a model, along with referenced \link{MxAlgebra}, \link{MxData} and \link{MxMatrix} objects.
}

\references{
McArdle, J. J. and MacDonald, R. P. (1984). Some algebraic properties of the Reticular Action Model for moment structures. \emph{British Journal of Mathematical and Statistical Psychology, 37,} 234-251.

The OpenMx User's guide can be found at \url{https://openmx.ssri.psu.edu/documentation/}.
}

\examples{

# Create and fit a model using mxMatrix, mxAlgebra,
#  mxExpectationNormal, and mxFitFunctionML

library(OpenMx)

# Simulate some data

x=rnorm(1000, mean=0, sd=1)
y= 0.5*x + rnorm(1000, mean=0, sd=1)
tmpFrame <- data.frame(x, y)
tmpNames <- names(tmpFrame)

# Define the matrices

matrixS <- mxMatrix(type = "Full", nrow = 2, ncol = 2, values=c(1,0,0,1),
              free=c(TRUE,FALSE,FALSE,TRUE), labels=c("Vx", NA, NA, "Vy"),
              name = "S")
matrixA <- mxMatrix(type = "Full", nrow = 2, ncol = 2, values=c(0,1,0,0),
              free=c(FALSE,TRUE,FALSE,FALSE), labels=c(NA, "b", NA, NA),
              name = "A")
matrixF <- mxMatrix(type="Iden", nrow=2, ncol=2, name="F")
matrixM <- mxMatrix(type = "Full", nrow = 1, ncol = 2, values=c(0,0),
              free=c(TRUE,TRUE), labels=c("Mx", "My"), name = "M")

# Define the expectation

expFunction <- mxExpectationRAM(M="M", dimnames = tmpNames)

# Choose a fit function

fitFunction <- mxFitFunctionML()

# Define the model

tmpModel <- mxModel(model="exampleRAMModel",
                    matrixA, matrixS, matrixF, matrixM,
                    expFunction, fitFunction,
                    mxData(observed=tmpFrame, type="raw"))

# Fit the model and print a summary

tmpModelOut <- mxRun(tmpModel)
summary(tmpModelOut)
}
