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

\name{mxFitFunctionAlgebra}
\alias{mxFitFunctionAlgebra}
\alias{MxFitFunctionAlgebra-class}
\alias{print,MxFitFunctionAlgebra-method}
\alias{show,MxFitFunctionAlgebra-method}

\title{Create MxFitFunctionAlgebra Object}

\description{
 mxFitFunctionAlgebra returns an MxFitFunctionAlgebra object.
}

\usage{
mxFitFunctionAlgebra(algebra, numObs = NA, numStats = NA, ..., gradient =
                 NA_character_, hessian = NA_character_, verbose = 0L,
   units="-2lnL", strict=TRUE)
}

\arguments{
   \item{algebra}{A character string indicating the name of an \link{MxAlgebra} or \link{MxMatrix} object to use for optimization.}
   \item{numObs}{(optional) An adjustment to the total number of observations in the model.}
   \item{numStats}{(optional) An adjustment to the total number of
     observed statistics in the model.}
   \item{...}{Not used.  Forces remaining arguments to be specified by name.}
   \item{gradient}{(optional) A character string indicating the name of
   an \link{MxAlgebra} object.}
   \item{hessian}{(optional) A character string indicating the name of
     an \link{MxAlgebra} object.}
   \item{verbose}{(optional An integer to increase the level of runtime
     log output.}
   \item{units}{(optional) The units of the fit statistic.}
   \item{strict}{Whether to require that all derivative entries reference
   free parameters.}
}

\details{
If you want to fit a multigroup model, the preferred way is to use \code{\link{mxFitFunctionMultigroup}}.

Fit functions are functions for which free parameter values are chosen such that the value of the objective function is minimized. While the other fit functions in OpenMx require an expectation function for the model, the \code{mxAlgebraObjective} function uses the referenced \code{\link{MxAlgebra}} or \code{\link{MxMatrix}} object as the function to be minimized.

If a model's  fit function is an \code{mxFitFunctionAlgebra} objective function, then the referenced algebra in the objective function must return a 1 x 1 matrix (when using OpenMx's default optimizer).  There is no restriction on the dimensions of an fit function that is not the primary, or \sQuote{topmost}, objective function.

To evaluate an algebra fit function, place the following objects in a \code{\link{MxModel}} object: a \code{mxFitFunctionAlgebra}, \code{\link{MxAlgebra}} and \code{\link{MxMatrix}} entities referenced by the \code{MxAlgebraObjective}, and optional \code{\link{MxBounds}} and \code{\link{MxConstraint}} objects. This model may then be evaluated using the \code{\link{mxRun}} function. The results of the optimization may be obtained using the \code{\link{mxEval}} function on the name of the \code{\link{MxAlgebra}}, after the model has been run.

First and second derivatives can be provided with the algebra fit
function. The dimnames on the gradient and hessian MxAlgebras are
matched against names of free variables. Names that do not match are
ignored. The fit is assumed to be in deviance units (-2 log
likelihood units). If you are working in log likelihood units, the -2
scaling factor is not applied automatically. You have to
multiply by -2 yourself.
}

\value{
Returns an MxFitFunctionAlgebra object.  MxFitFunctionAlgebra objects should be included with models with referenced \code{\link{MxAlgebra}} and \code{\link{MxMatrix}} objects.
}

\references{
The OpenMx User's guide can be found at \url{https://openmx.ssri.psu.edu/documentation/}.
}

\seealso{
Other fit functions:
\code{\link{mxFitFunctionMultigroup}}, \code{\link{mxFitFunctionML}},
\code{\link{mxFitFunctionWLS}}, \code{\link{mxFitFunctionGREML}},
\code{\link{mxFitFunctionR}}, \code{\link{mxFitFunctionRow}}

To create an algebra suitable as a reference function to be minimized see: \link{mxAlgebra}

More information about the OpenMx package may be found \link[=OpenMx]{here}.
}

\examples{

# Create and fit a very simple model that adds two numbers using mxFitFunctionAlgebra

library(OpenMx)

# Create a matrix 'A' with no free parameters
A <- mxMatrix('Full', nrow = 1, ncol = 1, values = 1, name = 'A')

# Create an algebra 'B', which defines the expression A + A
B <- mxAlgebra(A + A, name = 'B')

# Define the objective function for algebra 'B'
objective <- mxFitFunctionAlgebra('B')

# Place the algebra, its associated matrix and
# its objective function in a model
tmpModel <- mxModel(model="Addition", A, B, objective)

# Evalulate the algebra
tmpModelOut <- mxRun(tmpModel)

# View the results
tmpModelOut$output$minimum

}
