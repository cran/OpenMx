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

\name{mxRObjective}
\alias{mxRObjective}

\title{DEPRECATED: Create MxRObjective Object}

\description{
WARNING: Objective functions have been deprecated as of OpenMx 2.0.  

Please use mxFitFunctionR() instead.  As a temporary workaround, mxRObjective returns a list containing a NULL MxExpectation object and an MxFitFunctionR object.

All occurrences of

mxRObjective(fitfun, ...)

Should be changed to

mxFitFunctionR(fitfun, ...)
}

\arguments{
   \item{objfun}{A function that accepts two arguments.}
   \item{...}{The initial state information to the objective function.}
}

\details{
NOTE: THIS DESCRIPTION IS DEPRECATED.  Please change to using \link{mxExpectationNormal} and \link{mxFitFunctionML} as shown in the example below.

    The fitfun argument must be a function that accepts two arguments. The first argument
    is the mxModel that should be evaluated, and the second argument is some persistent 
    state information that can be stored between one iteration of optimization to the next
    iteration. It is valid for the function to simply ignore the second argument.

    The function must return either a single numeric value, or a list of exactly two elements.
    If the function returns a list, the first argument must be a single numeric value and the 
    second element will be the new persistent state information to be passed into this function
    at the next iteration. The single numeric value will be used by the optimizer to perform
    optimization.

    The initial default value for the persistent state information is NA.

    Throwing an exception (via stop) from inside fitfun may result
    in unpredictable behavior. You may want to wrap your code in
    tryCatch while experimenting.
}

\value{
Returns a list containing a NULL mxExpectation object and an MxFitFunctionR object. 
}

\references{
The OpenMx User's guide can be found at \url{https://openmx.ssri.psu.edu/documentation/}.
}

\examples{

# Create and fit a model using mxFitFunctionR

library(OpenMx)

A <- mxMatrix(nrow = 2, ncol = 2, values = c(1:4), free = TRUE, name = 'A')
squared <- function(x) { x ^ 2 }

# Define the objective function in R

objFunction <- function(model, state) {
    values <- model$A$values 
    return(squared(values[1,1] - 4) + squared(values[1,2] - 3) +
        squared(values[2,1] - 2) + squared(values[2,2] - 1))
}

# Define the expectation function

fitFunction <- mxFitFunctionR(objFunction)

# Define the model

tmpModel <- mxModel(model="exampleModel", A, fitFunction)

# Fit the model and print a summary

tmpModelOut <- mxRun(tmpModel)
summary(tmpModelOut)

}
