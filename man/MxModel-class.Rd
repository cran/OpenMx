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

\name{MxModel-class}
\Rdversion{1.1}
\docType{class}
\alias{MxModel-class}
\alias{MxModel}
\alias{$,MxModel-method}
\alias{$<-,MxModel-method}
\alias{[[,MxModel-method}
\alias{[[<-,MxModel-method}
\alias{names,MxModel-method}
\alias{print,MxModel-method}
\alias{show,MxModel-method}


\title{MxModel Class}

\description{
   MxModel is an S4 class. An MxModel object is a \link[=Named-entity]{named entity}.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{\link{mxModel}()}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{Character string.  The name of the model object. }
    \item{\code{matrices}:}{List of the model's \link{MxMatrix} objects.}
    \item{\code{algebras}:}{List of the model's \link{MxAlgebra} objects.}
    \item{\code{constraints}:}{List of the model's \link{MxConstraint} objects.}
    \item{\code{intervals}:}{List of the model's \link{MxInterval}
      objects, requested via \code{\link{mxCI}()}.}
    \item{\code{penalties}:}{List of the model's \link[=MxPenalty-class]{MxPenalty}
  objects.}
    \item{\code{latentVars}:}{"Latent variables;" object of class \code{"MxCharOrList"}.}
    \item{\code{manifestVars}:}{"Manifest variables;" object of class \code{"MxCharOrList"}.}
    \item{\code{data}:}{Object of class \link{MxData}.}
    \item{\code{submodels}:}{List of MxModel objects.}
    \item{\code{expectation}:}{Object of class \link{MxExpectation}; dictates the model's specification.}
    \item{\code{fitfunction}:}{Object of class \link{MxFitFunction}; dictates the cost function to be minimized when fitting the model.}
    \item{\code{compute}:}{Object of class \link{MxCompute}--the model's compute plan, which contains instructions on what the model is to compute and how to do so.}
    \item{\code{independent}:}{Logical; is the model to be run independently from other submodels?}
    \item{\code{options}:}{List of model-specific options, set by \code{\link{mxOption}()}.}
    \item{\code{output}:}{List of model output produced during a call to \code{\link{mxRun}()}.}
    \item{\code{.newobjects}:}{Logical; for internal use.}
    \item{\code{.resetdata}:}{Logical; for internal use.}
    \item{\code{.wasRun}:}{Logical; for internal use.}
    \item{\code{.modifiedSinceRun}:}{Logical; for internal use.}
    \item{\code{.version}:}{Object of class \code{"package_version"}; for internal use.}
  }
}

\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "MxModel")}: Accessor.  Accesses slots by slot-name.  Also accesses constituent \link[=Named-entity]{named entities}, by name.}
    \item{$<-}{\code{signature(x = "MxModel")}: Assignment.  Generally, this method will not allow the user to make unsafe changes to the MxModel object.}
    \item{[[}{\code{signature(x = "MxModel")}: Accessor for constituent \link[=Named-entity]{named entities}.}
    \item{[[<-}{\code{signature(x = "MxModel")}: Assignment for a \link[=Named-entity]{named entity}.}
    \item{names}{\code{signature(x = "MxModel")}: Returns names of slots and \link[=Named-entity]{named entities}.}
    \item{print}{\code{signature(x = "MxModel")}: "Print" method. }
    \item{show}{\code{signature(object = "MxModel")}: "Show" method. }
	 }
Note that \code{\link{imxInitModel}()}, \code{\link{imxModelBuilder}()}, \code{\link{imxTypeName}()}, and \code{\link{imxVerifyModel}()} are separately documented methods for class "MxModel".
}

\details{
The \sQuote{matrices} slot contains a list of the \link{MxMatrix} objects included in the model. These objects are listed by name. Two objects may not share the same name. If a new \link{MxMatrix} is added to an MxModel object with the same name as an \link{MxMatrix} object in that model, the added version replaces the previous version. There is no imposed limit on the number of \link{MxMatrix} objects that may be added here.

The \sQuote{algebras} slot contains a list of the \link{MxAlgebra} objects included in the model. These objects are listed by name. Two objects may not share the same name. If a new \link{MxAlgebra} is added to an MxModel object with the same name as an \link{MxAlgebra} object in that model, the added version replaces the previous version. All \link{MxMatrix} objects referenced in the included \link{MxAlgebra} objects must be included in the \sQuote{matrices} slot prior to estimation. There is no imposed limit on the number of \link{MxAlgebra} objects that may be added here.

The \sQuote{constraints} slot contains a list of the \link{MxConstraint} objects included in the model. These objects are listed by name. Two objects may not share the same name. If a new \link{MxConstraint} is added to an MxModel object with the same name as an \link{MxConstraint} object in that model, the added version replaces the previous version. All \link{MxMatrix} objects referenced in the included \link{MxConstraint} objects must be included in the \sQuote{matrices} slot prior to estimation. There is no imposed limit on the number of \link{MxConstraint} objects that may be added here.

The \sQuote{intervals} slot contains a list of the confidence intervals requested by included \link{MxCI} objects. These objects are listed by the free parameters, \link[=MxMatrix]{MxMatrices} and \link[=MxAlgebra]{MxAlgebras} referenced in the \link{MxCI} objects, not the list of \link{MxCI} objects themselves. If a new \link{MxCI} object is added to an MxModel object referencing one or more free parameters \link[=MxMatrix]{MxMatrices} or \link[=MxAlgebra]{MxAlgebras} previously listed in the \sQuote{intervals} slot, the new confidence interval(s) replace the existing ones. All listed confidence intervals must refer to free parameters \link[=MxMatrix]{MxMatrices} or \link[=MxAlgebra]{MxAlgebras} in the model.

The \sQuote{latentVars} slot contains a list of latent variable names,
which may be referenced by \link[=MxPath-class]{MxPath} objects. This
slot defaults to 'NA', and is only used when the \link{mxPath} function
is used. In the context of a RAM model, this slot accepts a character
vector of variable names. However, the LISREL model is partitioned into
exogenous and endogenous parts. Both exogenous and endogenous variables
can be specified using a list like, \code{list(endo='a',  exo='b')}.
If a character vector is passed to a LISREL model then those
variables will be assumed endogenous.

The \sQuote{manifestVars} slot contains a list of latent variable names,
which may be referenced by \link[=MxPath-class]{MxPath} objects. This
slot defaults to 'NA', and is only used when the \link{mxPath} function
is used. In the context of a RAM model, this slot accepts a character
vector of variable names. However, the LISREL model is partitioned into
exogenous and endogenous parts. Both exogenous and endogenous variables
can be specified using a list like, \code{list(endo='a',  exo='b')}.
If a character vector is passed to a LISREL model then those
variables will be assumed endogenous.

The \sQuote{data} slot contains an \link{MxData} object. This slot must be filled prior to execution when a fitfunction referencing data is used. Only one \link{MxData} object may be included per model, but submodels may have their own data in their own \sQuote{data} slots. If an \link{MxData} object is added to an MxModel which already contains an \link{MxData} object, the new object replaces the existing one.

The \sQuote{submodels} slot contains references to all of the MxModel objects included as submodels of this MxModel object. Models held as arguments in other models are considered to be submodels. These objects are listed by name. Two objects may not share the same name. If a new submodel is added to an MxModel object with the same name as an existing submodel, the added version replaces the previous version. When a model containing other models is executed using \link{mxRun}, all included submodels are executed as well. If the submodels are dependent on one another, they are treated as one larger model for purposes of estimation.

The \sQuote{independent} slot contains a logical value indicating whether or not the model is independent. If a model is independent (independent=TRUE), then the parameters of this model are not shared with any other model. An independent model may be estimated with no dependency on any other model. If a model is not independent (independent=FALSE), then this model shares parameters with one or more other models such that these models must be jointly estimated. These dependent models must be entered as submodels of another MxModel objects, so that they are simultaneously optimized.

The \sQuote{options} slot contains a list of options for the model. The name of each entry in the list is the option name to be used at runtime. The values in this list are the values of the optimizer options. The standard interface for updating options is through the \link{mxOption} function.

The \sQuote{output} slot contains a list of output added to the model by the \link{mxRun} function. Output includes parameter estimates, optimization information, model fit, and other information.  If a model has not been optimized using the \link{mxRun} function, the 'output' slot will be 'NULL'.

\link[=Named-entity]{Named entities} in \link{MxModel} objects may be viewed and referenced by name using the $ symbol.  For instance, for an MxModel named "yourModel" containing an MxMatrix named "yourMatrix", the contents of "yourMatrix" can be accessed as yourModel$yourMatrix. Slots (i.e., matrices, algebras, etc.) in an mxMatrix may also be referenced with the $ symbol (e.g., yourModel$matrices or yourModel$algebras). See the documentation for \link[methods]{Classes} and the examples in \link{mxModel} for more information.
}

\references{
The OpenMx User's guide can be found at \url{https://openmx.ssri.psu.edu/documentation/}.
}

\seealso{
\link{mxExpectationRAM}, \link{mxExpectationLISREL}, \link{mxModel} for creating MxModel objects. More information about the OpenMx package may be found \link[=OpenMx]{here}.
}

\keyword{classes}
