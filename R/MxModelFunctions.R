#
#   Copyright 2007-2021 by the individuals mentioned in the source code history
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


extractJoinModel <- function(model, matList) {
	if (length(matList) == 0) return(NULL)
	rawJoinModel <- sapply(matList, function(x) ifelse(.hasSlot(x, 'joinModel'), x@joinModel, NA))
	joinModel <- match(paste0(rawJoinModel, imxSeparatorChar, 'expectation'),
			   names(model@expectations))
	mismatch <- !is.na(rawJoinModel) & is.na(joinModel)
	if (any(mismatch)) {
		matNames <- names(matList)
		msg <- paste("The references", omxQuotes(rawJoinModel[mismatch]), "do not exist.",
			     "It is used by", matNames[mismatch])
		stop(msg, call. = FALSE)
	}
	joinModel
}

extractJoinKey <- function(model, matList) {
	sapply(matList, function(x) {
		if (!.hasSlot(x, 'joinKey') || is.na(x@joinKey)) return(NA_integer_)
		modelName <- unlist(strsplit(x@name, imxSeparatorChar, fixed = TRUE))[1]
		dataName <- paste0(modelName, imxSeparatorChar, 'data')
		data <- model@datasets[[ dataName ]]
		fkCol <- match(x@joinKey, colnames(data@observed))
		if (is.na(fkCol)) {
			msg <- paste("Foreign key", omxQuotes(x@joinKey), "not found in ", dataName)
			stop(msg, call. = FALSE)
		}
		fkCol
	})
}

generateMatrixTypes <- function(matList) {
	got <- match(sapply(matList, class),
		c("DiagMatrix", "FullMatrix", "IdenMatrix", "LowerMatrix",
			"SdiagMatrix", "StandMatrix", "SymmMatrix",
			"UnitMatrix", "ZeroMatrix"))
	if (any(is.na(got))) {
		stop(paste("Unknown matrix class",
			got[is.na(got)]))
	}
	got
}

generateMatrixList <- function(model) {
	matvalues <- lapply(model@matrices, generateMatrixValuesHelper)
	if (!length(matvalues)) return(NULL)
	names(matvalues) <- names(model@matrices)
	joinModel <- extractJoinModel(model, model@matrices)
	joinKey <- extractJoinKey(model, model@matrices)
	references <- generateMatrixReferences(model)
	types <- generateMatrixTypes(model@matrices)
	retval <- mapply(function(x1,x2,x3,x4,x5) { c(list(x1), x2, x3, x4, x5) },
		matvalues, joinModel, joinKey, types, references,
		SIMPLIFY = FALSE)
	return(retval)
}

generateAlgebraList <- function(model) {
	joinModel <- extractJoinModel(model, model@algebras)
	joinKey <- extractJoinKey(model, model@algebras)
	mNames <- names(model@matrices)
	aNames <- append(names(model@algebras), names(model@fitfunctions))
	mNumbers <- as.list(as.integer(-1 : (-length(mNames))))
	aNumbers <- as.list(as.integer(0 : (length(aNames) - 1)))
	names(mNumbers) <- mNames
	names(aNumbers) <- aNames
	retval <- mapply(generateAlgebraHelper, model@algebras, joinModel, joinKey,
			 MoreArgs=list(mNumbers, aNumbers), SIMPLIFY=FALSE, USE.NAMES=TRUE)
    return(retval)
}

findDependencies <- function(triple, flatModel, dependencies) {
	mNames <- names(flatModel@matrices)
	matrixNum <- triple[[1]] + 1
	matrixName <- mNames[[matrixNum]]
	return(dependencies[[matrixName]])
}


isExpectation <- function(name) {
	return(length(grep("expectation", name, fixed=TRUE)) > 0)
}

parameterDependencyList <- function(pList, flatModel, dependencies) {
	if (length(pList) == 3) {
		return(retval)
	}
	locations <- pList[4:length(pList)]
	deps <- lapply(locations, findDependencies, flatModel, dependencies)
	depnames <- Reduce(union, deps, character())
	depnames <- Filter(Negate(isExpectation), depnames)
	depnumbers <- sapply(depnames, doLocateIndex, flatModel, flatModel@name, USE.NAMES=FALSE)
	depnumbers <- as.integer(depnumbers)
	retval <- append(pList[1:3], list(depnumbers))
	append(retval, locations)
}

buildFreeVarGroupList <- function(flatModel) {
	if (is.null(flatModel@compute)) return()
	got <- getFreeVarGroup(flatModel@compute)
	if (length(got)) {
		if (anyDuplicated(unlist(got[seq(1,length(got),2)]))) {
			stop("freeSet IDs are not unique") #for debugging
		}
		members <- unlist(got[seq(2,length(got),2)])
		recog <- match(members, names(flatModel@matrices))
		if (any(is.na(recog))) {
			stop(paste("freeSet component", omxQuotes(members[is.na(recog)]), "not found"))
		}
	}
	got
}

generateParameterList <- function(flatModel, dependencies, freeVarGroups) {
	mList <- flatModel@matrices
	pList <- list()
	if (length(mList)) for(i in 1:length(mList)) {
		matrix <- mList[[i]]
		pList <- generateParameterListHelper(matrix, pList, i - 1L, freeVarGroups)
	}
	pList <- lapply(pList, parameterDependencyList, flatModel, dependencies)

	if (length(pList)) for(i in 1:length(pList)) {
		original <- pList[[i]]
    bounds <- unlist(original[1:2])
    if (all(!is.na(bounds)) && diff(bounds) <= 0) {
      stop(paste("Parameter", omxQuotes(names(pList)[i]),
                 "has a lower bound (", bounds[1], ") greater than or",
                 "equal to an upper bound (", bounds[2], ")"), call.=FALSE)
    }
		svalues <- original[5:length(original)]
		svalue <- NA
		if (length(svalues) > 1) {
			values <- sapply(svalues, generateValueHelper, mList)
			values <- values[!is.na(values)]
			if (length(values) == 0) {
				svalue <- NA
			} else {
				if (!all(values == values[[1]])) {
					warning(paste('Parameter',names(pList)[i],'has multiple start values.',
						      'Selecting', values[[1]]))
				}
				svalue <- values[[1]]
			}
		} else {
			svalue <- generateValueHelper(svalues[[1]], mList)
		}
		if (is.na(svalue)) {
			stop(paste("Parameter '",names(pList)[i],"' has no starting value",sep=""))
		}
		pList[[i]] <- c(original, svalue)
	}
	flatModel@parameters <- pList
	flatModel
}

definitionDependencyList <- function(pList, defVarName, flatModel, dependencies) {
	depnames <- findDependencies(pList[3], flatModel, dependencies)
	depnames <- Filter(Negate(isExpectation), depnames)
	depnumbers <- sapply(depnames, doLocateIndex, flatModel, flatModel@name, USE.NAMES=FALSE)
	depnumbers <- as.integer(depnumbers)
	retval <- c(unlist(pList), depnumbers)
	retval
}

generateDefinitionList <- function(flatModel, dependencies) {
	if (length(flatModel@matrices) == 0) return(list())
	result <- list()
	for(i in 1:length(flatModel@matrices)) {
		result <- matrixDefinitions(flatModel,
			flatModel@matrices[[i]],
			result, i - 1L)
	}
	result <- mapply(definitionDependencyList, result, names(result),
			 MoreArgs=list(flatModel, dependencies), SIMPLIFY=FALSE, USE.NAMES = TRUE)
	# data number, column number, matrix, row, col,
	# integer vector of dependencies (doLocateIndex coded)
	return(result)
}

generateValueHelper <- function(triple, mList) {
	mat <- triple[1] + 1
	row <- triple[2] + 1
	col <- triple[3] + 1
	val <- mList[[mat]]@values[row,col]
	return(val)
}

##' imxUpdateModelValues
##'
##' Deprecated. This function does not handle parameters with equality
##' constraints. Do not use.
##'
##' @param model model
##' @param flatModel flat model
##' @param values values to update
imxUpdateModelValues <- function(model, flatModel, values) {
	pList <- flatModel@parameters
	if(length(pList) != length(values)) {
		stop(paste("This model has", length(pList),
			"parameters, but you have given me", length(values),
			"values"))
	}
	if (length(pList) == 0) {
		return(model)
	}
	for(i in 1:length(pList)) {
		parameters <- pList[[i]]
		parameters <- parameters[5:(length(parameters)-1)]
		model <- updateModelValuesHelper(parameters, values[[i]], flatModel, model)
    }
	return(model)
}

updateModelValuesHelper <- function(triples, value, flatModel, model) {
	for(i in 1:length(triples)) {
		triple <- triples[[i]]
		mat <- triple[1] + 1
		row <- triple[2] + 1
		col <- triple[3] + 1
		name <- flatModel@matrices[[mat]]@name
		model[[name]]@values[row,col] <- value
	}
	return(model)
}

removeTail <- function(lst, tailSize) {
    newEnd <- length(lst) - tailSize
    if (newEnd == 0) {
        return(list())
    } else {
        return(lst[1 : newEnd])
    }
}

updateModelMatrices <- function(model, flatModel, values) {
	mList <- names(flatModel@matrices)
	if (length(mList) != length(values)) {
		stop(paste("This model has", length(mList),
			"matrices, but the backend has returned", length(values),
			"values"))
	}
	if (length(values) == 0) {
		return(model)
	}
	model <- updateModelEntitiesHelper(mList, values, model)
	return(model)
}


updateModelAlgebras <- function(model, flatModel, values) {
	aNames <- names(flatModel@algebras)
	oNames <- names(flatModel@fitfunctions)
	pNames <- names(flatModel@penalties)
	aList <- append(aNames, append(oNames, pNames))
	if(length(aList) != length(values)) {
		stop(paste("This model has", length(aList),
			"algebras, but the backend has returned", length(values),
			"values"))
	}
	if (length(aList) == 0) {
		return(model)
	}
	model <- updateModelEntitiesHelper(aList, values, model)
	return(model)
}

updateModelExpectations <- function(model, flatModel, values) {
	if (length(values) == 0) return(model)
	eNames <- names(flatModel@expectations)
	if (length(eNames) != length(values)) {
		stop(paste("This model has", length(eNames),
			   "expectations, but the backend has returned", length(values),
			   "values"))
	}
	if (length(eNames) == 0) return(model)
	updateModelEntitiesHelper(eNames, values, model)
}

updateModelData <- function(model, flatModel, values) {
	dNames <- names(flatModel@datasets)
	if (length(dNames) != length(values)) {
		stop(paste("This model has", length(dNames),
			   "data, but the backend has returned", length(values),
			   "values"))
	}
	if (length(dNames) == 0) return(model)
	updateModelEntitiesHelper(dNames, values, model)
}

updateModelEntitiesTargetModel <- function(model, entNames, values, modelNameMapping) {
    nextName <- model@name
    selectEnt <- entNames[modelNameMapping == nextName]
    selectVal <- values[modelNameMapping == nextName]
    if (length(selectEnt) > 0) {
		for(i in 1:length(selectEnt)) {
			name <- selectEnt[[i]]
			candidate <- model[[name]]
			value <- selectVal[[i]]
			if (!is.null(candidate) && length(value)) {
				if (is(candidate, "MxAlgebra")) {
					cdim <- sapply(dimnames(candidate), length)
					mask <- cdim != 0
					if (any(cdim[mask] != dim(value)[mask])) {
						warning(paste(paste(model@name, candidate@name, sep="."),
							      "is dimension", omxQuotes(dim(value)),
							      "but the dimnames imply dimension",
							      omxQuotes(cdim)))
					} else {
						dimnames(value) <- dimnames(candidate)
					}
					candidate@result <- value
					if (candidate@fixed && all(dim(value) == dim(candidate@initial)) &&
						    all(value == candidate@initial)) {
						warning(paste("Algebra",omxQuotes(candidate@name),
							"is set for onDemand recompute yet is still at",
							"initial values"))
					}
				} else if (is(candidate,"MxFitFunction")) {
					if(is(candidate,"MxFitFunctionGREML")){
						candidate@MLfit <- attr(value,"MLfit")
						candidate@result <- as.matrix(as.numeric(value))
            candidate@numObsAdjust <- attr(value,"numObsAdjust")
					} else{
						candidate@result <- as.matrix(value)
						attr <- attributes(value)
						attr$dim <- NULL
						candidate@info <- attr
					}
				} else if (is(candidate,"MxPenalty")) {
          candidate@result <- as.matrix(value)
				} else if(is(candidate, "MxMatrix")) {
					if(candidate@name=="filteredDataRow" && !candidate@.persist){next} #bit of a hack
					if (any(dim(candidate@values) != dim(value))) {
						msg <- paste("Backend returned a", omxQuotes(dim(value)),
							     "matrix for a", omxQuotes(dim(candidate@values)),
							     "matrix. Not sure how to proceed.",
							     "Please report this error to the OpenMx support team.")
						stop(msg)
					} else {
						dimnames(value) <- dimnames(candidate)
						candidate@values <- value
					}
				} else if (is(candidate, "MxExpectation")) {
					for (sl in names(attributes(value))) {
						slot(candidate, sl) <- attr(value, sl)
					}
				} else if (is(candidate, "MxDataStatic")) {
					value$numObs <- NULL
					if (!is.null(value[['rawData']])) {
						candidate@observed <- value[['rawData']]
						value[['rawData']] <- NULL
					}
					if (length(value)) candidate@observedStats <- value
				} else if (is(candidate, "MxDataDynamic")) {
					candidate@numObs <- value$numObs
				}
				model[[name]] <- candidate
			}
		}
	}
    if (length(model@submodels) > 0) {
        model@submodels <- lapply(model@submodels,
            updateModelEntitiesTargetModel, entNames, values, modelNameMapping)
    }
	return(model)
}

updateModelEntitiesHelper <- function(entNames, values, model) {
    modelNameMapping <- sapply(entNames, getModelNameString)
    model <- updateModelEntitiesTargetModel(model, entNames,
		values, modelNameMapping)
    return(model)
}

clearModifiedSinceRunRecursive <- function(model) {
	if (length(model@submodels) > 0) {
		model@submodels <- lapply(model@submodels, clearModifiedSinceRunRecursive)
	}
	model@.wasRun <- TRUE
	model@.modifiedSinceRun <- FALSE
	model
}

##' imxLocateIndex
##'
##' This is an internal function exported for those people who know
##' what they are doing.
##'
##' @param model model
##' @param name name
##' @param referant referant
imxLocateIndex <- function(model, name, referant) {
	if (length(name) == 0) return(name)
#	if (length(name) > 1) browser()
	if (all(is.na(name))) { return(as.integer(name)) }
  if (any(is.integer(name))) stop("imxLocateIndex called more than once, OpenMx bug")
	mNames <- names(model@matrices)
	aNames <- names(model@algebras)
	fNames <- names(model@fitfunctions)
  pNames <- names(model@penalties)
	eNames <- names(model@expectations)
	dNames <- names(model@datasets)
	matrixNumber <- match(name, mNames)
	algebraNumber <- match(name, append(aNames, c(fNames, pNames)))
	dataNumber <- match(name, dNames)
	expectationNumber <- match(name, eNames)
	if (all(is.na(matrixNumber)) && all(is.na(algebraNumber)) &&
        all(is.na(dataNumber)) && all(is.na(expectationNumber))) {
		reftype <- "named reference"
		if (typeof(referant) == "S4") {
			reftype <- 'object'
		}
		msg <- paste("The reference", omxQuotes(name),
			"does not exist.  It is used by",
			     reftype, omxQuotes(referant), ".")
		stop(msg, call.=FALSE)
	} else if (all(!is.na(matrixNumber))) {
		return(- matrixNumber)
	} else if (all(!is.na(dataNumber))) {
		return(dataNumber - 1L)
	} else if (all(!is.na(expectationNumber))) {
		return(expectationNumber - 1L)
	} else {
		return(algebraNumber - 1L)
	}
}

LocateOptionalMatrix <- function(model, name, referant) {
	if (is.na(name)) { return(as.integer(name)) }
	mNames <- names(model@matrices)
	aNames <- names(model@algebras)
	fNames <- names(model@fitfunctions)
	matrixNumber <- match(name, mNames)
	algebraNumber <- match(name, append(aNames, fNames))
	if (is.na(matrixNumber) && is.na(algebraNumber)) {
		return(NULL)
	} else if (!is.na(matrixNumber)) {
		return(- matrixNumber)
	} else {
		return(algebraNumber - 1L)
	}
}

zapExtraneousMatrices <- function(model){
  keepers <- unlist(lapply(model@matrices,function(x){
    class(try(x@.persist,silent=T))=="try-error" || x@.persist}))
  #^^^^^For the sake of backward compatibility, treat matrices with no .persist slot as having that slot be TRUE.
  if(is.logical(keepers)){model@matrices <- model@matrices[which(keepers)]}
  if(length(model@submodels)>0){model@submodels <- lapply(model@submodels,zapExtraneousMatrices)}
  return(model)
}

##' imxPreprocessModel
##'
##' This is an internal function exported for those people who know
##' what they are doing.
##'
##' @param model model
imxPreprocessModel <- function(model) {
	model@matrices <- lapply(model@matrices, findSquareBrackets)
	model@submodels <- lapply(model@submodels, imxPreprocessModel)
	return(model)
}


##' imxCheckMatrices
##'
##' This is an internal function exported for those people who know
##' what they are doing.
##'
##' @param model model
imxCheckMatrices <- function(model) {
	matrices <- model@matrices
	lapply(matrices, imxVerifyMatrix)
	submodels <- imxDependentModels(model)
	lapply(submodels, imxCheckMatrices)
}

getAllModelNames <- function(model){
	modNames <- character(0)
	getName <- function(model){
		modNames <<- c(modNames,model@name)
		if(length(model@submodels)){
			lapply(model@submodels,getName)
		}
	}
	getName(model)
	return(modNames)
}
