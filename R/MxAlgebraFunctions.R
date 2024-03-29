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

tr <- function(x) {
	if (is.matrix(x)) {
		return(sum(diag(x)))
	} else {
		return(NA)
	}
}

"%&%" <- function(x, y) {
	return(x %*% y %*% t(x))
}

"%^%" <- function(x, y) {
	return(kronecker(x, y, '^'))
}

cvectorize <- function(x) {
	return(matrix(x, length(x), 1))
}

rvectorize <- function(x) {
	return(matrix(t(x), length(x), 1))
}

vech <- function(x) {
	return(x[lower.tri(x, diag=TRUE)])
}

vechs <- function(x) {
	return(x[lower.tri(x, diag=FALSE)])
}

'vechs<-' <- function(x, value) {
	x[lower.tri(x, diag=FALSE)] <- value
	x
}

diag2vec <- function(x) {
	return(as.matrix(diag(as.matrix(x))))
}

vec2diag <- function(x) {
	x <- as.matrix(x)
	if (nrow(x) != 1 && ncol(x) != 1) {
		stop("argument must be a row or column vector")
	}
	if (nrow(x) * ncol(x) == 1) {
		return(x)
	} else {
		return(as.matrix(diag(as.numeric(x))))
	}
}

##' imxLookupSymbolTable
##'
##' This is an internal function exported for those people who know
##' what they are doing.
##'
##' @param name name
imxLookupSymbolTable <- function(name) {
	index <- which(omxSymbolTable["R.name"] == name)
	if(length(index) == 0) {
		stop(paste("Internal error, function",
			name, "cannot be found in OpenMx symbol table"),
			call. = FALSE)
	} else if (length(index) > 1) {
		stop(paste("Internal error, function",
			name, "appears twice in OpenMx symbol table"),
			call. = FALSE)
	}
	return(as.integer(index - 1))
}

omxNot <- function(x) {
	retval <- as.matrix(!x)
	return(apply(retval, c(1,2), as.numeric))
}

equalDimensions <- function(x, y) {
	if (!identical(dim(x), dim(y))) {
		msg <- paste("Arguments 'x' and 'y'",
			"are not of identical dimensions",
			"in", deparse(width.cutoff = 400L, sys.call(-1)))
		stop(msg, call. = FALSE)
	}
}

omxGreaterThan <- function(x, y) {
	x <- as.matrix(x)
	y <- as.matrix(y)
	equalDimensions(x, y)
	return(apply(x > y, c(1,2), as.numeric))
}

omxLessThan <- function(x, y){
	x <- as.matrix(x)
	y <- as.matrix(y)
	equalDimensions(x, y)
	return(apply(x < y, c(1,2), as.numeric))
}

omxAnd <- function(x, y){
	x <- as.matrix(x)
	y <- as.matrix(y)
	equalDimensions(x, y)
	return(apply(x & y, c(1,2), as.numeric))
}

omxOr <- function(x, y){
	x <- as.matrix(x)
	y <- as.matrix(y)
	equalDimensions(x, y)
	return(apply(x | y, c(1,2), as.numeric))
}

omxApproxEquals <- function(x, y, epsilon){
	x <- as.matrix(x)
	y <- as.matrix(y)
	epsilon <- as.matrix(epsilon)
	equalDimensions(x, y)
	if (!identical(dim(x), dim(epsilon))) {
		msg <- paste("Argument 'epsilon'",
                        "is not of the same dimensions",
			"as 'x' and 'y'")
                stop(msg)
        }
	return(omxLessThan(abs(x - y), epsilon))
}

omxMnor <- function(covariance, means, lbound, ubound) {
    covariance <- as.matrix(covariance)
    means <- as.matrix(means)
    lbound <- as.matrix(lbound)
    ubound <- as.matrix(ubound)

    if(nrow(covariance) != ncol(covariance)) {
        stop("Covariance must be square")
    }
    if(nrow(means) > 1 && ncol(means) > 1) {
    	stop("'means' argument must be row or column vector")
    }
    if(nrow(lbound) > 1 && ncol(lbound) > 1) {
    	stop("'lbound' argument must be row or column vector")
    }
    if(nrow(ubound) > 1 && ncol(ubound) > 1) {
    	stop("'ubound' argument must be row or column vector")
    }

    if(ncol(covariance) != length(means)) {
        stop("'means' must have length equal to diag(covariance)")
    }
    if(ncol(covariance) != length(lbound)) {
        stop("'lbound' must have length equal to diag(covariance)")
    }
    if(ncol(covariance) != length(ubound)) {
        stop("'ubound' must have length equal to diag(covariance)")
    }

    retVal <- .Call(callAlgebra,
    	list(covariance, means, lbound, ubound),
    	imxLookupSymbolTable("omxMnor"),
		    generateOptionsList(NULL, FALSE))
	if(single.na(retVal)){
		warning('Correlation with absolute value greater than one found.')
	}
    return(as.matrix(as.numeric(retVal)))

}

omxAllInt <- function(covariance, means, ...) {
    covariance <- as.matrix(covariance)
    means <- as.matrix(means)
    thresholdMats <- list(...)

    if(nrow(covariance) != ncol(covariance)) {
        stop("'covariance' must be square")
    }
    if(nrow(means) > 1 && ncol(means) > 1) {
    	stop("'means' argument must be row or column vector")
    }
    if(ncol(covariance) != length(means)) {
        stop("'means' must have length equal to diag(cov)")
    }

    if(sum(sapply(thresholdMats, ncol)) < ncol(covariance)) {
        stop("'thresholds' must have at least as many total columns as 'covariance'")
    }

    if(min(sapply(thresholdMats, nrow)) < 2) {
        stop("every column of 'thresholds' must have at least two rows: one lower bound and one upper")
    }

    retVal <- .Call(callAlgebra,
        c(list(covariance, means), thresholdMats),         # Flatten args into a single list
        imxLookupSymbolTable("omxAllInt"),
		    generateOptionsList(NULL, FALSE))

    return(as.matrix(as.numeric(retVal)))

}

eigenvec <- function(x) {
    x <- as.matrix(x)
    if(nrow(x) != ncol(x)) {
        stop("matrix must be square")
    }

    retval <- .Call(callAlgebra,
        list(x),         # Flatten args into a single list
        imxLookupSymbolTable("eigenvec"),
        generateOptionsList(NULL, FALSE))

    return(matrix(as.numeric(retval), nrow(x), ncol(x)))
}

ieigenvec <- function(x) {
    x <- as.matrix(x)
    if(nrow(x) != ncol(x)) {
        stop("matrix must be square")
    }

    retval <- .Call(callAlgebra,
        list(x),         # Flatten args into a single list
        imxLookupSymbolTable("ieigenvec"),
        generateOptionsList(NULL, FALSE))

    return(matrix(as.numeric(retval), nrow(x), ncol(x)))
}

eigenval <- function(x) {
    x <- as.matrix(x)
    if(nrow(x) != ncol(x)) {
        stop("matrix must be square")
    }

    retval <- .Call(callAlgebra,
        list(x),         # Flatten args into a single list
        imxLookupSymbolTable("eigenval"),
        generateOptionsList(NULL, FALSE))

    return(as.matrix(as.numeric(retval)))
}

ieigenval <- function(x) {
    x <- as.matrix(x)
    if(nrow(x) != ncol(x)) {
        stop("matrix must be square")
    }

    retval <- .Call(callAlgebra,
        list(x),         # Flatten args into a single list
        imxLookupSymbolTable("ieigenval"),
        generateOptionsList(NULL, FALSE))

    return(as.matrix(as.numeric(retval)))
}

omxSelectRows <- function(x, selector) {
    if(nrow(selector) != 1) selector <- t(selector)
    if(nrow(selector) != 1) {
        stop("Selector must have a single row or a single column")
    }
    if(nrow(x) != ncol(selector)) {
        stop("selector must have one column for each row of x")
    }
    return(x[as.logical(selector), , drop=FALSE])
}

omxSelectCols <- function(x, selector) {
    if(nrow(selector) != 1) selector <- t(selector)
    if(nrow(selector) != 1) {
        stop("Selector must have a single row or a single column")
    }
    if(ncol(x) != ncol(selector)) {
        stop("selector must have one column for each column of x")
    }
    return(x[, as.logical(selector), drop=FALSE])
}

omxSelectRowsAndCols <- function(x, selector) {
    if(nrow(selector) != 1) selector <- t(selector)
    if(nrow(selector) != 1) {
        stop("Selector must have a single row or a single column")
    }
    if(nrow(x) != ncol(selector) || ncol(x) != ncol(selector)) {
        stop("selector must have one column for each row and column of x")
    }
    selector <- as.logical(selector)
    return(x[selector, selector, drop=FALSE])
}

vech2full <- function(x) {

	if(is.matrix(x)) {
		if (nrow(x) > 1 && ncol(x) > 1) {
			stop("Input to the full vech2full must be a (1 x n) or (n x 1) matrix.")
		}

		dimension <- max(dim(x))

	} else if(is.vector(x)) {
		dimension <- length(x)
	} else {
		stop("Input to the function vech2full must be either a matrix or a vector.")
	}

	k <- sqrt(2.0 * dimension + 0.25) - 0.5

	ret <- matrix(0, nrow=k, ncol=k)
	if(nrow(ret) != k) {
		stop("Incorrect number of elements in vector to construct a matrix from a half-vectorization.")
	}
	ret[lower.tri(ret, diag=TRUE)] <- as.vector(x)
	ret[upper.tri(ret)] <- t(ret)[upper.tri(ret)]
	return(ret)
}

vechs2full <- function(x) {

	if(is.matrix(x)) {
		if (nrow(x) > 1 && ncol(x) > 1) {
			stop("Input to the full vechs2full must be a (1 x n) or (n x 1) matrix.")
		}

		dimension <- max(dim(x))

	} else if(is.vector(x)) {
		dimension <- length(x)
	} else {
		stop("Input to the function vechs2full must be either a matrix or a vector.")
	}

	k <- sqrt(2.0 * dimension + 0.25) + 0.5

	ret <- matrix(0, nrow=k, ncol=k)
	if(nrow(ret) != k) {
		stop("Incorrect number of elements in vector to construct a matrix from a strict half-vectorization.")
	}
	ret[lower.tri(ret, diag=FALSE)] <- as.vector(x)
	ret[upper.tri(ret)] <- t(ret)[upper.tri(ret)]
	return(ret)
}

p2z <- function(x){
  return(qnorm(x))
}
logp2z <- function(x){
	return(qnorm(p=x,log.p=TRUE))
}
lgamma1p <- function(x){
	x <- as.matrix(x)
	retVal <- .Call(callAlgebra, list(x), imxLookupSymbolTable("lgamma1p"),
									generateOptionsList(NULL, FALSE))
	return(retVal)
}

#These two functions accept a different number of arguments from their 'stats' counterparts.  Therefore, they need to have different symbols.
#Otherwise, collisions result when they appear in expressions passed to mxEval():
omxDnbinom <- function(x,size,prob,mu,give_log){
	x <- as.matrix(x)
	size <- as.matrix(size)
	prob <- as.matrix(prob)
	mu <- as.matrix(mu)
	give_log <- as.matrix(give_log)
	retval <- .Call(callAlgebra, list(x,size,prob,mu,give_log), imxLookupSymbolTable("omxDnbinom"),
									generateOptionsList(NULL, FALSE))
	return(retval)
}
omxPnbinom <- function(q,size,prob,mu,lower_tail,give_log){
	q <- as.matrix(q)
	size <- as.matrix(size)
	prob <- as.matrix(prob)
	mu <- as.matrix(mu)
	lower_tail <- as.matrix(lower_tail)
	give_log <- as.matrix(give_log)
	retval <- .Call(callAlgebra, list(q,size,prob,mu,lower_tail,give_log), imxLookupSymbolTable("omxPnbinom"),
									generateOptionsList(NULL, FALSE))
	return(retval)
}


##' A C implementation of dmvnorm
##'
##' This API is visible to permit testing. Please do not use.
##'
##' @param loc loc
##' @param mean mean
##' @param sigma sigma
imxDmvnorm <- function(loc, mean, sigma) .Call(Dmvnorm_wrapper, loc, mean, sigma)

mxEvaluateOnGrid <- function(algebra, abscissa) {
	stop(paste("mxEvaluateOnGrid is not compatible with mxEval.",
		   "It can only be evaluated in an mxRun context.",
		   "For an example of usage, see the manual ?mxEvaluateOnGrid"))
}

mxRobustLog <- function(pr) {
	result <- log(pr)
	result[pr == 0.0] <- -745
	result
}

mxPearsonSelCov <- function(origCov, newCov) {
  if (all(dim(origCov) == dim(newCov))) {
    used <- (colSums(origCov != newCov) + rowSums(origCov != newCov)) != 0
    m1 <- which(used)
    if (length(m1) == 0) return(origCov)
    newCov <- newCov[m1,m1]
  } else {
    m1 <- match(colnames(newCov), colnames(origCov))

    if (any(is.na(m1))) {
      stop(paste("schurComplementC: cannot find variables",
                 omxQuotes(colnames(newCov)[is.na(m1)])))
    }
  }

  rpp <- origCov[m1,m1,drop=F]
  rpq <- origCov[m1,-m1,drop=F]
  rqq <- origCov[-m1,-m1,drop=F]
  irpp <- solve(rpp)
  origCov[m1,m1] <- newCov
  tmp <- newCov %*% irpp %*% rpq
  origCov[-m1,m1] <- t(tmp)
  origCov[m1,-m1] <- tmp
  origCov[-m1,-m1] <- rqq - t(rpq) %*% (irpp - irpp %*% newCov %*% irpp) %*% rpq
  origCov
}

mxPearsonSelMean <- function(origCov, newCov, origMean) {
  if (all(dim(origCov) == dim(newCov))) {
    used <- (colSums(origCov != newCov) + rowSums(origCov != newCov)) != 0
    m1 <- which(used)
    if (length(m1) == 0) return(origMean)
    newCov <- newCov[m1,m1]
  } else {
    m1 <- match(colnames(newCov), colnames(origCov))

    if (any(is.na(m1))) {
      stop(paste("schurComplementC: cannot find variables",
                 omxQuotes(colnames(newCov)[is.na(m1)])))
    }
  }

  rpp <- origCov[m1,m1,drop=F]
  rqp <- origCov[-m1,m1,drop=F]
  irpp <- solve(rpp)
  origMean[-m1,] <- origMean[-m1,] + rqp %*% irpp %*% origMean[m1,,drop=F]
  origMean
}

mpinv <- MASS::ginv
