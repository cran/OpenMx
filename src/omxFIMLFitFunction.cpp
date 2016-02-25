/*
 *  Copyright 2007-2016 The OpenMx Project
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "omxDefines.h"
#include "omxSymbolTable.h"
#include "omxData.h"
#include "omxFIMLFitFunction.h"
#include "omxSadmvnWrapper.h"

/* FIML Function body */
void omxDestroyFIMLFitFunction(omxFitFunction *off) {
	if(OMX_DEBUG) { mxLog("Destroying FIML fit function object."); }
	omxFIMLFitFunction *argStruct = (omxFIMLFitFunction*) (off->argStruct);

	omxFreeMatrix(argStruct->smallMeans);
	omxFreeMatrix(argStruct->ordMeans);
	omxFreeMatrix(argStruct->contRow);
	omxFreeMatrix(argStruct->ordRow);
	omxFreeMatrix(argStruct->ordCov);
	omxFreeMatrix(argStruct->ordContCov);
	omxFreeMatrix(argStruct->halfCov);
	omxFreeMatrix(argStruct->reduceCov);

	omxFreeMatrix(argStruct->smallRow);
	omxFreeMatrix(argStruct->smallCov);
	omxFreeMatrix(argStruct->RCX);
	omxFreeMatrix(argStruct->rowLikelihoods);
	omxFreeMatrix(argStruct->rowLogLikelihoods);
	delete argStruct;
}

void omxPopulateFIMLAttributes(omxFitFunction *off, SEXP algebra) {
	if(OMX_DEBUG) { mxLog("Populating FIML Attributes."); }
	omxFIMLFitFunction *argStruct = ((omxFIMLFitFunction*)off->argStruct);
	SEXP expCovExt, expMeanExt, rowLikelihoodsExt;
	omxMatrix *expCovInt, *expMeanInt;
	expCovInt = argStruct->cov;
	expMeanInt = argStruct->means;
	
	Rf_protect(expCovExt = Rf_allocMatrix(REALSXP, expCovInt->rows, expCovInt->cols));
	for(int row = 0; row < expCovInt->rows; row++)
		for(int col = 0; col < expCovInt->cols; col++)
			REAL(expCovExt)[col * expCovInt->rows + row] =
				omxMatrixElement(expCovInt, row, col);
	if (expMeanInt != NULL && expMeanInt->rows > 0  && expMeanInt->cols > 0) {
		Rf_protect(expMeanExt = Rf_allocMatrix(REALSXP, expMeanInt->rows, expMeanInt->cols));
		for(int row = 0; row < expMeanInt->rows; row++)
			for(int col = 0; col < expMeanInt->cols; col++)
				REAL(expMeanExt)[col * expMeanInt->rows + row] =
					omxMatrixElement(expMeanInt, row, col);
	} else {
		Rf_protect(expMeanExt = Rf_allocMatrix(REALSXP, 0, 0));		
	}

	Rf_setAttrib(algebra, Rf_install("expCov"), expCovExt);
	Rf_setAttrib(algebra, Rf_install("expMean"), expMeanExt);
	
	if(argStruct->populateRowDiagnostics){
		omxMatrix *rowLikelihoodsInt = argStruct->rowLikelihoods;
		Rf_protect(rowLikelihoodsExt = Rf_allocVector(REALSXP, rowLikelihoodsInt->rows));
		for(int row = 0; row < rowLikelihoodsInt->rows; row++)
			REAL(rowLikelihoodsExt)[row] = omxMatrixElement(rowLikelihoodsInt, row, 0);
		Rf_setAttrib(algebra, Rf_install("likelihoods"), rowLikelihoodsExt);
	}
}

static void CallFIMLFitFunction(omxFitFunction *off, int want, FitContext *fc)
{
	omxFIMLFitFunction* ofiml = ((omxFIMLFitFunction*)off->argStruct);

	// TODO: Figure out how to give access to other per-iteration structures.
	// TODO: Current implementation is slow: update by filtering correlations and thresholds.
	// TODO: Current implementation does not implement speedups for sorting.
	// TODO: Current implementation may fail on all-continuous-missing or all-ordinal-missing rows.
	
	if (want & (FF_COMPUTE_PREOPTIMIZE)) {
		omxMatrix *means	= ofiml->means;
		omxExpectation* expectation = off->expectation;
		if (!means) complainAboutMissingMeans(expectation);
		return;
	}

    if(OMX_DEBUG) { 
	    mxLog("Beginning Joint FIML Evaluation.");
    }
	int returnRowLikelihoods = 0;

	omxMatrix* fitMatrix  = off->matrix;
	int numChildren = (int) fc->childList.size();

	omxMatrix *cov 		= ofiml->cov;
	omxMatrix *means	= ofiml->means;
	omxData* data           = ofiml->data;                            //  read-only
	omxMatrix *dataColumns	= ofiml->dataColumns;

	returnRowLikelihoods = ofiml->returnRowLikelihoods;   //  read-only
	omxExpectation* expectation = off->expectation;
	std::vector< omxThresholdColumn > &thresholdCols = expectation->thresholds;

	if (data->defVars.size() == 0 && !strEQ(expectation->expType, "MxExpectationStateSpace")) {
		if(OMX_DEBUG) {mxLog("Precalculating cov and means for all rows.");}
		omxExpectationRecompute(fc, expectation);
		// MCN Also do the threshold formulae!
		
		for(int j=0; j < dataColumns->cols; j++) {
			int var = omxVectorElement(dataColumns, j);
			if (!omxDataColumnIsFactor(data, var)) continue;
			if (j < int(thresholdCols.size()) && thresholdCols[j].numThresholds > 0) { // j is an ordinal column
				omxMatrix* nextMatrix = thresholdCols[j].matrix;
				omxRecompute(nextMatrix, fc);
				checkIncreasing(nextMatrix, thresholdCols[j].column, thresholdCols[j].numThresholds, fc);
				for(int index = 0; index < numChildren; index++) {
					FitContext *kid = fc->childList[index];
					omxMatrix *target = kid->lookupDuplicate(nextMatrix);
					omxCopyMatrix(target, nextMatrix);
				}
			} else {
				Rf_error("No threshold given for ordinal column '%s'",
					 omxDataColumnName(data, j));
			}
		}

		double *corList 	= ofiml->corList;
		double *weights		= ofiml->weights;

		if (corList) {
			omxStandardizeCovMatrix(cov, corList, weights, fc);	// Calculate correlation and covariance
		}
		for(int index = 0; index < numChildren; index++) {
			FitContext *kid = fc->childList[index];
			omxMatrix *childFit = kid->lookupDuplicate(fitMatrix);
			omxFIMLFitFunction* childOfiml = ((omxFIMLFitFunction*) childFit->fitFunction->argStruct);
			omxCopyMatrix(childOfiml->cov, cov);
			omxCopyMatrix(childOfiml->means, means);
			if (corList) {
				memcpy(childOfiml->weights, weights, sizeof(double) * cov->rows);
				memcpy(childOfiml->corList, corList, sizeof(double) * (cov->rows * (cov->rows - 1)) / 2);
			}
		}
		if(OMX_DEBUG) { omxPrintMatrix(cov, "Cov"); }
		if(OMX_DEBUG) { omxPrintMatrix(means, "Means"); }
    }

	memset(ofiml->rowLogLikelihoods->data, 0, sizeof(double) * data->rows);
    
	int parallelism = (numChildren == 0) ? 1 : numChildren;

	if (parallelism > data->rows) {
		parallelism = data->rows;
	}

	FIMLSingleIterationType singleIter = ofiml->SingleIterFn;

	bool failed = false;
	if (parallelism > 1) {
		int stride = (data->rows / parallelism);

#pragma omp parallel for num_threads(parallelism) reduction(||:failed)
		for(int i = 0; i < parallelism; i++) {
			FitContext *kid = fc->childList[i];
			omxMatrix *childMatrix = kid->lookupDuplicate(fitMatrix);
			omxFitFunction *childFit = childMatrix->fitFunction;
			if (i == parallelism - 1) {
				failed |= singleIter(kid, childFit, off, stride * i, data->rows - stride * i);
			} else {
				failed |= singleIter(kid, childFit, off, stride * i, stride);
			}
		}
	} else {
		failed |= singleIter(fc, off, off, 0, data->rows);
	}
	if (failed) {
		omxSetMatrixElement(off->matrix, 0, 0, NA_REAL);
		return;
	}

	if(!returnRowLikelihoods) {
		double val, sum = 0.0;
		// floating-point addition is not associative,
		// so we serialized the following reduction operation.
		for(int i = 0; i < data->rows; i++) {
			val = omxVectorElement(ofiml->rowLogLikelihoods, i);
//			mxLog("%d , %f, %llx\n", i, val, *((unsigned long long*) &val));
			sum += val;
		}	
		if(OMX_DEBUG) {mxLog("Total Likelihood is %3.3f", sum);}
		omxSetMatrixElement(off->matrix, 0, 0, sum);
	}
}

void omxInitFIMLFitFunction(omxFitFunction* off)
{
	if(OMX_DEBUG) {
		mxLog("Initializing FIML fit function function.");
	}
	off->canDuplicate = TRUE;
	SEXP rObj = off->rObj;

	int numOrdinal = 0, numContinuous = 0;
	omxMatrix *cov, *means;

	omxFIMLFitFunction *newObj = new omxFIMLFitFunction;
	omxExpectation* expectation = off->expectation;
	if(expectation == NULL) {
		omxRaiseError("FIML cannot fit without model expectations.");
		return;
	}

	cov = omxGetExpectationComponent(expectation, "cov");
	if(cov == NULL) { 
		omxRaiseError("No covariance expectation in FIML evaluation.");
		return;
	}

	means = omxGetExpectationComponent(expectation, "means");
	
	if(OMX_DEBUG) {
		mxLog("FIML Initialization Completed.");
	}
	
    newObj->cov = cov;
    newObj->means = means;
    newObj->smallMeans = NULL;
    newObj->ordMeans   = NULL;
    newObj->contRow    = NULL;
    newObj->ordRow     = NULL;
    newObj->ordCov     = NULL;
    newObj->ordContCov = NULL;
    newObj->halfCov    = NULL;
    newObj->reduceCov  = NULL;
    
    off->computeFun = CallFIMLFitFunction;
    newObj->corList = NULL;
    newObj->weights = NULL;
	
    newObj->SingleIterFn = omxFIMLSingleIterationJoint;

	off->destructFun = omxDestroyFIMLFitFunction;
	off->populateAttrFun = omxPopulateFIMLAttributes;

	if(OMX_DEBUG) {
		mxLog("Accessing data source.");
	}
	newObj->data = off->expectation->data;

	if(OMX_DEBUG) {
		mxLog("Accessing row likelihood option.");
	}
	newObj->returnRowLikelihoods = Rf_asInteger(R_do_slot(rObj, Rf_install("vector")));
	newObj->rowLikelihoods = omxInitMatrix(newObj->data->rows, 1, TRUE, off->matrix->currentState);
	newObj->rowLogLikelihoods = omxInitMatrix(newObj->data->rows, 1, TRUE, off->matrix->currentState);
	
	
	if(OMX_DEBUG) {
		mxLog("Accessing row likelihood population option.");
	}
	newObj->populateRowDiagnostics = Rf_asInteger(R_do_slot(rObj, Rf_install("rowDiagnostics")));


	if(OMX_DEBUG) {
		mxLog("Accessing variable mapping structure.");
	}
	newObj->dataColumns = off->expectation->dataColumns;

	if(OMX_DEBUG) {
		mxLog("Accessing Threshold matrix.");
	}
	numOrdinal = off->expectation->numOrdinal;
	numContinuous = newObj->dataColumns->cols - numOrdinal;

	omxSetContiguousDataColumns(&(newObj->contiguous), newObj->data, newObj->dataColumns);
	
    /* Temporary storage for calculation */
    int covCols = newObj->cov->cols;
	if(OMX_DEBUG){mxLog("Number of columns found is %d", covCols);}
    // int ordCols = omxDataNumFactor(newObj->data);        // Unneeded, since we don't use it.
    // int contCols = omxDataNumNumeric(newObj->data);
    newObj->smallRow = omxInitMatrix(1, covCols, TRUE, off->matrix->currentState);
    newObj->smallCov = omxInitMatrix(covCols, covCols, TRUE, off->matrix->currentState);
    newObj->RCX = omxInitMatrix(1, covCols, TRUE, off->matrix->currentState);
//  newObj->zeros = omxInitMatrix(1, newObj->cov->cols, TRUE, off->matrix->currentState);

    omxCopyMatrix(newObj->smallCov, newObj->cov);          // Will keep its aliased state from here on.
    if (means) {
	    newObj->smallMeans = omxInitMatrix(covCols, 1, TRUE, off->matrix->currentState);
	    omxCopyMatrix(newObj->smallMeans, newObj->means);
	    newObj->ordMeans = omxInitMatrix(covCols, 1, TRUE, off->matrix->currentState);
	    omxCopyMatrix(newObj->ordMeans, newObj->means);
    }
    newObj->contRow = omxInitMatrix(covCols, 1, TRUE, off->matrix->currentState);
    omxCopyMatrix(newObj->contRow, newObj->smallRow );
    newObj->ordCov = omxInitMatrix(covCols, covCols, TRUE, off->matrix->currentState);
    omxCopyMatrix(newObj->ordCov, newObj->cov);
    newObj->ordRow = omxInitMatrix(covCols, 1, TRUE, off->matrix->currentState);
    omxCopyMatrix(newObj->ordRow, newObj->smallRow );
    newObj->Infin = (int*) R_alloc(covCols, sizeof(int));

    off->argStruct = (void*)newObj;

    //if (strEQ(expectation->expType, "MxExpectationStateSpace")) {
	//    newObj->SingleIterFn = omxFIMLSingleIteration;  // remove this TODO
    //}

    if(numOrdinal > 0 && numContinuous <= 0) {
        if(OMX_DEBUG) {
            mxLog("Ordinal Data detected.  Using Ordinal FIML.");
        }
        newObj->weights = (double*) R_alloc(covCols, sizeof(double));
        newObj->corList = (double*) R_alloc(covCols * (covCols + 1) / 2, sizeof(double));
        newObj->smallCor = (double*) R_alloc(covCols * (covCols + 1) / 2, sizeof(double));
        newObj->lThresh = (double*) R_alloc(covCols, sizeof(double));
        newObj->uThresh = (double*) R_alloc(covCols, sizeof(double));
    } else if(numOrdinal > 0) {
        if(OMX_DEBUG) {
            mxLog("Ordinal and Continuous Data detected.  Using Joint Ordinal/Continuous FIML.");
        }

        newObj->weights = (double*) R_alloc(covCols, sizeof(double));
        newObj->ordContCov = omxInitMatrix(covCols, covCols, TRUE, off->matrix->currentState);
        newObj->halfCov = omxInitMatrix(covCols, covCols, TRUE, off->matrix->currentState);
        newObj->reduceCov = omxInitMatrix(covCols, covCols, TRUE, off->matrix->currentState);
        omxCopyMatrix(newObj->ordContCov, newObj->cov);
        newObj->corList = (double*) R_alloc(covCols * (covCols + 1) / 2, sizeof(double));
        newObj->lThresh = (double*) R_alloc(covCols, sizeof(double));
        newObj->uThresh = (double*) R_alloc(covCols, sizeof(double));
    }
}
