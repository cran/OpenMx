/*
 *  Copyright 2007-2015 The OpenMx Project
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

/**
 * Based on:
 *
 * Paul Gilbert and Ravi Varadhan (2012). numDeriv: Accurate Numerical Derivatives. R package
 * version 2012.9-1. http://CRAN.R-project.org/package=numDeriv
 *
 **/

#include <stdio.h>
#include <sys/types.h>
#include <errno.h>

#include "omxDefines.h"
#include "glue.h"
#include "omxState.h"
#include "omxMatrix.h"
#include "omxAlgebra.h"
#include "omxFitFunction.h"
#include "omxNPSOLSpecific.h"
#include "omxExportBackendState.h"
#include "Compute.h"
#include "omxBuffer.h"

class omxComputeNumericDeriv : public omxCompute {
	typedef omxCompute super;
	double stepSize;
	int numIter;
	bool parallel;
	int totalProbeCount;
	int verbose;
	double *knownHessian;
	std::vector<int> khMap;

	omxMatrix *fitMat;
	double minimum;
	Eigen::ArrayXd optima;
	int numParams;
	double *gradient;
	double *hessian;

	void omxPopulateHessianWork(struct hess_struct *hess_work, FitContext* fc);
	void omxEstimateHessianOnDiagonal(int i, struct hess_struct* hess_work);
	void omxEstimateHessianOffDiagonal(int i, int l, struct hess_struct* hess_work);
	void doHessianCalculation(int numChildren, struct hess_struct *hess_work);

 public:
        virtual void initFromFrontend(omxState *, SEXP rObj);
        virtual void computeImpl(FitContext *fc);
        virtual void reportResults(FitContext *fc, MxRList *slots, MxRList *out);
};

struct hess_struct {
	int probeCount;
	double* Haprox;
	double* Gaprox;
	FitContext *fc;
	omxMatrix* fitMatrix;
};

void omxComputeNumericDeriv::omxPopulateHessianWork(struct hess_struct *hess_work, FitContext* fc)
{
	hess_work->Haprox = (double*) Calloc(numIter, double);		// Hessian Workspace
	hess_work->Gaprox = (double*) Calloc(numIter, double);		// Gradient Workspace
	hess_work->fitMatrix = fc->lookupDuplicate(fitMat);
	hess_work->fc = fc;
	memcpy(fc->est, optima.data(), numParams * sizeof(double));
}

/**
  @params i              parameter number
  @params hess_work      local copy
  @params optima         shared read-only variable
  @params gradient       shared write-only variable
  @params hessian        shared write-only variable
 */
void omxComputeNumericDeriv::omxEstimateHessianOnDiagonal(int i, struct hess_struct* hess_work)
{
	static const double v = 2.0; //Note: NumDeriv comments that this could be a parameter, but is hard-coded in the algorithm

	double *Haprox             = hess_work->Haprox;
	double *Gaprox             = hess_work->Gaprox;
	omxMatrix* fitMatrix = hess_work->fitMatrix; 
	FitContext* fc = hess_work->fc; 
	double *freeParams         = fc->est;

	/* Part the first: Gradient and diagonal */
	double iOffset = std::max(fabs(stepSize * optima[i]), stepSize);
	if(verbose >= 2) {mxLog("Hessian estimation: iOffset: %f.", iOffset);}
	for(int k = 0; k < numIter; k++) {			// Decreasing step size, starting at k == 0
		if(verbose >= 2) {mxLog("Hessian estimation: Parameter %d at refinement level %d (%f). One Step Forward.", i, k, iOffset);}
		freeParams[i] = optima[i] + iOffset;

		
		fc->copyParamToModel();

		++hess_work->probeCount;
		omxRecompute(fitMatrix, fc);
		double f1 = omxMatrixElement(fitMatrix, 0, 0);

		freeParams[i] = optima[i] - iOffset;

		fc->copyParamToModel();

		++hess_work->probeCount;
		omxRecompute(fitMatrix, fc);
		double f2 = omxMatrixElement(fitMatrix, 0, 0);

		Gaprox[k] = (f1 - f2) / (2.0*iOffset); 						// This is for the gradient
		Haprox[k] = (f1 - 2.0 * minimum + f2) / (iOffset * iOffset);		// This is second derivative
		freeParams[i] = optima[i];									// Reset parameter value
		iOffset /= v;
		if(verbose >= 2) {mxLog("Hessian estimation: (%d, %d)--Calculating F1: %f F2: %f, Haprox: %f...", i, i, f1, f2, Haprox[k]);}
	}

	for(int m = 1; m < numIter; m++) {						// Richardson Step
		for(int k = 0; k < (numIter - m); k++) {
			Gaprox[k] = (Gaprox[k+1] * pow(4.0, m) - Gaprox[k])/(pow(4.0, m)-1); // NumDeriv Hard-wires 4s for r here. Why?
			Haprox[k] = (Haprox[k+1] * pow(4.0, m) - Haprox[k])/(pow(4.0, m)-1); // NumDeriv Hard-wires 4s for r here. Why?
		}
	}

	if(verbose >= 2) {
		mxLog("Hessian estimation: Populating Hessian ([%d, %d] = %d) with value %f...",
		      i, i, i*numParams+i, Haprox[0]);
	}
	gradient[i] = Gaprox[0];
	hessian[i*numParams + i] = Haprox[0];

	if(verbose >= 2) {mxLog("Done with parameter %d.", i);}

}

void omxComputeNumericDeriv::omxEstimateHessianOffDiagonal(int i, int l, struct hess_struct* hess_work)
{
    static const double v = 2.0; //Note: NumDeriv comments that this could be a parameter, but is hard-coded in the algorithm

	double *Haprox             = hess_work->Haprox;
	omxMatrix* fitMatrix = hess_work->fitMatrix; 
	FitContext* fc = hess_work->fc; 
	double *freeParams         = fc->est;

	double iOffset = std::max(fabs(stepSize*optima[i]), stepSize);
	double lOffset = std::max(fabs(stepSize*optima[l]), stepSize);

	for(int k = 0; k < numIter; k++) {
		freeParams[i] = optima[i] + iOffset;
		freeParams[l] = optima[l] + lOffset;

		fc->copyParamToModel();

		++hess_work->probeCount;
		omxRecompute(fitMatrix, fc);
		double f1 = omxMatrixElement(fitMatrix, 0, 0);

		freeParams[i] = optima[i] - iOffset;
		freeParams[l] = optima[l] - lOffset;

		fc->copyParamToModel();

		++hess_work->probeCount;
		omxRecompute(fitMatrix, fc);
		double f2 = omxMatrixElement(fitMatrix, 0, 0);

		Haprox[k] = (f1 - 2.0 * minimum + f2 - hessian[i*numParams+i]*iOffset*iOffset -
						hessian[l*numParams+l]*lOffset*lOffset)/(2.0*iOffset*lOffset);
		if(verbose >= 2) {
			mxLog("Hessian first off-diagonal calculation: Haprox = %f, iOffset = %f, lOffset=%f from params %f, %f and %f, %f and %d (also: %f, %f and %f)",
			      Haprox[k], iOffset, lOffset, f1, hessian[i*numParams+i], hessian[l*numParams+l],
			      v, k, pow(v, k), stepSize*optima[i], stepSize*optima[l]);
		}

		freeParams[i] = optima[i];				// Reset parameter values
		freeParams[l] = optima[l];

		iOffset = iOffset / v;					//  And shrink step
		lOffset = lOffset / v;
	}

	for(int m = 1; m < numIter; m++) {						// Richardson Step
		for(int k = 0; k < (numIter - m); k++) {
			//if(OMX_DEBUG) {mxLog("Hessian off-diagonal calculation: Haprox = %f, iOffset = %f, lOffset=%f from params %f, %f and %f, %f and %d (also: %f, %f and %f, and %f).", Haprox[k], iOffset, lOffset, stepSize, optima[i], optima[l], v, m, pow(4.0, m), stepSize*optima[i], stepSize*optima[l], k);}
			Haprox[k] = (Haprox[k+1] * pow(4.0, m) - Haprox[k]) / (pow(4.0, m)-1);
		}
	}

	if(verbose >= 2) {
		mxLog("Hessian estimation: Populating Hessian"
		      " ([%d, %d] = %d and %d) with value %f...",
		      i, l, i*numParams+l, l*numParams+i, Haprox[0]);
	}
	hessian[i*numParams+l] = Haprox[0];
	hessian[l*numParams+i] = Haprox[0];
}

void omxComputeNumericDeriv::doHessianCalculation(int numChildren, struct hess_struct *hess_work)
{
	// gcc does not detect the usage of the following variable
	// in the omp parallel pragma, and marks the variable as
	// unused, so the attribute is placed to silence the Rf_warning.
    int __attribute__((unused)) parallelism = (numChildren == 0) ? 1 : numChildren;

	std::vector<std::pair<int,int> > todo;
	todo.reserve(numParams * (numParams-1) / 2);
	for(int i = 0; i < numParams; i++) {
		for(int j = i - 1; j >= 0; j--) {
			if (std::isfinite(hessian[i*numParams + j])) continue;
			todo.push_back(std::make_pair(i,j));
		}
	}

	if (numChildren) {
#pragma omp parallel for num_threads(parallelism)
		for(int i = 0; i < numParams; i++) {
			if (std::isfinite(hessian[i*numParams + i])) continue;
			int threadId = (numChildren < 2) ? 0 : omx_absolute_thread_num();
			omxEstimateHessianOnDiagonal(i, hess_work + threadId);
		}

#pragma omp parallel for num_threads(parallelism)
		for(int i = 0; i < int(todo.size()); i++) {
			int threadId = (numChildren < 2) ? 0 : omx_absolute_thread_num();
			omxEstimateHessianOffDiagonal(todo[i].first, todo[i].second, hess_work + threadId);
		}
	} else {
		for(int i = 0; i < numParams; i++) {
			if (std::isfinite(hessian[i*numParams + i])) continue;
			omxEstimateHessianOnDiagonal(i, hess_work);
		}
		for(int i = 0; i < int(todo.size()); i++) {
			omxEstimateHessianOffDiagonal(todo[i].first, todo[i].second, hess_work);
		}
	}
}

void omxComputeNumericDeriv::initFromFrontend(omxState *state, SEXP rObj)
{
	super::initFromFrontend(state, rObj);

	if (state->conList.size()) {
		Rf_error("Cannot compute estimated Hessian with constraints (%d constraints found)",
		      state->conList.size());
	}

	fitMat = omxNewMatrixFromSlot(rObj, state, "fitfunction");
	setFreeVarGroup(fitMat->fitFunction, varGroup);

	SEXP slotValue;

	Rf_protect(slotValue = R_do_slot(rObj, Rf_install("iterations")));
	numIter = INTEGER(slotValue)[0];
	if (numIter < 2) Rf_error("stepSize must be 2 or greater");

	Rf_protect(slotValue = R_do_slot(rObj, Rf_install("parallel")));
	parallel = Rf_asLogical(slotValue);

	Rf_protect(slotValue = R_do_slot(rObj, Rf_install("verbose")));
	verbose = Rf_asInteger(slotValue);

	Rf_protect(slotValue = R_do_slot(rObj, Rf_install("stepSize")));
	stepSize = REAL(slotValue)[0];
	if (stepSize <= 0) Rf_error("stepSize must be positive");

	knownHessian = NULL;
	{
		ScopedProtect(slotValue, R_do_slot(rObj, Rf_install("knownHessian")));
		if (!Rf_isNull(slotValue)) {
			knownHessian = REAL(slotValue);
			SEXP dimnames;
			ScopedProtect pdn(dimnames, Rf_getAttrib(slotValue, R_DimNamesSymbol));
			{
				SEXP names;
				ScopedProtect p1(names, VECTOR_ELT(dimnames, 0));
				{
					int nlen = Rf_length(names);
					khMap.assign(nlen, -1);
					for (int nx=0; nx < nlen; ++nx) {
						const char *vname = CHAR(STRING_ELT(names, nx));
						for (int vx=0; vx < int(varGroup->vars.size()); ++vx) {
							if (strEQ(vname, varGroup->vars[vx]->name)) {
								khMap[nx] = vx;
								if (verbose >= 1) mxLog("%s: knownHessian[%d] '%s' mapped to %d",
											name, nx, vname, vx);
								break;
							}
						}
					}
				}
			}
		}
	}
}

void omxComputeNumericDeriv::computeImpl(FitContext *fc)
{
	int newWanted = fc->wanted | FF_COMPUTE_HESSIAN | FF_COMPUTE_GRADIENT;
	bool hasGradient = fc->wanted & FF_COMPUTE_GRADIENT;
	numParams = int(fc->numParam);
	if (numParams <= 0) Rf_error("Model has no free parameters");

	optima.resize(numParams);
	memcpy(optima.data(), fc->est, sizeof(double) * numParams);

	if (parallel) fc->createChildren();

	// TODO: Check for nonlinear constraints and adjust algorithm accordingly.
	// TODO: Allow more than one hessian value for calculation

	int numChildren = 0;
	if (parallel) numChildren = fc->childList.size();

	omxRecompute(fitMat, fc);
	minimum = omxMatrixElement(fitMat, 0, 0);
	if (!std::isfinite(minimum)) {
		if (verbose >= 1) mxLog("%s: reference fit is %f; skipping", name, minimum);
		return;
	}

	struct hess_struct* hess_work;
	if (numChildren < 2) {
		hess_work = Calloc(1, struct hess_struct);
		omxPopulateHessianWork(hess_work, fc);
	} else {
		hess_work = Calloc(numChildren, struct hess_struct);
		for(int i = 0; i < numChildren; i++) {
			omxPopulateHessianWork(hess_work + i, fc->childList[i]);
		}
	}
	if(verbose >= 1) mxLog("Numerical Hessian approximation (%d children, ref fit %.2f)",
			       numChildren, minimum);

	hessian = fc->getDenseHessUninitialized();
	Eigen::Map< Eigen::MatrixXd > eH(hessian, numParams, numParams);
	eH.setConstant(NA_REAL);

	fc->grad.resize(numParams);
	Eigen::VectorXd oldGrad;
	if (hasGradient) {
		oldGrad = fc->grad;
	}
	fc->grad.setConstant(NA_REAL);
	gradient = fc->grad.data();
  
	if (knownHessian) {
		int khSize = int(khMap.size());
		Eigen::Map< Eigen::MatrixXd > kh(knownHessian, khSize, khMap.size());
		for (int rx=0; rx < khSize; ++rx) {
			for (int cx=0; cx < khSize; ++cx) {
				if (khMap[rx] < 0 || khMap[cx] < 0) continue;
				eH(khMap[rx], khMap[cx]) = kh(rx, cx);
			}
		}
	}

	doHessianCalculation(numChildren, hess_work);

	totalProbeCount = 0;

	if (numChildren < 2) {
		totalProbeCount = hess_work->probeCount;
		Free(hess_work->Haprox);
		Free(hess_work->Gaprox);
	    Free(hess_work);
	} else {
		for(int i = 0; i < numChildren; i++) {
			struct hess_struct *hw = hess_work + i;
			totalProbeCount += hw->probeCount;
			Free(hw->Haprox);
			Free(hw->Gaprox);
		}
		Free(hess_work);
	}

	if (hasGradient && ((oldGrad - fc->grad).array().abs() > .1).any()) {
		// We could actually estimate the forward, backward,
		// and central gradient here without any more fit
		// evaluations or relying on fc->grad. It might be
		// nice to return these estimates separately in the
		// $output slot.
		if (std::isfinite((oldGrad - fc->grad).norm())) {
			if (verbose >= 1) {
				// fc->grad may not be the forward difference
				mxLog("Central difference gradient estimates differs from forward difference by %f",
				      (oldGrad - fc->grad).norm());
			}
			if (fc->inform < INFORM_NOT_AT_OPTIMUM) fc->inform = INFORM_NOT_AT_OPTIMUM;
			for (int px=0; px < oldGrad.size(); ++px) {
				if (fabs(oldGrad[px] - fc->grad[px]) > .1) fc->grad[px] = NA_REAL;
			}
		}
	}

	memcpy(fc->est, optima.data(), sizeof(double) * numParams);
	fc->copyParamToModel();
	// auxillary information like per-row likelihoods need a refresh
	omxRecompute(fitMat, fc);
	fc->wanted = newWanted;
}

void omxComputeNumericDeriv::reportResults(FitContext *fc, MxRList *slots, MxRList *result)
{
	SEXP calculatedHessian;
	Rf_protect(calculatedHessian = Rf_allocMatrix(REALSXP, numParams, numParams));
	fc->copyDenseHess(REAL(calculatedHessian));
	result->add("calculatedHessian", calculatedHessian);

	MxRList out;
	out.add("probeCount", Rf_ScalarInteger(totalProbeCount));
	slots->add("output", out.asR());
}

omxCompute *newComputeNumericDeriv()
{
	return new omxComputeNumericDeriv;
}

