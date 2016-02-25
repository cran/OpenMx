/*
  Copyright 2015 Joshua Nathaniel Pritikin and contributors

  This is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

// Named in honor of Fellner (1987) "Sparse matrices, and the
// estimation of variance components by likelihood methods"
// Fellner was probably the first to apply sparse matrix algorithms
// to this kind of problem.

#include "glue.h"
#include <iterator>
#include <exception>
#include <stdexcept>
#include <Rconfig.h>
#include <Rmath.h>
#include "omxFitFunction.h"
#include "RAMInternal.h"
#include <Eigen/Cholesky>

namespace FellnerFitFunction {
	struct state {
		int verbose;
		int parallel;
		int numProfiledOut;
		std::vector<int> olsVarNum;     // index into fc->est
		Eigen::MatrixXd olsDesign;      // a.k.a "X"

		int computeCov(RelationalRAMExpectation::independentGroup &ig);
		void compute(omxFitFunction *oo, int want, FitContext *fc);
		void setupProfiledParam(omxFitFunction *oo, FitContext *fc);
		bool parallelEnabled(omxFitFunction *oo);
	};

	static void compute(omxFitFunction *oo, int want, FitContext *fc)
	{
		state *st = (state *) oo->argStruct;
		st->compute(oo, want, fc);
	}

	void state::setupProfiledParam(omxFitFunction *oo, FitContext *fc)
	{
		omxExpectation *expectation             = oo->expectation;
		omxRAMExpectation *ram = (omxRAMExpectation*) expectation->argStruct;

		if (numProfiledOut) ram->forceSingleGroup = true;
		omxExpectationCompute(fc, expectation, "nothing", "flat");
		
		if (numProfiledOut == 0) return;

		RelationalRAMExpectation::state *rram = ram->rram;
		if (rram->group.size() > 1) {
			Rf_error("Cannot profile out parameters when problem is split into independent groups");
		}

		RelationalRAMExpectation::independentGroup &ig = *rram->group[0];
		omxData *data               = expectation->data;
		fc->profiledOut.assign(fc->numParam, false);

		olsVarNum.reserve(numProfiledOut);
		olsDesign.resize(ig.dataVec.size(), numProfiledOut);

		ProtectedSEXP Rprofile(R_do_slot(oo->rObj, Rf_install("profileOut")));
		for (int px=0; px < numProfiledOut; ++px) {
			const char *pname = CHAR(STRING_ELT(Rprofile, px));
			int vx = fc->varGroup->lookupVar(pname);
			if (vx < 0) {
				mxLog("Parameter [%s] not found", pname);
				continue;
			}

			omxFreeVar &fv = *fc->varGroup->vars[vx];
			olsVarNum.push_back(vx);
			bool found = false;
			bool moreThanOne;
			const omxFreeVarLocation *loc =
				fv.getOnlyOneLocation(ram->M, moreThanOne);
			if (loc) {
				if (moreThanOne) {
					mxLog("Parameter [%s] appears in more than one spot in %s",
					      pname, ram->M->name());
					continue;
				}
				found = true;
				int vnum = loc->row + loc->col;
				// Should ensure the loading is fixed and not a defvar TODO
				// Should ensure zero variance & no cross-level links TODO
				olsDesign.col(px) = (ig.dataColumn.array() == vnum).cast<double>();
			}
			loc = fv.getOnlyOneLocation(ram->A, moreThanOne);
			if (loc) {
				if (moreThanOne) {
					mxLog("Parameter [%s] appears in more than one spot in %s",
					      pname, ram->A->name());
					continue;
				}
				found = true;
				int vnum = loc->col;
				EigenMatrixAdaptor eA(ram->A);
				int rnum;
				eA.col(vnum).array().abs().maxCoeff(&rnum);
				// ensure only 1 nonzero in column TODO
				for (size_t ax=0; ax < ig.placements.size(); ++ax) {
					RelationalRAMExpectation::placement &pl = ig.placements[ax];
					RelationalRAMExpectation::addr &a1 = rram->layout[ pl.aIndex ];
					if (a1.model != expectation) continue;
					data->handleDefinitionVarList(ram->M->currentState, a1.row);
					double weight = omxVectorElement(ram->M, vnum);
					olsDesign.col(px).segment(pl.obsStart, a1.numObs()) =
						weight * (ig.dataColumn.segment(pl.obsStart, a1.numObs()) == rnum).cast<double>();
				}
			}
			if (!found) Rf_error("oops");

			fc->profiledOut[vx] = true;
		}
	}

	int state::computeCov(RelationalRAMExpectation::independentGroup &ig)
	{
		if (0 == ig.dataVec.size()) return 0;

		ig.computeCov2();

		/*
		if (!ig.analyzedCov) {
			ig.fullCov.makeCompressed();
			ig.covDecomp.analyzePattern(ig.fullCov);
			ig.analyzedCov = true;
		}
		ig.covDecomp.factorize(ig.fullCov);
		*/

		Eigen::MatrixXd denseCov = ig.fullCov;
		ig.covDecomp.compute(denseCov);

		if (ig.covDecomp.info() != Eigen::Success || !(ig.covDecomp.vectorD().array() > 0.0).all()) return 1;

		ig.covDecomp.refreshInverse();
		return 0;
	}

	bool state::parallelEnabled(omxFitFunction *oo)
	{
		if (parallel == NA_INTEGER) {
			omxExpectation *expectation             = oo->expectation;
			omxRAMExpectation *ram = (omxRAMExpectation*) expectation->argStruct;
			RelationalRAMExpectation::state *rram   = ram->rram;
			return int(rram->group.size()) > 2*Global->numThreads;
		}
		return parallel;
	}

	void state::compute(omxFitFunction *oo, int want, FitContext *fc)
	{
		omxExpectation *expectation             = oo->expectation;
		omxRAMExpectation *ram = (omxRAMExpectation*) expectation->argStruct;

		if (want & (FF_COMPUTE_PREOPTIMIZE)) {
			setupProfiledParam(oo, fc);

			RelationalRAMExpectation::state *rram   = ram->rram;
			if (verbose >= 1) {
				mxLog("%s: %d groups, parallel = %d", oo->name(),
				      int(rram->group.size()), parallelEnabled(oo));
			}
			return;
		}

		if (!(want & (FF_COMPUTE_FIT | FF_COMPUTE_INITIAL_FIT))) Rf_error("Not implemented");

		double lpOut = NA_REAL;
		try {
			if (!ram->rram) {
				// possible to skip FF_COMPUTE_PREOPTIMIZE (e.g. omxCompute)
				omxExpectationCompute(fc, expectation, "nothing", "flat");
			}

			RelationalRAMExpectation::state *rram   = ram->rram;
			double lp = 0.0;
			for (size_t gx=0; gx < rram->group.size(); ++gx) {
				RelationalRAMExpectation::independentGroup &ig = *rram->group[gx];
				if (0 == ig.dataVec.size()) continue;
				ig.computeCov1(fc);
			}

			int covFailed = 0;
			if (parallelEnabled(oo)) {
#pragma omp parallel for num_threads(Global->numThreads) reduction(+:covFailed)
				for (size_t gx=0; gx < rram->group.size(); ++gx) {
					covFailed += computeCov(*rram->group[gx]);
				}
			} else {
				for (size_t gx=0; gx < rram->group.size(); ++gx) {
					covFailed += computeCov(*rram->group[gx]);
				}
			}
			if (covFailed) {
				throw std::runtime_error("Cholesky decomposition failed");
			}

			double remlAdj = 0.0;
			if (numProfiledOut) {
				RelationalRAMExpectation::independentGroup &ig = *rram->group[0];
				const Eigen::MatrixXd &iV = ig.covDecomp.getInverse();
				Eigen::MatrixXd constCov =
					olsDesign.transpose() * iV.selfadjointView<Eigen::Lower>() * olsDesign;
				Eigen::LLT< Eigen::MatrixXd > cholConstCov;
				cholConstCov.compute(constCov);
				if(cholConstCov.info() != Eigen::Success){
					// ought to report error detail TODO
					throw std::exception();
				}
				remlAdj = 2*Eigen::MatrixXd(cholConstCov.matrixL()).diagonal().array().log().sum();

				Eigen::MatrixXd ident = Eigen::MatrixXd::Identity(numProfiledOut, numProfiledOut);
				Eigen::MatrixXd cholConstPrec = cholConstCov.solve(ident).triangularView<Eigen::Lower>();
				Eigen::VectorXd param =
					(cholConstPrec.selfadjointView<Eigen::Lower>() *
					 olsDesign.transpose() * iV.selfadjointView<Eigen::Lower>() * ig.dataVec);

				for (int px=0; px < numProfiledOut; ++px) {
					fc->est[ olsVarNum[px] ] = param[px];
					fc->varGroup->vars[ olsVarNum[px] ]->copyToState(ram->M->currentState, param[px]);
				}
				lp += remlAdj - M_LN_2PI * numProfiledOut;
			}

			omxExpectationCompute(fc, expectation, "mean", "flat");

			for (size_t gx=0; gx < rram->group.size(); ++gx) {
				RelationalRAMExpectation::independentGroup &ig = *rram->group[gx];
				if (0 == ig.dataVec.size()) continue;

				//mxPrintMat("dataVec", ig.dataVec);
				//mxPrintMat("fullMeans", ig.fullMeans);
				//ig.applyRotationPlan(ig.expectedMean);
				//mxPrintMat("expectedMean", ig.expectedMean);

				Eigen::VectorXd resid = ig.dataVec - ig.expectedMean;
				//mxPrintMat("resid", resid);

				int clumps = ig.placements.size() / ig.clumpSize;

				double logDet = clumps * ig.covDecomp.log_determinant();
				const Eigen::MatrixXd &iV = ig.covDecomp.getInverse();
				// Eigen::Map< Eigen::MatrixXd > iV(ig.covDecomp.getInverseData(),
				// 				 ig.fullCov.rows(), ig.fullCov.rows());
				double iqf = 0.0;
				// OpenMP seems counterproductive here
				//#pragma omp parallel for num_threads(Global->numThreads) reduction(+:iqf)
				for (int cx=0; cx < clumps; ++cx) {
					iqf += (resid.segment(cx*ig.clumpObs, ig.clumpObs).transpose() *
						iV.selfadjointView<Eigen::Lower>() *
						resid.segment(cx*ig.clumpObs, ig.clumpObs));
				}
				double cterm = M_LN_2PI * ig.dataVec.size();
				if (verbose >= 2) mxLog("log det %f iqf %f cterm %f", logDet, iqf, cterm);
				lp += logDet + iqf + cterm;
			}
			lpOut = lp;
		} catch (const std::exception& e) {
			if (fc) fc->recordIterationError("%s: %s", oo->name(), e.what());
		}
		oo->matrix->data[0] = lpOut;
	}

	static void popAttr(omxFitFunction *oo, SEXP algebra)
	{
		// use Eigen_cholmod_wrap to return a sparse matrix? TODO
		// always return it?

		/*
		state *st                               = (state *) oo->argStruct;
		SEXP expCovExt, expMeanExt;
		if (st->fullCov.rows() > 0) {
			Rf_protect(expCovExt = Rf_allocMatrix(REALSXP, expCovInt->rows, expCovInt->cols));
			memcpy(REAL(expCovExt), expCovInt->data, sizeof(double) * expCovInt->rows * expCovInt->cols);
			Rf_setAttrib(algebra, Rf_install("expCov"), expCovExt);
		}

		if (expMeanInt && expMeanInt->rows > 0) {
			Rf_protect(expMeanExt = Rf_allocMatrix(REALSXP, expMeanInt->rows, expMeanInt->cols));
			memcpy(REAL(expMeanExt), expMeanInt->data, sizeof(double) * expMeanInt->rows * expMeanInt->cols);
			Rf_setAttrib(algebra, Rf_install("expMean"), expMeanExt);
			}   */
	}

	static void destroy(omxFitFunction *oo)
	{
		state *st = (state*) oo->argStruct;
		delete st;
	}

	static void init(omxFitFunction *oo)
	{
		omxExpectation* expectation = oo->expectation;
		if(expectation == NULL) {
			omxRaiseErrorf("%s cannot fit without a model expectation", oo->fitType);
			return;
		}
		if (!strEQ(expectation->expType, "MxExpectationRAM")) {
			Rf_error("%s: only MxExpectationRAM is implemented", oo->matrix->name());
		}

		oo->computeFun = FellnerFitFunction::compute;
		oo->destructFun = FellnerFitFunction::destroy;
		oo->populateAttrFun = FellnerFitFunction::popAttr;
		FellnerFitFunction::state *st = new FellnerFitFunction::state;
		oo->argStruct = st;

		ProtectedSEXP Rparallel(R_do_slot(oo->rObj, Rf_install("parallel")));
		st->parallel = Rf_asLogical(Rparallel);

		ProtectedSEXP Rprofile(R_do_slot(oo->rObj, Rf_install("profileOut")));
		st->numProfiledOut = Rf_length(Rprofile);

		{
			SEXP tmp;
			ScopedProtect p1(tmp, R_do_slot(oo->rObj, Rf_install("verbose")));
			st->verbose = Rf_asInteger(tmp) + OMX_DEBUG;
		}
	}
};

void InitFellnerFitFunction(omxFitFunction *oo)
{
	FellnerFitFunction::init(oo);
}
