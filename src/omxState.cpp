/*
 *  Copyright 2007-2021 by the individuals mentioned in the source code history
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

#include <time.h>
#include <stdarg.h>
#include <errno.h>
#include <set>

#include "omxState.h"
#include "Compute.h"
#include "omxImportFrontendState.h"
#include "npsolswitch.h"
#include "EnableWarnings.h"

struct std::unique_ptr<omxGlobal> Global;
static bool mxLogEnabled = false;

SEXP enableMxLog()
{
	mxLogEnabled = true;
	return Rf_ScalarLogical(1);
}

FreeVarGroup *omxGlobal::findVarGroup(int id)
{
	size_t numGroups = Global->freeGroup.size();
	for (size_t vx=0; vx < numGroups; ++vx) {
		std::vector<int> &ids = Global->freeGroup[vx]->id;
		for (size_t ix=0; ix < ids.size(); ++ix) {
			if (ids[ix] == id) return Global->freeGroup[vx];
		}
	}
	return NULL;
}

FreeVarGroup *omxGlobal::findOrCreateVarGroup(int id)
{
	FreeVarGroup *old = findVarGroup(id);
	if (old) return old;

	FreeVarGroup *fvg = new FreeVarGroup;
	fvg->id.push_back(id);
	Global->freeGroup.push_back(fvg);
	return fvg;
}

bool FreeVarGroup::hasSameVars(FreeVarGroup *g2)
{
	if (vars.size() != g2->vars.size()) return false;

	for (size_t vx=0; vx < vars.size(); ++vx) {
		if (vars[vx] != g2->vars[vx]) return false;
	}
	return true;
}

int FreeVarGroup::lookupVar(int matrix, int row, int col)
{
	for (size_t vx=0; vx < vars.size(); ++vx) {
		std::vector<omxFreeVarLocation> &locVec = vars[vx]->locations;
		for (size_t lx=0; lx < locVec.size(); lx++) {
			const omxFreeVarLocation &loc = locVec[lx];
			if (loc.matrix != matrix) continue;
			if (loc.row == row && loc.col == col) return vx;
		}
	}
	return -1;
}

int FreeVarGroup::lookupVar(omxMatrix *matrix, int row, int col)
{
	return lookupVar(~matrix->matrixNumber, row, col);
}

int FreeVarGroup::lookupVar(const char *name)
{
  auto got = byName.find(name);
  if (got == byName.end()) return -1;
  return got->second;
}

/* might be useful?
int FreeVarGroup::lookupVar(int id)
{
	std::vector<int>::iterator low =
		std::lower_bound(vars.begin(), vars.end(), id);
	if (low == vars.end()) return -1;
	int got = low - vars.begin();
	if (vars[got]->id == id) return got;
	return -1;
}
*/

void FreeVarGroup::reIndex()
{
  byName.clear();
	for (int vx = 0; vx < int(vars.size()); vx++) {
		omxFreeVar *fv = vars[vx];
    // In FREEVARGROUP_ALL, vx == fv->id
    auto ret = byName.emplace(fv->name, vx);
    if (!ret.second) mxThrow("Two free variables with same name '%s'", fv->name);
  }
}

void FreeVarGroup::cacheDependencies(omxState *os)
{
	size_t numMats = os->matrixList.size();
	size_t numAlgs = os->algebraList.size();

	dependencies.assign(numMats + numAlgs, false);
	locations.assign(numMats, false);

	for (size_t vx = 0; vx < vars.size(); vx++) {
		omxFreeVar *fv = vars[vx];
		auto deps = fv->getDeps();
		for (int index = 0; index < deps.size(); index++) {
			dependencies[deps[index] + numMats] = true;
		}
		for (size_t lx=0; lx < fv->locations.size(); ++lx) {
			locations[fv->locations[lx].matrix] = true;
		}
	}

	for(size_t i = 0; i < numMats; i++) {
		if (!locations[i]) continue;
		os->matrixList[i]->unshareMemoryWithR();
	}

	// Everything is set up. This is a good place to log.
	if (OMX_DEBUG) { log(os); }
}

static int freeVarComp(omxFreeVar *fv1, omxFreeVar *fv2)
{ return fv1->id < fv2->id; }

// NOTE: This assumes that free variables are sorted.
bool FreeVarGroup::isDisjoint(FreeVarGroup *other)
{
	std::vector< omxFreeVar* > overlap(std::max(vars.size(), other->vars.size()));
	std::vector< omxFreeVar* >::iterator it =
		std::set_intersection(vars.begin(), vars.end(),
				      other->vars.begin(), other->vars.end(),
				      overlap.begin(), freeVarComp);
	return it - overlap.begin() == 0;
}

void FreeVarGroup::markDirty(omxState *os)
{
	size_t numMats = os->matrixList.size();
	size_t numAlgs = os->algebraList.size();

        for(size_t i = 0; i < numMats; i++) {
		if (!locations[i]) continue;

		// The point of this is to increment the version numbers
		// on matrices holding free parameters. Otherwise there
		// is no way to tell which free parameters changes when
		// freeSet is used to partition parameters.
		omxMarkClean(os->matrixList[i]);
	}

	for(size_t i = 0; i < numMats; i++) {
		if (dependencies[i]) {
			int offset = ~(i - numMats);
			omxMarkDirty(os->matrixList[offset]);
		}
	}

	for(size_t i = 0; i < numAlgs; i++) {
		if (dependencies[i + numMats]) {
			omxMarkDirty(os->algebraList[i]);
		}
	}
}

void FreeVarGroup::log(omxState *os)
{
	size_t numMats = os->matrixList.size();
	size_t numAlgs = os->algebraList.size();
	std::string str;

	str += string_snprintf("FreeVarGroup(id=%d", id[0]);
	for (size_t ix=1; ix < id.size(); ++ix) {
		str += string_snprintf(",%d", id[ix]);
	}
	str += string_snprintf(") with %d variables:", (int) vars.size());

	for (size_t vx=0; vx < vars.size(); ++vx) {
		str += " ";
		str += vars[vx]->name;
	}
	if (vars.size()) str += "\nwill dirty:";

	for(size_t i = 0; i < numMats; i++) {
		if (dependencies[i]) {
			int offset = ~(i - numMats);
			str += " ";
			str += os->matrixList[offset]->name();
		}
	}

	for(size_t i = 0; i < numAlgs; i++) {
		if (dependencies[i + numMats]) {
			str += " ";
			str += os->algebraList[i]->name();
		}
	}
	str += "\n";

	mxLogBig(str);
}

omxGlobal::omxGlobal()
{
	RNGCheckedOut = false;
	mpi = 0;
	silent = true;
	ComputePersist = false;
	startTime = time(0);
	maxSeconds = 0;
	timedOut = false;
	lastProgressReport = startTime;
	mxLogSetCurrentRow(-1);
	numThreads = 1;
	analyticGradients = 0;
	llScale = -2.0;
	debugProtectStack = OMX_DEBUG;
	rowLikelihoodsWarning = false;
	RAMmultilevelWarning = false;
	unpackedConfidenceIntervals = false;
	topFc = NULL;
	intervals = true;
	gradientTolerance = 1e-6;
	boundsUpdated = false;
	dataTypeWarningCount = 0;
	userInterrupted = false;
	lastIndexDone=0;
	lastIndexDoneTime=0;
  NPSOL_HACK = 0;

  gradientAlgo = GradientAlgorithm_Auto;
  gradientIter = 1;
  gradientStepSize = NA_REAL;

	RAMInverseOpt = true;
	RAMMaxDepth = 30;

	FreeVarGroup *fvg = new FreeVarGroup;
	fvg->id.push_back(FREEVARGROUP_ALL);   // all variables
	freeGroup.push_back(fvg);

	fvg = new FreeVarGroup;
	fvg->id.push_back(FREEVARGROUP_NONE);  // no variables
	freeGroup.push_back(fvg);

	// Preallocate a large buffer to avoid reallocations. CRAN runs clang's ASAN
	// to look for problems. If extensions like gwsem are instrumented and OpenMx
	// is not instrumented then false positives can result,
	// https://github.com/google/sanitizers/wiki/AddressSanitizerContainerOverflow
	checkpointColnames.reserve(100);

  globalState = std::make_unique<omxState>();
}

void omxGlobal::setDefaultGradientAlgo()
{
  if (gradientAlgo == GradientAlgorithm_Auto) {
    if (engine == OptEngine_CSOLNP || engine == OptEngine_SD) {
      gradientAlgo = GradientAlgorithm_Forward;
    } else {
      gradientAlgo = GradientAlgorithm_Central;
    }
  }

  if (!std::isfinite(gradientStepSize)) {
    gradientStepSize = 1e-7;
    if (engine == OptEngine_NLOPT) {
      gradientStepSize *= GRADIENT_FUDGE_FACTOR(2.0);
    }
  }

  if (OMX_DEBUG) {
    mxLog("omxGlobal::setDefaultGradientAlgo algo=%d iter=%d stepsize=%g",
          gradientAlgo, gradientIter, gradientStepSize);
  }
}

void omxState::restoreParam(const Eigen::Ref<const Eigen::VectorXd> point)
{
	if (!hasFakeParam) mxThrow("Cannot restore; fake parameters not loaded");
	hasFakeParam = false;

	auto varGroup = Global->findVarGroup(FREEVARGROUP_ALL);
	size_t numParam = varGroup->vars.size();
	for(size_t k = 0; k < numParam; k++) {
		omxFreeVar* freeVar = varGroup->vars[k];
		freeVar->copyToState(this, point[k]);
	}
}

omxMatrix *omxState::getMatrixFromIndex(int matnum) const
{
	return matnum<0? matrixList[~matnum] : algebraList[matnum];
}

omxMatrix *omxState::lookupDuplicate(omxMatrix *element) const
{
  if (!element) return 0;
	if (!element->hasMatrixNumber) mxThrow("lookupDuplicate without matrix number");
	return getMatrixFromIndex(element->matrixNumber);
}

omxExpectation *omxState::getParent(omxExpectation *element) const
{
	auto *st = this;
	if (parent) st = parent;
	return st->expectationList[element->expNum];
}

omxExpectation *omxState::lookupDuplicate(omxExpectation *element) const
{
	return expectationList[element->expNum];
}

void omxState::setWantStage(int stage)
{
	if (wantStage == stage) mxThrow("omxState::setWantStage(%d) is redundant", stage);
	wantStage = stage;
	if (OMX_DEBUG) mxLog("wantStage set to 0x%x", stage);
}

omxMatrix *ConfidenceInterval::getMatrix(omxState *st) const
{
	return st->getMatrixFromIndex(matrixNumber);
}

struct ciCmp {
	bool operator() (const ConfidenceInterval* x, const ConfidenceInterval* y) const
	{
		if (x->matrixNumber != y->matrixNumber) {
			return x->matrixNumber < y->matrixNumber;
		} else if (x->row != y->row) {
			return x->row < y->row;
		} else {
			return x->col < y->col;
		}
	}
};

void omxGlobal::unpackConfidenceIntervals(omxState *currentState)
{
	if (unpackedConfidenceIntervals) return;
	unpackedConfidenceIntervals = true;

	// take care to preserve order
	std::vector<ConfidenceInterval*> tmp;
	std::swap(tmp, intervalList);
	std::set<ConfidenceInterval*, ciCmp> uniqueCIs;

	for (int ix=0; ix < (int) tmp.size(); ++ix) {
		ConfidenceInterval *ci = tmp[ix];
		if (!ci->isWholeAlgebra()) {
			auto iter = uniqueCIs.find(ci);
			if (iter == uniqueCIs.end()) {
				uniqueCIs.insert(ci);
				intervalList.push_back(ci);
			} else if (ci->cmpBoundAndType(**iter)) {
				Rf_warning("Different confidence intervals '%s' and '%s' refer to the same thing",
					   ci->name.c_str(), (*iter)->name.c_str());
        delete ci;
			}
			continue;
		}
		omxMatrix *mat = ci->getMatrix(currentState);
		for (int cx=0; cx < mat->cols; ++cx) {
			for (int rx=0; rx < mat->rows; ++rx) {
				ConfidenceInterval *cell = new ConfidenceInterval(*ci);
				cell->name = string_snprintf("%s[%d,%d]", ci->name.c_str(), 1+rx, 1+cx);
				cell->row = rx;
				cell->col = cx;
				auto iter = uniqueCIs.find(cell);
				if (iter == uniqueCIs.end()) {
					uniqueCIs.insert(cell);
					intervalList.push_back(cell);
				} else {
					if (cell->cmpBoundAndType(**iter)) {
						Rf_warning("Different confidence intervals '%s' and '%s' refer to the same thing",
							   cell->name.c_str(), (*iter)->name.c_str());
					}
					delete cell;
				}
			}
		}
		delete ci;
	}
}

void omxGlobal::deduplicateVarGroups()
{
	for (size_t g1=0; g1 < freeGroup.size(); ++g1) {
		for (size_t g2=freeGroup.size()-1; g2 > g1; --g2) {
			if (freeGroup[g1]->hasSameVars(freeGroup[g2])) {
				freeGroup[g1]->id.insert(freeGroup[g1]->id.end(),
							 freeGroup[g2]->id.begin(), freeGroup[g2]->id.end());
				delete freeGroup[g2];
				freeGroup.erase(freeGroup.begin() + g2);
			}
		}
	}
	for (size_t g1=0; g1 < freeGroup.size(); ++g1) {
    freeGroup[g1]->reIndex();
  }
}

int omxState::nextId = 0;

void omxState::init()
{
	// We use FF_COMPUTE_INITIAL_FIT because an expectation
	// could depend on the value of an algebra. However, we
	// don't mark anything clean because an algebra could
	// depend on an expectation (via a fit function).

	stateId = ++nextId;
	setWantStage(FF_COMPUTE_INITIAL_FIT);
}

void omxState::loadDefinitionVariables(bool start)
{
	if (OMX_DEBUG) {
		mxLog("omxState[%d]::loadDefinitionVariables(start=%d)", getId(), start);
	}
	for(int ex = 0; ex < int(dataList.size()); ++ex) {
		omxData *e1 = dataList[ex];
		if (e1->defVars.size() == 0) continue;
		if (start && e1->nrows() != 1) {
			e1->loadFakeData(this, NA_REAL);
			continue;
		}
		e1->loadDefVars(this, 0);
	}
}

omxState::omxState(omxState *src, bool isTeam) :
  wantStage(0), parent(src), workBoss(isTeam? src : 0), hasFakeParam(false)
{
	init();

	dataList			= src->dataList;

	for(size_t mx = 0; mx < src->matrixList.size(); mx++) {
		// TODO: Smarter inference for which matrices to duplicate
		matrixList.push_back(omxDuplicateMatrix(src->matrixList[mx], this));
	}

	for(size_t j = 0; j < src->expectationList.size(); j++) {
		// TODO: Smarter inference for which expectations to duplicate
		expectationList.push_back(omxDuplicateExpectation(src->expectationList[j], this));
	}

	for(size_t j = 0; j < src->algebraList.size(); j++) {
		// TODO: Smarter inference for which algebras to duplicate
		algebraList.push_back(omxDuplicateMatrix(src->algebraList[j], this));
	}

	for(size_t j = 0; j < algebraList.size(); j++) {
		omxDuplicateAlgebra(algebraList[j], src->algebraList[j], this);
	}

	for(size_t mx = 0; mx < src->matrixList.size(); mx++) {
		// TODO: Smarter inference for which matrices to duplicate
		matrixList[mx]->copyAttr(src->matrixList[mx]);
	}

	for (size_t xx=0; xx < src->conListX.size(); ++xx) {
		conListX.push_back(src->conListX[xx]->duplicate(this));
	}
}

void omxState::initialRecalc(FitContext *fc)
{
	omxInitialMatrixAlgebraCompute(fc);

	for(size_t j = 0; j < expectationList.size(); j++) {
		// TODO: Smarter inference for which expectations to duplicate
		omxCompleteExpectation(expectationList[j]);
	}

	for (int ax=0; ax < (int) algebraList.size(); ++ax) {
		omxMatrix *matrix = algebraList[ax];
		if (!matrix->fitFunction) continue;
		omxCompleteFitFunction(matrix);
    omxRecompute(matrix, fc);
	}

	setWantStage(FF_COMPUTE_FIT);
}

void StateInvalidator::doData()
{
	for (int ax=0; ax < int(st.dataList.size()); ++ax) {
		auto d1 = st.dataList[ax];
		d1->invalidateCache();
	}
}

void StateInvalidator::doMatrix()
{
	for (int ax=0; ax < (int) st.matrixList.size(); ++ax) {
		omxMatrix *matrix = st.matrixList[ax];
		omxMarkDirty(matrix);
	}
}

void StateInvalidator::doExpectation()
{
	for(size_t ex = 0; ex < st.expectationList.size(); ex++) {
		st.expectationList[ex]->invalidateCache();
	}
}

void StateInvalidator::doAlgebra()
{
	for (int ax=0; ax < (int) st.algebraList.size(); ++ax) {
		omxMatrix *matrix = st.algebraList[ax];
		if (!matrix->fitFunction) {
			omxMarkDirty(matrix);
		} else {
			matrix->fitFunction->invalidateCache();
		}
	}
}

void omxState::invalidateCache()
{
	StateInvalidator si(*this);
	si();
}

void omxState::connectToData()
{
	for(size_t ex = 0; ex < expectationList.size(); ex++) {
		expectationList[ex]->connectToData();
	}
}

void omxState::reportConstraints(MxRList &out)
{
	if (!conListX.size()) return;

  SEXP cn, cr, cc;

  Rf_protect(cn = Rf_allocVector( STRSXP, conListX.size() ));
  Rf_protect(cr = Rf_allocVector( INTSXP, conListX.size() ));
  Rf_protect(cc = Rf_allocVector( INTSXP, conListX.size() ));
  for(int i=0; i < int(conListX.size()); i++){
    auto &con = *conListX[i];
    SET_STRING_ELT( cn, i, Rf_mkChar(con.name) );
    int rows, cols;
    con.getDim(&rows, &cols);
    INTEGER(cr)[i] = rows;
    INTEGER(cc)[i] = cols;
  }
  out.add("constraintNames", cn);
  out.add("constraintRows", cr);
  out.add("constraintCols", cc);
}

omxState::~omxState()
{
	if(OMX_DEBUG) { mxLog("Freeing %d Constraints.", (int) conListX.size());}
	for(int k = 0; k < (int) conListX.size(); k++) {
		delete conListX[k];
	}

	for(size_t ax = 0; ax < algebraList.size(); ax++) {
		algebraList[ax]->disconnect();
	}

	for(size_t ax = 0; ax < algebraList.size(); ax++) {
		algebraList[ax]->hasMatrixNumber = false;
		omxFreeMatrix(algebraList[ax]);
	}

	if(OMX_DEBUG) { mxLog("Freeing %d Matrices.", (int) matrixList.size());}
	for(size_t mk = 0; mk < matrixList.size(); mk++) {
		matrixList[mk]->hasMatrixNumber = false;
		omxFreeMatrix(matrixList[mk]);
	}

	if(OMX_DEBUG) { mxLog("Freeing %d Model Expectations.", (int) expectationList.size());}
	for(size_t ex = 0; ex < expectationList.size(); ex++) {
		omxFreeExpectationArgs(expectationList[ex]);
	}
}

omxGlobal::~omxGlobal()
{
	// Data are not modified and not copied. The same memory
	// is shared across all instances of state.
	// NOTE: This may need to change for MxDataDynamic
	for(size_t dx = 0; dx < globalState->dataList.size(); dx++) {
		omxFreeData(globalState->dataList[dx]);
	}

	if (!previousReport.empty()) {
		std::string empty;
		reportProgressStr(empty);
	}
	if (topFc) {
		delete topFc;
	}
	for (size_t cx=0; cx < intervalList.size(); ++cx) {
		delete intervalList[cx];
	}
	for (size_t cx=0; cx < checkpointList.size(); ++cx) {
		delete checkpointList[cx];
	}
	if (freeGroup.size()) {
		std::vector< omxFreeVar* > &vars = freeGroup[0]->vars;  // has all vars
		for (size_t vx=0; vx < vars.size(); ++vx) {
			delete vars[vx];
		}
	}
	for (size_t gx=0; gx < freeGroup.size(); ++gx) {
		delete freeGroup[gx];
	}
}

void string_vsnprintf(const char *fmt, va_list orig_ap, std::string &dest)
{
	// Cannot stack allocate std::string due to compiler bug that
	// causes clash with va_list when optimization (-O2) is enabled.
    int size = 100;
    while (1) {
        dest.resize(size);
	va_list ap;
	va_copy(ap, orig_ap);
        int n = vsnprintf((char *)dest.c_str(), size, fmt, ap);
	va_end(ap);
        if (n > -1 && n < size) {
            dest.resize(n);
            return;
        }
        if (n > -1)
            size = n + 1;
        else
            size *= 2;
    }
}

std::string string_snprintf(const char *fmt, ...)
{
	std::string str;
	va_list ap;
        va_start(ap, fmt);
	string_vsnprintf(fmt, ap, str);
        va_end(ap);
	return str;
}

#if OMX_DEBUG
// __thread is a gcc extension. I'm not sure about portability to
// other compilers. This is only here for debugging so you can remove
// it if you have trouble getting it through the compiler on your
// system.

static __thread int mxLogCurrentRow;

void mxLogSetCurrentRow(int row)
{
	mxLogCurrentRow = row;
}
#else
static const int mxLogCurrentRow = -1;
void mxLogSetCurrentRow(int row) {}
#endif

static ssize_t mxLogWriteSynchronous(const char *outBuf, int len)
{
	if (!mxLogEnabled) return len;

	int maxRetries = 20;
	ssize_t wrote = 0;
	ssize_t got;
#pragma omp critical
	{
		while (--maxRetries > 0) {
			got = write(2, outBuf + wrote, len - wrote);
			if (got == -EINTR) continue;
			if (got < 0) break;
			wrote += got;
			if (wrote == len) break;
		}
	}
	return wrote;
}

static const bool onlyThreadZero = false;

void mxLogBig(const std::string &str)   // thread-safe
{
	ssize_t len = ssize_t(str.size());
	if (len == 0) mxThrow("Attempt to log 0 characters with mxLogBig");

	if (onlyThreadZero && omx_absolute_thread_num() != 0) return;

	std::string fullstr;
	if (mxLogCurrentRow == -1) {
		fullstr = string_snprintf("[%d] ", omx_absolute_thread_num());
	} else {
		fullstr = string_snprintf("[%d@%d] ", omx_absolute_thread_num(), mxLogCurrentRow);
	}
	fullstr += str;
	len = ssize_t(fullstr.size());

	const char *outBuf = fullstr.c_str();
	ssize_t wrote = mxLogWriteSynchronous(outBuf, len);
	if (wrote != len) mxThrow("mxLogBig only wrote %d/%d, errno %d", int(wrote), int(len), errno);
}

void mxLog(const char* msg, ...)   // thread-safe
{
	if (onlyThreadZero && omx_absolute_thread_num() != 0) return;

	const int maxLen = 240;
	char buf1[maxLen];
	char buf2[maxLen];

	va_list ap;
	va_start(ap, msg);
	vsnprintf(buf1, maxLen, msg, ap);
	va_end(ap);

	int len;
	if (mxLogCurrentRow == -1) {
		len = snprintf(buf2, maxLen, "[%d] %s\n", omx_absolute_thread_num(), buf1);
	} else {
		len = snprintf(buf2, maxLen, "[%d@%d] %s\n", omx_absolute_thread_num(), mxLogCurrentRow, buf1);
	}

	ssize_t wrote = mxLogWriteSynchronous(buf2, len);
	if (wrote != len) mxThrow("mxLog only wrote %d/%d, errno=%d", int(wrote), len, errno);
}

void omxGlobal::reportProgressStr(std::string &str)
{
	ProtectedSEXP theCall(Rf_allocVector(LANGSXP, 3));
	SETCAR(theCall, Rf_install("imxReportProgress"));
	ProtectedSEXP Rmsg(Rf_allocVector(STRSXP, 1));
	SET_STRING_ELT(Rmsg, 0, Rf_mkChar(str.c_str()));
	SETCADR(theCall, Rmsg);
	SETCADDR(theCall, Rf_ScalarInteger(previousReport.length())); // not UTF8 safe TODO
	Rf_eval(theCall, R_GlobalEnv);
	previousReport = str;
}

void omxGlobal::reportProgress(const char *context, FitContext *fc)
{
	reportProgress1(context, fc->asProgressReport());
	interrupted();
}

bool omxGlobal::interrupted()
{
	if (omp_get_thread_num() != 0 && omp_get_num_threads() != 1) {
		mxLog("omxGlobal::interrupted called from thread %d/%d (report this bug to developers)",
		      omp_get_thread_num(), omp_get_num_threads());
		return false;
	}

	// see Rcpp's checkUserInterrupt
	auto checkInterruptFn = [](void *dummy){ R_CheckUserInterrupt(); };
	bool got = R_ToplevelExec(checkInterruptFn, NULL) == FALSE;
	if (got) {
		omxRaiseErrorf("User interrupt");
		userInterrupted = true;
	}
	return got;
}

void omxGlobal::reportProgress1(const char *context, std::string detail)
{
	if (omp_get_thread_num() != 0 && omp_get_num_threads() != 1) {
		mxLog("omxGlobal::reportProgress(%s,%s) called from thread %d/%d (report this bug to developers)",
		      context, detail.c_str(), omp_get_thread_num(), omp_get_num_threads());
		return;
	}

	time_t now = time(0);
	if (Global->maxSeconds > 0 && now > Global->startTime + Global->maxSeconds && !Global->timedOut) {
		Global->timedOut = true;
		Rf_warning("Time limit of %d minutes %d seconds exceeded",
			   Global->maxSeconds/60, Global->maxSeconds % 60);
	}
	if (silent || now - lastProgressReport < 1) return;

	lastProgressReport = now;

	auto &cli = Global->computeLoopIter;
	auto &clm = Global->computeLoopMax;
	std::string str;
	if (cli.size() == 1 && cli[0] != lastIndexDone) {
		lastIndexDone = cli[0];
		lastIndexDoneTime = now;
	}
	if (cli.size() == 1 && clm[0] != 0 && cli[0] <= clm[0] && lastIndexDone > 0) {
		str += "[";
		double pctDone = lastIndexDone / double(clm[0]);
		auto elapsed = lastIndexDoneTime - Global->startTime;
		auto estTotal = elapsed / pctDone;
		int estRemain = estTotal - elapsed;
		if (estTotal < 60*60) {
			str += string_snprintf("%02d:%02d", estRemain/60, estRemain%60);
		} else if (estTotal < 60*60*24) {
			int hours = estRemain/(60*60);
			int min = (estRemain - hours*60*60) / 60;
			int sec = estRemain % 60;
			str += string_snprintf("%02d:%02d:%02d", hours, min, sec);
		} else {
			int days = estRemain/(24*60*60);
			estRemain -= days*24*60*60;
			int hours = estRemain/(60*60);
			estRemain -= hours*60*60;
			int min = (estRemain) / 60;
			int sec = estRemain % 60;
			str += string_snprintf("%d %02d:%02d:%02d", days, hours, min, sec);
		}
		str += "] ";
	} else if (cli.size() > 1) {
		str += "[";
		for (int x1=0; x1 < int(cli.size()); ++x1) {
			std::ostringstream os_temp;
			os_temp << cli[x1];
			str += os_temp.str();
			if (x1 < int(cli.size())-1) str += "/";
		}
		str += "] ";
	}
	str += context;
	str += " ";
	str += detail;
	reportProgressStr(str);
}

void diagParallel(int verbose, const char* msg, ...)
{
	if (!verbose && !Global->parallelDiag) return;

	const int maxLen = 240;
	char buf1[maxLen];

	va_list ap;
	va_start(ap, msg);
	vsnprintf(buf1, maxLen, msg, ap);
	va_end(ap);

	if (verbose) {
		mxLog("%s", buf1);
	} else if (Global->parallelDiag) {
		ProtectedSEXP theCall(Rf_allocVector(LANGSXP, 2));
		SETCAR(theCall, Rf_install("message"));
		ProtectedSEXP Rmsg(Rf_allocVector(STRSXP, 1));
		SET_STRING_ELT(Rmsg, 0, Rf_mkChar(buf1));
		SETCADR(theCall, Rmsg);
		Rf_eval(theCall, R_GlobalEnv);
	}
}

void u_omxRaiseError()
{
	// keep for debugger breakpoints
}

void omxRaiseErrorf(const char* msg, ...)
{
	std::string str;
	va_list ap;
	va_start(ap, msg);
	string_vsnprintf(msg, ap, str);
	va_end(ap);
	u_omxRaiseError();

	if(OMX_DEBUG) {
		mxLog("Error raised: %s", str.c_str());
	}

	bool overflow = false;
#pragma omp critical(bads)
        {
		if (Global->bads.size() > 100) {
			overflow = true;
		} else {
			Global->bads.push_back(str);
		}
	}

        // mxLog takes a lock too, so call it outside of critical section
        if (overflow) mxLog("Too many errors: %s", str.c_str());
}

const char *omxGlobal::getBads()
{
	if (bads.size() == 0) return NULL;

	std::string str;
	for (size_t mx=0; mx < bads.size(); ++mx) {
		if (bads.size() > 1) str += string_snprintf("%d:", (int)mx+1);
		str += bads[mx];
		if (str.size() > (1<<14)) break;
		if (mx < bads.size() - 1) str += "\n";
	}

	size_t sz = str.size();
	char *mem = R_alloc(sz+1, 1);  // use R's memory
	memcpy(mem, str.c_str(), sz);
	mem[sz] = 0;
	return mem;
}

void omxGlobal::checkpointMessage(FitContext *fc, const char *fmt, ...)
{
	std::string str;
	va_list ap;
        va_start(ap, fmt);
	string_vsnprintf(fmt, ap, str);
        va_end(ap);

	if (OMX_DEBUG) mxLog("checkpointMessage: %s", str.c_str());

	for(size_t i = 0; i < checkpointList.size(); i++) {
		checkpointList[i]->message(fc, str.c_str());
	}
}

void omxGlobal::checkpointPostfit(const char *callerName, FitContext *fc, bool force)
{
	for(size_t i = 0; i < checkpointList.size(); i++) {
		checkpointList[i]->postfit(callerName, fc, force);
	}
}

void omxConstraint::setInitialSize(int sz)
{
  origSize = sz;
	size = sz;
  redundant.assign(size, false);
  seenActive.assign(size, false);
	if (sz == 0) {
		Rf_warning("Constraint '%s' evaluated to a 0x0 matrix and "
               "will have no effect", name);
	}

  int maxVars = Global->findVarGroup(FREEVARGROUP_ALL)->vars.size();
  initialJac.resize(origSize, maxVars);
  initialJac.setConstant(NA_REAL);
}

void UserConstraint::prep(FitContext *fc)
{
	refresh(fc);
  setInitialSize(pad->rows * pad->cols);

	if(jacobian){
		jacMap.resize(jacobian->cols);
		auto &jacColNames = jacobian->colnames;
		for (size_t nx=0; nx < jacColNames.size(); ++nx) {
			int to = fc->varGroup->lookupVar(jacColNames[nx]);
      if (strict && to < 0) {
        mxThrow("Constraint '%s' has a Jacobian entry for unrecognized "
                "parameter '%s'. If this is not an mistake and you "
                "have merely fixed this parameter then you can "
                "use the strict=FALSE argument to mxConstraint "
                "to turn off this precautionary check", name, jacColNames[nx]);
      }
			jacMap[nx] = to;
		}
	}
}

void UserConstraint::preeval(FitContext *fc)
{
	omxRecompute(pad, fc);
}

UserConstraint::UserConstraint(FitContext *fc, const char *u_name, omxMatrix *arg1,
                               omxMatrix *arg2, omxMatrix *jac, int u_verbose) :
	super(u_name)
{
	omxState *state = fc->state;
	omxMatrix *args[2] = {arg1, arg2};
	pad = omxNewAlgebraFromOperatorAndArgs(10, args, 2, state); // 10 = binary subtract
	jacobian = jac;
  verbose = u_verbose;
}

void UserConstraint::getDim(int *rowsOut, int *colsOut) const
{
  *rowsOut = pad->rows;
  *colsOut = pad->cols;
}

omxConstraint *UserConstraint::duplicate(omxState *dest) const
{
	omxMatrix *args[2] = {
		dest->lookupDuplicate(pad->algebra->algArgs[0]),
		dest->lookupDuplicate(pad->algebra->algArgs[1])
	};

	UserConstraint *uc = new UserConstraint(name);
	uc->opCode = opCode;
  uc->redundant = redundant;
  uc->size = size;
	uc->pad = omxNewAlgebraFromOperatorAndArgs(10, args, 2, dest); // 10 = binary subtract
	uc->jacobian = jacobian;
  uc->jacMap = jacMap;
  uc->verbose = verbose;
	return uc;
}

void UserConstraint::refreshAndGrab(FitContext *fc, double *out)
{
	refresh(fc);

	for(int k = 0, d = 0; k < int(redundant.size()); k++) {
    if (redundant[k]) continue;
		double got = pad->data[k];
    // ineq constraints are always translated to LESS_THAN
		if (opCode == GREATER_THAN) got = -got;
		out[d++] = got;
	}
}

void UserConstraint::analyticJac(FitContext *fc, MatrixStoreFn out)
{
  if (!jacobian) return;

  omxRecompute(jacobian, fc);
  EigenArrayAdaptor cj(jacobian);

  for (int rx=0, dx=0; rx < int(redundant.size()); ++rx) {
    if (redundant[rx]) continue;

    for (int c=0; c < jacobian->cols; c++){
      if (jacMap[c] < 0) continue;
      out(dx, jacMap[c], cj(rx, c));
    }
    dx += 1;
  }
}

int UserConstraint::getVerbose() const
{
  return verbose;
}

UserConstraint::~UserConstraint()
{
	omxFreeMatrix(pad);
}

void UserConstraint::refresh(FitContext *fc)
{
	omxRecompute(pad, fc);
}

void omxConstraint::recalcSize()
{
  size = std::count(redundant.begin(), redundant.end(), false);
  if (verbose >= 1) mxLog("%s::recalcSize %d/%d constraints not redundant",
                          name, size, int(redundant.size()));
}

ConstraintVec::ConstraintVec(FitContext *fc, const char *u_name,
                             ConstraintVec::ClassifyFn u_cf) :
  name(u_name), cf(u_cf), ineqAlwaysActive(false)
{
  verbose = 0;
  count = 0;
  anyAnalyticJacCache = false;
  auto &conList = fc->state->conListX;
  for (int j=0; j < int(conList.size()); j++) {
    omxConstraint &con = *conList[j];
    if (!cf(con)) continue;
    count += con.size;
    verbose = std::max(verbose, con.getVerbose());
    anyAnalyticJacCache |= con.hasAnalyticJac(fc);
  }
  verifyJac = verbose >= 3;
}

void ConstraintVec::allocJacTool(FitContext *fc)
{
  if (jacTool) return;
  jacTool =
    std::unique_ptr< AutoTune<JacobianGadget> >(new AutoTune<JacobianGadget>(name));
  jacTool->setWork(std::unique_ptr<JacobianGadget>(new JacobianGadget(fc->getNumFree())));
  jacTool->setMaxThreads(fc->numOptimizerThreads());
  if (verbose >= 1) mxLog("%s: allocJacTool count=%d", name, getCount());
  if (verifyJac) mxLog("%s: constraint Jacobian verification enabled", name);
}

// This is called once before the compute plan begins execution
void ConstraintVec::markUselessConstraints(FitContext *fc)
{
  if (!count) return;

  Eigen::ArrayXd constr(count);
  Eigen::ArrayXXd ej(count, fc->getNumFree());
  ej.setConstant(NA_REAL);
  eval(fc, constr.data(), ej.data());

  //mxPrintMat("ej", ej);
  auto *state = fc->state;

	for (int j=0, cur=0, d1=0; j < int(state->conListX.size()); j++) {
		omxConstraint &con = *state->conListX[j];
    if (!cf(con)) continue;
    if (con.opCode != omxConstraint::EQUALITY) OOPS;
    for (int kk=0; kk < con.size; ++kk) {
      if ((ej.row(cur+kk) == 0.0).all()) {
        con.redundant[kk] = true;
        count -= 1;
        if (con.getVerbose()) {
          mxLog("Ignoring constraint '%s[%d]' because it does not depend "
                "on any free parameters", con.name, 1+kk);
        }
      }
      if (d1 < cur+kk) ej.row(d1) = ej.row(cur+kk);
      if (!con.redundant[kk]) d1 += 1;
    }
    cur += con.size;
    con.recalcSize();
  }

  if (count <= 1) return;

  // pad with dummy parameters so #constraints <= #param
  Eigen::MatrixXd tmp(std::max(count,ej.cols()), count);
  tmp.setZero();
  tmp.block(0,0,ej.cols(),count) = ej.block(0,0,count,ej.cols()).transpose();
  Eigen::FullPivHouseholderQR<Eigen::MatrixXd> qrOp(tmp);
  Eigen::ArrayXi perm = qrOp.colsPermutation().indices();
  Eigen::MatrixXd qr = qrOp.matrixQR();
  double thr = qrOp.maxPivot() * qrOp.threshold();

  for (int j=0, cur=0; j < int(state->conListX.size()); j++) {
    omxConstraint &con = *state->conListX[j];
    if (!cf(con)) continue;
    if (con.opCode != omxConstraint::EQUALITY) OOPS;
    for (int kk=0, dx=0; kk < int(con.redundant.size()); ++kk) {
      if (con.redundant[kk]) continue;
      int xx = perm[cur+dx];
      if (abs(qr(xx,xx)) < thr) {
        con.redundant[kk] = true;
        if (con.getVerbose()) {
          mxLog("Ignoring constraint '%s[%d]' because it is redundant (|%f| < %f)",
                con.name, 1+kk, qr(xx,xx), thr);
        }
      }
      dx += 1;
    }
    cur += con.size;
    con.recalcSize();
  }
}

void ConstraintVec::eval(FitContext *fc, double *constrOut, double *jacOut)
{
	if (count==0) return;

  auto *state = fc->state;
  Eigen::Map< Eigen::ArrayXd > constr(constrOut, count);
  Eigen::Map< Eigen::ArrayXXd > constrJac(jacOut, count, fc->getNumFree());
  if (!constrOut && jacOut) mxThrow("Can't request jacOut without constrOut");
  if (!constrOut) OOPS;

	for (int j=0, cur=0; j < int(state->conListX.size()); j++) {
		omxConstraint &con = *state->conListX[j];
		if (!cf(con) || con.size == 0) continue;

    con.refreshAndGrab(fc, &constr(cur));

    if (jacOut) {
      auto &vars = fc->varGroup->vars;
      for (int kk=0, dx=0; kk < int(con.redundant.size()); ++kk) {
        if (con.redundant[kk]) continue;
        for (int vx=0, px=0; vx < int(vars.size()); ++vx) {
          if (fc->profiledOutZ[vx]) continue;
          constrJac(cur+dx, px++) = con.initialJac(kk, vars[vx]->id);
        }
        dx += 1;
      }

      con.analyticJac(fc, [&constrJac, cur](int r, int c, double val){
                            constrJac(cur+r,c) = val; });
    }
    if (con.opCode != omxConstraint::EQUALITY) {
      if (ineqAlwaysActive) {
        // For CSOLNP, inequality constraints are always active.
        // This is by design, since it's an interior-point algorithm.
      } else {
        // NPSOL/SLSQP require inactive inequality constraint to be held constant at zero:
        for (int cx=cur; cx < cur+con.size; ++cx) {
          constr[cx] = std::max(0.0, constr[cx]);
          if(jacOut && constr[cx] == 0) {
            /*The Jacobians of each inactive constraint are set to zero here;
              as their elements will be zero rather than NaN,
              the code in finiteDifferences.h will leave them alone:*/
            constrJac.row(cx).setZero();
          }
        }
      }
    }
		cur += con.size;
	}

	fc->incrComputeCount();

  if (jacOut) {
    Eigen::ArrayXXd analyticJac;
    if (verifyJac) {
      analyticJac = constrJac;
      constrJac.setConstant(NA_REAL);
    }

    allocJacTool(fc);
    (*jacTool)([&](double *myPars, int thrId, auto &result) {
                FitContext *fc2 = thrId >= 0? fc->childList[thrId] : fc;
                Eigen::Map< Eigen::VectorXd > Est(myPars, fc2->getNumFree());
                // Only 1 parameter is different so we could
                // update only that parameter instead of all
                // of them.
                fc2->setEstFromOptimizer(Est);
                this->eval(fc2, result.data(), 0);
              }, constr, [&fc](){ return fc->getCurrentFree(); }, true, constrJac);

    if (verifyJac) {
      for (int cx=0; cx < analyticJac.cols(); ++cx) {
        for (int rx=0; rx < analyticJac.rows(); ++rx) {
          if (std::isfinite(analyticJac(rx,cx))) continue;
          analyticJac(rx,cx) = constrJac(rx,cx);
        }
      }

      for (int rx=0; rx < constrJac.rows(); ++rx) {
        Eigen::ArrayXd diff = analyticJac.row(rx) - constrJac.row(rx);
        if (diff.abs().maxCoeff() < 1e-7) continue;
        std::string info = string_snprintf("%s[%d,]-true", name, rx);
        mxPrintMat(info.c_str(), diff);
      }
      // restore
      constrJac = analyticJac;
    }

    for (int j=0, cur=0; j < int(state->conListX.size()); j++) {
      omxConstraint &con = *state->conListX[j];
      if (!cf(con) || con.size == 0) continue;
      auto &vars = fc->varGroup->vars;
      for (int kk=0, dx=0; kk < int(con.redundant.size()); ++kk) {
        if (con.redundant[kk]) continue;
        if (constr[cur+dx] != 0.0 && !con.seenActive[kk]) {
          for (int vx=0, px=0; vx < int(vars.size()); ++vx) {
            if (fc->profiledOutZ[vx]) continue;
            if (constrJac(cur+dx, px) == 0.0) {
              con.initialJac(kk, vars[vx]->id) = 0;
              if (con.getVerbose() >= 2) {
                mxLog("Assuming Jacobian %s[%d,%s] is zero",
                      con.name, 1+kk, vars[vx]->name);
              }
            }
            px+=1;
          }
          con.seenActive[kk] = true;
        }
        dx += 1;
      }
      cur += con.size;
    }
  }
}

omxCheckpoint::omxCheckpoint() : wroteHeader(false), lastCheckpoint(0), lastIterations(0),
				 lastEvaluation(0),
				 timePerCheckpoint(0), iterPerCheckpoint(0), evalsPerCheckpoint(0), file(NULL)
{}

omxCheckpoint::~omxCheckpoint()
{
	if (file) fclose(file);
}

/* We need to re-design checkpointing when it is possible to run
   more than 1 optimization in parallel. */
void omxCheckpoint::omxWriteCheckpointHeader()
{
	if (wroteHeader) return;
	std::vector< omxFreeVar* > &vars = Global->findVarGroup(FREEVARGROUP_ALL)->vars;
	size_t numParam = vars.size();

	// New columns should use the OpenMx prefit to avoid clashing with
	// free parameter names.
	fprintf(file, "OpenMxContext\tOpenMxNumFree\tOpenMxEvals\titerations\ttimestamp");
	for(size_t j = 0; j < numParam; j++) {
		fprintf(file, "\t\"%s\"", vars[j]->name);
	}
	fprintf(file, "\tobjective\n");
	fflush(file);
	wroteHeader = true;
}

void omxCheckpoint::message(FitContext *fc, const char *msg)
{
	postfit(msg, fc, true);
}

void omxCheckpoint::postfit(const char *context, FitContext *fc, bool force)
{
	const int timeBufSize = 32;
	char timeBuf[timeBufSize];
	time_t now = time(NULL); // avoid checking unless we need it
	int curEval = fc->getGlobalComputeCount();

	bool doit = force;
	if ((timePerCheckpoint && timePerCheckpoint <= now - lastCheckpoint) ||
	    (iterPerCheckpoint && iterPerCheckpoint <= fc->iterations - lastIterations) ||
	    (evalsPerCheckpoint && evalsPerCheckpoint <= curEval - lastEvaluation)) {
		doit = true;
	}
	if (!doit) return;

#pragma omp critical
	{
		omxWriteCheckpointHeader();

		std::vector< omxFreeVar* > &vars = fc->varGroup->vars;
		struct tm *nowTime = localtime(&now);
		strftime(timeBuf, timeBufSize, "%b %d %Y %I:%M:%S %p", nowTime);
		fprintf(file, "%s\t%d\t%d\t%d\t%s", context, int(vars.size()), lastEvaluation, lastIterations, timeBuf);

		size_t lx=0;
		size_t numParam = Global->findVarGroup(FREEVARGROUP_ALL)->vars.size();
		for (size_t px=0; px < numParam; ++px) {
			if (lx < vars.size() && vars[lx]->id == (int)px) {
				fprintf(file, "\t%.10g", fc->est[lx]);
				++lx;
			} else {
				fprintf(file, "\tNA");
			}
		}
		fprintf(file, "\t%.10g\n", fc->getFit());
		fflush(file);
		lastCheckpoint = now;
		lastIterations = fc->iterations;
		lastEvaluation = curEval;
	}
}

const omxFreeVarLocation *omxFreeVar::getLocation(int matrix) const
{
	for (size_t lx=0; lx < locations.size(); lx++) {
		const omxFreeVarLocation &loc = locations[lx];
		if (loc.matrix == matrix) return &loc;
	}
	return NULL;
}

const omxFreeVarLocation *omxFreeVar::getLocation(omxMatrix *mat) const
{ return getLocation(~mat->matrixNumber); }

const omxFreeVarLocation *omxFreeVar::getOnlyOneLocation(int matrix, bool &moreThanOne) const
{
	moreThanOne = false;
	const omxFreeVarLocation *result = NULL;
	for (size_t lx=0; lx < locations.size(); lx++) {
		const omxFreeVarLocation &loc = locations[lx];
		if (loc.matrix == matrix) {
			if (result) { moreThanOne = true; return NULL; }
			result = &loc;
		}
	}
	return result;
}

const omxFreeVarLocation *omxFreeVar::getOnlyOneLocation(omxMatrix *mat, bool &moreThanOne) const
{ return getOnlyOneLocation(~mat->matrixNumber, moreThanOne); }

void omxFreeVar::markDirty(omxState *os)
{
	auto deps = getDeps();
	for (int dx=0; dx < deps.size(); ++dx) {
		int dep = deps[dx];
		if (dep < 0) {
			omxMarkDirty(os->matrixList[~dep]);
		} else {
			omxMarkDirty(os->algebraList[dep]);
		}
	}

	for (int lx=0; lx < int(locations.size()); ++lx) {
		omxMarkClean(os->matrixList[locations[lx].matrix]);
	}
}

void omxFreeVar::copyToState(omxState *os, double val)
{
	for(size_t l = 0; l < locations.size(); l++) {
		omxFreeVarLocation *loc = &locations[l];
		omxMatrix *matrix = os->matrixList[loc->matrix];
		int row = loc->row;
		int col = loc->col;
		omxSetMatrixElement(matrix, row, col, val);
		if (OMX_DEBUG) {
			mxLog("free var %s, matrix %s[%d, %d] = %.17f",
			      name, matrix->name(), row, col, val);
		}
	}
}

double omxFreeVar::getCurValue(omxState *os)
{
	omxFreeVarLocation &loc = locations[0];
	EigenMatrixAdaptor Emat(os->matrixList[loc.matrix]);
	return Emat(loc.row, loc.col);
}

OptEngine nameToGradOptEngine(const char *engineName)
{
  OptEngine engine;
	if (strEQ(engineName, "CSOLNP")) {
		engine = OptEngine_CSOLNP;
	} else if (strEQ(engineName, "SLSQP")) {
		engine = OptEngine_NLOPT;
	} else if (strEQ(engineName, "NPSOL")) {
#if HAS_NPSOL
		engine = OptEngine_NPSOL;
#else
		mxThrow("NPSOL is not available in this build. See ?omxGetNPSOL() to download this optimizer");
#endif
	} else if(strEQ(engineName, "SD")){
		engine = OptEngine_SD;
	} else {
		mxThrow("Gradient descent engine '%s' is not recognized", engineName);
	}
  return engine;
}
