/*
 * Copyright 2012-2017 Joshua Nathaniel Pritikin and contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef u_OMX_EXPECTATIONBA81_H_
#define u_OMX_EXPECTATIONBA81_H_

#include "omxExpectation.h"
#include "ba81quad.h"

enum expectation_type {
	EXPECTATION_AUGMENTED, // E-M
	EXPECTATION_OBSERVED,  // regular
};

template <typename T>
struct BA81Estep {
	void begin(ifaGroup *state);
	void addRow(class ifaGroup *state, int px, int thrId);
	void recordTable(class ifaGroup *state);
	bool hasEnd() { return true; }
};

template <typename T>
struct BA81LatentFixed {
	bool wantSummary() { return false; };
	void normalizeWeights(class ifaGroup *state, T extraData, int px, double weight, int thrid);
	void end(class ifaGroup *state, T extraData) {};
	bool hasEnd() { return false; }
};

template <typename T>
struct BA81LatentSummary {
	bool wantSummary() { return true; };
	void normalizeWeights(class ifaGroup *state, T extraData, int px, double weight, int thrId);
	void end(class ifaGroup *state, T extraData);
	bool hasEnd() { return true; }
};

class BA81Expect : public omxExpectation {
	typedef omxExpectation super;
 public:
	virtual ~BA81Expect();
	virtual void init() override;
  virtual void connectToData() override;
	virtual void compute(FitContext *fc, const char *what, const char *how) override;
	virtual void populateAttr(SEXP expectation) override;
	virtual omxMatrix *getComponent(const char*) override;

	class ifaGroup grp;
	int totalOutcomes() { return grp.totalOutcomes; }
	const double *itemSpec(int ix) { return grp.spec[ix]; }
	int numItems() { return grp.numItems(); }
	int getNumUnique() { return (int) grp.rowMap.size(); }
	int itemOutcomes(int ix) { return grp.itemOutcomes[ix]; }

	double LogLargestDouble;       // should be const but need constexpr
	double LargestDouble;          // should be const but need constexpr

	// data characteristics
	double freqSum;                // sum of rowFreq

	// quadrature related
	class ba81NormalQuad &getQuad() { return grp.quad; }

	// estimation related
	omxMatrix *itemParam;
	double *EitemParam;
	double SmallestPatternLik;
	bool expectedUsed;
	int ElatentVersion;

	omxMatrix *u_latentMeanOut;
	omxMatrix *u_latentCovOut;
	template <typename Tmean, typename Tcov>
	void getLatentDistribution(FitContext *fc, Eigen::MatrixBase<Tmean> &mean, Eigen::MatrixBase<Tcov> &cov);

	omxMatrix *estLatentMean;
	omxMatrix *estLatentCov;

	unsigned itemParamVersion;
	unsigned latentParamVersion;
	enum expectation_type type;
	int verbose;
	bool debugInternal;
	struct omxFitFunction *fit;  // weak pointer

	BA81Expect(omxState *st, int num) :
		super(st, num), grp(true)
	{
		grp.quad.setNumThreads(Global->numThreads);
	};
	const char *getLatentIncompatible(BA81Expect *other);

	void refreshPatternLikelihood(bool hasFreeLatent);
	virtual void invalidateCache() override;
};

template <typename Tmean, typename Tcov>
void BA81Expect::getLatentDistribution(FitContext *fc, Eigen::MatrixBase<Tmean> &mean, Eigen::MatrixBase<Tcov> &cov)
{
	int dim = grp.quad.abilities();
	mean.derived().resize(dim);
	if (!u_latentMeanOut) {
		mean.setZero();
	} else {
		omxRecompute(u_latentMeanOut, fc);
		memcpy(mean.derived().data(), u_latentMeanOut->data, sizeof(double) * dim);
	}

	cov.derived().resize(dim, dim);
	if (!u_latentCovOut) {
		cov.setIdentity();
	} else {
		omxRecompute(u_latentCovOut, fc);
		memcpy(cov.derived().data(), u_latentCovOut->data, sizeof(double) * dim * dim);
	}
}

extern const struct rpf *Grpf_model;
extern int Grpf_numModels;

void ba81RefreshQuadrature(omxExpectation* oo);

void ba81AggregateDistributions(std::vector<class omxExpectation *> &expectation,
				int *version, omxMatrix *meanMat, omxMatrix *covMat);

#endif
