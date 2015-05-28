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

#ifndef _NPSOLWRAP_H
#define _NPSOLWRAP_H

#include <exception>
#include <string>

#include "omxState.h"

#undef PROTECT_WITH_INDEX
#undef UNPROTECT

/* Functions for Export */
SEXP omxBackend(SEXP constraints, SEXP matList,
		SEXP varList, SEXP algList, SEXP expectList, SEXP computeList,
		SEXP data, SEXP intervalList, SEXP checkpointList, SEXP options);

SEXP omxCallAlgebra(SEXP matList, SEXP algNum, SEXP options);
SEXP findIdenticalRowsData(SEXP data, SEXP missing, SEXP defvars,
	SEXP skipMissingness, SEXP skipDefvars);

class omxManageProtectInsanity {
	PROTECT_INDEX initialpix;
 public:
	omxManageProtectInsanity() {
		R_ProtectWithIndex(R_NilValue, &initialpix);
		Rf_unprotect(1);
	}
	PROTECT_INDEX getDepth() {
		PROTECT_INDEX pix;
		R_ProtectWithIndex(R_NilValue, &pix);
		PROTECT_INDEX diff = pix - initialpix;
		Rf_unprotect(1);
		return diff;
	}
	~omxManageProtectInsanity() {
		Rf_unprotect(getDepth());
	}
};

typedef std::vector< std::pair<const char *, SEXP> > MxRListBase;
class MxRList : private MxRListBase {
 public:
	size_t size() const { return MxRListBase::size(); }
	SEXP asR();
	void add(const char *key, SEXP val) {
		Rf_protect(val);
		push_back(std::make_pair(key, val));
	};
};

class ScopedProtect {
	PROTECT_INDEX initialpix;
 public:
	ScopedProtect(SEXP &var, SEXP src) {
		R_ProtectWithIndex(R_NilValue, &initialpix);
		Rf_unprotect(1);
		Rf_protect(src);
		var = src;
	}
	~ScopedProtect() {
		PROTECT_INDEX pix;
		R_ProtectWithIndex(R_NilValue, &pix);
		PROTECT_INDEX diff = pix - initialpix;
		if (diff != 1) Rf_error("Depth %d != 1, ScopedProtect was nested", diff);
		Rf_unprotect(2);
	}
};

void string_to_try_Rf_error( const std::string& str) __attribute__ ((noreturn));

void exception_to_try_Rf_error( const std::exception& ex ) __attribute__ ((noreturn));

void getMatrixDims(SEXP r_theta, int *rows, int *cols);

void markAsDataFrame(SEXP list);

#endif // #define _NPSOLWRAP_H
