 /*
 *  Copyright 2007-2016 The OpenMx Project
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

#include "omxFitFunction.h"
#include "omxGREMLfitfunction.h"
#include "omxGREMLExpectation.h"
#include "omxMatrix.h"
#include "omxAlgebra.h"
#include <Eigen/Core>
#include <Eigen/Cholesky>
#include <Eigen/Dense>

struct omxGREMLFitState { 
  //TODO(?): Some of these members might be redundant with what's stored in the FitContext, 
  //and could therefore be cut
  omxMatrix *y, *X, *cov, *invcov, *means, *origVdim_om;
  std::vector< omxMatrix* > dV;
  std::vector< const char* > dVnames;
  std::vector<int> indyAlg; //will keep track of which algebras don't get marked dirty after dropping cases
  std::vector<int> origdVdim;
  void dVupdate(FitContext *fc);
  void dVupdate_final();
  int dVlength, usingGREMLExpectation;
  double nll, REMLcorrection;
  Eigen::VectorXd gradient;
  Eigen::MatrixXd avgInfo; //the Average Information matrix
  FreeVarGroup *varGroup;
	std::vector<int> gradMap;
  void buildParamMap(FreeVarGroup *newVarGroup);
  omxMatrix *aug, *augGrad, *augHess;
  std::vector<int> dAugMap;
  double pullAugVal(int thing, int row, int col);
  void recomputeAug(int thing, FitContext *fc);
}; 


void omxInitGREMLFitFunction(omxFitFunction *oo){
  
  if(OMX_DEBUG) { mxLog("Initializing GREML fitfunction."); }
  SEXP rObj = oo->rObj;
  SEXP dV, dVnames, aug, augGrad, augHess;
  int i=0;
  
  oo->units = FIT_UNITS_MINUS2LL;
  oo->computeFun = omxCallGREMLFitFunction;
  oo->ciFun = loglikelihoodCIFun;
  oo->destructFun = omxDestroyGREMLFitFunction;
  oo->populateAttrFun = omxPopulateGREMLAttributes;
  
  omxGREMLFitState *newObj = new omxGREMLFitState;
  oo->argStruct = (void*)newObj;
  omxExpectation* expectation = oo->expectation;
  omxState* currentState = expectation->currentState;
  newObj->usingGREMLExpectation = (strcmp(expectation->expType, "MxExpectationGREML")==0 ? 1 : 0);
  if(!newObj->usingGREMLExpectation){
    //Maybe someday GREML fitfunction could be made compatible with another expectation, but not at present:
    Rf_error("GREML fitfunction is currently only compatible with GREML expectation");
  }
  else{
    omxGREMLExpectation* oge = (omxGREMLExpectation*)(expectation->argStruct);
    oge->alwaysComputeMeans = 0;
  }

  newObj->y = omxGetExpectationComponent(expectation, "y");
  newObj->cov = omxGetExpectationComponent(expectation, "cov");
  newObj->invcov = omxGetExpectationComponent(expectation, "invcov");
  newObj->X = omxGetExpectationComponent(expectation, "X");
  newObj->means = omxGetExpectationComponent(expectation, "means");
  newObj->origVdim_om = omxGetExpectationComponent(expectation, "origVdim_om");
  newObj->nll = 0;
  newObj->REMLcorrection = 0;
  newObj->varGroup = NULL;
  newObj->augGrad = NULL;
  newObj->augHess = NULL;
  
  //Augmentation:
  {
  ScopedProtect p1(aug, R_do_slot(rObj, Rf_install("aug")));
  if(Rf_length(aug)){
  	int* augint = INTEGER(aug);
  	newObj->aug = omxMatrixLookupFromStateByNumber(augint[0], currentState);
  }
  else{newObj->aug = NULL;}
  }
  
  //Derivatives of V:
  {
  ScopedProtect p1(dV, R_do_slot(rObj, Rf_install("dV")));
  ScopedProtect p2(dVnames, R_do_slot(rObj, Rf_install("dVnames")));
  newObj->dVlength = Rf_length(dV);  
  newObj->dV.resize(newObj->dVlength);
  newObj->indyAlg.resize(newObj->dVlength);
  newObj->dVnames.resize(newObj->dVlength);
  newObj->origdVdim.resize(newObj->dVlength);
	if(newObj->dVlength){
    if(!newObj->usingGREMLExpectation){
      //Probably best not to allow use of dV if we aren't sure means will be calculated GREML-GLS way:
      Rf_error("derivatives of 'V' matrix in GREML fitfunction only compatible with GREML expectation");
    }
    if(OMX_DEBUG) { mxLog("Processing derivatives of V."); }
		int* dVint = INTEGER(dV);
    for(i=0; i < newObj->dVlength; i++){
      newObj->dV[i] = omxMatrixLookupFromStateByNumber(dVint[i], currentState);
      SEXP elem;
      {ScopedProtect p3(elem, STRING_ELT(dVnames, i));
			newObj->dVnames[i] = CHAR(elem);}
	}}
  }
  
  if(newObj->dVlength){
    oo->gradientAvailable = true;
    newObj->gradient.setZero(newObj->dVlength,1);
    oo->hessianAvailable = true;
    newObj->avgInfo.setZero(newObj->dVlength,newObj->dVlength);
    for(i=0; i < newObj->dVlength; i++){
    	/*Each dV must either (1) match the dimensions of V, OR (2) match the length of y if that is less than the 
    	dimension of V (implying downsizing due to missing observations):*/
      if( ((newObj->dV[i]->rows == newObj->cov->rows)&&(newObj->dV[i]->cols == newObj->cov->cols)) ||
          ((newObj->y->cols < newObj->cov->rows)&&(newObj->dV[i]->rows == newObj->y->cols)&&
          	(newObj->dV[i]->cols == newObj->y->cols)) ){
      	newObj->origdVdim[i] = newObj->dV[i]->rows;
      }
      else{
        Rf_error("all derivatives of V must have the same dimensions as V");
  }}}
  
  //Augmentation derivatives:
	if(newObj->dVlength && newObj->aug){
	//^^^Ignore derivatives of aug unless aug itself and objective derivatives are supplied.	
		ScopedProtect p1(augGrad, R_do_slot(rObj, Rf_install("augGrad")));
		ScopedProtect p2(augHess, R_do_slot(rObj, Rf_install("augHess")));
		if(!Rf_length(augGrad)){
			if(Rf_length(augHess)){
				Rf_error("if argument 'augHess' has nonzero length, then argument 'augGrad' must as well");
			}
			else{
				Rf_error("if arguments 'dV' and 'aug' have nonzero length, then 'augGrad' must as well");
		}}
		else{
			int* augGradint = INTEGER(augGrad);
			newObj->augGrad = omxMatrixLookupFromStateByNumber(augGradint[0], currentState);
			if(Rf_length(augHess)){
				int* augHessint = INTEGER(augHess);
				newObj->augHess = omxMatrixLookupFromStateByNumber(augHessint[0], currentState);
			}
			else{oo->hessianAvailable = false;}
		}
	}
}



void omxCallGREMLFitFunction(omxFitFunction *oo, int want, FitContext *fc){
  if (want & (FF_COMPUTE_PREOPTIMIZE)) return;
  
  //Recompute Expectation:
  omxExpectation* expectation = oo->expectation;
  omxExpectationCompute(fc, expectation, NULL);
    
  omxGREMLFitState *gff = (omxGREMLFitState*)oo->argStruct; //<--Cast generic omxFitFunction to omxGREMLFitState
  
  //Ensure that the pointer in the GREML fitfunction is directed at the right FreeVarGroup
  //(not necessary for most compute plans):
  if(fc && gff->varGroup != fc->varGroup){
    gff->buildParamMap(fc->varGroup);
	}
  
  gff->recomputeAug(0, fc);
  
  //Declare local variables used in more than one scope in this function:
  const double Scale = fabs(Global->llScale); //<--absolute value of loglikelihood scale
  const double NATLOG_2PI = 1.837877066409345483560659472811;	//<--log(2*pi)
  int i;
  Eigen::Map< Eigen::MatrixXd > Eigy(omxMatrixDataColumnMajor(gff->y), gff->y->cols, 1);
  Eigen::Map< Eigen::MatrixXd > Vinv(omxMatrixDataColumnMajor(gff->invcov), gff->invcov->rows, gff->invcov->cols);
  EigenMatrixAdaptor EigX(gff->X);
  Eigen::MatrixXd P, Py;
  P.setZero(gff->invcov->rows, gff->invcov->cols);
  double logdetV=0, logdetquadX=0, ytPy=0;
  
  if(want & (FF_COMPUTE_FIT | FF_COMPUTE_GRADIENT | FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){
    if(gff->usingGREMLExpectation){
      omxGREMLExpectation* oge = (omxGREMLExpectation*)(expectation->argStruct);
      
      //Check that factorizations of V and the quadratic form in X succeeded:
      if(oge->cholV_fail_om->data[0]){
        oo->matrix->data[0] = NA_REAL;
        if (fc) fc->recordIterationError("expected covariance matrix is non-positive-definite");
        return;
      }
      if(oge->cholquadX_fail){
        oo->matrix->data[0] = NA_REAL;
        if (fc) fc->recordIterationError("Cholesky factorization failed; possibly, the matrix of covariates is rank-deficient");
        return;
      }
      
      //Log determinant of V:
      logdetV = oge->logdetV_om->data[0];
      
      //Log determinant of quadX:
      for(i=0; i < gff->X->cols; i++){
        logdetquadX += log(oge->cholquadX_vectorD[i]);
      }
      logdetquadX *= 2;
      gff->REMLcorrection = Scale*0.5*logdetquadX;
      
      //Finish computing fit (negative loglikelihood):
      P.triangularView<Eigen::Lower>() = (Vinv.selfadjointView<Eigen::Lower>() * //P = Vinv * (I-Hatmat)
        (Eigen::MatrixXd::Identity(Vinv.rows(), Vinv.cols()) - 
          (EigX * oge->quadXinv.selfadjointView<Eigen::Lower>() * oge->XtVinv))).triangularView<Eigen::Lower>();
      Py = P.selfadjointView<Eigen::Lower>() * Eigy;
      ytPy = (Eigy.transpose() * Py)(0,0);
      if(OMX_DEBUG) {mxLog("ytPy is %3.3f",ytPy);}
      oo->matrix->data[0] = gff->REMLcorrection + 
      	Scale*0.5*( (((double)gff->y->cols) * NATLOG_2PI) + logdetV + ytPy) + Scale*gff->pullAugVal(0L,0,0);
      gff->nll = oo->matrix->data[0];
      if(OMX_DEBUG){mxLog("augmentation is %3.3f",gff->pullAugVal(0L,0,0));}
    }
    else{ //If not using GREML expectation, deal with means and cov in a general way to compute fit...
      //Declare locals:
      EigenMatrixAdaptor yhat(gff->means);
      EigenMatrixAdaptor EigV(gff->cov);
      double logdetV=0, logdetquadX=0;
      Eigen::MatrixXd Vinv, quadX;
      Eigen::LLT< Eigen::MatrixXd > cholV(gff->cov->rows);
      Eigen::LLT< Eigen::MatrixXd > cholquadX(gff->X->cols);
      Eigen::VectorXd cholV_vectorD, cholquadX_vectorD;
      
      //Cholesky factorization of V:
      cholV.compute(EigV);
      if(cholV.info() != Eigen::Success){
        omxRaiseErrorf("expected covariance matrix is non-positive-definite");
        oo->matrix->data[0] = NA_REAL;
        return;
      }
      //Log determinant of V:
      cholV_vectorD = (( Eigen::MatrixXd )(cholV.matrixL())).diagonal();
      for(i=0; i < gff->X->rows; i++){
        logdetV += log(cholV_vectorD[i]);
      }
      logdetV *= 2;
      
      Vinv = cholV.solve(Eigen::MatrixXd::Identity( EigV.rows(), EigV.cols() )); //<-- V inverse
      
      quadX = EigX.transpose() * Vinv * EigX; //<--Quadratic form in X
      
      cholquadX.compute(quadX); //<--Cholesky factorization of quadX
      if(cholquadX.info() != Eigen::Success){
        omxRaiseErrorf("Cholesky factorization failed; possibly, the matrix of covariates is rank-deficient");
        oo->matrix->data[0] = NA_REAL;
        return;
      }
      cholquadX_vectorD = (( Eigen::MatrixXd )(cholquadX.matrixL())).diagonal();
      for(i=0; i < gff->X->cols; i++){
        logdetquadX += log(cholquadX_vectorD[i]);
      }
      logdetquadX *= 2;
      gff->REMLcorrection = Scale*0.5*logdetquadX;
      
      //Finish computing fit:
      oo->matrix->data[0] = gff->REMLcorrection + Scale*0.5*( ((double)gff->y->rows * NATLOG_2PI) + logdetV + 
        ( Eigy.transpose() * Vinv * (Eigy - yhat) )(0,0));
      gff->nll = oo->matrix->data[0]; 
      return;
    }
  }
  
  if(want & (FF_COMPUTE_GRADIENT | FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){
    //This part requires GREML expectation:
    omxGREMLExpectation* oge = (omxGREMLExpectation*)(expectation->argStruct);
  	
  	//Recompute derivatives:
  	gff->dVupdate(fc);
  	gff->recomputeAug(1, fc);
    
    //Declare local variables for this scope:
    int nThreadz = Global->numThreads;
    
    fc->grad.resize(gff->dVlength); //<--Resize gradient in FitContext
    
    //Set up new HessianBlock:
    HessianBlock *hb = new HessianBlock;
    if(want & (FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){
      hb->vars.resize(gff->dVlength);
      hb->mat.resize(gff->dVlength, gff->dVlength);
      gff->recomputeAug(2, fc);
    }
    
    //Begin looping thru free parameters:
#pragma omp parallel num_threads(nThreadz)
{
		int i=0, j=0, t1=0, t2=0, a1=0, a2=0;
		Eigen::MatrixXd PdV_dtheta1;
		Eigen::MatrixXd dV_dtheta1(Eigy.rows(), Eigy.rows()); //<--Derivative of V w/r/t parameter i.
		Eigen::MatrixXd dV_dtheta2(Eigy.rows(), Eigy.rows()); //<--Derivative of V w/r/t parameter j.
		int threadID = omx_absolute_thread_num();
		int istart = threadID * gff->dVlength / nThreadz;
		int iend = (threadID+1) * gff->dVlength / nThreadz;
		if(threadID == nThreadz-1){iend = gff->dVlength;}
		for(i=istart; i < iend; i++){
			t1 = gff->gradMap[i]; //<--Parameter number for parameter i.
			if(t1 < 0){continue;}
			a1 = gff->dAugMap[i]; //<--Index of augmentation derivatives to use for parameter i.
			if(want & (FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){hb->vars[i] = t1;}
			if( oge->numcases2drop && (gff->dV[i]->rows > Eigy.rows()) ){
				dropCasesAndEigenize(gff->dV[i], dV_dtheta1, oge->numcases2drop, oge->dropcase, 1, gff->origdVdim[i]);
			}
			else{dV_dtheta1 = Eigen::Map< Eigen::MatrixXd >(omxMatrixDataColumnMajor(gff->dV[i]), gff->dV[i]->rows, gff->dV[i]->cols);}
			//PdV_dtheta1 = P.selfadjointView<Eigen::Lower>() * dV_dtheta1.selfadjointView<Eigen::Lower>();
			PdV_dtheta1 = P.selfadjointView<Eigen::Lower>();
			PdV_dtheta1 = PdV_dtheta1 * dV_dtheta1.selfadjointView<Eigen::Lower>();
			for(j=i; j < gff->dVlength; j++){
				if(j==i){
					gff->gradient(t1) = Scale*0.5*(PdV_dtheta1.trace() - (Eigy.transpose() * PdV_dtheta1 * Py)(0,0)) + 
						Scale*gff->pullAugVal(1,a1,0);
					fc->grad(t1) += gff->gradient(t1);
					if(want & (FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){
						gff->avgInfo(t1,t1) = Scale*0.5*(Eigy.transpose() * PdV_dtheta1 * PdV_dtheta1 * Py)(0,0) + 
							Scale*gff->pullAugVal(2,a1,a1);
					}
				}
				else{if(want & (FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){
					t2 = gff->gradMap[j]; //<--Parameter number for parameter j.
					if(t2 < 0){continue;}
					a2 = gff->dAugMap[j]; //<--Index of augmentation derivatives to use for parameter j.
					if( oge->numcases2drop && (gff->dV[j]->rows > Eigy.rows()) ){
						dropCasesAndEigenize(gff->dV[j], dV_dtheta2, oge->numcases2drop, oge->dropcase, 1, gff->origdVdim[j]);
					}
					else{dV_dtheta2 = Eigen::Map< Eigen::MatrixXd >(omxMatrixDataColumnMajor(gff->dV[j]), gff->dV[j]->rows, gff->dV[j]->cols);}
					gff->avgInfo(t1,t2) = Scale*0.5*(Eigy.transpose() * PdV_dtheta1 * P.selfadjointView<Eigen::Lower>() * 
						dV_dtheta2.selfadjointView<Eigen::Lower>() * Py)(0,0) + Scale*gff->pullAugVal(2,a1,a2);
					gff->avgInfo(t2,t1) = gff->avgInfo(t1,t2);
				}}}}
}
    //Assign upper triangle elements of avgInfo to the HessianBlock:
    if(want & (FF_COMPUTE_HESSIAN | FF_COMPUTE_IHESSIAN)){
      for (size_t d1=0, h1=0; h1 < gff->dV.size(); ++h1) {
		    for (size_t d2=0, h2=0; h2 <= h1; ++h2) {
				  	hb->mat(d2,d1) = gff->avgInfo(h2,h1);
				    ++d2;
        }
			  ++d1;	
		  }
		  fc->queue(hb);
  }}
  return;
}



void omxDestroyGREMLFitFunction(omxFitFunction *oo){
  if(OMX_DEBUG) {mxLog("Freeing GREML FitFunction.");}
    if(oo->argStruct == NULL) return;
    omxGREMLFitState* owo = ((omxGREMLFitState*)oo->argStruct);
    delete owo;
}


static void omxPopulateGREMLAttributes(omxFitFunction *oo, SEXP algebra){
	omxGREMLFitState *gff = (omxGREMLFitState*)oo->argStruct;
	gff->dVupdate_final();
  if(OMX_DEBUG) { mxLog("Populating GREML Attributes."); }
  SEXP rObj = oo->rObj;
  SEXP nval, mlfitval;
  int userSuppliedDataNumObs = (int)(( (omxGREMLExpectation*)(oo->expectation->argStruct) )->data2->numObs);
  
  //Tell the frontend fitfunction counterpart how many observations there are...:
  {
  //ScopedProtect p1(nval, R_do_slot(rObj, Rf_install("numObs")));
  ScopedProtect p1(nval, Rf_allocVector(INTSXP, 1));
  INTEGER(nval)[0] = 1L - userSuppliedDataNumObs;
  R_do_slot_assign(rObj, Rf_install("numObs"), nval);
  /*^^^^The end result is that number of observations will be reported as 1 in summary()...
  which is always correct with GREML.  This is a bit of a hack, since it is sneaking this
  negative numObs into the pre-backend fitfunction that summary() looks at...*/
	}
	
	{
	//ScopedProtect p1(mlfitval, R_do_slot(rObj, Rf_install("MLfit")));
	ScopedProtect p1(mlfitval, Rf_allocVector(REALSXP, 1));
	REAL(mlfitval)[0] = gff->nll - gff->REMLcorrection;
	Rf_setAttrib(algebra, Rf_install("MLfit"), mlfitval);
	}
}


void omxGREMLFitState::buildParamMap(FreeVarGroup *newVarGroup)
{
	if(OMX_DEBUG) { mxLog("Building parameter map for GREML fitfunction."); }
	varGroup = newVarGroup;
	if(dVlength){
		std::vector< omxMatrix* > dV_temp = dV;
		std::vector< const char* > dVnames_temp = dVnames;
		std::vector<int> origdVdim_temp = origdVdim;
		gradMap.resize(dVlength);
		dAugMap.resize(dVlength);
		int gx=0;
		for (int vx=0; vx < int(varGroup->vars.size()); ++vx) {
			for (int nx=0; nx < dVlength; ++nx) {
				if (strEQ(dVnames_temp[nx], varGroup->vars[vx]->name)) {
					gradMap[gx] = vx;
					dV[gx] = dV_temp[nx];
					dVnames[gx] = dVnames_temp[nx]; //<--Probably not strictly necessary...
					origdVdim[gx] = origdVdim_temp[nx];
					dAugMap[gx] = nx;
					indyAlg[gx] = ( dV_temp[nx]->algebra && !(dV_temp[nx]->dependsOnParameters()) ) ? 1 : 0;
					++gx;
					break;
				}
			}
		}
		if (gx != dVlength) Rf_error("Problem in dVnames mapping"); //possibly, argument 'dV' has elements not named with free parameter labels
		if( gx < int(varGroup->vars.size()) ){Rf_error("At least one free parameter has no corresponding element in 'dV'");}
		
		if(augGrad){
			int ngradelem = std::max(augGrad->rows, augGrad->cols);
			if(ngradelem != dVlength){
				Rf_error("matrix referenced by 'augGrad' must have same number of elements as argument 'dV'");
			}
			if(augHess){
				if (augHess->rows != augHess->cols) {
					Rf_error("matrix referenced by 'augHess' must be square (instead of %dx%d)",
              augHess->rows, augHess->cols);
				}
				if(augHess->rows != ngradelem){
					Rf_error("Augmentation derivatives non-conformable (gradient is size %d and Hessian is %dx%d)",
              ngradelem, augHess->rows, augHess->cols);
			}}
		}
	}
}


double omxGREMLFitState::pullAugVal(int thing, int row, int col){
	double val=0;
	switch(thing){
	case 0:
		if(aug){val = aug->data[0];}
		break;
	case 1:
		if(augGrad){val = augGrad->data[row+col];} //<--Remember that at least one of 'row' and 'col' should be 0.
		break;
	case 2:
		if(augHess){val = omxMatrixElement(augHess,row,col);}
		break;
	}
	return(val);
}


void omxGREMLFitState::recomputeAug(int thing, FitContext *fc){
	switch(thing){
	case 0:
		if(aug){omxRecompute(aug, fc);}
		break;
	case 1:
		if(augGrad){omxRecompute(augGrad, fc);} 
		break;
	case 2:
		if(augHess){omxRecompute(augHess, fc);}
		break;
	}
}


void omxGREMLFitState::dVupdate(FitContext *fc){
	for(int i=0; i < dVlength; i++){
		if(OMX_DEBUG){
			mxLog("dV %d has matrix number? %s", i, dV[i]->hasMatrixNumber ? "True." : "False." );
			mxLog("dV %d is clean? %s", i, omxMatrixIsClean(dV[i]) ? "True." : "False." );
		}
		//Recompute if needs update and if NOT a parameter-independent algebra:
		if( omxNeedsUpdate(dV[i]) && !(indyAlg[i]) ){
			if(OMX_DEBUG){
				mxLog("Recomputing dV %d, %s %s", i, dV[i]->getType(), dV[i]->name());
			}
			omxRecompute(dV[i], fc);
		}
	}
}


void omxGREMLFitState::dVupdate_final(){
	for(int i=0; i < dVlength; i++){
		if(indyAlg[i]){
			if(OMX_DEBUG){
				mxLog("dV %d has matrix number? %s", i, dV[i]->hasMatrixNumber ? "True." : "False." );
				mxLog("dV %d is clean? %s", i, omxMatrixIsClean(dV[i]) ? "True." : "False." );
			}
			if( omxNeedsUpdate(dV[i]) ){
				if(OMX_DEBUG){
					mxLog("Recomputing dV %d, %s %s", i, dV[i]->getType(), dV[i]->name());
				}
				omxRecompute(dV[i], NULL);
			}
		}
	}
}


