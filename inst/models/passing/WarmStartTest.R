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


require(OpenMx)

if (mxOption(NULL,"Default optimizer")!='NPSOL') stop("SKIP")

if (0) {
   mxOption(NULL,"Print level",20)
   mxOption(NULL,"Print file",1)
   mxOption(NULL,"Verify level",3)
}

set.seed(1611150)
x <- matrix(rnorm(1000))
colnames(x) <- "x"

m1 <- mxModel(
	"m1",
	mxData(observed=x,type="raw"),
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=0,labels="mu",name="Mu"),
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=1,labels="sigma2",name="Sigma",lbound=0),
	mxExpectationNormal(covariance="Sigma",means="Mu",dimnames=c("x")),
	mxFitFunctionML()
)
run1 <- mxRun(m1)

hess.ini <- matrix(-2*c(-1000/1,0,0,-1000/2),nrow=2,ncol=2)
ws <- chol(hess.ini)

plan <- mxComputeSequence(steps=list(
	mxComputeGradientDescent(engine="NPSOL",warmStart=ws),
	mxComputeNumericDeriv(analytic=FALSE),
	mxComputeStandardError(),
	mxComputeHessianQuality(),
	mxComputeReportDeriv(),
	mxComputeReportExpectation()
))

m2 <- mxModel(
	"m2",
	plan,
	mxData(observed=x,type="raw"),
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=0,labels="mu",name="Mu"),
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=1,labels="sigma2",name="Sigma",lbound=0),
	mxExpectationNormal(covariance="Sigma",means="Mu",dimnames=c("x")),
	mxFitFunctionML()
)

if (0) {
  m2 <- mxOption(m2,"Always Checkpoint","Yes")
  m2 <- mxOption(m2,"Checkpoint Units","evaluations")
  m2 <- mxOption(m2,"Checkpoint Count",1)
  m2 <- mxOption(m2, "Checkpoint Fullpath", "/dev/fd/2")
}

run2 <- mxRun(m2)

omxCheckTrue(run2$output$evaluations < run1$output$evaluations)

plan2 <- mxComputeSequence(steps=list(
	mxComputeGradientDescent(engine="NPSOL",warmStart=matrix(1,1,1)),
	mxComputeNumericDeriv(analytic=FALSE),
	mxComputeStandardError(),
	mxComputeHessianQuality(),
	mxComputeReportDeriv(),
	mxComputeReportExpectation()
))

m3 <- mxModel(
	"m3",
	plan2,
	mxData(observed=x,type="raw"),
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=-0.3,labels="mu",name="Mu"),
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=0.97,labels="sigma2",name="Sigma",lbound=0),
	mxExpectationNormal(covariance="Sigma",means="Mu",dimnames=c("x")),
	mxFitFunctionML()
)
omxCheckWarning(mxRun(m3),"MxComputeGradientDescent: warmStart size 1 does not match number of free parameters 2 (ignored)")


#Test "internal" warm start:
plan3 <- mxComputeSequence(steps=list(
	mxComputeOnce(from="m4.fitfunction",what="hessian"),
	mxComputeGradientDescent(engine="NPSOL"),
	mxComputeNumericDeriv(analytic=FALSE),
	mxComputeStandardError(),
	mxComputeHessianQuality(),
	mxComputeReportDeriv(),
	mxComputeReportExpectation()
))

m4 <- mxModel(
	"m4",
	mxModel(
		"sub",
		mxData(observed=x,type="raw"),
		mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=0,labels="mu",name="Mu"),
		mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=1,labels="sigma2",name="Sigma",lbound=0),
		mxExpectationNormal(covariance="Sigma",means="Mu",dimnames=c("x")),
		mxFitFunctionML()
	),
	plan3,
	mxMatrix(type="Full",nrow=1,ncol=1,free=T,values=1,labels="sigma2",name="Sigma",lbound=0),
	mxAlgebra(-2*rbind(
		cbind(-1000/Sigma, 0),
		cbind(0, -1000/(2*Sigma^2))
	), name="hess",dimnames=list(c("mu","sigma2"),c("mu","sigma2")) ),
	mxFitFunctionAlgebra(algebra="sub.fitfunction",hessian="hess",numObs=1000)
)

if (0) {
  m4 <- mxOption(m4,"Always Checkpoint","Yes")
  m4 <- mxOption(m4,"Checkpoint Units","evaluations")
  m4 <- mxOption(m4,"Checkpoint Count",1)
  m4 <- mxOption(m4, "Checkpoint Fullpath", "/dev/fd/2")
}

run4 <- mxRun(m4)

omxCheckTrue(run4$output$evaluations < run1$output$evaluations)

# There are two additional evaluations:
# 1 To initialize sub.fitfunction
# 2 to evaluate sub.fitfunction
omxCheckCloseEnough(run4$output$evaluations, run2$output$evaluations+2)
