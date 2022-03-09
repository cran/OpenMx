## ---- include = FALSE---------------------------------------------------------
is_CRAN <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (!is_CRAN) {
   options(mc.cores = parallel::detectCores())
} else {
  knitr::opts_chunk$set(eval = FALSE)
  knitr::knit_hooks$set(evaluate.inline = function(x, envir) x)
}

## -----------------------------------------------------------------------------
library(OpenMx)
data(myFADataRaw)

dataRawOneFactor <- mxData( observed=myFADataRaw[,c("x1","x2","x3","x4","x5","x6")], type="raw" )

## -----------------------------------------------------------------------------
myFADataCov<-matrix(
	c(0.997, 0.642, 0.611, 0.672, 0.637, 0.677,
	  0.642, 1.025, 0.608, 0.668, 0.643, 0.676,
	  0.611, 0.608, 0.984, 0.633, 0.657, 0.626,
	  0.672, 0.668, 0.633, 1.003, 0.676, 0.665,
	  0.637, 0.643, 0.657, 0.676, 1.028, 0.654,
	  0.677, 0.676, 0.626, 0.665, 0.654, 1.020),
	nrow=6,
	dimnames=list(
		c("x1","x2","x3","x4","x5","x6"),
		c("x1","x2","x3","x4","x5","x6"))
)

myFADataMeans <- c(2.988, 3.011, 2.986, 3.053, 3.016, 3.010)
names(myFADataMeans) <- c("x1","x2","x3","x4","x5","x6")

dataCovOneFactor  <- mxData( observed=myFADataCov, type="cov", numObs=500,
                        mean=myFADataMeans )

## -----------------------------------------------------------------------------
dataRawTwoFactor <- mxData( observed=myFADataRaw[,c("x1","x2","x3","y1","y2","y3")], type="raw" )

## -----------------------------------------------------------------------------
twoFactorCov <- matrix(
      c(0.997, 0.642, 0.611, 0.342, 0.299, 0.337,
        0.642, 1.025, 0.608, 0.273, 0.282, 0.287,
        0.611, 0.608, 0.984, 0.286, 0.287, 0.264,
        0.342, 0.273, 0.286, 0.993, 0.472, 0.467,
        0.299, 0.282, 0.287, 0.472, 0.978, 0.507,
        0.337, 0.287, 0.264, 0.467, 0.507, 1.059),
      nrow=6,
      dimnames=list(
          c("x1", "x2", "x3", "y1", "y2", "y3"),
          c("x1", "x2", "x3", "y1", "y2", "y3")),
)

twoFactorMeans <- c(2.988, 3.011, 2.986, 2.955, 2.956, 2.967)
names(twoFactorMeans) <- c("x1", "x2", "x3", "y1", "y2", "y3")
# Prepare Data
# -----------------------------------------------------------------------------

dataCovTwoFactor  <- mxData( observed=twoFactorCov, type="cov", numObs=500, means=twoFactorMeans )

## -----------------------------------------------------------------------------
latentVars <- "F1"
manifestVars=c("x1","x2","x3","x4","x5","x6")

## -----------------------------------------------------------------------------
# latent variance
latVar       <- mxPath( from="F1", arrows=2,
                        free=TRUE, values=1, labels ="varF1" )

## -----------------------------------------------------------------------------
# residual variances
resVars      <- mxPath( from=c("x1","x2","x3","x4","x5","x6"), arrows=2,
                        free=TRUE, values=c(1,1,1,1,1,1),
                        labels=c("e1","e2","e3","e4","e5","e6") )

## -----------------------------------------------------------------------------
# factor loadings
facLoads     <- mxPath( from="F1", to=c("x1","x2","x3","x4","x5","x6"), arrows=1,
                        free=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE), values=c(1,1,1,1,1,1),
                        labels =c("l1","l2","l3","l4","l5","l6") )

## -----------------------------------------------------------------------------
# means
means        <- mxPath( from="one", to=c("x1","x2","x3","x4","x5","x6","F1"), arrows=1,
                        free=c(T,T,T,T,T,T,FALSE), values=c(1,1,1,1,1,1,0),
                        labels =c("meanx1","meanx2","meanx3","meanx4","meanx5","meanx6",NA) )

## -----------------------------------------------------------------------------
oneFactorPathModel <- mxModel("Common Factor Model Path Specification", type="RAM",
                        manifestVars=c("x1","x2","x3","x4","x5","x6"), latentVars="F1",
                        dataRawOneFactor, resVars, latVar, facLoads, means)


## -----------------------------------------------------------------------------
manifestVars <- c("x1","x2","x3","y1","y2","y3")
latentVars <- c("F1","F2")

# residual variances
resVars      <- mxPath( from=c("x1", "x2", "x3", "y1", "y2", "y3"), arrows=2,
                        free=TRUE, values=c(1,1,1,1,1,1),
                        labels=c("e1","e2","e3","e4","e5","e6") )
# means
means        <- mxPath( from="one", to=c("x1","x2","x3","y1","y2","y3","F1","F2"),
                        arrows=1,
                        free=c(T,T,T,T,T,T,F,F), values=c(1,1,1,1,1,1,0,0),
                        labels=c("meanx1","meanx2","meanx3",
                                 "meany1","meany2","meany3",NA,NA) )

## -----------------------------------------------------------------------------
# latent variances and covariance
latVars      <- mxPath( from=c("F1","F2"), arrows=2, connect="unique.pairs",
                        free=TRUE, values=c(1,.5,1), labels=c("varF1","cov","varF2") )


## -----------------------------------------------------------------------------
print(latVars)

## -----------------------------------------------------------------------------
# factor loadings for x variables
facLoadsX    <- mxPath( from="F1", to=c("x1","x2","x3"), arrows=1,
                        free=c(F,T,T), values=c(1,1,1), labels=c("l1","l2","l3") )
# factor loadings for y variables
facLoadsY    <- mxPath( from="F2", to=c("y1","y2","y3"), arrows=1,
                        free=c(F,T,T), values=c(1,1,1), labels=c("l4","l5","l6") )

## -----------------------------------------------------------------------------
twoFactorPathModel <- mxModel("Two Factor Model Path Specification", type="RAM",
                        manifestVars=c("x1", "x2", "x3", "y1", "y2", "y3"),
                        latentVars=c("F1","F2"),
                        dataRawTwoFactor, resVars, latVars, facLoadsX, facLoadsY, means)

## -----------------------------------------------------------------------------


# asymmetric paths
matrA        <- mxMatrix( type="Full", nrow=7, ncol=7,
                          free=  c(F,F,F,F,F,F,F,
                                   F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,F),
                          values=c(0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,0),
                          labels=c(NA,NA,NA,NA,NA,NA,"l1",
                                   NA,NA,NA,NA,NA,NA,"l2",
                                   NA,NA,NA,NA,NA,NA,"l3",
                                   NA,NA,NA,NA,NA,NA,"l4",
                                   NA,NA,NA,NA,NA,NA,"l5",
                                   NA,NA,NA,NA,NA,NA,"l6",
                                   NA,NA,NA,NA,NA,NA,NA),
                          byrow=TRUE, name="A" )

## -----------------------------------------------------------------------------
# symmetric paths
matrS        <- mxMatrix( type="Symm", nrow=7, ncol=7,
                          free=  c(T,F,F,F,F,F,F,
                                   F,T,F,F,F,F,F,
                                   F,F,T,F,F,F,F,
                                   F,F,F,T,F,F,F,
                                   F,F,F,F,T,F,F,
                                   F,F,F,F,F,T,F,
                                   F,F,F,F,F,F,T),
                          values=c(1,0,0,0,0,0,0,
                                   0,1,0,0,0,0,0,
                                   0,0,1,0,0,0,0,
                                   0,0,0,1,0,0,0,
                                   0,0,0,0,1,0,0,
                                   0,0,0,0,0,1,0,
                                   0,0,0,0,0,0,1),
                          labels=c("e1",NA,  NA,  NA,  NA,  NA,  NA,
                                   NA, "e2", NA,  NA,  NA,  NA,  NA,
                                   NA,  NA, "e3", NA,  NA,  NA,  NA,
                                   NA,  NA,  NA, "e4", NA,  NA,  NA,
                                   NA,  NA,  NA,  NA, "e5", NA,  NA,
                                   NA,  NA,  NA,  NA,  NA, "e6", NA,
                                   NA,  NA,  NA,  NA,  NA,  NA, "varF1"),
                          byrow=TRUE, name="S" )

## -----------------------------------------------------------------------------
# filter matrix
matrF        <- mxMatrix( type="Full", nrow=6, ncol=7,
                          free=FALSE,
                          values=c(1,0,0,0,0,0,0,
                                   0,1,0,0,0,0,0,
                                   0,0,1,0,0,0,0,
                                   0,0,0,1,0,0,0,
                                   0,0,0,0,1,0,0,
                                   0,0,0,0,0,1,0),
                          byrow=TRUE, name="F" )


## -----------------------------------------------------------------------------
# means
matrM        <- mxMatrix( type="Full", nrow=1, ncol=7,
                          free=c(T,T,T,T,T,T,F),
                          values=c(1,1,1,1,1,1,0),
                          labels=c("meanx1","meanx2","meanx3",
                                   "meanx4","meanx5","meanx6",NA),
                          name="M" )

## -----------------------------------------------------------------------------
exp          <- mxExpectationRAM("A","S","F","M",
                                  dimnames=c(manifestVars, latentVars))
funML        <- mxFitFunctionML()

## -----------------------------------------------------------------------------
oneFactorMatrixModel <- mxModel("Common Factor Model Matrix Specification",
                          dataRawOneFactor, matrA, matrS, matrF, matrM, exp, funML)

## -----------------------------------------------------------------------------
# asymmetric paths
matrA        <- mxMatrix( type="Full", nrow=8, ncol=8,
                          free=  c(F,F,F,F,F,F,F,F,
                                   F,F,F,F,F,F,T,F,
                                   F,F,F,F,F,F,T,F,
                                   F,F,F,F,F,F,F,F,
                                   F,F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,F,T,
                                   F,F,F,F,F,F,F,F,
                                   F,F,F,F,F,F,F,F),
                          values=c(0,0,0,0,0,0,1,0,
                                   0,0,0,0,0,0,1,0,
                                   0,0,0,0,0,0,1,0,
                                   0,0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,0,1,
                                   0,0,0,0,0,0,0,0,
                                   0,0,0,0,0,0,0,0),
                          labels=c(NA,NA,NA,NA,NA,NA,"l1",NA,
                                   NA,NA,NA,NA,NA,NA,"l2",NA,
                                   NA,NA,NA,NA,NA,NA,"l3",NA,
                                   NA,NA,NA,NA,NA,NA,NA,"l4",
                                   NA,NA,NA,NA,NA,NA,NA,"l5",
                                   NA,NA,NA,NA,NA,NA,NA,"l6",
                                   NA,NA,NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA,NA,NA),
                          byrow=TRUE, name="A" )

## -----------------------------------------------------------------------------
# symmetric paths
matrS        <- mxMatrix( type="Symm", nrow=8, ncol=8,
                          free=  c(T,F,F,F,F,F,F,F,
                                   F,T,F,F,F,F,F,F,
                                   F,F,T,F,F,F,F,F,
                                   F,F,F,T,F,F,F,F,
                                   F,F,F,F,T,F,F,F,
                                   F,F,F,F,F,T,F,F,
                                   F,F,F,F,F,F,T,T,
                                   F,F,F,F,F,F,T,T),
                          values=c(1,0,0,0,0,0,0,0,
                                   0,1,0,0,0,0,0,0,
                                   0,0,1,0,0,0,0,0,
                                   0,0,0,1,0,0,0,0,
                                   0,0,0,0,1,0,0,0,
                                   0,0,0,0,0,1,0,0,
                                   0,0,0,0,0,0,1,.5,
                                   0,0,0,0,0,0,.5,1),
                          labels=c("e1",NA,  NA,  NA,  NA,  NA,  NA,  NA,
                                   NA, "e2", NA,  NA,  NA,  NA,  NA,  NA,
                                   NA,  NA, "e3", NA,  NA,  NA,  NA,  NA,
                                   NA,  NA,  NA, "e4", NA,  NA,  NA,  NA,
                                   NA,  NA,  NA,  NA, "e5", NA,  NA,  NA,
                                   NA,  NA,  NA,  NA,  NA, "e6", NA,  NA,
                                   NA,  NA,  NA,  NA,  NA,  NA,"varF1","cov",
                                   NA,  NA,  NA,  NA,  NA,  NA,"cov","varF2"),
                          byrow=TRUE, name="S" )

## -----------------------------------------------------------------------------
matrF        <- mxMatrix( type="Full", nrow=6, ncol=8,
                          free=FALSE,
                          values=c(1,0,0,0,0,0,0,0,
                                   0,1,0,0,0,0,0,0,
                                   0,0,1,0,0,0,0,0,
                                   0,0,0,1,0,0,0,0,
                                   0,0,0,0,1,0,0,0,
                                   0,0,0,0,0,1,0,0),
                          byrow=TRUE, name="F" )
matrM        <- mxMatrix( type="Full", nrow=1, ncol=8,
                          free=c(T,T,T,T,T,T,F,F),
                          values=c(1,1,1,1,1,1,0,0),
                          labels=c("meanx1","meanx2","meanx3",
                                   "meanx4","meanx5","meanx6",NA,NA),
                          name="M" )

## -----------------------------------------------------------------------------
exp          <- mxExpectationRAM("A","S","F","M",
                                  dimnames=c(manifestVars, latentVars))
funML        <- mxFitFunctionML()

twoFactorMatrixModel <- mxModel("Two Factor Model Matrix Specification",
                          dataRawTwoFactor, matrA, matrS, matrF, matrM, exp, funML)

## -----------------------------------------------------------------------------
twoFactorMatrixFit <- mxRun(twoFactorMatrixModel)

## -----------------------------------------------------------------------------
summary(twoFactorMatrixFit)

## ----eval=FALSE---------------------------------------------------------------
#  output(twoFactorMatrixFit)

