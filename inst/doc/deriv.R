## ---- include = FALSE---------------------------------------------------------
is_CRAN <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (!is_CRAN) {
   options(mc.cores = parallel::detectCores())
} else {
  knitr::opts_chunk$set(eval = FALSE)
  knitr::knit_hooks$set(evaluate.inline = function(x, envir) x)
}

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(OpenMx)

aPlan <- mxComputeSequence(list(  #analytic
  mxComputeOnce('fitfunction', c('gradient')),
  mxComputeReportDeriv()))

nPlan <- mxComputeSequence(list(  #numerical
  mxComputeNumericDeriv(analytic = FALSE, hessian=FALSE, checkGradient = FALSE),
  mxComputeReportDeriv()))

## -----------------------------------------------------------------------------
mat1 <- mxMatrix("Full", rnorm(1), free=TRUE, nrow=1, ncol=1, labels="m1", name="mat1")
obj <- mxAlgebra(-.5 * (log(2*pi) + log(2) + (mat1[1,1])^2/2), name = "obj")
grad <- mxAlgebra(-(mat1[1,1]) + 2, name = "grad", dimnames=list("m1", NULL))
mv1 <- mxModel("mv1", mat1, obj, grad,
                  mxFitFunctionAlgebra("obj", gradient="grad"))


## -----------------------------------------------------------------------------
nu <- mxRun(mxModel(mv1, nPlan), silent = TRUE)
an <- mxRun(mxModel(mv1, aPlan), silent = TRUE)

cbind(numerical=nu$output$gradient, analytic=an$output$gradient)


## -----------------------------------------------------------------------------
p1 <- runif(2, -10,10)
mv1 <- omxSetParameters(mv1, labels = 'm1', values=p1)

nu <- mxRun(mxModel(mv1, nPlan), silent = TRUE)
an <- mxRun(mxModel(mv1, aPlan), silent = TRUE)

cbind(numerical=nu$output$gradient, analytic=an$output$gradient)


## -----------------------------------------------------------------------------

grad <- mxAlgebra(-(mat1[1,1])/2, name = "grad", dimnames=list("m1", NULL))
mv2 <- mxModel(mv1, grad)


## -----------------------------------------------------------------------------
nu <- mxRun(mxModel(mv2, nPlan), silent = TRUE)
an <- mxRun(mxModel(mv2, aPlan), silent = TRUE)

cbind(numerical=nu$output$gradient, analytic=an$output$gradient)


## -----------------------------------------------------------------------------
mv2 <- omxSetParameters(mv2, labels = 'm1', values=rnorm(1))

nu <- mxRun(mxModel(mv2, nPlan), silent = TRUE)
an <- mxRun(mxModel(mv2, aPlan), silent = TRUE)

cbind(numerical=nu$output$gradient, analytic=an$output$gradient)

