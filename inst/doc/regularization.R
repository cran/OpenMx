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

manifests <- c(paste0('x',1:8), paste0('y',1:5))

set.seed(12)

sim.mod <- mxModel(
  "sim", type="RAM", manifestVars = manifests, latentVars = 'f1',
  mxPath(paste0('x',1:8), 'f1', values=c(0,0,0,0,0,.2,.5,.8),
         labels=paste0('c',1:8)),
  mxPath('f1', paste0('y',1:5), values=1),
  mxPath(paste0('x',1:8), arrows=2, connect = "unique.bivariate",
         values=rnorm(8*7/2, sd=.2)),
  mxPath(paste0('x',1:8), arrows=2, values=1),
  mxPath(paste0('y',1:5), arrows=2, values=1),
  mxPath('f1', arrows=2, values=1, free=FALSE),
  mxPath('one', manifests),
  mxPath('one', 'f1', free=FALSE))

dat.sim = mxGenerateData(sim.mod, nrows = 100)


## -----------------------------------------------------------------------------
run.mod <- mxModel(sim.mod, mxData(dat.sim, type="raw"))

fit <- mxRun(run.mod)
#summary(fit)
summary(fit)$parameters[1:8,]

## ----results='hide'-----------------------------------------------------------
regFit <- mxPenaltySearch(mxModel(
  fit, mxPenaltyLASSO(paste0('c',1:8),"lasso",lambda.step=1.2),
  mxMatrix('Full',1,1,free=TRUE,values=0, labels="lambda")))


## -----------------------------------------------------------------------------
round(regFit$output$gradient, 2)

## -----------------------------------------------------------------------------

detail <- regFit$compute$steps$PS$output$detail
table(detail$statusCode)


## -----------------------------------------------------------------------------

range(detail$lambda)
range(detail$EBIC)
best <- which(detail$EBIC == min(detail$EBIC))
detail[best, 'lambda']


## ----fig.width=5,fig.height=5-------------------------------------------------

library(reshape2)
library(ggplot2)

est <- detail[,c(paste0('c',1:8), 'lambda')]
ggplot(melt(est, id.vars = 'lambda')) +
  geom_line(aes(x=lambda, y=value, color=variable)) +
  geom_vline(aes(xintercept=coef(regFit)['lambda']),
             linetype="dashed", alpha=.5)


## -----------------------------------------------------------------------------
summary(regFit)$parameters[1:8,]

