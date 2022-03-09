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
data(HS.ability.data)

## -----------------------------------------------------------------------------
HS.ability.data$ageym <- HS.ability.data$agey*12 + HS.ability.data$agem
HS.ability.data$male <- as.numeric(HS.ability.data$Gender == 'Male')

# Specify variables
indicators <- c('visual','cubes','paper','flags','paperrev','flagssub',
                'general','paragrap','sentence','wordc','wordm')
covariates <- c("male","ageym","grade")
latents = c("g", covariates)

# Build the model
mimicModel <- mxModel(
  "MIMIC", type="RAM",
  manifestVars = indicators, latentVars = latents,

  # Set up exogenous predictors
  mxPath("one", covariates, labels=paste0('data.',covariates), free=FALSE),

  # Fix factor variance
  mxPath('g', arrows=2, free=FALSE, values=1),

  # Error variances:
  mxPath(from=c(indicators), arrows=2, free=TRUE, values=10),

  # Means (saturated means model):
  mxPath(from="one", to=indicators, values=rep(5, length(indicators))),

  # Loadings:
  mxPath(from="g", to=indicators, values=.5),

  # Covariate paths
  mxPath(covariates, "g", labels=covariates),

  # Data
  mxData(observed = HS.ability.data, type = "raw"))

# Get some good starting values for regularization. This
# saves 2-3 minutes on my laptop.
mimicModel <- mxRun(mimicModel)


## -----------------------------------------------------------------------------
mimicModel <- mxModel(
  mimicModel,
  mxMatrix('Full',1,1,free=TRUE,values=0,labels="lambda",name="hparam"),
  # Set scale to ML estimates for adaptive lasso
  mxPenaltyLASSO(what=covariates, name="LASSO",
                    scale = coef(mimicModel)[covariates],
                    lambda =  0, lambda.max =2, lambda.step=.04)
)

## -----------------------------------------------------------------------------

regMIMIC <- mxPenaltySearch(mimicModel)

detail <- regMIMIC$compute$steps$PS$output$detail

library(reshape2)
library(ggplot2)

est <- detail[,c(covariates, 'lambda')]
ggplot(melt(est, id.vars = 'lambda')) +
  geom_line(aes(x=lambda, y=value, color=variable)) +
  geom_vline(aes(xintercept=coef(regMIMIC)['lambda']),
             linetype="dashed", alpha=.5)


## -----------------------------------------------------------------------------

detail[detail$EBIC == min(detail$EBIC), covariates]


## -----------------------------------------------------------------------------

regMIMIC <- mxPenaltyZap(regMIMIC)
regMIMIC <- mxRun(regMIMIC)

summary(regMIMIC)


