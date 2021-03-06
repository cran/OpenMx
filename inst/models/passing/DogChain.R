library(OpenMx)

if(mxOption(NULL,"Default optimizer")=="CSOLNP"){stop("SKIP")}

m1 <- mxModel("dogChain",
              mxMatrix(name="link", nrow=4, ncol=1, free=TRUE, lbound=-1, values=.1),
              mxMatrix(name="dog", nrow=1, ncol=1, free=TRUE, values=-1), # violate constraint
              mxFitFunctionAlgebra("dog"),
              mxConstraint(dog > link[1,1] + link[2,1] + link[3,1] + link[4,1]))

m1sa <- mxModel(m1, mxComputeSimAnnealing(method='tsallis1996', control=list(stepsPerTemp=5)))
m1sa <- mxRun(m1sa)
omxCheckCloseEnough(m1sa$dog$values, -4, .4)
omxCheckCloseEnough(m1sa$link$values[,1], rep(-1, 4), .1)

m1 <- mxRun(m1)
omxCheckCloseEnough(m1$dog$values, -4, 1e-4)
omxCheckCloseEnough(m1$link$values[,1], rep(-1, 4), 1e-4)

m2 <- mxModel("bentDogChain",
              mxMatrix(name="link", nrow=4, ncol=1, free=TRUE, lbound=-1, values=.1),
              mxMatrix(name="dog", nrow=1, ncol=1, free=TRUE, values=-1), # violate constraint
              mxFitFunctionAlgebra("dog"),
              mxConstraint(dog > link[1,1] + link[2,1] + link[3,1] + link[4,1]),
	      mxConstraint(abs(link[1,1]) == link[2,1] * link[2,1]))
#m2 <- mxOption(m2, key="Major iterations", value=100)
m2 <- mxTryHard(m2)
print(summary(m2))
omxCheckCloseEnough(m2$dog$values, -4.0309, .031)
omxCheckCloseEnough(m2$link$values[,1], rep(-1.014, 4), .015)
