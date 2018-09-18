#include "omxSymbolTable.h"
#include "AlgebraFunctions.h"
const omxAlgebraTableEntry omxAlgebraSymbolTable[] = {
{ 0,	"*SPECIAL*",	"*NONE*",	 0,	NULL,	NULL},
{ 1,	"Inversion",	"solve",	 1,	omxMatrixInvertCheck,	omxMatrixInvert},
{ 2,	"Transposition",	"t",	 1,	omxMatrixTransposeCheck,	omxMatrixTranspose},
{ 3,	"Element powering",	"^",	 2,	omxElementPowerCheck,	omxElementPower},
{ 4,	"Multiplication",	"%*%",	 2,	omxMatrixMultCheck,	omxMatrixMult},
{ 5,	"Dot product",	"*",	 2,	omxMatrixElementMultCheck,	omxMatrixElementMult},
{ 6,	"Kronecker product",	"%x%",	 2,	omxKroneckerProdCheck,	omxKroneckerProd},
{ 7,	"Quadratic product",	"%&%",	 2,	omxQuadraticProdCheck,	omxQuadraticProd},
{ 8,	"Element division",	"/",	 2,	omxElementDivideCheck,	omxElementDivide},
{ 9,	"Addition",	"+",	 2,	omxMatrixAddCheck,	omxMatrixAdd},
{10,	"Subtraction (binary)",	"-",	 2,	omxMatrixSubtractCheck,	omxMatrixSubtract},
{11,	"Subtraction (unary)",	"-",	 1,	omxUnaryMinusCheck,	omxUnaryMinus},
{12,	"Horizontal adhesion",	"cbind",	-1,	omxMatrixHorizCatOpCheck,	omxMatrixHorizCatOp},
{13,	"Vertical adhesion",	"rbind",	-1,	omxMatrixVertCatOpCheck,	omxMatrixVertCatOp},
{14,	"Determinant",	"det",	 1,	omxMatrixDeterminantCheck,	omxMatrixDeterminant},
{15,	"Trace",	"tr",	 1,	omxMatrixTraceOpCheck,	omxMatrixTraceOp},
{16,	"Sum",	"sum",	-1,	omxMatrixTotalSumCheck,	omxMatrixTotalSum},
{17,	"Product",	"prod",	-1,	omxMatrixTotalProductCheck,	omxMatrixTotalProduct},
{18,	"Maximum",	"max",	-1,	omxMatrixMaximumCheck,	omxMatrixMaximum},
{19,	"Minimum",	"min",	-1,	omxMatrixMinimumCheck,	omxMatrixMinimum},
{20,	"Absolute value",	"abs",	 1,	omxMatrixAbsoluteCheck,	omxMatrixAbsolute},
{21,	"Cosine",	"cos",	 1,	omxElementCosineCheck,	omxElementCosine},
{22,	"Hyperbolic cosine",	"cosh",	 1,	omxElementCoshCheck,	omxElementCosh},
{23,	"Sine",	"sin",	 1,	omxElementSineCheck,	omxElementSine},
{24,	"Hyperbolic sine",	"sinh",	 1,	omxElementSinhCheck,	omxElementSinh},
{25,	"Tangent",	"tan",	 1,	omxElementTangentCheck,	omxElementTangent},
{26,	"Hyperbolic tangent",	"tanh",	 1,	omxElementTanhCheck,	omxElementTanh},
{27,	"Element Exponent",	"exp",	 1,	omxElementExponentCheck,	omxElementExponent},
{28,	"Element Natural Log",	"log",	 1,	omxElementNaturalLogCheck,	omxElementNaturalLog},
{29,	"Element Square Root",	"sqrt",	 1,	omxElementSquareRootCheck,	omxElementSquareRoot},
{30,	"Submatrix extract",	"[",	 3,	omxMatrixExtractCheck,	omxMatrixExtract},
{31,	"Half vectorization",	"vech",	 1,	omxMatrixVechCheck,	omxMatrixVech},
{32,	"1/2 vector (strict)",	"vechs",	 1,	omxMatrixVechsCheck,	omxMatrixVechs},
{33,	"Diagonal To Vector",	"diag2vec",	 1,	omxMatrixDiagonalCheck,	omxMatrixDiagonal},
{34,	"Vector To Diagonal",	"vec2diag",	 1,	omxMatrixFromDiagonalCheck,	omxMatrixFromDiagonal},
{35,	"Multivariate Normal Integration",	"omxMnor",	 4,	omxMultivariateNormalIntegrationCheck,	omxMultivariateNormalIntegration},
{36,	"All Cells mnor",	"omxAllInt",	-1,	omxAllIntegrationNormsCheck,	omxAllIntegrationNorms},
{37,	"Colon",	":",	 2,	omxSequenceGeneratorCheck,	omxSequenceGenerator},
{38,	"Kronecker powering",	"%^%",	 2,	omxKroneckerPowerCheck,	omxKroneckerPower},
{39,	"Vectorize by row",	"rvectorize",	 1,	omxRowVectorizeCheck,	omxRowVectorize},
{40,	"Vectorize by column",	"cvectorize",	 1,	omxColVectorizeCheck,	omxColVectorize},
{41,	"Real Eigenvectors",	"eigenvec",	 1,	omxRealEigenvectorsCheck,	omxRealEigenvectors},
{42,	"Real Eigenvalues",	"eigenval",	 1,	omxRealEigenvaluesCheck,	omxRealEigenvalues},
{43,	"Imaginary Eigenvectors",	"ieigenvec",	 1,	omxImaginaryEigenvectorsCheck,	omxImaginaryEigenvectors},
{44,	"Imaginary Eigenvalues",	"ieigenval",	 1,	omxImaginaryEigenvaluesCheck,	omxImaginaryEigenvalues},
{45,	"Unary negation",	"omxNot",	 1,	omxUnaryNegationCheck,	omxUnaryNegation},
{46,	"Row Selection",	"omxSelectRows",	 2,	omxSelectRowsCheck,	omxSelectRows},
{47,	"Column Selection",	"omxSelectCols",	 2,	omxSelectColsCheck,	omxSelectCols},
{48,	"Row And Col Selection",	"omxSelectRowsAndCols",	 2,	omxSelectRowsAndColsCheck,	omxSelectRowsAndCols},
{49,	"Arithmetic Mean",	"mean",	 1,	omxMatrixArithmeticMeanCheck,	omxMatrixArithmeticMean},
{50,	"Binary Greater Than",	"omxGreaterThan",	 2,	omxBinaryGreaterThanCheck,	omxBinaryGreaterThan},
{51,	"Binary Less Than",	"omxLessThan",	 2,	omxBinaryLessThanCheck,	omxBinaryLessThan},
{52,	"Binary And",	"omxAnd",	 2,	omxBinaryAndCheck,	omxBinaryAnd},
{53,	"Binary Or",	"omxOr",	 2,	omxBinaryOrCheck,	omxBinaryOr},
{54,	"Binary Approximate Equals",	"omxApproxEquals",	 3,	omxBinaryApproxEqualsCheck,	omxBinaryApproxEquals},
{55,	"Matrix Exponential",	"omxExponential",	 1,	omxExponentialCheck,	omxExponential},
{56,	"Matrix Exponential order",	"omxExponential",	 2,	omxExponentialCheck,	omxExponential},
{57,	"Cholesky Decomposition",	"chol",	 1,	omxCholeskyCheck,	omxCholesky},
{58,	"Covariance to Correlation",	"cov2cor",	 1,	omxCovToCorCheck,	omxCovToCor},
{59,	"Inverse Half vectorization",	"vech2full",	 1,	omxVechToMatrixCheck,	omxVechToMatrix},
{60,	"Inverse 1/2 vector (strict)",	"vechs2full",	 1,	omxVechsToMatrixCheck,	omxVechsToMatrix},
{61,	"Matrix Logarithm",	"logm",	 1,	mxMatrixLogCheck,	mxMatrixLog},
{62,	"Broadcast",	"",	 1,	omxBroadcastCheck,	omxBroadcast},
{63,	"Matrix Exponential",	"expm",	 1,	omxExponentialCheck,	omxExponential},
{64,	"Standard-normal quantile",	"p2z",	 1,	omxElementPtoZCheck,	omxElementPtoZ},
{65,	"Log-gamma",	"lgamma",	 1,	omxElementLgammaCheck,	omxElementLgamma},
{66,	"Arcsine",	"asin",	 1,	omxElementArcSineCheck,	omxElementArcSine},
{67,	"Arccosine",	"acos",	 1,	omxElementArcCosineCheck,	omxElementArcCosine},
{68,	"Arctangent",	"atan",	 1,	omxElementArcTangentCheck,	omxElementArcTangent},
{69,	"Inverse Hyperbolic Sine",	"asinh",	 1,	omxElementAsinhCheck,	omxElementAsinh},
{70,	"Inverse Hyperbolic Cosine",	"acosh",	 1,	omxElementAcoshCheck,	omxElementAcosh},
{71,	"Inverse Hyperbolic Tangent",	"atanh",	 1,	omxElementAtanhCheck,	omxElementAtanh},
{72,	"Log-gamma x+1 For Small x",	"lgamma1p",	 1,	omxElementLgamma1pCheck,	omxElementLgamma1p},
{73,	"Std-normal quantile from log(p)",	"logp2z",	 1,	omxElementLogPtoZCheck,	omxElementLogPtoZ},
{74,	"Beta PDF",	"dbeta",	 5,	omxElementDbetaCheck,	omxElementDbeta},
{75,	"Beta CDF",	"pbeta",	 6,	omxElementPbetaCheck,	omxElementPbeta},
{76,	"Modified Bessel F. of 1st Kind",	"besselI",	 3,	omxElementBesselICheck,	omxElementBesselI},
{77,	"Bessel F. of 1st Kind",	"besselJ",	 2,	omxElementBesselJCheck,	omxElementBesselJ},
{78,	"Modified Bessel F. of 3rd Kind",	"besselK",	 3,	omxElementBesselKCheck,	omxElementBesselK},
{79,	"Bessel F. of 2nd Kind",	"besselY",	 2,	omxElementBesselYCheck,	omxElementBesselY},
{80,	"Poisson PMF",	"dpois",	 3,	omxElementDpoisCheck,	omxElementDpois},
{81,	"Poisson CDF",	"ppois",	 4,	omxElementPpoisCheck,	omxElementPpois},
{82,	"Negative-Binomial PMF",	"omxDnbinom",	 5,	omxElementDnbinomCheck,	omxElementDnbinom},
{83,	"Negative-Binomial CDF",	"omxPnbinom",	 6,	omxElementPnbinomCheck,	omxElementPnbinom},
{84,	"Chi-square PDF",	"dchisq",	 4,	omxElementDchisqCheck,	omxElementDchisq},
{85,	"Chi-square CDF",	"pchisq",	 5,	omxElementPchisqCheck,	omxElementPchisq},
{86,	"Binomial PMF",	"dbinom",	 4,	omxElementDbinomCheck,	omxElementDbinom},
{87,	"Binomial CDF",	"pbinom",	 5,	omxElementPbinomCheck,	omxElementPbinom},
{88,	"Cauchy PDF",	"dcauchy",	 4,	omxElementDcauchyCheck,	omxElementDcauchy},
{89,	"Cauchy CDF",	"pcauchy",	 5,	omxElementPcauchyCheck,	omxElementPcauchy},
{90,	"Row sums",	"rowSums",	 1,	omxRowSumsCheck,	omxRowSums},
{91,	"Column sums",	"colSums",	 1,	omxColSumsCheck,	omxColSums},
{92,	"Evaluate on grid",	"mxEvaluateOnGrid",	 2,	evaluateOnGridCheck,	evaluateOnGrid},
{93,	"Element-wise robust log",	"mxRobustLog",	 1,	omxElementRobustLogCheck,	omxElementRobustLog},
{94,	"Pearson Selection covariance",	"mxPearsonSelCov",	 2,	pearsonSelCovCheck,	pearsonSelCov},
{95,	"Pearson Selection mean",	"mxPearsonSelMean",	 3,	pearsonSelMeanCheck,	pearsonSelMean},
};
