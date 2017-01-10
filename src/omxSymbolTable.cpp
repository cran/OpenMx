#include "omxSymbolTable.h"
#include "AlgebraFunctions.h"
const omxAlgebraTableEntry omxAlgebraSymbolTable[] = {
{ 0,	"*SPECIAL*",	"*NONE*",	 0,	NULL},
{ 1,	"Inversion",	"solve",	 1,	omxMatrixInvert},
{ 2,	"Transposition",	"t",	 1,	omxMatrixTranspose},
{ 3,	"Element powering",	"^",	 2,	omxElementPower},
{ 4,	"Multiplication",	"%*%",	 2,	omxMatrixMult},
{ 5,	"Dot product",	"*",	 2,	omxMatrixElementMult},
{ 6,	"Kronecker product",	"%x%",	 2,	omxKroneckerProd},
{ 7,	"Quadratic product",	"%&%",	 2,	omxQuadraticProd},
{ 8,	"Element division",	"/",	 2,	omxElementDivide},
{ 9,	"Addition",	"+",	 2,	omxMatrixAdd},
{10,	"Subtraction (binary)",	"-",	 2,	omxMatrixSubtract},
{11,	"Subtraction (unary)",	"-",	 1,	omxUnaryMinus},
{12,	"Horizontal adhesion",	"cbind",	-1,	omxMatrixHorizCatOp},
{13,	"Vertical adhesion",	"rbind",	-1,	omxMatrixVertCatOp},
{14,	"Determinant",	"det",	 1,	omxMatrixDeterminant},
{15,	"Trace",	"tr",	 1,	omxMatrixTraceOp},
{16,	"Sum",	"sum",	-1,	omxMatrixTotalSum},
{17,	"Product",	"prod",	-1,	omxMatrixTotalProduct},
{18,	"Maximum",	"max",	-1,	omxMatrixMaximum},
{19,	"Minimum",	"min",	-1,	omxMatrixMinimum},
{20,	"Absolute value",	"abs",	 1,	omxMatrixAbsolute},
{21,	"Cosine",	"cos",	 1,	omxElementCosine},
{22,	"Hyperbolic cosine",	"cosh",	 1,	omxElementCosh},
{23,	"Sine",	"sin",	 1,	omxElementSine},
{24,	"Hyperbolic sine",	"sinh",	 1,	omxElementSinh},
{25,	"Tangent",	"tan",	 1,	omxElementTangent},
{26,	"Hyperbolic tangent",	"tanh",	 1,	omxElementTanh},
{27,	"Element Exponent",	"exp",	 1,	omxElementExponent},
{28,	"Element Natural Log",	"log",	 1,	omxElementNaturalLog},
{29,	"Element Square Root",	"sqrt",	 1,	omxElementSquareRoot},
{30,	"Submatrix extract",	"[",	 3,	omxMatrixExtract},
{31,	"Half vectorization",	"vech",	 1,	omxMatrixVech},
{32,	"1/2 vector (strict)",	"vechs",	 1,	omxMatrixVechs},
{33,	"Diagonal To Vector",	"diag2vec",	 1,	omxMatrixDiagonal},
{34,	"Vector To Diagonal",	"vec2diag",	 1,	omxMatrixFromDiagonal},
{35,	"Multivariate Normal Integration",	"omxMnor",	 4,	omxMultivariateNormalIntegration},
{36,	"All Cells mnor",	"omxAllInt",	-1,	omxAllIntegrationNorms},
{37,	"Colon",	":",	 2,	omxSequenceGenerator},
{38,	"Kronecker powering",	"%^%",	 2,	omxKroneckerPower},
{39,	"Vectorize by row",	"rvectorize",	 1,	omxRowVectorize},
{40,	"Vectorize by column",	"cvectorize",	 1,	omxColVectorize},
{41,	"Real Eigenvectors",	"eigenvec",	 1,	omxRealEigenvectors},
{42,	"Real Eigenvalues",	"eigenval",	 1,	omxRealEigenvalues},
{43,	"Imaginary Eigenvectors",	"ieigenvec",	 1,	omxImaginaryEigenvectors},
{44,	"Imaginary Eigenvalues",	"ieigenval",	 1,	omxImaginaryEigenvalues},
{45,	"Unary negation",	"omxNot",	 1,	omxUnaryNegation},
{46,	"Row Selection",	"omxSelectRows",	 2,	omxSelectRows},
{47,	"Column Selection",	"omxSelectCols",	 2,	omxSelectCols},
{48,	"Row And Col Selection",	"omxSelectRowsAndCols",	 2,	omxSelectRowsAndCols},
{49,	"Arithmetic Mean",	"mean",	 1,	omxMatrixArithmeticMean},
{50,	"Binary Greater Than",	"omxGreaterThan",	 2,	omxBinaryGreaterThan},
{51,	"Binary Less Than",	"omxLessThan",	 2,	omxBinaryLessThan},
{52,	"Binary And",	"omxAnd",	 2,	omxBinaryAnd},
{53,	"Binary Or",	"omxOr",	 2,	omxBinaryOr},
{54,	"Binary Approximate Equals",	"omxApproxEquals",	 3,	omxBinaryApproxEquals},
{55,	"Matrix Exponential",	"omxExponential",	 1,	omxExponential},
{56,	"Matrix Exponential order",	"omxExponential",	 2,	omxExponential},
{57,	"Cholesky Decomposition",	"chol",	 1,	omxCholesky},
{58,	"Covariance to Correlation",	"cov2cor",	 1,	omxCovToCor},
{59,	"Inverse Half vectorization",	"vech2full",	 1,	omxVechToMatrix},
{60,	"Inverse 1/2 vector (strict)",	"vechs2full",	 1,	omxVechsToMatrix},
{61,	"Matrix Logarithm",	"logm",	 1,	mxMatrixLog},
{62,	"Broadcast",	"",	 1,	omxBroadcast},
{63,	"Matrix Exponential",	"expm",	 1,	omxExponential},
{64,	"Standard-normal quantile",	"p2z",	 1,	omxElementPtoZ},
{65,	"Log-gamma",	"lgamma",	 1,	omxElementLgamma},
{66,	"Arcsine",	"asin",	 1,	omxElementArcSine},
{67,	"Arccosine",	"acos",	 1,	omxElementArcCosine},
{68,	"Arctangent",	"atan",	 1,	omxElementArcTangent},
{69,	"Inverse Hyperbolic Sine",	"asinh",	 1,	omxElementAsinh},
{70,	"Inverse Hyperbolic Cosine",	"acosh",	 1,	omxElementAcosh},
{71,	"Inverse Hyperbolic Tangent",	"atanh",	 1,	omxElementAtanh},
{72,	"Log-gamma x+1 For Small x",	"lgamma1p",	 1,	omxElementLgamma1p},
{73,	"Std-normal quantile from log(p)",	"logp2z",	 1,	omxElementLogPtoZ},
{74,	"Beta PDF",	"dbeta",	 5,	omxElementDbeta},
{75,	"Beta CDF",	"pbeta",	 6,	omxElementPbeta},
{76,	"Modified Bessel F. of 1st Kind",	"besselI",	 3,	omxElementBesselI},
{77,	"Bessel F. of 1st Kind",	"besselJ",	 2,	omxElementBesselJ},
{78,	"Modified Bessel F. of 3rd Kind",	"besselK",	 3,	omxElementBesselK},
{79,	"Bessel F. of 2nd Kind",	"besselY",	 2,	omxElementBesselY},
{80,	"Poisson PMF",	"dpois",	 3,	omxElementDpois},
{81,	"Poisson CDF",	"ppois",	 4,	omxElementPpois},
{82,	"Negative-Binomial PMF",	"dnbinom",	 5,	omxElementDnbinom},
{83,	"Negative-Binomial CDF",	"pnbinom",	 6,	omxElementPnbinom},
{84,	"Chi-square PDF",	"dchisq",	 4,	omxElementDchisq},
{85,	"Chi-square CDF",	"pchisq",	 5,	omxElementPchisq},
{86,	"Binomial PMF",	"dbinom",	 4,	omxElementDbinom},
{87,	"Binomial CDF",	"pbinom",	 5,	omxElementPbinom},
{88,	"Cauchy PDF",	"dcauchy",	 4,	omxElementDcauchy},
{89,	"Cauchy CDF",	"pcauchy",	 5,	omxElementPcauchy},
};
