\name{simple.bp}
\alias{simple.bp}
\title{Simple Bivariate Poisson Model}
\description{Produces a "list" object which gives details regarding the fit of a simple bivariate Poisson model of the form 
$(X,Y) \sim BP(\lambda_1, \lambda_2, \lambda_3)$.
}
\usage{
simple.bp(x, y, ini3 = 1, maxit = 300, pres = 1e-08)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{x}{Matrix or Vector containing the data. If \code{x} is a matrix then we consider as $x$ the first column and $y$ the second column. 
						Additional columns and \code{y} are ignored.}
	\item{y}{Vector containing the data of $y$. It is used only if \code{x} is also a vector. Vectors \code{x} and \code{y} should be of equal length.}
  \item{ini3}{Initial value for $\lambda_3$. }
  \item{maxit}{Maximum number of EM steps.}
  \item{pres}{Precision used in log-likelihood improvement.}
}
\details{
During the run of the algorithm the following details are printed: the iteration number, lambda1, lambda2, lambda3, the log-likelihood and the relative difference of the log-likelihood.
}
\value{
A list object returned with the following variables. 
\item{lambda}{ Vector with parameter values $\lambda_1$ , $\lambda_2$, $\lambda_3$}
\item{loglikelihood}{Ìaximized log-likelihood of the fitted model. This is given in a vector form (one value per iteration).Using this we may monitor the log-likelihood improvement and how EM algorithm works.}
\item{AIC, BIC}{AIC and BIC of the model. Values are also given for the double Poisson model and the saturated model.}
\item{parameters}{Number of parameters.}
\item{iterations}{Number of iterations.}
}
\references{
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 

2.      Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}

\author{
1.      Dimitris Karlis, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{karlis@aueb.gr} .

2.      Ioannis Ntzoufras, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{ntzoufras@aueb.gr}  .
}

\seealso{ \code{\link{pbivpois}},  \code{\link{lm.bp}}, \code{\link{lm.dibp}} } 

\examples{
#
# Generation of BP(1,2,3) data
x3<-rpois(100, 3)
x1<-rpois(100, 1)+x3
x2<-rpois(100, 2)+x3
#
# fits the model
x<-simple.bp(x1, x2)
#
# Monitors parameters
x$lambda1
x$lambda2
x$lambda3
#
# alternatively (for 10 iterations)
x<-simple.bp( cbind(x1, x2), maxit=2 )

}
\keyword{distribution}
\keyword{models}
