\name{pbivpois}
\alias{pbivpois}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Probability Function of the Bivariate Poisson Distribution}
\description{Returns the probability the probability (or the log) of the bivariate poisson distribution for values x and y.}
\usage{
pbivpois(x, y=NULL, lambda = c(1, 1, 1), log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ 
	\item{x}{Matrix or Vector containing the data. If \code{x} is a matrix then we consider as $x$ the first column and $y$ the second column. 
						Additional columns and \code{y} are ignored.}
	\item{y}{Vector containing the data of $y$. It is used only if \code{x} is also a vector. Vectors \code{x} and \code{y} should be of equal length.}
	\item{lambda}{Vector (of length 3) containing values of the parameters $\lambda_1$, $\lambda_2$ and $\lambda_3$ of the bivariate Poisson distribution.
	}
 	\item{log}{Logical argument for calculating the log probability or the probability function. The default value is \code{FALSE}. }
}
\details{This function evaluates the probability function (or the log) of the bivariate Poisson distribution with parameters $\lambda_1$, $\lambda_2$ and $\lambda_3$. Much faster than \code{bivpois.table} since it avoid `for-loops' and does not use recursive relations.}

\value{A vector of values of the probabilities of $PD(\lambda_1, \lambda_2, \lambda_3)$ evaluated at $(x,y)$ when \code{log=FALSE} or the log-probabilities of $PD(\lambda_1, \lambda_2, \lambda_3)$ evaluated at $(x,y)$ when \code{log=TRUE}. }

\references{
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 

2.      Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}

\author{
1.      Dimitris Karlis, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{karlis@aueb.gr} .

2.      Ioannis Ntzoufras, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{ntzoufras@aueb.gr}  .
}


\seealso{
	\code{\link{bivpois.table}},
	\code{\link{simple.bp}},
	\code{\link{lm.bp}},
	\code{\link{lm.dibp}}. 
	}

\examples{

# probability function of (x,y)=(3,1) for lambda_1=1, lambda_2=1, lambda_3=1
pbivpois(3, 1)                              
# probability function of (x,y)=(3,1) for lambda_1=3, lambda_2=1, lambda_3=1
pbivpois(3, 1, lambda=c(3,1,1))             
# log-probability function of (x,y)=(3,1) for lambda_1=1, lambda_2=1, lambda_3=1
pbivpois(3, 1, lambda=c(3,1,1), log=TRUE)   
#
# evaluates f(1,1), f(1,3) and f(3,1) for PD(2,1,0.2)
pbivpois( c(1,3,1), c(1,1,3), c( 2,1,0.2 ) )               
# same as above
pbivpois( cbind(c(1,3,1), c(1,1,3)), lambda=c( 2,1,0.2 ) ) 

}

\keyword{distribution}

