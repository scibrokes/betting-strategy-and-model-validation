\name{bivpois.table}
\alias{bivpois.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Probability of Bivariate Poisson Using Recursive relations}
\description{Returns the probability of the bivariate Poisson distribution using recursive relations. }
\usage{
bivpois.table(x, y, lambda = c(1, 1, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{x, y}{
		single values containing which values should evaluated (x and y should be at least 1)
		}
	\item{lambda}{
		Vector (of length 3) containing values of the parameters lambda1, lambda2 and lambda3 of the bivariate poisson distribution.
	}
}
\details{In order to calculate bivpoison probability values we use recursive relationships. This function is much slower than pbivpois}
\value{A matrix with dimension (x+1) X (y+1) is returned. Cell ij contains the probability P(X=i-1, Y=j-1).}


\references{
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 

2.      Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}


\author{
1.      Dimitris Karlis, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{karlis@aueb.gr} .

2.      Ioannis Ntzoufras, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{ntzoufras@aueb.gr}  .
}

\seealso{
	\code{\link{pbivpois}}.
	\code{\link{simple.bp}}.
	\code{\link{lm.bp}}.
	\code{\link{lm.dibp}}. 
	}

\keyword{distribution}
