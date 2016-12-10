\name{lm.bp}
\alias{lm.bp}
\title{General Bivariate Poisson Model}
\description{
Produces a "list" object which gives details regarding the fit of a bivariate Poisson regression model of the form 

\hspace{1cm} $X_i,Y_i \sim BP( \lambda_{1i},  \lambda_{2i}, \lambda_{3i} )$ for $i =1, 2, \dots , n$, with 

\hspace{1cm} $\log{\underline \lambda}_1 = {\bf w}_1 {\underline \beta}_1$, 
						$\log{\underline \lambda}_2 = {\bf w}_2 {\underline \beta}_2$ and 
						$\log{\underline \lambda}_3 = {\bf w}_3 {\underline \beta}_3$ ; 
						
where 
		\describe{
    \item{--}{$n$ is the sample size.}
		\item{--}{${\underline \lambda}_k = ( \lambda_{k1}, \lambda_{k2}, \dots , \lambda_{kn} )^T$ for $k=1,2,3$ are vectors of length $n$ with the estimated lambda for each observation.}
    \item{--}{${\bf w}_1, {\bf w_2}$  are $n \times p$ data matrices containing explanatory variables for $\lambda_1$ and $\lambda_2$.} 
		\item{--}{${\bf w_3}$ is a $n \times p_2$ data matrix containing explanatory variables for $\lambda_3$.}
    \item{--}{${\underline \beta}_1, {\underline \beta}_2, {\underline \beta}_3$ are parameter vectors used in the linear predictors of  
$\lambda_1$, $\lambda_2$ and $\lambda_3$.} }
}
\usage{
lm.bp( l1, l2, l1l2=NULL, l3=~1, data, common.intercept=FALSE,  
       zeroL3=FALSE, maxit=300, pres=1e-8, verbose=getOption("verbose") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l1}{Formula of the form ``$x\sim X_1+\dots+X_p$" for parameters of $\log\lambda_1$.}
  \item{l2}{Formula of the form ``$y\sim X_1+\dots+X_p$" for parameters of $\log\lambda_2$.}
  \item{l1l2}{Formula of the form ``$\sim X_1+\dots+X_p$" for the common parameters of $\log\lambda_1$ and $\log\lambda_2$. 
			If the explanatory variable is also found on \code{l1} and/or \code{l2} then a model using interaction type parameters is fitted
			(one parameter common for both predictors [main effect] and differences from this for the other predictor [interaction type effect] ).
			Special terms of the form ``\code{c(X1,X2)}" can be also used here. 
			These terms imply common parameters of $\lambda_1$ and $\lambda_2$ on different variables. 
			For example if \code{c(x1,x2)} is used then use the same beta for the effect of $x_1$ on $\log\lambda_1$ and the effect of $x_2$ on 
			$\log\lambda_2$. For details see example 4 - dataset \code{ex4.ita91}.
}
  \item{l3}{Formula of the form ``$\sim X_1+\dots+X_p$" for the parameters of $\log\lambda_3$.}
  \item{data}{Data frame containing the variables in the model.}
  \item{common.intercept}{Logical function specifying whether a common intercept on $\log\lambda_1$ and $\log\lambda_2$ should be used.
				The default value is \code{FALSE}.}
  \item{zeroL3}{Logical argument controlling whether $\lambda_3$ should be set equal to zero (therefore fits a double Poisson model).
}
  \item{maxit}{Maximum number of EM steps. Default value is 300 iterations.}
  \item{pres}{Precision used in stopping the EM algorithm. The algorithm stops when the relative log-likelihood difference is lower than 
  						the value of  pres.}
  \item{verbose}{Logical argument controlling whether beta parameters will we printed while EM runs. Default value is taken equal to the 
  								value of \linebreak \code{options()\$verbose}. 
  								If \code{verbose=FALSE} then only the iteration number, the loglikelihood and its relative difference from the previous iteration 
  								are printed. 
  								If  \code{verbose=TRUE} then the model parameters $\beta_1$, $\beta_2$ and $\beta_3$ are additionally printed}
}
\value{
A list object returned with the following variables. 

\item{coefficients}{Estimates of the model parameters for $\beta_1$, $\beta_2$ and $\beta_3$. 
 										When a factor is used then its default set of constraints is used.}

\item{fitted.values}{Data frame with $n$ lines and 2 columns containing the fitted values for $x$ and $y$. 
											For the bivariate Poisson model the fitted values are given by $\lambda_1+\lambda_3$ and $\lambda_2+\lambda_3$ respectively.}

\item{residuals}{Data frame with $n$ lines and 2 columns containing the residuals of the model for $x$ and $y$. 
										For the bivariate Poisson model the residual values are given by $x-E(x)$ and $y-E(y)$ respectively;
										where $E(x)=\lambda_1+\lambda_3$ and $E(y)=\lambda_2+\lambda_3$.}
 
\item{beta1,beta2, beta3}{Vectors $\beta_1, \beta_2$ and $\beta_3$ containing the coefficients involved in the linear  predictors of  
													$\lambda_1 , \lambda_2$ and $\lambda_3$ respectively. When \code{zeroL3=TRUE} then \code{beta3} is not calculated.}
 
\item{lambda1, lambda2}{Vectors of length $n$ containing the estimated $\lambda_1$ and $\lambda_2$ for each observation}
 
\item{lambda3}{vector containing the values of $\lambda_3$. If \code{zeroL3=TRUE} then \code{lambda3} is equal to zero and is not provided.}
 
\item{loglikelihood}{ Maximized log-likelihood of the fitted model. This is given in a vector form (one value per iteration).
											Using this vector we can monitor the log-likelihood evolution in each EM step.}
 
\item{AIC, BIC}{ AIC and BIC of the model. Values are also provided for the double Poisson model and the saturated model.}
 
\item{parameters}{Number of parameters.}
 
\item{iterations}{Number of iterations.}

\item{call}{Argument providing the exact calling details of the \code{lm.bp} function.}

}

\references{
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 

2.      Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}

\author{
1.      Dimitris Karlis, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{karlis@aueb.gr} .

2.      Ioannis Ntzoufras, Department of Statistics, Athens University of Economics and Business, Athens, Greece, \email{ntzoufras@aueb.gr}  .
}

\seealso{ 
\code{\link{pbivpois}},
\code{\link{simple.bp}},
\code{\link{lm.dibp}}.
}

\examples{
data(ex1.sim)
# Fit Double and Bivariate Poisson models ()
#
# Model 2: DblPoisson(l1, l2)
ex1.m2<-lm.bp(x~1 , y~1 , data=ex1.sim, zeroL3=TRUE)
#
# Model 3: BivPoisson(l1, l2, l3); same as simple.bp(ex1.sim$x, ex1.sim$y)
ex1.m3<-lm.bp(x~1 , y~1 , data=ex1.sim)
# Model 4: DblPoisson (l1=Full, l2=Full) 
ex1.m4<-lm.bp(x~. , y~. , data=ex1.sim, zeroL3=TRUE)
#
# for models 4-11 maximum number of iterations is set to 2
#
# Model 5: BivPoisson(l1=full, l2=full, l3=constant)
ex1.m5<-lm.bp(x~. , y~. , data=ex1.sim, maxit=2)
# Model 6: DblPois(l1,l2)
ex1.m6<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, data=ex1.sim, zeroL3=TRUE)
# Model 7: BivPois(l1,l2,l3=constant)
ex1.m7<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, data=ex1.sim, maxit=2)
# Model 8: BivPoisson(l1=full, l2=full, l3=full)
ex1.m8<-lm.bp(x~. , y~. , l3=~., data=ex1.sim, maxit=2)
# Model 9: BivPoisson(l1=full, l2=full, l3=z1+z2+z3+z4)
ex1.m9<-lm.bp(x~. , y~. , l3=~.-z5, data=ex1.sim, maxit=2)
# Model 10: BivPoisson(l1, l2, l3=full)
ex1.m10<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, l3=~., data=ex1.sim, maxit=2)
# Model 11: BivPoisson(l1, l2, l3= z1+z2+z3+z4)
ex1.m11<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, l3=~.-z5, data=ex1.sim, maxit=2)

}
\keyword{models}
\keyword{regression}

