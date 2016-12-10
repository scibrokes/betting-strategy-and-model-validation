\name{lm.dibp}
\alias{lm.dibp}
\title{General Diagonal Inflated Bivariate Poisson Model}
\description{
Produces a "list" object which gives details regarding the fit of a bivariate diagonal inflated Poisson regression model of the form

\hspace{1cm} $(X_i,Y_i) \sim DIBP( \lambda_{1i},  \lambda_{2i}, \lambda_{3i} , D(\theta) )$ which is equivalent to 

\hspace{2cm} $(Xi,Yi) \sim (1-p)BP( x_i, y_i| \lambda_{1i},  \lambda_{2i}, \lambda_{3i} )$ if $x_i \ne y_i$

\hspace{2cm} $(Xi,Yi) \sim (1-p)BP( x_i, y_i| \lambda_{1i},  \lambda_{2i}, \lambda_{3i} )+pD( x_i | \theta )$ if $x_i=y_i$ for $i =1, 2, \dots, n$ with 

\hspace{2cm}
$\log {\underline \lambda}_1 = {\bf w}_1 {\underline \beta}$, 
$\log {\underline \lambda}_2 = {\bf w}_2 {\underline \beta}$ and
$\log {\underline \lambda}_3 = {\bf w}_3 {\underline \beta}_3$ ; 

where 

\describe{
		\item{--}{$n$ is the sample size.}
		\item{--}{${\underline \lambda}_k = ( \lambda_{k1}, \lambda_{k2}, \dots, \lambda_{kn} )^T$ for $k=1,2,3$ are vectors of length $n$ 
		containing the estimated lambda for each observation.}
		\item{--}{${\bf w}_1$, ${\bf w}_2$  are $n \times p$ data matrices containing explanatory variables for $\lambda_1$ and $\lambda_2$.}
		\item{--}{${\bf w}_3$		are $n\times p_2$ data matrix containing explanatory variables for $\lambda_3$.}
		\item{--}{${\underline \beta}$ 							is a vector of length $p$ which is common for $\lambda_1$ and $\lambda_2$ in order to allow for common effects. }
		\item{--}{${\underline \beta}_3$ vector of length $p_2$.}
		\item{--}{$D(\theta)$ is a discrete distribution with parameter vector $\theta$ used to inflate the diagonal.}
		\item{--}{$p$ is the mixing proportion.}
		}	
}
\usage{
lm.dibp( l1, l2, l1l2=NULL, l3=~1, data, common.intercept=FALSE, 
         zeroL3 = FALSE, distribution = "discrete", jmax = 2, maxit = 300, 
         pres = 1e-08, verbose=getOption("verbose") )
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{l1}{Formula of the form ``$x\sim X_1+\dots+X_p$" for parameters of $\log\lambda_1$.}
  \item{l2}{Formula of the form ``$y\sim X_1+\dots+X_p$" for parameters of $\log\lambda_2$.}
  \item{l1l2}{Formula of the form ``$\sim X_1+\dots+X_p$" for the common parameters of $\log\lambda_1$ and $\log\lambda_2$. 
			If the explanatory variable is also found on \code{l1} and/or \code{l2} then a model using interaction type parameters is fitted
			(one parameter common for both predictors [main effect] and differences from this for the other predictor [interaction type effect] ).
			Special terms of the form ``\code{c(X1,X2)}" can be also used here. 
			These terms imply common parameters of $\lambda_1$ and $\lambda_2$ on different variables. 
			For example if \code{c(x1,x2)} is used then use the same beta for the effect of $x_1$ on $\log\lambda_1$ and the effect of $x_2$ on 
			$\log\lambda_2$. For details see example 4 - dataset \code{ex4.ita91}.}
  \item{l3}{Formula of the form ``$\sim X_1+\dots+X_p$" for the parameters of $\log\lambda_3$.}
  \item{data}{Data frame containing the variables in the model.}
  \item{common.intercept}{Logical function specifying whether a common intercept on $\log\lambda_1$ and $\log\lambda_2$ should be used.
				The default value is \code{FALSE}.}
  \item{zeroL3}{Logical argument controlling whether $\lambda_3$ should be set equal to zero (therefore fits a double Poisson model).}
  \item{distribution}{Specifies the type of inflated distribution; =\code{"discrete"}:  Discrete(J=jmax), =\code{"poisson"}  :  Poisson($\theta$) 
											=\code{"geometric"}: Geometric($\theta$). }
  \item{jmax}{Number of parameters used in $Discrete$ distribution. This argument is not used for the Poisson or the Geometric distributions are used as for the inflation of the diagonal.}
  \item{maxit}{Maximum number of EM steps. Default value is 300 iterations. }
  \item{pres}{Precision used in stopping the EM algorithm. The algorithm stops when the relative log-likelihood difference is lower than the value of  pres.}
  \item{verbose}{Logical argument controlling whether beta parameters will we printed while EM runs. Default value is taken equal to the 
  								value of \linebreak \code{options()\$verbose}. 
  								If \code{verbose=FALSE} then only the iteration number, the loglikelihood and its relative difference from the previous iteration 
  								are printed. 
  								If  \code{verbose=TRUE} then the model parameters $\beta_1$, $\beta_2$ and $\beta_3$ are additionally printed}}
\value{
A list object returned with the following variables. 


\item{coefficients}{Estimates of the model parameters for $\beta_1$, $\beta_2$ and $\beta_3$, $p$ and $\theta$.}

\item{fitted.values}{Data frame with $n$ lines and 2 columns containing the fitted values for $x$ and $y$.}

\item{residuals}{Data frame with $n$ lines and 2 columns containing the residuals of the model for $x$ and $y$ given by $x-E(x)$ and $y-E(y)$ respectively; where $E(x)$ and $E(y)$ are given by the \code{fitted.values} .}
 
\item{beta1,beta2, beta3}{Vectors $\beta_1, \beta_2$ and $\beta_3$ containing the coefficients involved in the linear  predictors of  
													$\lambda_1 , \lambda_2$ and $\lambda_3$ respectively. When \code{zeroL3=TRUE} then \code{beta3} is not calculated.}
 
\item{lambda1, lambda2}{Vectors of length $n$ containing the estimated $\lambda_1$ and $\lambda_2$ for each observation}
 
\item{lambda3}{vector containing the values of $\lambda_3$. If \code{zeroL3=TRUE} then \code{lambda3} is equal to zero and is not provided.}
 
\item{loglikelihood}{ Maximized log-likelihood of the fitted model. This is given in a vector form (one value per iteration).
											Using this vector we can monitor the log-likelihood evolution in each EM step.}
 
\item{AIC, BIC}{ AIC and BIC of the model. Values are also provided for the double Poisson model and the saturated model.}
 
\item{diagonal.distribution}{label for the diagonal inflated distribution used.}
 
\item{p}{mixing proportion.}

\item{theta}{Parameter vector of the diagonal distribution. For \code{discrete} distribution theta has length equal to jmax with $\theta_i=$\code{theta[i]} and $\theta_0 =1-\sum_{i=1}^{JMAX}\theta_i$; for the Poisson distribution theta is the mean; for the Geometric distribution theta is the success probability.}

\item{parameters}{Number of parameters.}
 
\item{iterations}{Number of iterations.}

\item{call}{Argument providing the exact calling details of the \code{lm.dibp} function.}
  

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
	\code{\link{lm.bp}}.
	}

	
\examples{
data(ex2.sim)
#
# Model 1: BivPois
ex2.m1<-lm.bp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim )
# Model 2: Zero Inflated BivPois 
ex2.m2<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=0)
#
# for models 3-10, the maximum number of iterations is set to 2
#
# Model 3: Diagonal Inflated BivPois with DISCRETE(1) diagonal distribution
ex2.m3<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=1, maxit=2)
# Model 4: Diagonal Inflated BivPois with DISCRETE(2) diagonal distribution
ex2.m4<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=2, maxit=2)
# Model 5: Diagonal Inflated BivPois with DISCRETE(3) diagonal distribution
ex2.m5<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=3, maxit=2)
# Model 6: Diagonal Inflated BivPois with DISCRETE(4) diagonal distribution
ex2.m6<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=4, maxit=2)
# Model 7: Diagonal Inflated BivPois with DISCRETE(5) diagonal distribution
ex2.m7<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=5, maxit=2)
# Model 8: Diagonal Inflated BivPois with DISCRETE(6) diagonal distribution
ex2.m8<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=6, maxit=2)
# Model 9: Diagonal Inflated BivPois with POISSON diagonal distribution
ex2.m9<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , 
                 distribution="poisson", maxit=2)
# Model 10: Diagonal Inflated BivPois with GEOMETRIC diagonal distribution
ex2.m10<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , 
                  distribution="geometric", maxit=2)
#
# printing parameters of model 7
ex2.m7$beta1
ex2.m7$beta2
ex2.m7$beta3
ex2.m7$p
ex2.m7$theta
#
# printing parameters of model 9
ex2.m9$beta1
ex2.m9$beta2
ex2.m9$beta3
ex2.m9$p
ex2.m9$theta
}

\keyword{models}
\keyword{regression}


