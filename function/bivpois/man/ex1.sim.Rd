\name{ex1.sim}
\alias{ex1.sim}
\docType{data}
\title{Bivpois Example 1 Dataset: Simulated Data}
\description{
The data has one pair $(x,y)$ of bivariate Poisson variables and five variables ($z_1,\dots,z_5$) generated from  $N(0, 0.01)$ distribution. Hence

\hspace{1cm} $Xi, Yi \sim BP( \lambda_{1i}, \lambda_{2i}, \lambda_{3i}  ) $ with 

\hspace{2cm} $\log\lambda_{1i} = 1.8 + 2 Z_{1i} + 3 Z_{3i}$
 
\hspace{2cm} $\log\lambda_{2i} = 0.7 -   Z_{1i} - 3 Z_{3i} + 3 Z_{5i}$

\hspace{2cm} $\log\lambda_{3i} = 1.7 +   Z_{1i} - 2 Z_{2i} + 2 Z_{3i} - 2 Z_{4i}.$
}
\usage{data(ex1.sim)}
\format{
  A data frame with 100 observations on the following 7 variables.
  \describe{
    \item{x,y}{Simulated Bivariate Poisson Variables used as response}
    \item{z1,z2,z3,z4,z5}{Simulated N(0,0.01) explanatory variables}
  }
}
\details{
	This data is used as example one in Karlis and Ntzoufras (2004). 
}
\source{
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 
}

\references{Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}
\examples{
#
# -------------------------------------------------------------------------------
#  Double and Bivariate Poisson models can be fitted using the command
#  demo(ex1, package='bivpois')
#
#  Here we present the same commands but iterations of the EM were restricted to 2 to save time

library(bivpois)       # load bivpois library
data(ex1.sim)          # load data of example 1
# -------------------------------------------------------------------------------
# Simple Bivariate Poisson Model
ex1.simple<-simple.bp( ex1.sim$x, ex1.sim$y, maxit=2) # fit simple model of section 4.1.1
names(ex1.simple)      # monitor output variables
ex1.simple$lambda      # view lambda1 
ex1.simple$BIC         # view BIC
ex1.simple             # view all results of the model
#
# -------------------------------------------------------------------------------
# Fit Double and Bivariate Poisson models ()
#
# Model 2: DblPoisson(l1, l2)
ex1.m2<-lm.bp(x~1 , y~1 , data=ex1.sim, zeroL3=TRUE)
# Model 3: BivPoisson(l1, l2, l3); same as simple.bp(ex1.sim$x, ex1.sim$y)
ex1.m3<-lm.bp(x~1 , y~1 , data=ex1.sim, maxit=2)
# Model 4: DblPoisson (l1=Full, l2=Full) 
ex1.m4<-lm.bp(x~. , y~. , data=ex1.sim, zeroL3=TRUE)
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
#
ex1.m11$coef  # monitor all beta parameters of model 11
#
ex1.m11$beta1 # monitor all beta parameters of lambda1 of model 11
ex1.m11$beta2 # monitor all beta parameters of lambda2 of model 11
ex1.m11$beta3 # monitor all beta parameters of lambda3 of model 11




}
\keyword{datasets}
\keyword{models}
\keyword{regression}
