\name{ex2.sim}
\alias{ex2.sim}
\docType{data}
\title{Bivpois Example 2 Dataset: Simulated Data}
\description{
The data has one pair $(x,y)$ of diagonal inflated bivariate Poisson variables and five variables $(z_1,\dots,z_5)$ generated from $N(0, 0.12)$ distribution. Hence

\hspace{1cm} $X_i, Y_i \sim DIBP( \lambda_{1i}, \lambda_{2i}, \lambda_{3i}  , p=0.30,  Poisson(2) ) $ with

\hspace{2cm} $\log\lambda_{1i} = 1.8 + 2 Z_{1i} + 3 Z_{3i}$\

\hspace{2cm} $\log\lambda_{2i} = 0.7 -   Z_{1i} - 3 Z_{3i} + 3 Z_{5i}$

\hspace{2cm} $\log\lambda_{3i} = 1.7 +   Z_{1i} - 2 Z_{2i} + 2 Z_{3i} - 2 Z_{4i}.$
}
\usage{data(ex2.sim)}
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
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). }

\references{Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}
\examples{
#  Models of example 2 can be fitted using the command
#  demo(ex2, package='bivpois')
#
#  Here we present the same commands but iterations of the EM were restricted to 2 to save time

library(bivpois) # load bivpois library
data(ex2.sim)    # load ex2.sim data from bivpois library
#
# Model 1: BivPois
ex2.m1<-lm.bp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim, maxit=2 )
# Model 2: Zero Inflated BivPois 
ex2.m2<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=0, maxit=2 )
# Model 3: Diagonal Inflated BivPois with DISCRETE(1) diagonal  distribution
ex2.m3<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=1, maxit=2 )
# Model 4: Diagonal Inflated BivPois with DISCRETE(2) diagonal  distribution
ex2.m4<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=2, maxit=2 )
# Model 5: Diagonal Inflated BivPois with DISCRETE(3) diagonal  distribution
ex2.m5<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=3, maxit=2 )
# Model 6: Diagonal Inflated BivPois with DISCRETE(4) diagonal  distribution
ex2.m6<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=4, maxit=2 )
# Model 7: Diagonal Inflated BivPois with DISCRETE(5) diagonal  distribution
ex2.m7<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=5, maxit=2 )
# Model 8: Diagonal Inflated BivPois with DISCRETE(6) diagonal  distribution
ex2.m8<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=6, maxit=2 )
# Model 9: Diagonal Inflated BivPois with POISSON diagonal distribution
ex2.m9<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , 
                 distribution="poisson", maxit=2 )
# Model 10: Diagonal Inflated BivPois with GEOMETRIC diagonal distribution
ex2.m10<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , 
                  distribution="geometric", maxit=2 )
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
\keyword{datasets}
\keyword{models}
\keyword{regression}
