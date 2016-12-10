\name{ex4.ita91}
\alias{ex4.ita91}
\docType{data}
\title{Bivpois Example 4 Dataset: Italian Series A Football Scores for Season 1991-92}
\description{Italian Serie A football scores for season 1991-92.}
\usage{data(ex4.ita91)}
\format{
  A data frame with 306 observations on the following 4 variables.
  \describe{
    \item{g1}{Goals scored by the home team}
    \item{g2}{Goals scored by the away team}
    \item{team1}{
    a factor indicating the home team with levels \code{Ascoli    } \code{Atalanta  } \code{Bari      } \code{Cagliari  } \code{Cremonese } \code{Fiorentina} \code{Foggia    } \code{Genoa     } \code{Inter     } \code{Juventus  } \code{Lazio     } \code{Milan     } \code{Napoli    } \code{Parma     } \code{Roma      } \code{Sampdoria } \code{Torino    } \code{Verona    }}
    \item{team2}{a factor indicating the away team with levels \code{Ascoli    } \code{Atalanta  } \code{Bari      } \code{Cagliari  } \code{Cremonese } \code{Fiorentina} \code{Foggia    } \code{Genoa     } \code{Inter     } \code{Juventus  } \code{Lazio     } \code{Milan     } \code{Napoli    } \code{Parma     } \code{Roma      } \code{Sampdoria } \code{Torino    } \code{Verona    }}
  }
}
\details{
Data were originally used in Karlis and Ntzoufras (2003). The data consist of pairs of counts indicating the number of goals scored by each of the two competing teams. As covariates we have used dummy variables to model the team strength. In modelling outcomes of football games, it has been observed an excess of draws and small over-dispersion. Introducing diagonal inflated models we correct for both the over-dispersion and the excess of draws.
}

\source{
Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393. 
}

\references{
1.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 
}


\examples{
#  Models 1-12 of example 4 can be fully reproduced using the command
#  demo(ex4, package='bivpois')
#
#  Here we present the same commands but iterations of the EM were restricted to 10 to save time

#
# Models 1-12 can be run using the demo command demo(ex4,package='bivpois')
#

library(bivpois) # loading of bivpois library
data(ex4.ita91)  # loading ex4.ita91 data from bivpois library
#
# formula for modeling of lambda1 and lambda2
form1 <- ~c(team1,team2)+c(team2,team1)
#
# Model 1: Double Poisson
ex4.m1<-lm.bp( g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=ex4.ita91)
#
# Models 2-5: bivariate Poisson models
ex4.m2<-lm.bp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, maxit=2)
ex4.m3<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1, data=ex4.ita91, maxit=2)
ex4.m4<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team2, data=ex4.ita91, maxit=2)
ex4.m5<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1+team2, data=ex4.ita91, maxit=2)
#
# Model 6: Zero Inflated Model
ex4.m6 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=0, maxit=2)
#
# Models 7-11: Diagonal Inflated Bivariate Poisson Models
ex4.m7 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution="geometric" , maxit=2)
ex4.m8 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=1, maxit=2)
ex4.m9 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=2, maxit=2)
ex4.m10<-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=3, maxit=2)
ex4.m11<-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution="poisson" , maxit=2)
#
# Models 12: Diagonal Inflated Double Poisson Model
ex4.m12 <- lm.dibp( g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution="poisson", 
                    zeroL3=TRUE , maxit=2)
# --------------------------------------------------------------------------

#
# --------------------------------------------------------------------------
# monitoring parameters for model 1: Dbl Poisson
ex4.m1$coef     # all parameters
ex4.m1$beta1    # model parameters for lambda1
ex4.m1$beta2    # model parameters for lambda2. 
                # All are the same as in beta1 except the intercept
ex4.m1$beta2[1] # Intercpept for lambda2. 
ex4.m1$beta2[1]-ex4.m1$beta2[2]	# estimated home effect

# estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m1$coef[ 2:18]) 
# estimating the effect for 18th level of defence(team2..team1) [Verona]
-sum(ex4.m1$coef[19:35]) 
#
# --------------------------------------------------------------------------
# monitoring parameters for model 2: BivPoisson(lamdba1,lambda2,constant lamdba3)
#
#
# monitoring parameters for model 1: Dbl Poisson
ex4.m2$beta1      # model parameters for lambda1
ex4.m2$beta2      # model parameters for lambda2. 
                  # All are the same as in beta1 except the intercept
ex4.m2$beta3      # model parameters for lambda3 (Here only the intercept)
ex4.m2$beta2[1]   # Intercpept for lambda2. 
ex4.m2$beta2[1]-ex4.m2$beta2[2]	# estimated home effect

# estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m2$coef[ 2:18]) 
# estimating the effect for 18th level of defence(team2..team1) [Verona]
-sum(ex4.m2$coef[19:35]) 
#
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# monitoring parameters for model 8: Biv.Poisson with Dis(1) diagonal distribution
#
#
# monitoring parameters for model 1: Dbl Poisson
ex4.m8$beta1      # model parameters for lambda1
ex4.m8$beta2      # model parameters for lambda2. 
                  # All are the same as in beta1 except the intercept
ex4.m8$beta3      # model parameters for lambda3. Here beta3 has only the intercept
ex4.m8$beta2[1]   # Intercpept for lambda2. 
ex4.m8$beta2[1]-ex4.m8$beta2[2]	# estimated home effect

# estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m8$coef[ 2:18]) 
# estimating the effect for 18th level of defence(team2..team1) [Verona]
-sum(ex4.m8$coef[19:35]) 

ex4.m8$beta3                 # parameters for lambda3 (here the intercept)
exp(ex4.m8$beta3)            # lambda3 (here constant)
ex4.m8$diagonal.distribution # printing details for the diagonal distribution
ex4.m8$p                     # mixing proportion
ex4.m8$theta                 # printing theta parameters


}

\keyword{datasets}
\keyword{models}
\keyword{regression}

