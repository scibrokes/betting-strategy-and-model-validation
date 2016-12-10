\name{ex3.health}
\alias{ex3.health}
\docType{data}
\title{Bivpois Example 3 Dataset: Health Care Data}
\description{
Demand for health care in Australia data (Cameron and Trivedi, 1986). The data refer to the Australian Health survey for 1977-1978 with sample size equal to 5190. 
}
\usage{data(ex3.health)}
\format{
  A data frame with 5190 observations on the following 20 variables.
  \describe{
    \item{doctorco}{Number of consultations with a doctor or specialist  in the past 2 weeks}
    \item{prescrib}{Total number of prescribed medications used in past 2 days}
    \item{sex}{1 if female, 0 if male}
    \item{age}{Age in years divided by 100 (measured as mid-point of 10 age groups from 15-19 years to 65-69 with 70 or more coded treated as 72)
}
    \item{agesq}{AGE squared}
    \item{income}{Annual income in Australian dollars divided by 1000  (measured as mid-point of coded ranges Nil, <200, 200-1000, 1001-, 2001-, 3001-, 4001-, 5001-, 6001-, 7001-, 8001-10000, 10001-12000, 12001-14000, with 14001- treated as 15000 ).
}
    \item{levyplus}{1 if covered by private health insurance fund for private patient in public hospital (with doctor of choice), 0 otherwise
}
    \item{freepoor}{1 if covered by government because low income, recent immigrant, unemployed, 0 otherwise
}
    \item{freepera}{1 if covered by government because low income, recent immigrant, unemployed, 0 otherwise
}
    \item{illness}{Number of illnesses in past 2 weeks with 5 or more coded as 5
}
    \item{actdays}{Number of days of reduced activity in past two weeks due to illness or injury
}
    \item{hscore}{General health questionnaire score using Goldberg's method. High score indicates bad health.
}
    \item{chcond1}{1 if chronic condition(s) but not limited in activity, 0 otherwise}
    \item{chcond2}{1 if chronic condition(s) and limited in activity, 0 otherwise}
    \item{nondocco}{Number of consultations with non-doctor health professionals (chemist, optician, physiotherapist, social worker, district community nurse, chiropodist or chiropractor) in the past 2 weeks}
    \item{hospadmi}{Number of admissions to a hospital, psychiatric hospital, nursing or convalescent home in the past 12 months (up to 5 or more admissions which is coded as 5)}
    \item{hospdays}{Number of nights in a hospital, etc. during most recent admission: taken, where appropriate, as the mid-point of the intervals 1, 2, 3, 4, 5, 6, 7, 8-14, 15-30, 31-60, 61-79 with 80 or more admissions coded as 80. If no admission in past 12 months then equals zero.}
    \item{medicine}{Total number of prescribed and nonprescribed medications used in past 2 days}
    \item{nonpresc}{Total number of nonprescribed medications used in past 2 days}
    \item{constant}{Constant term}
  }
}
\details{
Details can be found in Cameron and Trivedi (1986). This data is used as example three in Karlis and Ntzoufras (2005). In this illustration two variables are used as response: the number of consultations with a doctor or a specialist and the total number of prescribed medications used in past 2 days (doctorco, prescrib). Three variables have been used as covariates: the gender (1 female, 0 male), the age in years divided by 100 (measured as midpoints of age groups) and the annual income in Australian dollars divided by 1000 (measured as midpoint of coded ranges) {sex, age, income}.
}
\source{
  Cameron, A.C.  and Trivedi, P.K. (1986).  Econometric Models Based on Count Data: Comparisons and Applications of  Some Estimators and Tests, Journal of Applied Econometrics, 1, 29 - 54.
}
\references{
1.			Cameron, A.C.,  Trivedi, P.K., Milne, F. and Piggott, J. (1988). A Microeconometric Model of the Demand for Health Care and Health Insurance in Australia, Review of Economic Studies, 55, 85 - 106.

2.      Cameron, A.C.  and Trivedi, P.K. (1993). Tests of Independence in Parametric Models with Applications and Illustrations, Journal of Business & Economics Statistics, 11, 29 - 43.

3.      Karlis, D. and Ntzoufras, I. (2005). Bivariate Poisson and Diagonal Inflated Bivariate Poisson Regression Models in R. Journal of Statistical Software (to appear). 

4.      Karlis, D. and Ntzoufras, I. (2003). Analysis of Sports Data Using Bivariate Poisson Models. Journal of the Royal Statistical Society, D, (Statistician), 52, 381 - 393.
}
\examples{
#  Models of example 3 can be fitted using the command
#  demo(ex3, package='bivpois')
#
#  Here we present the commands for the same models commented out in order to save time
#
#library(bivpois)
#data(ex3.health)
# Bivariate Poisson models
#ex3.model.a<-lm.bp(doctorco~sex+age+income, prescrib~sex+age+income, 
#                   data=ex3.health) 
#ex3.model.b<-lm.bp(doctorco~sex+age+income, prescrib~sex+age+income, l3=~sex, 
#                   data=ex3.health) 
# Double Poisson model
#ex3.model.c<-lm.bp(doctorco~sex+age+income, prescrib~sex+age+income,
#                   data=ex3.health, zeroL3=TRUE) 
# 
# diagonal inflated models
#ex3.dibp.a<-lm.dibp(doctorco~sex+age+income, prescrib~sex+age+income, 
#                    data=ex3.health, jmax=0)                         # model (a)
#ex3.dibp.b<-lm.dibp(doctorco~sex+age+income, prescrib~sex+age+income,l3=~sex, 
#                    data=ex3.health, jmax=0)                         # model (b)
}

\keyword{datasets}
\keyword{models}
\keyword{regression}

