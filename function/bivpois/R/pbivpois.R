"pbivpois" <-
function(x, y=NULL, lambda = c(1, 1, 1), log=FALSE) {
# ------------------------------------------------------------------------------
# Karlis and Ntzoufras (2003, 2004)
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------
# x      : matrix or vector of length n
# y      : vector of length n. If x is matrix then it is not used
# lambda : parameters of the bivariate poisson distribution
# log    : argument controlling the calculation of the log-probability or the 
#          probability function. 
# ------------------------------------------------------------------------------
#	
	if ( is.matrix(x) ) {
		var1<-x[,1]
		var2<-x[,2]
	}
	else if (is.vector(x)&is.vector(y)){
		if (length(x)==length(y)){
			var1<-x
			var2<-y
		}
		else{
			stop('lengths of x and y are not equal')
		}	
	}
	else{
		stop('x is not a matrix or x and y are not vectors')
	}
	n <- length(var1)
 	logbp<-vector(length=n)
#
	for (k in 1:n){
		x0<-var1[k]
		y0<-var2[k]
		xymin<-min( x0,y0 )
		lambdaratio<-lambda[3]/(lambda[1]*lambda[2])
#	
		i<-0:xymin
		sums<- -lgamma(var1[k]-i+1)-lgamma(i+1)-lgamma(var2[k]-i+1)+i*log(lambdaratio)
		maxsums <- max(sums)
		sums<- sums - maxsums
		logsummation<- log( sum(exp(sums)) ) + maxsums 
		logbp[k]<- -sum(lambda) + var1[k] * log( lambda[1] ) + var2[k] * log( lambda[2] ) + logsummation 
	}
	if (log) { result<-    logbp }
	else     { result<-exp(logbp)  }
	result
#	end of function bivpois
}
