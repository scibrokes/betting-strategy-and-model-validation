"simple.bp" <-
function(x, y, ini3=1.0, maxit=300, pres=1e-8)
{
#
# ------------------------------------------------------------------------------
# Karlis and Ntzoufras (2003, 2004)
# (last revision 25/8/2005)
# Athens University of Economics and Business
#
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------
#
# x       : matrix or vector of length n
# y       : vector of length n. If x is matrix then it is not used
# ini3    : initial value for lambda3
# maxit   : maximum number of iterations 
# pres    : precision of the relative likelihood difference after which EM stops
# ------------------------------------------------------------------------------
# Data length
#
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

#
#
#
n<-length(var1)
#
# initial values
s<-rep(0,n)
like<-1:n*0
zero<- ( var1==0 )|( var2==0 )
#
#
#
# Initial values for lambda

lambda3<- ini3
lambda1<- max( 0.1, mean(var1)-lambda3 )
lambda2<- max( 0.1, mean(var2)-lambda3 )
#
#
difllike<-1000.0
loglike0<-1000.0
i<-0
loglike<-rep(0,maxit)
while ( (difllike>pres) && (i <= maxit) ) {
	i<-i+1
	#####   E step  ######
	for (j in 1:n) {
		if (zero[j]) {
			s[j]<-0;
			like[j]<- log(dpois(var1[j], lambda1)) + log(dpois(var2[j], lambda2))-lambda3;
			}
		else {
			lbp1<- pbivpois( var1[j]-1, var2[j]-1, lambda=c(lambda1,lambda2,lambda3), log=TRUE );
			lbp2<- pbivpois( var1[j]  , var2[j]  , lambda=c(lambda1,lambda2,lambda3) , log=TRUE );
			
			s[j]<-exp( log(lambda3) + lbp1 - lbp2 );
			like[j]<-lbp2;
		}
	}
	##### end of E step  ######
	x1<-var1-s
	x2<-var2-s
	loglike[i]<-sum(like)
	difllike<-abs( (loglike0-loglike[i])/loglike0 )
	loglike0<-loglike[i]
	#
	#
	#####   M step  ######
	#
	# 	fit model on lambda3
	lambda1<-mean(x1)
	lambda2<-mean(x2)
	lambda3<-mean(s)
	#####   end of M step  ######
	printpars<-c(i,lambda1, lambda2, lambda3, loglike[i] )
	names(printpars)<-c('Iter.', 'lambda1', 'lambda2', 'lambda3','loglike' )
	print( round(printpars ,3 ) )
	cat( 'Relative Difference in Loglike:', difllike, '\n' ) 
	}
#
#	calculation of BIC and AIC of Bivariate Poisson model
	noparams<- 3 
	AIC<- -2*loglike[i] + noparams * 2
	BIC<- -2*loglike[i] + noparams * log(2*n)
#	
#		
#	Calculation of BIC, AIC of Poisson saturated model
	x.mean<-var1
	x.mean[var1==0]<-1e-12
	y.mean<-var2
	y.mean[var2==0]<-1e-12
	AIC.sat <-  sum( log( dpois( var1 , x.mean ) ) + log( dpois(var2 , y.mean) ) )
	BIC.sat <-  -2 * AIC.sat + (2*n)* log(2*n)
	AIC.sat <-  -2 * AIC.sat + (2*n)* 2
#
#		
#	Calculation of BIC, AIC of simple Poisson model
	x.mean<-mean(var1)
	y.mean<-mean(var2)
	AIC.pois <-  sum(log( dpois( var1 , x.mean ) ) + log( dpois( var2 , y.mean ) ))
	BIC.pois <-  -2 * AIC.pois + 2* log(2*n)
	AIC.pois <-  -2 * AIC.pois + 2* 2
	
	AICtotal<-c(AIC.sat, AIC.pois, AIC)
	BICtotal<-c(BIC.sat, BIC.pois, BIC )
	
	names(AICtotal)<- c( 'Saturated', 'DblPois', 'BivPois' )
	names(BICtotal)<- c( 'Saturated', 'DblPois', 'BivPois' )
#
# Calculation of fitted values
	result<-list(lambda=c(lambda1, lambda2, lambda3),loglikelihood=loglike[1:i],
			parameters=noparams, AIC=AICtotal, BIC=BICtotal ,iterations=i )
#
result
#
#
}

