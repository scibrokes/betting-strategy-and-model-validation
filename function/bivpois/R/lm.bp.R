"lm.bp" <-
function( l1, l2, l1l2=NULL, l3=~1, data, common.intercept=FALSE, zeroL3=FALSE, maxit=300, pres=1e-8, verbose=getOption('verbose') )
#
#
{
options(warn=-1)
#
# definition of function call 
templist<-list( l1=l1, l2=l2, l1l2=l1l2, l3=l3, data=substitute(data), common.intercept=common.intercept, zeroL3=zeroL3, maxit=maxit, pres=pres, verbose=verbose)
tempcall<-as.call( c(expression(lm.bp), templist))
rm(templist)
#
#
# ------------------------------------------------------------------------------------
# Karlis and Ntzoufras (2003, 2004, 2005)
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------------
#
# l1       				: formula for the first  linear predictor (of lambda1)
# l2       				: formula for the second linear predictor (of lambda2)
# l1l2     				: formula for common variables on both lambda1 and lambda2
# l3       				: formula for the third first linear predictor/covariance parameter (lambda3)
# common.intercept: logical argument defining whether common intercept should be used for lamdba1,lambda2
#
# data     : data.frame which contains data {required arguement}
# zeroL3   : Logical argument controlling whether lambda3 is zero (DblPoisson) or not
# maxit    : maximum number of iterations 
# pres     : precision of the relative likelihood difference after which EM stops
# verbose	 : Logical argument controlling whether beta parameters will we 
#                printed while EM runs. Default value is taken options()$verbose value. 
# ------------------------------------------------------------------------------------
#
#
#
# set common or noncommon intercept
if (common.intercept){ formula1.terms<-'1'	}
else {formula1.terms<-'internal.data1$noncommon'	}
#
#
namex<-as.character(l1[2])
namey<-as.character(l2[2])
x<-data[,names(data)==namex]
y<-data[,names(data)==namey]
#
# Data length
n<-length(x)
lengthpvec<-1
#
#
#
# initial values
s<-rep(0,n)
like<-1:n*0
zero<- ( x==0 )|( y==0 )
if   (zeroL3) { lambda3<-rep(0,n) } 
else          { lambda3<-rep( max(0.1, cov(x,y,use='complete.obs')), n) } 
#
#
# form dataframes used
# data1 includes modelling on lambda1 and lambda2
# data2 includes modelling on lambda3 
# internal.data1 and internal.data2 are data frames used for additional internal variables
#
internal.data1<-data.frame( y1y2=c( x, y) )
internal.data2<-data.frame( y3 = rep(0, n ) )
#
p<-length(as.data.frame(data))
data1<-rbind(data, data) 
names(data1)<-names(data)
#
# removing x and y
data1<-data1[ , names(data1)!=namex]
data1<-data1[ , names(data1)!=namey]
#
#
# define full model
if (as.character(l1[3])=='.') { l1<-formula( paste( as.character(l1[2]), paste( names(data1),'',collapse='+',sep='' ), sep='~') 	)   }
if (as.character(l2[3])=='.') { l2<-formula( paste( as.character(l2[2]), paste( names(data1),'',collapse='+',sep='' ), sep='~')  )   }
if (as.character(l3[2])=='.') { l3<-formula( paste( '', paste( names(data1),'',collapse='+',sep='' ) , sep='~') )   }
#
# define the formula used for covariance term
formula2<-formula(paste('internal.data2$y3~',as.character(l3[2]),sep=''))
#
internal.data1$noncommon<- as.factor(c(1:n*0,1:n*0+1))
contrasts(internal.data1$noncommon)<-contr.treatment(2, base=1)
internal.data1$indct1<-c(1:n*0+1,1:n*0  )
internal.data1$indct2<-c(1:n*0  ,1:n*0+1)
#
#
if (!zeroL3){	
		data2<-data1[1:n,]
		names(data2)<-names(data1)
		}
#
#####
#
# add the common terms
#
if ( !is.null(l1l2) ) {
	formula1.terms<-paste( formula1.terms, as.character(l1l2[2]),sep='+')
	}
#
# add the special common terms (if any)
#
#
#
# in this section we identify non-common parameters 
# if a variable X is common in all formulas the we use term x*noncommon to include x+x:noncommon terms
# otherwise use I(internal.data1$indct1*x) to add sepererate parameter on lambda1
#
templ1<- labels(terms(l1))
#
# run this only if there are terms in l1 formula
if (length( templ1 )>0){ 
	for ( k1 in 1:length( templ1 ) ){
		if ( !is.null(l1l2) ) { checkvar1<-sum(labels(terms(l1l2))==templ1[k1] )==1	}
		else{ checkvar1<-FALSE }		
		checkvar2<-sum(labels(terms(l2))==templ1[k1] )==1
		if (checkvar1&checkvar2) {formula1.terms<-paste(formula1.terms, paste('internal.data1$noncommon*',templ1[k1],sep=''), sep='+')	}
		else{	
			formula1.terms<-paste(formula1.terms, paste('+I(internal.data1$indct1*',templ1[k1],sep=''), sep='')	
			formula1.terms<-paste(formula1.terms, ')',sep='')
			}
	}
}	
#
# if a variable X is not common st
# otherwise use I(internal.data1$indct1*x) to add sepererate parameter on lambda1
#
templ2<- labels(terms(l2))
#
# run this only if there are terms in l1 formula
if (length( templ2 )>0){ 
	for ( k1 in 1:length( templ2 ) ){
		if ( !is.null(l1l2) ) {checkvar1<-(sum(labels(terms(l1l2))==templ2[k1] )+sum(labels(terms(l1))==templ2[k1] ))!=2		}
		else{ checkvar1<-TRUE }		
		if ( checkvar1 ) { 
			formula1.terms<-paste(formula1.terms, paste('+I(internal.data1$indct2*',templ2[k1],sep=''), sep='')
			formula1.terms<-paste(formula1.terms, ')',sep='')
			}
	}
}
#
rm(templ1)
rm(templ2)
rm(Checkvar1)
rm(Checkvar2)
#
#
#
#
#
# 
# This bit creates labels for special terms of type c(x1,x2) used in l1l2 
#
#
formula1<-formula(paste('internal.data1$y1y2~',formula1.terms,sep=''))
tmpform1<-as.character(formula1[3])
newformula<-formula1
while( regexpr('c\\(',tmpform1) != -1)
{	
	temppos1<-regexpr('c\\(',tmpform1)[1]
	tempfor <-substring( tmpform1, first = temppos1+ 2 )
	temppos2<-regexpr('\\)' , tempfor)[1]
	tempvar <-substring( tempfor , first = 1,  last = temppos2-1 )
	temppos3<-regexpr(', ' , tempvar)[1]
	tempname1<-substring(tempfor    , first = 1,  last = temppos3-1 )
	tempname2<-substring(tempfor    , first = temppos3+2, last=temppos2-1)
	tempname2<-sub( '\\)','', tempname2  )
	tempvar1<-data[, names(data)==tempname1]
	tempvar2<-data[, names(data)==tempname2]
	data1$newvar1<-c(tempvar1, tempvar2)
#
	if( is.factor(tempvar1)& is.factor(tempvar2) ){
		data1$newvar1<-as.factor(data1$newvar1)	
		if (all(levels(tempvar1)==levels(tempvar2))){
			attributes(data1$newvar1)<-attributes(tempvar1)}
	}			
	tempvar<-sub( ', ' , '..',  tempvar )
	names(data1)[names(data1)=='newvar1']<-tempvar
	newformula<-sub( 'c\\(','', tmpform1  )
	newformula<-sub( '\\)','', newformula  )
	newformula<-sub( ', ' , '..', newformula  )
	tmpform1<-newformula
	formula1<-formula(paste('internal.data1$y1y2~',newformula,sep=''))
}
#####
rm(temppos1)
rm(temppos2)
rm(temppos3)
rm(tmpform1)
rm(tempfor)
rm(tempvar)
rm(tempvar1)
rm(tempvar2)
rm(tempname1)
rm(tempname2)
#
#
# Initial values for lambda
#
lambda<-glm(formula1,family=poisson, data=data1)$fitted
#
lambda1<-lambda[1:n]
lambda2<-lambda[(n+1):(2*n)]
#
difllike<-100.0
loglike0<-1000.0
i<-0
#
# fitting the Double Poisson Model
if (zeroL3) {
	#
	# fit the double Poisson model
	y0<-c(x,y)
	m<-glm( formula1, family=poisson, data=data1 )
	p3<-length(m$coef)
	beta<-m$coef	
# ----------------------------------------------------
#	creating names for parameters
#	
	names(beta)<-newnamesbeta( beta )
#
# 	end of name creations (l1, l2, l2-l1, blank)
#  -------------------------------------------------
	betaparameters<-splitbeta( beta )
#	
	lambda<-fitted(m)
	lambda1<-lambda[1:n]
	lambda2<-lambda[(n+1):(2*n)]
	like<-dpois(x, lambda1) * dpois( y, lambda2 )
	loglike<-sum(log(like))
#
#	calculation of BIC and AIC for bivpoisson model
	noparams<- m$rank 
	AIC<- -2*loglike + noparams * 2
	BIC<- -2*loglike + noparams * log(2*n)
#	
#		
#	Calculation of BIC, AIC of Poisson saturated model
	x.mean<-x
	x.mean[x==0]<-1e-12
	y.mean<-y
	y.mean[y==0]<-1e-12
	AIC.sat <-  sum(log( dpois( x , x.mean ) ) + log( dpois( y , y.mean ) ))
	BIC.sat <-  -2 * AIC.sat + (2*n)* log(2*n)
	AIC.sat <-  -2 * AIC.sat + (2*n)* 2
#
#
	AICtotal<-c(AIC.sat, AIC); 
	BICtotal<-c(BIC.sat, BIC );
	names(AICtotal)<-c('Saturated', 'DblPois')
	names(BICtotal)<-c('Saturated', 'DblPois')
#
#	putting all betas in one vector
	allbeta<-c(betaparameters$beta1,betaparameters$beta2)
	names(allbeta)<-c( paste( '(l1):', names(betaparameters$beta1), sep='' ),paste('(l2):', names(betaparameters$beta2), sep='' ) )

	result<-list(coefficients=allbeta, fitted.values=data.frame(x=m$fitted[1:n],y=m$fitted[(n+1):(2*n)]), 
	residuals=data.frame(x=x-m$fitted[1:n],y=y-m$fitted[(n+1):(2*n)]),	
	beta1=betaparameters$beta1, beta2=betaparameters$beta2, lambda1=m$fitted[1:n], lambda2=m$fitted[(n+1):(2*n)], lambda3=0, loglikelihood=loglike, iterations=1, parameters=noparams, AIC=AICtotal, BIC=BICtotal, call=tempcall)
}
else {	
	loglike<-rep(0,maxit)
	while ( (difllike>pres) && (i <= maxit) ) {
	i<-i+1
	#####   E step  ######
	for (j in 1:n) {
		if (zero[j]) {
			s[j]<-0.0;
			like[j]<- log(dpois(x[j], lambda1[j]))+log(dpois(y[j],lambda2[j])) 				  -lambda3[j];
			}
		else {
			lbp1<-pbivpois(x[j]-1, y[j]-1,lambda=c(lambda1[j],lambda2[j],lambda3[j]), log=TRUE);
			lbp2<-pbivpois(x[j]  , y[j]  ,lambda=c(lambda1[j],lambda2[j],lambda3[j]), log=TRUE);
#
			s[j]<-exp(log(lambda3[j])+lbp1-lbp2);
			like[j]<-lbp2;
		}
	}
	##### end of E step  ######
	x1<-x-s
	x2<-y-s
	
	x1[ (x1<0)&(x1>-1.0e-8)]<-0.00
	x2[ (x2<0)&(x2>-1.0e-8)]<-0.00

	loglike[i]<-sum(like)
	difllike<-abs( (loglike0-loglike[i])/loglike0 )
	loglike0<-loglike[i]
	#
	#
	#####   M step  ######
	#
	# 	fit model on lambda3
	internal.data2$y3<-s
	m0<-glm( formula2, family=poisson, data=data2 )
	beta3<-m0$coef
	lambda3<-m0$fitted
	#
	#	fit model on lambda1 & lambda2
	internal.data1$y1y2<-c(x1,x2)

	m<-glm( formula1, family=poisson, data=data1 )
	p3<-length(m$coef)
	beta<-m$coef
#	creating names for parameters
	names(beta)<-newnamesbeta( beta )
#	
#	

	lambda<-fitted(m)
	lambda1<-lambda[1:n]
	lambda2<-lambda[(n+1):(2*n)]
	#####   end of M step  ######
#
#	detailed or compressed printing during the EM iterations
	if (verbose) {
		printvector<-c( i, beta, beta3,loglike[i], difllike )
		names(printvector)<-c( 'iter', names(beta), paste('(l3):',names(beta3),sep=''), 'loglike', 'Rel.Dif.loglike')}
	else {
		printvector<-c( i, loglike[i], difllike )
		names(printvector)<-c( 'iter', 'loglike', 'Rel.Dif.loglike')}
#		
	lengthpvec<-length(printvector)	
	print.default( printvector, digits=4 )
	}
#
#	calculation of BIC and AIC for bivpoisson model
	noparams<- m$rank + m0$rank
	AIC<- -2*loglike[i] + noparams * 2
	BIC<- -2*loglike[i] + noparams * log(2*n)
#	
#		
#	Calculation of BIC, AIC of Poisson saturated model
	x.mean<-x
	x.mean[x==0]<-1e-12
	y.mean<-y
	y.mean[y==0]<-1e-12
	AIC.sat <-  sum(log( dpois( x , x.mean ) ) + log( dpois( y , y.mean ) ))
	BIC.sat <-  -2 * AIC.sat + (2*n)* log(2*n)
	AIC.sat <-  -2 * AIC.sat + (2*n)* 2
#
#
	AICtotal<-c(AIC.sat, AIC); 
	BICtotal<-c(BIC.sat, BIC );
	names(AICtotal)<-c('Saturated', 'BivPois')
	names(BICtotal)<-c('Saturated', 'BivPois')
#
#	spliting parameter vector
	betaparameters<-splitbeta( beta )
#
#	putting all betas in one vector
	allbeta<-c(betaparameters$beta1,betaparameters$beta2, beta3)
	names(allbeta)<-c( paste( '(l1):', names(betaparameters$beta1), sep='' ),paste('(l2):', names(betaparameters$beta2), sep='' ),paste('(l3):', names(beta3), sep='' ) )
#
#  Calculation of output 
	result<-list(coefficients=allbeta, fitted.values=data.frame(x=m$fitted[1:n]+lambda3,y=m$fitted[(n+1):(2*n)]+lambda3), 
	residuals=data.frame(x=x-m$fitted[1:n]-lambda3,y=y-m$fitted[(n+1):(2*n)]-lambda3),	
	beta1=betaparameters$beta1, beta2=betaparameters$beta2, beta3=beta3, lambda1=m$fitted[1:n], lambda2=m$fitted[(n+1):(2*n)], lambda3=lambda3, loglikelihood=loglike[1:i], parameters=noparams, AIC=AICtotal, BIC=BICtotal,iterations=i, call=tempcall )
#
#
} # end of elseif
options(warn=0)
#
class(result)<-c('lm.bp', 'lm')

result
#
#
}

