"lm.dibp" <-
function
( l1, l2, l1l2=NULL, l3=~1, data, common.intercept=FALSE, zeroL3=FALSE, distribution='discrete', jmax=2,maxit=300, pres=1e-8, verbose=getOption('verbose') )
{
options(warn=-1)
#
# definition of function call 
templist<-list( l1=l1, l2=l2, l1l2=l1l2, l3=l3, data=substitute(data), common.intercept=common.intercept, zeroL3=zeroL3, distribution=distribution, jmax=jmax, maxit=maxit, pres=pres, verbose=verbose)
tempcall<-as.call( c(expression(lm.dibp), templist))
rm(templist)
#
#
#
# ------------------------------------------------------------------------------------
# Karlis and Ntzoufras (2003, 2004, 2005)
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------------
#
# PARAMETERS COMMON WITH lm.bp
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
#
# PARAMETERS ADDITIONAL TO lm.bp
# distribution : Selection of diagonal inflation distribution. 
#                Three choices are available: 
#                ='discrete'  : Discrete, jmax is the number of diagonal elements [0,1,...,]
#                ='poisson'   : Poisson with mean theta.  
#                ='geometrics': Geometric with success probability theta. 
#                Default is DISCRETE(2). theta[1] and theta[2] stand for theta_1, theta_2
#                                        while theta_0=1-theta[1]-theta[2].
# jmax         : Used only for DISCRETE diagonal distribution (distribution='discrete').
#                Indicates the number of parameters of the DISCRETE distribution.
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
#
# Data length
n<-length(x)
lengthprintvec<-1
#
#
#
# definition of diagonal inflated distribution
	maxy<-max(c(x,y))
#
#  changing distribution to codes 1,2,3	
	dist<-distribution
	if      ( charmatch( dist, 'poisson'  , nomatch=0) ==1 ) {distribution<-2} 
	else if ( charmatch( dist, 'geometric', nomatch=0) ==1 ) {distribution<-3}	
	else if ( charmatch( dist, 'discrete' , nomatch=0) ==1 ) {distribution<-1}
	if ( distribution==1 ){ 
			dilabel<-paste('Inflation Distribution: Discrete with J=',jmax)
			if (jmax==0) {theta<-0}
			else         { theta<-1:jmax*0+1/(jmax+1) }
			di.f<-function (x, theta){
			JMAX<-length(theta)
			if      (x>JMAX) { res<-0 }
			else if (x==0)   { res<-1-sum(theta) }
			else             { res<-theta[x] } 
			res
			}
	}
	else if ( distribution==2 ){ 
			dilabel<-'Inflation Distribution: Poisson'
			theta<-1.0;
			di.f<-function (x, theta){
							if (theta>0) { res<-dpois( x, theta ) }
							else {
								if (x==0) { res<-1}
								else {res<-1e-12}
							}
			}
	}								 
	else if ( distribution==3 ){ 
			dilabel<-'Inflation Distribution: Geometric'
			theta<-0.5;
			di.f<-function (x, theta){
							if (theta>0) { 
								if(theta==1) {theta<-0.9999999}
								res<-dgeom( x, theta ) }
							else if (theta==1){
								if (x==0) { res<-1}
								else {res<-1e-12}
							}
							else {res<-1e-12}
					 }
	}
	else {
		stop(paste(distribution, 'Not known distribution.', sep=': '))
	}
# ------
# setting up data frames, vectors and data
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
# ------
# initial values for parameters
prob<-0.20
s<-rep(0,n)
vi<-1:n*0
v1<-1-c(vi,vi)
like<-1:n*0
zero<- ( x==0 )|( y==0 )
if   (zeroL3) { lambda3<-rep(0,n) } 
else          { lambda3<-rep( max(0.1, cov(x,y,use='complete.obs')), n) } 
#
#
#
#
# Initial values for lambda

internal.data1$v1<-1-c(vi,vi);

lambda<-glm( formula1, family=poisson, data=data1, weights=internal.data1$v1, maxit=100)$fitted
#
lambda1<-lambda[1:n]
lambda2<-lambda[(n+1):(2*n)]
#
difllike<-100.0
loglike0<-1000.0
i<-0
ii<-0

if (zeroL3) {
	#
	# fit double poisson diagonal inflated model
	loglike<-rep(0, maxit)
	lambda3<-1:n*0
	while ( (difllike>pres) && (i <= maxit) ) {
	i<-i+1
	#####   E step  ######
	for (j in 1:n) {
		if (zero[j]) {
			s[j]<-0;		
#			calculation of log-likelihood			
			if (x[j]==y[j]) {
						density.di<-di.f( 0.0, theta )
						like[j]<-log( (1-prob)*exp(-lambda1[j]-lambda2[j])+prob*density.di );
						vi[j]<-prob*density.di*exp(-like[j]) }
			else{
			      like[j]<-log(1-prob)+log(dpois(x[j],lambda1[j]))+log(dpois(y[j],lambda2[j]));
						vi[j]<-0.0 ;}
		}			
		else {
			if (x[j]==y[j]) {
					density.di<-di.f( x[j],theta );
					like[j]<-log( (1-prob)*dpois( x[j],lambda1[j] )*dpois( y[j],lambda2[j] ) + prob*density.di );
					vi[j]  <- prob*density.di*exp( -like[j] ) }
			else	{
					vi[j]<-0.0;
					like[j]<-log(1-prob)+log( dpois(x[j],lambda1[j])*dpois(y[j],lambda2[j]) )}
		}
	}
#### end of E-step #########
	x1<-x;
	x2<-y;
	loglike[i]<-sum( like ) ; 
	difllike<-abs( (loglike0-loglike[i])/loglike0 )
	loglike0<-loglike[i]
	#
	#
########### M-step ############
	# estimate mixing proportion
	prob<-sum(vi)/n
	#
	# maximization of each theta parameter
	if ( distribution == 1 ) {
		# calculation of theta_j, j=1,...,jmax ; theta_0=1-sum(theta)
		if (jmax==0) { theta<-0 }
		else {
			for (ii in 1:jmax) {
				temp<-as.numeric(( (x==ii) & (y==ii) ));
				theta[ii]<-sum(temp*vi)/sum(vi) 
				}
			}
	}			
	else if (distribution==2){		
			# calculation of theta for poisson diagonal inflation
			theta<- sum(vi*x)/sum(vi) }
	else if (distribution==3){		
			# calculation of theta for geometric diagonal inflation
			theta<- sum(vi)/( sum(vi*x)+sum(vi) ) }
	#
	#	fit model on lambda1 & lambda2
	#
	internal.data1$v1<- 1-c(vi,vi);
	internal.data1$v1[ (internal.data1$v1<0)&(internal.data1$v1>-1.0e-10) ]<-0.0
#	
	x1[(x1<0)&(x1>-1.0e-10)]<-0.0
	x2[(x2<0)&(x2>-1.0e-10)]<-0.0
	internal.data1$y1y2<-c(x1,x2)
	m<-glm( formula1, family=poisson, data=data1, weights=internal.data1$v1 , maxit=100)

	p3<-length(m$coef)
	beta<-m$coef 
# ----------------------------------------------------
#	creating names for parameters
	names(beta)<-newnamesbeta( beta )
#
# 	end of name creations (l1, l2, l2-l1, blank)
#  -------------------------------------------------
	betaparameters<-splitbeta( beta )
#	
	lambda<-fitted(m)
	lambda1<-lambda[1:n]
	lambda2<-lambda[(n+1):(2*n)]
	#
	#####   end of M step  ######
#
#  printing also beta
	if (verbose) {
		printvec<- c( i,beta,100.0*prob, theta,  loglike[i], difllike );	
		names(printvec)<-c( 'iter', names(beta),  'Mix.p(%)', paste( 'theta', 1:length(theta),sep='' ), 'loglike', 'Rel.Dif.loglike')
		}
#  limited print out
	else {		
		printvec<- c( i, 100.0*prob, theta,  loglike[i], difllike );	
		names(printvec)<-c( 'iter','Mix.p(%)', paste( 'theta', 1:length(theta),sep='' ), 'loglike', 'Rel.Dif.loglike')
	}
	lengthprintvec<-length(printvec)
	print.default( printvec, digits=4 )
	}

#
#	calculation of BIC and AIC for double poisson model
	if ( (distribution==1)&&(jmax==0) ){noparams<- m$rank +1}
	else                              {noparams<- m$rank + length( theta ) +1}
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
	names(AICtotal)<-c('Saturated', 'DblPois')
	names(BICtotal)<-c('Saturated', 'DblPois')
#
	allbeta<-c(betaparameters$beta1,betaparameters$beta2)
	names(allbeta)<-c( paste( '(l1):', names(betaparameters$beta1), sep='' ),paste('(l2):', names(betaparameters$beta2), sep='' ) )
	allparameters<-c(allbeta, prob, theta)
	if (distribution==1){ names(allparameters)<-c( names(allbeta), 'p', paste('theta', 1:length(theta),sep='') ) }
	else {names(allparameters)<-c( names(allbeta), 'p', 'theta') }
#
# 	calculation of fitted values
#  ----------------------------
	fittedval1<-(1-prob)*m$fitted[1:n]
	fittedval2<-(1-prob)*m$fitted[(n+1):(2*n)]
#  	
	meandiag<-0
	if ((distribution==1)&&(jmax>0)) { meandiag<-sum( theta[1:jmax]*1:jmax ) }
	else if (distribution==2) { meandiag<-theta }
	else if (distribution==3) { meandiag<- (1-theta)/theta }
#	
	fittedval1[x==y]<-prob*meandiag + fittedval1[x==y]
	fittedval2[x==y]<-prob*meandiag + fittedval2[x==y]
#
	result<-list(coefficients=allparameters, 	
					fitted.values=data.frame(x=fittedval1,y=fittedval2), residuals=data.frame(x=x-fittedval1,y=y-fittedval2),	
					beta1=betaparameters$beta1, beta2=betaparameters$beta2, p=prob, theta=theta, diagonal.distribution=dilabel,
					lambda1=m$fitted[1:n], lambda2=m$fitted[(n+1):(2*n)], loglikelihood=loglike[1:i], parameters=noparams, AIC=AICtotal, 	
					BIC=BICtotal,iterations=i , call=tempcall)
#
#
# end of diagonal inflated double poisson model
}
else {	
	loglike<-rep(0,maxit)
	while ( (difllike>pres) && (i <= maxit) ) {
	i<-i+1
	#####   E step  ######
	for (j in 1:n) {
		if (zero[j]) {
			s[j]<-0;		
#			calculation of log-likelihood			
			if (x[j]==y[j]) {
				density.di<-di.f( 0.0, theta )
				like[j]<- log( (1-prob)*exp(-lambda1[j]-lambda2[j]-lambda3[j])+prob*density.di );
				vi[j]<-prob*density.di*exp(-like[j]) }
			else{
			        like[j]<-log(1-prob)-lambda3[j] +log(dpois(x[j],lambda1[j])) 									+log(dpois(y[j],lambda2[j]));
				vi[j]<-0.0 ;}
		}			
		else {
			lbp1<-pbivpois(x[j]-1, y[j]-1, lambda=c(lambda1[j],lambda2[j],lambda3[j]),  log=TRUE );
			lbp2<-pbivpois(x[j]  , y[j]  , lambda=c(lambda1[j],lambda2[j],lambda3[j]), log=TRUE );	
			s[j]<-exp( log(lambda3[j]) + lbp1 - lbp2 );
#			like[j]<-lbp2;
			if (x[j]==y[j]) {
					density.di<-di.f( x[j],theta );
					like[j]<-log( (1-prob)*exp(lbp2) + prob*density.di );
					vi[j]  <- prob*density.di*exp( -like[j] ) }
			else	{
					vi[j]<-0.0;
					like[j]<-log(1-prob)+lbp2 }
		}
	}
#### end of E-step #########
	x1<-x-s;
	x2<-y-s;
	loglike[i]<-sum( like ) ; 
	difllike<-abs( (loglike0-loglike[i])/loglike0 )
	loglike0<-loglike[i]
	#
	#
########### M-step ############
	# estimate mixing proportion
	prob<-sum(vi)/n
	#
	# maximization of each theta parameter
	if ( distribution == 1 ) {
		# calculation of theta_j, j=1,...,jmax ; theta_0=1-sum(theta)
#		cat (c('1:discrete, jmax=', jmax), '\n')
		if (jmax==0){ theta<-0}
		else{
			for (ii in 1:jmax) {
				temp<-as.numeric(( (x==ii) & (y==ii) ));
				theta[ii]<-sum(temp*vi)/sum(vi) 
#				print( c(ii, sum(temp), sum(vi), sum(temp*vi) ) )
				}
#				cat( c('2:discrete, jmax=', jmax), '\n')
			}
	}			
	else if (distribution==2){		
			# calculation of theta for poisson diagonal inflation
			theta<- sum(vi*x)/sum(vi) }
	else if (distribution==3){		
#	else {		
			# calculation of theta for geometric diagonal inflation
			theta<- sum(vi)/( sum(vi*x)+sum(vi) ) }
	# 	fit model on lambda3
	internal.data2$v1<- 1-vi;
	internal.data2$v1[ (internal.data2$v1<0)&(internal.data2$v1>-1.0e-10) ]<-0.0
#
	internal.data2$y3<-s;
	m0<-glm( formula2, family=poisson, data=data2, weights=internal.data2$v1 , maxit=100)
	beta3<-m0$coef
	lambda3<-m0$fitted
	#
	#	fit model on lambda1 & lambda2
	internal.data1$v1<- 1-c(vi,vi);
	internal.data1$v1[ (internal.data1$v1<0)&(internal.data1$v1>-1.0e-10) ]<-0.0
#
	x1[(x1<0)&(x1>-1.0e-10)]<-0.0
	x2[(x2<0)&(x2>-1.0e-10)]<-0.0
	internal.data1$y1y2<-c(x1,x2)
	m<-glm( formula1, family=poisson, data=data1, weights=internal.data1$v1 , maxit=100)
	p3<-length(m$coef)
	beta<-m$coef
#  ----	
#	creating names for parameters
	names(beta)<-newnamesbeta( beta )
#  ----	
	lambda<-fitted(m)
	lambda1<-lambda[1:n]
	lambda2<-lambda[(n+1):(2*n)]
	#
	#####   end of M step  ######
#
#	print all parameters including beta
	if (verbose) {
		printvec<- c( i,beta,beta3,100.0*prob, theta,  loglike[i], difllike );	
		names(printvec)<-c( 'iter', names(beta),paste('l3_',names(beta3),sep=''), 'Mix.p(%)', paste( 'theta', 1:length(theta),sep='' ), 'loglike', 'Rel.Dif.loglike')
		}
#
#  limited print out
	else {		
		printvec<- c( i, 100.0*prob, theta,  loglike[i], difllike );	
		names(printvec)<-c( 'iter', 'Mix.p(%)', paste( 'theta', 1:length(theta),sep='' ), 'loglike', 'Rel.Dif.loglike')
		}
#
	lengthprintvec<-length(printvec)
	print.default( printvec, digits=4 )
	}
#
#	calculation of BIC and AIC for bivpoisson model
	if ( (distribution==1)&&(jmax==0) ){noparams<- m$rank + m0$rank + 1}
	else                               {noparams<- m$rank + m0$rank + length( theta ) +1}
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
#  -------
#
#	spliting parameter vector
	betaparameters<-splitbeta( beta )
#
#	putting all betas in one vector
	allbeta<-c(betaparameters$beta1,betaparameters$beta2, beta3)
	names(allbeta)<-c( paste( '(l1):', names(betaparameters$beta1), sep='' ),paste('(l2):', names(betaparameters$beta2), sep='' ),paste('(l3):', names(beta3), sep='' ) )
	allparameters<-c(allbeta, prob, theta)
	if (distribution==1){ names(allparameters)<-c( names(allbeta), 'p', paste('theta', 1:length(theta),sep='') ) }
	else {names(allparameters)<-c( names(allbeta), 'p', 'theta') }
#
# 	calculation of fitted values
#  ----------------------------
	fittedval1<-(1-prob)*(m$fitted[1:n]         + lambda3)
	fittedval2<-(1-prob)*(m$fitted[(n+1):(2*n)] + lambda3)
#  	
	meandiag<-0
	if ((distribution==1)&&(jmax>0)) { meandiag<-sum( theta[1:jmax]*1:jmax ) }
	else if (distribution==2) { meandiag<-theta }
	else if (distribution==3) { meandiag<- (1-theta)/theta }
#	
	fittedval1[x==y]<-prob*meandiag + fittedval1[x==y]
	fittedval2[x==y]<-prob*meandiag + fittedval2[x==y]
#
#
#	saving output
	result<-list(coefficients=allparameters, fitted.values=data.frame(x=fittedval1,y=fittedval2), residuals=data.frame(x=x-fittedval1,y=y-fittedval2),	
			beta1=betaparameters$beta1, beta2=betaparameters$beta2, beta3=beta3,  p=prob, theta=theta, diagonal.distribution=dilabel,
			lambda1=m$fitted[1:n], lambda2=m$fitted[(n+1):(2*n)], lambda3=lambda3, loglikelihood=loglike[1:i], 
			parameters=noparams, AIC=AICtotal, 	BIC=BICtotal,iterations=i , call=tempcall)
#
} # end of elseif
#
options(warn=0)
class(result)<-c('lm.dibp', 'lm')
#
result
#
#
}

