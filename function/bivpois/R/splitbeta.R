"splitbeta" <-
function( bvec ){
#
# ------------------------------------------------------------------------------------
# Karlis and Ntzoufras (2004)
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------------
#	Internal function for spliting beta parameters according to their interpretation
#
#	(c) June 2004 I. Ntzoufras, Chios, Greece
# ------------------------------------------------------------------------------------
#
	p3<-length(bvec)

	indx1<-grep( '\\(l1\\):', names(bvec) ) # identify parameters for lambda1
	indx2<-grep( '\\(l2\\):', names(bvec) ) # identify parameters for lambda2
	indx3<-grep( '\\(l2-l1\\):', names(bvec) ) # identify difference parameters for lambda2
#
#	create temporary labels to identify common parameters
	tempnames<-sub( '\\(l2-l1)\\:', 'k', names(bvec)  )
	tempnames<-sub( '\\(l2)\\:', 'k', tempnames  )
	tempnames<-sub( '\\(l1)\\:', 'k', tempnames  )

	indx4<-tempnames%in%names(bvec) # common parameters are identified as TRUE
#	
	beta1<-c(bvec[indx4],bvec[indx1])
	beta2<-c(bvec[indx4],bvec[indx3],bvec[indx2])
	indexbeta2<-c( rep(0,sum(indx4)), rep(1,length(indx3)), rep(2,length(indx2)) )

	names(beta1)<-sub('\\(l1\\):','',names(beta1))
	names(beta2)<-sub('\\(l2\\):','',names(beta2))
	names(beta2)<-sub('\\(l2-l1\\):','',names(beta2))

	beta1<-beta1[order(names(beta1))]
	indexbeta2<-indexbeta2[order(names(beta2))]
	beta2<-beta2[order(names(beta2))]
	ii<-1:length(beta2)
	ii<-ii[indexbeta2==0]
	for ( i in ii )	{
#		beta2[i]<-sum( beta2[ grep( names(beta2)[i], names(beta2) ) ] )
		beta2[i]<-sum( beta2[ names(beta2)[i]==names(beta2) ] )
	}
	beta2<-beta2[indexbeta2%in%c(0,2)]

	btemp<-list(beta1=beta1,beta2=beta2)
	btemp
}

