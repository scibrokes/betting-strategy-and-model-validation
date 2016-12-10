"newnamesbeta" <-
function( bvec ) {
#
# ------------------------------------------------------------------------------------
# Karlis and Ntzoufras (2004)
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------------
#	Internal function for renaming parameters according to their interpretation
#
#	(c) June 2004 I. Ntzoufras, Chios, Greece
#	(c) Revised on May 2005 by I. Ntzoufras, Athens, Greece	
# ------------------------------------------------------------------------------------
#
#	(l1): is parameter rererring to the log(lamdba1)
#	(l2): is parameter rererring to the log(lamdba2)
#	(l2-l1): this parameter is added to the current parameter of l1
#	no label: the parameter is common for both log(lambda1) and log(lambda2)
# ------------------------------------------------------------------------------------

	names(bvec)<-sub('\\)','',names(bvec)) 							#remove right parenthesis

	names(bvec)<-sub('\\(Intercept','(Intercept)',names(bvec)) 				# replace "(Intercept" with "(Intercept)"
	names(bvec)[pmatch('internal.data1$noncommon2',names(bvec))]<-'(l2-l1):(Intercept)'	# replace 'internal.data1$noncommon2' with 'l2-l1' for intercept
	names(bvec)<-sub('internal.data1\\$noncommon2:','(l2-l1):',names(bvec))			# the same for the rest of parameters
	names(bvec)<-sub('internal.data1\\$noncommon0:','(l1):',names(bvec))			# replace 'internal.data1\\$noncommon0:' by '(l1)'
	names(bvec)<-sub('internal.data1\\$noncommon1:','(l2):',names(bvec))			# replace 'internal.data1\\$noncommon1:' by '(l2)'
	
	names(bvec)<-sub(':internal.data1\\$noncommon2','(l2-l1):',names(bvec))			# same as above with ":" in front of expressions
	names(bvec)<-sub(':internal.data1\\$noncommon0','(l1):',names(bvec))
	names(bvec)<-sub(':internal.data1\\$noncommon1','(l2):',names(bvec))
	
	names(bvec)<-sub('I\\(internal.data1\\$indct1 \\* ','(l1):',names(bvec))		# replace 'I(internal.data1$indct1 * ' with '(l1):'
	names(bvec)<-sub('I\\(internal.data1\\$indct2 \\* ','(l2):',names(bvec))		# replace 'I(internal.data1$indct2 * ' with '(l2):'
	
	names(bvec)
}

