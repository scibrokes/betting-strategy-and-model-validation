# --------------------------------------------
# COMMANDS FOR FITTING CONSTANT MODEL FOR EX1
# SECTION 4.1.1
# --------------------------------------------

library(bivpois)       # load bivpois library
data(ex1.sim)          # load data of example 1
# -------------------------------------------------------------------------------

xtemp<-readline(prompt = "Press Enter to Continue")

# Simple Bivariate Poisson Model
ex1.simple<-simple.bp( ex1.sim$x, ex1.sim$y ) # fit simple model of section 4.1.1

names(ex1.simple)      # monitor output variables
ex1.simple$lambda      # view lambda1 
ex1.simple$BIC         # view BIC
ex1.simple             # view all results of the model
xtemp<-readline(prompt = "Press Enter to Continue")
#
# plot of loglikelihood vs. iterations
win.graph()
plot( 1:ex1.simple$iterations, ex1.simple$loglikelihood, xlab='Iterations',ylab='Log-likelihood', type='l' )

# -------------------------------------
# COMMANDS FOR FITTING MODELS FOR EX1
# SECTION 4.1.2
# -------------------------------------
ex1.m2<-lm.bp(x~1 , y~1 , data=ex1.sim, zeroL3=TRUE)			# Model 2: DblPoisson(l1, l2)
ex1.m3<-lm.bp(x~1 , y~1 , data=ex1.sim)					# Model 3: BivPoisson(l1, l2, l3)
ex1.m4<-lm.bp(x~. , y~. , data=ex1.sim, zeroL3=TRUE)			# Model 4: DblPoisson (l1=Full, l2=Full) 
ex1.m5<-lm.bp(x~. , y~. , data=ex1.sim)					# Model 5: BivPoisson(l1=full, l2=full, l3=constant)
ex1.m6<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, data=ex1.sim, zeroL3=TRUE)	# Model 6: DblPois(l1,l2)
ex1.m7<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, data=ex1.sim)			# Model 7: BivPois(l1,l2,l3=constant)
ex1.m8<-lm.bp(x~. , y~. , l3=~., data=ex1.sim)				# Model 8: BivPoisson(l1=full, l2=full, l3=full)
ex1.m9<-lm.bp(x~. , y~. , l3=~.-z5, data=ex1.sim)			# Model 9: BivPoisson(l1=full, l2=full, l3=z1+z2+z3+z4)
ex1.m10<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, l3=~., data=ex1.sim)		# Model 10: BivPoisson(l1, l2, l3=full)
ex1.m11<-lm.bp(x~z1 , y~z1+z5 , l1l2=~z3, l3=~.-z5, data=ex1.sim)	# Model 11: BivPoisson(l1, l2, l3= z1+z2+z3+z4)
ex1.m11$coef  # monitor all beta parameters of model 11
#
ex1.m11$beta1 # monitor all beta parameters of lambda1 of model 11
ex1.m11$beta2 # monitor all beta parameters of lambda2 of model 11
ex1.m11$beta3 # monitor all beta parameters of lambda3 of model 11


