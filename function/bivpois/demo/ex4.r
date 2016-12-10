

library(bivpois) # loading of bivpois library
data(ex4.ita91)  # loading ex4.ita91 data from bivpois library
#
# formula for modeling of lambda1 and lambda2
form1 <- ~c(team1,team2)+c(team2,team1)
xtemp<-readline(prompt = "Press Enter to Continue")
#
# Model 1: Double Poisson
ex4.m1<-lm.bp( g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=ex4.ita91)
#
# Models 2-5: bivariate Poisson models
ex4.m2<-lm.bp(g1~1,g2~1, l1l2=form1, data=ex4.ita91)
ex4.m3<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1, data=ex4.ita91)
ex4.m4<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team2, data=ex4.ita91)
ex4.m5<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1+team2, data=ex4.ita91)
#
# Model 6: Zero Inflated Model
ex4.m6 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=0)
#
# Models 7-11: Diagonal Inflated Bivariate Poisson Models
ex4.m7 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution='geometric' )
ex4.m8 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=1)
ex4.m9 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=2)
ex4.m10<-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=3)
ex4.m11<-lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution='poisson' )
#
# Models 12: Diagonal Inflated Double Poisson Model
ex4.m12 <- lm.dibp( g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution='poisson', zeroL3=TRUE )
# --------------------------------------------------------------------------

#
# --------------------------------------------------------------------------
# monitoring parameters for model 1: Dbl Poisson
ex4.m1$coef		# all parameters
ex4.m1$beta1		# model parameters for lambda1
ex4.m1$beta2		# model parameters for lambda2. 
			# All are the same as in beta1 except the intercept
ex4.m1$beta2[1]		# Intercpept for lambda2. 
ex4.m1$beta2[1]-ex4.m1$beta2[2]	# estimated home effect


-sum(ex4.m1$coef[ 2:18]) # estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m1$coef[19:35]) # estimating the effect for 18th level of defence(team2..team1) [Verona]
xtemp<-readline(prompt = "Press Enter to Continue")
#
# --------------------------------------------------------------------------
# monitoring parameters for model 2: Biv Poisson (lamdba1, lambda2, constant lamdba3)
#
#
# monitoring parameters for model 1: Dbl Poisson
ex4.m2$beta1		# model parameters for lambda1
ex4.m2$beta2		# model parameters for lambda2. 
			# All are the same as in beta1 except the intercept
ex4.m2$beta3		# model parameters for lambda3. Here beta3 has only the intercept
ex4.m2$beta2[1]		# Intercpept for lambda2. 
ex4.m2$beta2[1]-ex4.m2$beta2[2]	# estimated home effect

-sum(ex4.m2$coef[ 2:18]) # estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m2$coef[19:35]) # estimating the effect for 18th level of defence(team2..team1) [Verona]
xtemp<-readline(prompt = "Press Enter to Continue")
#
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# monitoring parameters for model 8: Biv Poisson with Dis(1) diagonal distribution
#
#
# monitoring parameters for model 1: Dbl Poisson
ex4.m8$beta1		# model parameters for lambda1
ex4.m8$beta2		# model parameters for lambda2. 
			# All are the same as in beta1 except the intercept
ex4.m8$beta3		# model parameters for lambda3. Here beta3 has only the intercept
ex4.m8$beta2[1]		# Intercpept for lambda2. 
ex4.m8$beta2[1]-ex4.m8$beta2[2]	# estimated home effect

-sum(ex4.m8$coef[ 2:18]) # estimating the effect for 18th level of attack (team1..team2) [Verona]
-sum(ex4.m8$coef[19:35]) # estimating the effect for 18th level of defence(team2..team1) [Verona]

ex4.m8$beta3		 # parameters for lambda3 (here the intercept)
exp(ex4.m8$beta3)	 # lambda3 (here constant)
ex4.m8$diagonal.distribution # printing details for the diagonal distribution
ex4.m8$p		 # mixing proportion
ex4.m8$theta		 # printing theta parameters
xtemp<-readline(prompt = "Press Enter to Continue")
#
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# monitoring parameters for model 8: Biv Poisson with Dis(1) diagonal distribution
#
#
ex4.m8$diagonal.distribution            # printing details for the diagonal distribution
round(ex4.m8$beta1,2)		        # model parameters for lambda1
round(ex4.m8$beta2[1],2)		# Intercpept for lambda2. 
round(ex4.m8$beta2[1]-ex4.m8$beta2[2],2)# estimated home effect
#
# estimating the effect for 18th level of attack (team1..team2) [Verona]
round(-sum(ex4.m8$coef[ 2:18]),2) 
# estimating the effect for 18th level of defence(team2..team1) [Verona]
round(-sum(ex4.m8$coef[19:35]),2) 
#
ex4.m8$beta3		 # parameters for lambda3 (here the intercept)
exp(ex4.m8$beta3)	 # lambda3 (here constant)
ex4.m8$p		 # mixing proportion
ex4.m8$theta		 # printing theta parameters
#
# --------------------------------------------------------------------------


