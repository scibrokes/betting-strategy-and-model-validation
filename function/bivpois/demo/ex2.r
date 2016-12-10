library(bivpois) # load bivpois library
data(ex2.sim)    # load ex2.sim data from bivpois library
xtemp<-readline(prompt = "Press Enter to Continue")
#
ex2.m1<-lm.bp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim )		# Model  1: BivPois
ex2.m2<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=0)	# Model  2: Zero Inflated BivPois 
ex2.m3<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=1)	# Model  3: Diagonal Inflated BivPois with DISCRETE(1) diagonal inflation distribution
ex2.m4<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=2)	# Model  4: Diagonal Inflated BivPois with DISCRETE(2) diagonal inflation distribution
ex2.m5<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=3)	# Model  5: Diagonal Inflated BivPois with DISCRETE(3) diagonal inflation distribution
ex2.m6<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=4)	# Model  6: Diagonal Inflated BivPois with DISCRETE(4) diagonal inflation distribution
ex2.m7<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=5)	# Model  7: Diagonal Inflated BivPois with DISCRETE(5) diagonal inflation distribution
ex2.m8<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , jmax=6)	# Model  8: Diagonal Inflated BivPois with DISCRETE(6) diagonal inflation distribution
ex2.m9<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , distribution='poisson')	# Model  9: Diagonal Inflated BivPois with POISSON diagonal inflation distribution
ex2.m10<-lm.dibp( x~z1 , y~z1+z5, l1l2=~z3, l3=~.-z5, data=ex2.sim , distribution='geometric')	# Model 10: Diagonal Inflated BivPois with GEOMETRIC diagonal inflation distribution
#
# printing parameters of model 7
ex2.m7$beta1
ex2.m7$beta2
ex2.m7$beta3
ex2.m7$p
ex2.m7$theta
#
# printing parameters of model 9
ex2.m9$beta1
ex2.m9$beta2
ex2.m9$beta3
ex2.m9$p
ex2.m9$theta



