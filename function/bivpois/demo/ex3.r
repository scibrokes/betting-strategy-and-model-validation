library(bivpois)
data(ex3.health)
# Bivariate Poisson models
ex3.model.a<-lm.bp(doctorco~sex+age+income, prescrib~sex+age+income, data=ex3.health) 
ex3.model.b<-lm.bp(doctorco~sex+age+income, prescrib~sex+age+income,l3=~sex, data=ex3.health) 
# Double Poisson model
ex3.model.c<-lm.bp(doctorco~sex+age+income, prescrib~sex+age+income,data=ex3.health, zeroL3=TRUE) 
# 
# diagonal inflated models
ex3.dibp.a<-lm.dibp(doctorco~sex+age+income, prescrib~sex+age+income,  data=ex3.health, jmax=0)          # model (a)
ex3.dibp.b<-lm.dibp(doctorco~sex+age+income, prescrib~sex+age+income,l3=~sex, data=ex3.health, jmax=0)   # model (b)


# save models
dump('ex3.model.a','c:/ex3bp1.r')
dump('ex3.model.b','c:/ex3bp2.r')
dump('ex3.model.c','c:/ex3bp3.r')

dump('ex3.dibp.a','c:/ex3dibp1.r')
dump('ex3.dibp.b','c:/ex3dibp2.r')
#
# load models
#source('c:/ex3bp1.r')
#source('c:/ex3bp2.r')
#source('c:/ex3bp3.r')

#source('c:/ex3dibp1.r')
#source('c:/ex3dibp2.r')
