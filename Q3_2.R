#######################################
#      Devoir maison L3 BMM - BISM    #
#         Systemes dynamiques         #
#        BACCAM Emmy  11401990        #
#######################################



# Question 3 Simulations numeriques
###################################################################

#Chroniques

#ATTENTION:
#avec l'utilisation de par(mfrow=c(2,2)) il peut y avoir des erreurs dans la
#compilation du code si la fenetre d'affichage des graphiques n'est pas assez grande

#Si les librairies ne sont pas installees ou mises a jour
# install.packages("deSolve")
# install.packages("phaseR")
# install.packages("rootSolve")

library(rootSolve)
library(deSolve)
library(phaseR)

rm(list=ls())

syst1=function(t,x,parms)
{
  #parms[1]=r, parms[2]=s, parms[3]=e,parms[4]=m
  #x[1]=N, x[2]=P
  dx1=parms[1]*x[1]*(1-(x[1]/parms[2])-((parms[3]*x[2])/parms[2]))
  dx2=parms[4]*x[2]*(1-(x[2]/parms[5])-((parms[6]*x[1])/parms[5]))
  list(c(dx1,dx2))
}

# Plan (N,P)
temps= seq(0,50,by=0.1)

#Parametres pour la population N est superieure a la population P
init1=c(100,10)
init2=c(10,100)

r=0.9
k=100
s=0.9
m=100

#cas1
a=0.5
b=0.5

#cas2
# a=2
# b=2

# cas3
# a=2
# b=0.5

# cas4
# a=0.5
# b=2


solution1=lsoda(y=init1,times=temps,func=syst1,parms=c(r,k,a,s,m,b))
solution2=lsoda(y=init2,times=temps,func=syst1,parms=c(r,k,a,s,m,b))


par(mfrow=c(1,2))
plot(temps,solution1[,2],type="l",ylab = "Densite de population",main="N(0)=100;P(0)=10",ylim = c(0,100))
lines(temps,solution1[,3],lty=2,col=2)
legend("topright",legend=c("N(t)","P(t)"),lty=c(1,2),col=c(1,2))

plot(temps,solution2[,2],type="l",ylab = "Densite de population",main="N(0)=10;P(0)=100",ylim = c(0,100))
lines(temps,solution2[,3],lty=2,col=2)
legend("topright",legend=c("N(t)","P(t)"),lty=c(1,2),col=c(1,2))