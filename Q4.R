#######################################
#      Devoir maison L3 BMM - BISM    #
#         Systemes dynamiques         #
#        BACCAM Emmy  11401990        #
#######################################



# Question 4 Influence des parametres sur le comportement du modele
###################################################################

#portrait de phase, chroniques et calcul des points d'equilibres avec multiroot

#ATTENTION:
#avec l'utilisation de par(mfrow=c(1,2)) il peut y avoir des erreurs dans la
#compilation du code si la fenetre d'affichage des graphiques n'est pas assez grande

#Si les librairies ne sont pas installees ou mises à jour
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
init1 = c(200,200)
r=0.9
k=500
a=0.1
s=0.9
m=500
b=0.1

solution1=lsoda(y=init1,times=temps,func=syst1,parms=c(r,k,a,s,m,b))
par(mfrow=c(1,2))
plot(temps,solution1[,2],type="l",ylab = "Densites de population",ylim = c(0,1000),main="a=b=0.1")
lines(temps,solution1[,3],lty=2)
legend(35,800,legend=c("N(t)","P(t)"),lty=c(1,2))

syst2=function(t,y,parameters)
{
  #dN=r*N*(1-(N/K)-((a*P)/K))
  #dP=s*P*(1-(P/M)-((b*N)/M))
  #parms[1]=r, parms[2]=s, parms[3]=K, parms[4]=M, parms[5]=a, parms[6]=b
  #x[1]=N, x[2]=P
  dy=numeric(2)
  dy[1]=parameters[1]*y[1]*(1-(y[1]/parameters[2])-((parameters[3]*y[2])/parameters[2]))
  dy[2]=parameters[4]*y[2]*(1-(y[2]/parameters[5])-((parameters[6]*y[1])/parameters[5]))
  list(dy)
}

vv = flowField(syst2,x.lim=c(0,1000),y.lim=c(0,1000),parameters=c(r,k,a,s,m,b),points=19,add=FALSE,xlab="N(t)",ylab="P(t)")
isocline=nullclines(syst2,x.lim=c(0,1000),y.lim=c(0,1000),parameters=c(r,k,a,s,m,b),points=500,colour=c("black","green"))
trajectoire1=trajectory(syst2,y0=init1,t.end=100,parameters=c(r,k,a,s,m,b))


#Calcul des coordonnées point d'équilibre
#Méthode avec multiroot
Equi = function(r,k,a,s,m,b,initN,initP){
  model=function(x){
    dx1=r*x[1]*(1-(x[1]/k)-((a*x[2])/k))
    dx2=s*x[2]*(1-(x[2]/m)-((b*x[1])/m))
    c(dx1,dx2)
  }
  Eq = multiroot(model,start=c(initN,initP),positive=TRUE)
  return(Eq)
}
Equi(0.9,500,0.5,0.9,500,0.5,200,200)

#Methode sans le multiroot
rm(list=ls())
calc_equilibre = function(){
  Equilibre = function(r,k,a,s,m,b)
  {
    p1=c(0,0)
    p2=c(k,0)
    p3=c(0,m)
    p4=c((k-(a*m))/(1-(a*b)),(m-(b*k))/(1-(a*b)))
    rbind(p1,p2,p3,p4)
  }
  eq=Equilibre(0.9,500,0.5,0.9,500,0.5)
  return(eq)
}
calc_equilibre()

