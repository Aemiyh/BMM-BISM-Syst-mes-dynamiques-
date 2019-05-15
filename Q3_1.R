#######################################
#      Devoir maison L3 BMM - BISM    #
#         Systemes dynamiques         #
#        BACCAM Emmy  11401990        #
#######################################



# Question 3 Simulations numeriques
###################################################################

#Portrait de phase et stabilite des points d'equilibres
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

#Cas 1
###################################################################
syst1=function(t,y,parameters)
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

r=0.9
k=100
a=0.5
s=0.9
m=100
b=0.5

# Plan (N,P)
init1=c(100,10) # population N a sa capacite limite
init2=c(10,15) #populations P et N en dessous de leur capacite limite
init3=c(50,90)
init4=c(100,100) #populations P et N sont  leur capacite limite

par(mfrow=c(2,2))
vv = flowField(syst1,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=19,add=FALSE,xlab="N(t)",ylab="P(t)")
isocline=nullclines(syst1,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=500,colour=c("black","black"))
trajectoire1=trajectory(syst1,y0=init1,t.end=100,parameters=c(r,k,a,s,m,b),col=3)
trajectoire2=trajectory(syst1,y0=init2,t.end=100,parameters=c(r,k,a,s,m,b),col=2)
trajectoire3=trajectory(syst1,y0=init3,t.end=100,parameters=c(r,k,a,s,m,b),col=4)
trajectoire4=trajectory(syst1,y0=init4,t.end=100,parameters=c(r,k,a,s,m,b),col=5)

legend("topright",c("N(0)=100;P(0)=10","N(0)=10;P(0)=15","N(0)=50;P(0)=90","N(0)=100;P(0)=100"),col=c(3,2,4,5),lty=c(1,1),bg="white")

#Cas 2
###################################################################
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

a=2
b=2

# Plan (N,P)
init1 = c(100,30) # population N a sa capacite limite
init2= c(10,15) # population P a  sa capacite limite
init3=c(50,90) #populations P et N en dessous de leur capacite limite
init4=c(50,20) #populations P et N sont  leur capacite limite

vv = flowField(syst2,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=19,add=FALSE,xlab="N(t)",ylab="P(t)")
isocline=nullclines(syst2,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=500,colour=c("black","black"))
trajectoire1=trajectory(syst2,y0=init1,t.end=100,parameters=c(r,k,a,s,m,b),col=3)
trajectoire2=trajectory(syst2,y0=init2,t.end=100,parameters=c(r,k,a,s,m,b),col=2)
trajectoire3=trajectory(syst2,y0=init3,t.end=100,parameters=c(r,k,a,s,m,b),col=4)
trajectoire4=trajectory(syst2,y0=init4,t.end=100,parameters=c(r,k,a,s,m,b),col=5)

legend("topright",c("N(0)=100;P(0)=30","N(0)=10;P(0)=15","N(0)=50;P(0)=90","N(0)=50;P(0)=20"),col=c(3,2,4,5),lty=c(1,1),bg="white")

#Cas 3
###################################################################
syst3=function(t,y,parameters)
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

a=2
b=0.5

# Plan (N,P)
init1 = c(100,30) # population N a sa capacite limite
init2= c(10,15) # population P a  sa capacite limite
init3=c(50,90) #populations P et N en dessous de leur capacite limite
init4=c(50,20) #populations P et N sont  leur capacite limite

vv = flowField(syst3,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=19,add=FALSE,xlab="N(t)",ylab="P(t)")
isocline=nullclines(syst3,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=500,colour=c("black","black"))
trajectoire1=trajectory(syst3,y0=init1,t.end=100,parameters=c(r,k,a,s,m,b),col=3)
trajectoire2=trajectory(syst3,y0=init2,t.end=100,parameters=c(r,k,a,s,m,b),col=2)
trajectoire3=trajectory(syst3,y0=init3,t.end=100,parameters=c(r,k,a,s,m,b),col=4)
trajectoire4=trajectory(syst3,y0=init4,t.end=100,parameters=c(r,k,a,s,m,b),col=5)

legend("topright",c("N(0)=100;P(0)=30","N(0)=10;P(0)=15","N(0)=50;P(0)=90","N(0)=50;P(0)=20"),col=c(3,2,4,5),lty=c(1,1),bg="white")

#Cas 4
###################################################################
syst4=function(t,y,parameters)
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

a=0.5
b=2

# Plan (N,P)
init1 = c(100,30) # population N a sa capacite limite
init2= c(10,15) # population P a  sa capacite limite
init3=c(50,90) #populations P et N en dessous de leur capacite limite
init4=c(50,20) #populations P et N sont  leur capacite limite

vv = flowField(syst4,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=19,add=FALSE,xlab="N(t)",ylab="P(t)")
isocline=nullclines(syst4,x.lim=c(0,200),y.lim=c(0,200),parameters=c(r,k,a,s,m,b),points=500,colour=c("black","black"))
trajectoire1=trajectory(syst4,y0=init1,t.end=100,parameters=c(r,k,a,s,m,b),col=3)
trajectoire2=trajectory(syst4,y0=init2,t.end=100,parameters=c(r,k,a,s,m,b),col=2)
trajectoire3=trajectory(syst4,y0=init3,t.end=100,parameters=c(r,k,a,s,m,b),col=4)
trajectoire4=trajectory(syst4,y0=init4,t.end=100,parameters=c(r,k,a,s,m,b),col=5)

legend("topright",c("N(0)=100;P(0)=30","N(0)=10;P(0)=15","N(0)=50;P(0)=90","N(0)=50;P(0)=20"),col=c(1,2,4,5),lty=c(1,1),bg="white")

#Calcul des 4 points d'equilibres pour chaque cas
#Cas 1
###################################################################
stab1 = stability(syst1,y.star=c(0,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab2 = stability(syst1,y.star=c(k,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab3 = stability(syst1,y.star=c(0,m),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab4 = stability(syst1,y.star=c((k-(a*m))/(1-(a*b)),(m-(b*k))/(1-(a*b))),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
#Cas 2
###################################################################
stab5 = stability(syst2,y.star=c(0,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab6 = stability(syst2,y.star=c(k,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab7 = stability(syst2,y.star=c(0,m),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab8 = stability(syst2,y.star=c((k-(a*m))/(1-(a*b)),(m-(b*k))/(1-(a*b))),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
#Cas 3
###################################################################
stab9 = stability(syst3,y.star=c(0,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab10 = stability(syst3,y.star=c(k,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab11 = stability(syst3,y.star=c(0,m),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab12 = stability(syst3,y.star=c((k-(a*m))/(1-(a*b)),(m-(b*k))/(1-(a*b))),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
#Cas 4
###################################################################
stab13 = stability(syst4,y.star=c(0,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab14 = stability(syst4,y.star=c(k,0),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab15= stability(syst4,y.star=c(0,m),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
stab16 = stability(syst4,y.star=c((k-(a*m))/(1-(a*b)),(m-(b*k))/(1-(a*b))),parameters=c(r,k,a,s,m,b), system = "two.dim", h = 1e-7,summary = TRUE)
