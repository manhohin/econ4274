# Code for Problem Set 4 #

rm(list=ls())
setwd("D:/Dropbox/Teaching/Applied Econometrics/Code/data")

#---------------------------------------------------------------------
# Question 1: hospital choice
#---------------------------------------------------------------------
hc=read.csv("hospital_choice.csv")
library(stargazer)

m1=lm(Ducla~distance+income+old,data=hc)
m2=glm(Ducla~distance+income+old,data=hc,family=binomial(link="logit"))
m3=glm(Ducla~distance+income+old+distance*old,data=hc,family=binomial(link="logit"))
m4=glm(Ducla~distance+income+old,data=hc,family=binomial(link="probit"))
m5=glm(Ducla~distance+income+old+distance*old,data=hc,family=binomial(link="probit"))
stargazer(m1,m2,m3,m4,type="html",out="tab.html"
          ,omit.stat=c("rsq","f","ser") )


#McFadden pseudo-R2
R2ols=summary(m1)$r.squared
R2m2=1-m2$deviance/m2$null.deviance
R2m3=1-m3$deviance/m3$null.deviance
R2m4=1-m4$deviance/m4$null.deviance
R2m5=1-m5$deviance/m5$null.deviance
c(R2ols,R2m2,R2m3,R2m4,R2m5)


#---------------------------------------------------------------------
# Question 2: MLE  #
#---------------------------------------------------------------------
rm(list=ls())

#a. 
set.seed(1)
sigma=2
beta1=1
beta2=0.5
n=500
e=rnorm(n,0,sigma)
x=rgamma(n,2)
y=beta1+beta2*x+e

#b.
logL<-function(theta){
  l=-sum(log(1/sqrt(2*pi*theta[3]^2)*exp(-(y-theta[1]-theta[2]*x)^2/(2*theta[3]^2))))
  return(l)}
initial=c(1,1,1)
logL(initial)
mle=optim(initial,logL)
parest=mle$par
parest

#c.
ols=lm(y~x)
summary(ols)$coef
#They are very similar
#OLS is efficient given classical assumptions


#---------------------------------------------------------------------
# Question 3: Discrete Choice  #
#---------------------------------------------------------------------

#a. data
rm(list=ls())
library(mlogit)
# setwd("D:/Dropbox/Teaching/Applied Econometrics/Code/data")
heating=read.csv("heating.csv")
# heating=mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))
heating=mlogit.data(heating, shape = "long", choice = "depvar", alt.var="alt")
summary(heating)
lheat=mlogit.data(heating,choice="depvar",shape="long",
                  alt.levels=c("ec","er","gc","gr","hp"))

#b. 
heating$f.region=as.factor(heating$region)

m1=mlogit(depvar~ic+oc+0, data=heating)
m2=mlogit(depvar~ic+oc | income+agehed+rooms+0, data=heating)
m3=mlogit(depvar~ic+oc | income+agehed+rooms+f.region+0, data=heating)


library(stargazer)
stargazer(m1,m2,m3,type="html",out="tab.html")

#c. 
observed.CP=table(heating$alt[heating$depvar==TRUE])/sum(heating$depvar==TRUE)
predict.CP=colMeans(fitted(m3,outcome=FALSE))

#Reduce ec installation cost by 10%
heating1=heating
heating1$ic[heating$alt=="ec"]=heating$ic[heating$alt=="ec"]*0.9
new.CP1=colMeans(predict(m3,newdata=heating1))

#Reduce ec operational cost by 10%
heating2=heating
heating2$oc[heating$alt=="ec"]=heating$oc[heating$alt=="ec"]*0.9
new.CP2=colMeans(predict(m3,newdata=heating2))

rbind(observed.CP,predict.CP,new.CP1,new.CP2)

#---------------------------------------------------------------------
# Question 4: Demand Estimation #
#---------------------------------------------------------------------
product=read.csv("product.csv")
N=10000
M=max(product$market)
product$MS=0
for (m in 1:M){
  product[product$market==m,]$MS=product[product$market==m,]$sales/N
}

X=rbind(c(1,1,1,2),c(2,2,2,7),c(3,1,1,6))
initial=rep(0.1,4)
dim(X)
dim(initial)

#Criterion function
Q<-function(b){
  # b=initial
  ujvec= X %*% b
  MS_hatm=exp(ujvec)/(1+sum(exp(ujvec)))
  MS_hat=rep(MS_hatm,M)
  d=sum((product$MS-MS_hat)^2)
  return(d)
}
NLS=optim(initial,Q)
cbind(NLS$par,beta)


#DGP
# beta=c(3,1,2,-1)
# X=rbind(c(1,1,1,2),c(2,2,2,7),c(3,1,1,6))
# 
# DATA=NULL
# M=50
# J=3
# N=10000
# for (m in 1:M){
#   choice_vec=NULL
#   for (i in 1:N){
#     u=X %*% beta + rgumbel(J)
#     u0=rgumbel(1)
#     if (max(u)<u0) j=0 else j=which.max(u)  #choice
#     choice_vec=c(choice_vec,j)
#   }
#   sales=c(sum(choice_vec==1),sum(choice_vec==2),sum(choice_vec==3))
#   mDATA=cbind(X,rep(m,3),sales)
#   DATA=rbind(DATA,mDATA)
# }
# write.csv(DATA,"product.csv",row.names=F)

