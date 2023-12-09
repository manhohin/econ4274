# Code for Problem Set 3 #


#---------------------------------------------------------------------
# Question 1: cigarette tax
#---------------------------------------------------------------------

cigarette=read.csv("cigarette.csv")
nrow(cigarette)
sum(cigarette$TAX==1)
str(cigarette)
cigarette$Cdiff=cigarette$C2006-cigarette$C2000
c0=mean(cigarette[cigarette$TAX==0,]$C2000)
t0=mean(cigarette[cigarette$TAX==1,]$C2000)
c1=mean(cigarette[cigarette$TAX==0,]$C2006)
t1=mean(cigarette[cigarette$TAX==1,]$C2006)
TAB=rbind(c(c0,c1,c1-c0),c(t0,t1,t1-t0),c(t0-c0,t1-c1,t1-c1-(t0-c0)))


#This is a treatment effect example. We shall use diff-in-diff to study it
m1=lm(C2000~TAX,data=cigarette)
m2=lm(C2006~TAX,data=cigarette)

#We need to create proper dummy variables
C=c(cigarette$C2000,cigarette$C2006)
n=length(cigarette$C2000)
Dyear=c(rep(0,n),rep(1,n))
Dtax=c(cigarette$TAX,cigarette$TAX)
m3=lm(C~Dtax+Dyear+Dtax*Dyear)

library(stargazer)

stargazer(m1,m2,m3
          ,type="html",out="tab.html"
          ,omit.stat=c("adj.rsq","f","ser")
)



#---------------------------------------------------------------------
# Question 2: Colonial Origin #
#---------------------------------------------------------------------

rm(list=ls())
library(foreign)
colony=read.dta("colony.dta")

#a. Subsample
colonybase=subset(colony,baseco==1)
nrow(colonybase)
colonynoeu=subset(colony,baseco==1 & rich4==0)
nrow(colonynoeu)

#b. Graph
png("colony1.png",width=450,height=350,family="serif")
plot(colonybase$logem4,colonybase$logpgp95,pch=20,col="grey"
     ,xlab="Log of settler mortality",ylab="Log GDP per capital, PPP, 1995"
     ,main="Figure 1 of Acemoglu et al. (2001)")
text(colonybase$logem4,colonybase$logpgp95,label=colonybase$shortnam,pos=3,cex=0.8)
abline(lm(logpgp95~logem4,data=colonybase),lwd=1.5)
dev.off()

png("colony2.png",width=450,height=350,family="serif")
plot(colonybase$avexpr,colonybase$logpgp95,pch=20,col="grey"
     ,xlab="Average Expropriation Risk 1985-95",ylab="Log GDP per capital, PPP, 1995"
     ,main="Figure 2 of Acemoglu et al. (2001)")
text(colonybase$avexpr,colonybase$logpgp95,label=colonybase$shortnam,pos=3,cex=0.7)
abline(lm(logpgp95~avexpr,data=colonybase),lwd=1.5)
dev.off()

#b. regression
library(AER)
library(stargazer)

iv1=ivreg(logpgp95~avexpr | logem4,data=colonybase)
ols1=lm(logpgp95~avexpr,data=colonybase)
iv1first=lm(avexpr~logem4,data=colonybase)
iv2=ivreg(logpgp95~avexpr+lat_abst | logem4+lat_abst,data=colonybase)
ols2=lm(logpgp95~avexpr+lat_abst,data=colonybase)
iv2first=lm(avexpr~logem4+lat_abst,data=colonybase)

stargazer(iv1,iv2,type="html",out="tab.html")
stargazer(ols1,ols2,type="html",out="tab.html")
stargazer(iv1first,iv2first,type="html",out="tab.html")

iv3=ivreg(logpgp95~avexpr | logem4,data=colonynoeu)
ols3=lm(logpgp95~avexpr,data=colonynoeu)
iv3first=lm(avexpr~logem4,data=colonynoeu)
iv4=ivreg(logpgp95~avexpr+lat_abst | logem4+lat_abst,data=colonynoeu)
ols4=lm(logpgp95~avexpr+lat_abst,data=colonynoeu)
iv4first=lm(avexpr~logem4+lat_abst,data=colonynoeu)

stargazer(iv3,iv4,type="html",out="tab.html")
stargazer(ols3,ols4,type="html",out="tab.html")
stargazer(iv3first,iv4first,type="html",out="tab.html")


#---------------------------------------------------------------------
# Question 3: Return to Schooling  #
#---------------------------------------------------------------------

#Using Geographic Variation in College Proximity to Estimate the Return to Schooling, David Card (1995)
library(foreign)
library(stargazer)
# card=NULL
card=read.csv("card.csv")

#a. organizing data 
# card=read.csv("card.csv")
# card$wage76=as.numeric(card$wage76)
# summary(card$wage76)
# card$wage78=as.numeric(card$wage78)
# summary(card$wage78)
# card$wage80=as.numeric(card$wage80)
# card$lwage76=log(card$wage76)
# card$lwage78=log(card$wage78)
# card$lwage80=log(card$wage80)
# summary(card$wage80)
# card$kww=as.numeric(card$kww)
# card$iq=as.numeric(card$iq)
# write.csv(card,"card.csv",row.names=F)

#a. Figure
m1=lm(lwage76~educ,data=card)
m2=lm(lwage78~educ,data=card)
m3=lm(lwage80~educ,data=card)

png("linearfit.png",width=1000,height=400,family="serif")
par(mfrow=c(1,3),font=1,font.main=1,font.lab=1,mar=c(4,4,4,1),family="serif",cex=1.2)

plot(lwage76~educ,pch=16,data=card,col=8,xlab="years of schooling, 1976",
     ylab="log(wage), 1976",ylim=c(3,8),main=paste("(A) slope=",round(coef(m1)[2],digits=4)))
abline(m1,lwd=2)

plot(lwage78~educ,pch=16,data=card,col=8,xlab="years of schooling, 1976",
     ylab="log(wage), 1978",ylim=c(3,8),main=paste("(B) slope=",round(coef(m2)[2],digits=4)))
abline(m2,lwd=2)

plot(lwage80~educ,pch=16,data=card,col=8,xlab="years of schooling, 1976",
     ylab="log(wage), 1980",ylim=c(3,8),main=paste("(C) slope=",round(coef(m3)[2],digits=4)))
abline(m3,lwd=2)
dev.off()

#b. OLS 
card$f.famed=as.factor(card$famed)
m1=lm(lwage76~educ+kww+iq+age+black+smsa+south+f.famed,data=card)
m2=lm(lwage78~educ+kww+iq+age+black+smsa+south+f.famed,data=card)
m3=lm(lwage80~educ+kww+iq+age+black+smsa+south+f.famed,data=card)
# stargazer(m1,m2,m3,type="html",out="tab.html",omit.stat=c("adj.rsq","f","ser"))

#c. IV regression
install.packages("AER")
library(AER)

#first stage
f1=lm(educ~nearc2+kww+iq+age+black+smsa+south+f.famed,data=card)
f2=lm(educ~nearc4+kww+iq+age+black+smsa+south+f.famed,data=card)
f3=lm(educ~nearc4a+kww+iq+age+black+smsa+south+f.famed,data=card)
f4=lm(educ~nearc4b+kww+iq+age+black+smsa+south+f.famed,data=card)
f5=lm(educ~nearc4+nearc4a+kww+iq+age+black+smsa+south,data=card)
stargazer(f1,f2,f3,f4,f5,type="html",out="tab.html")

s1=ivreg(lwage76~educ+kww+iq+age+black+smsa+south+f.famed | 
           nearc4+nearc4a+kww+iq+age+black+smsa+south+f.famed,data=card)
s2=ivreg(lwage78~educ+kww+iq+age+black+smsa+south+f.famed | 
           nearc4+nearc4a+kww+iq+age+black+smsa+south+f.famed,data=card)
s3=ivreg(lwage80~educ+kww+iq+age+black+smsa+south+f.famed | 
           nearc4+nearc4a+kww+iq+age+black+smsa+south+f.famed,data=card)
stargazer(m1,s1,m2,s2,m3,s3,type="html",out="tab.html",omit.stat=c("adj.rsq","f","ser"))

#e. 
sum(is.na(card$lwage76))
sum(is.na(card$lwage78))
sum(is.na(card$lwage80))
card$work76=1*(!is.na(card$lwage76))
card$work78=1*(!is.na(card$lwage78))
card$work80=1*(!is.na(card$lwage80))

m1=glm(work76~educ+kww+iq+age+black+smsa+south+f.famed,
       data=card,family=binomial(link="logit"))
m2=glm(work76~educ+kww+iq+age+black+smsa+south+f.famed,
       data=card,family=binomial(link="probit"))
m3=glm(work78~educ+kww+iq+age+black+smsa+south+f.famed,
       data=card,family=binomial(link="logit"))
m4=glm(work78~educ+kww+iq+age+black+smsa+south+f.famed,
       data=card,family=binomial(link="probit"))
m5=glm(work80~educ+kww+iq+age+black+smsa+south+f.famed,
       data=card,family=binomial(link="logit"))
m6=glm(work80~educ+kww+iq+age+black+smsa+south+f.famed,
       data=card,family=binomial(link="probit"))
stargazer(m1,m2,m3,m4,m5,m6,type="html",out="tab.html")

R2.m1=1-m1$deviance/m1$null.deviance
R2.m2=1-m2$deviance/m2$null.deviance
R2.m3=1-m3$deviance/m3$null.deviance
R2.m4=1-m4$deviance/m4$null.deviance
R2.m5=1-m5$deviance/m5$null.deviance
R2.m6=1-m3$deviance/m6$null.deviance
round(c(R2.m1,R2.m2,R2.m3,R2.m4,R2.m5,R2.m6),digits=3)


#---------------------------------------------------------------------
# Question 4: Monte Carlo for IV #
#---------------------------------------------------------------------

#a. DGP of data1
n=100
beta1=1
beta2=0.5
beta3=2
# install.packages("MASS")
library(MASS)
Sigma=cbind(c(1,0.8),c(0.8,1))   #covariance matrix
n=100      #sample size
eu=mvrnorm(n,c(0,0),Sigma)      #generate e_i and u_I 
e=eu[,1]
u=eu[,2]
z1=rnorm(n,1,1)
z2=rgamma(n,2)
z3=rnorm(n,2,0.5)
x1=rnorm(n,1,2)
x2=z1+0.5*z2+0.01*z3+u
y=beta1+beta2*x1+beta3*x2+e

#b. OLS
nseq=seq(100,5000,by=100)
olsmat=NULL
seolsmat=NULL
for (n in nseq){
  eu=mvrnorm(n,c(0,0),Sigma)      #generate e_i and u_I 
  e=eu[,1]
  u=eu[,2]
  z1=rnorm(n,1,1)
  z2=rgamma(n,2)
  z3=rnorm(n,2,0.5)
  x1=rnorm(n,1,2)
  x2=z1+0.5*z2+0.01*z3+u
  y=beta1+beta2*x1+beta3*x2+e
  ols=summary(lm(y~x1+x2))
  olsmat=rbind(olsmat,ols$coef[,1])
  seolsmat=rbind(seolsmat,ols$coef[,2])
}
par(mfrow=c(1,3))
plot(nseq,olsmat[,1],type="l",ylim=c(0,3),ylab="",xlab="n",main=expression(beta[1]))
abline(h=beta1,col=2,lty=2)
plot(nseq,olsmat[,2],type="l",ylim=c(0,3),ylab="",xlab="n",main=expression(beta[1]))
abline(h=beta2,col=2,lty=2)
plot(nseq,olsmat[,3],type="l",ylim=c(0,3),ylab="",xlab="n",main=expression(beta[1]))
abline(h=beta3,col=2,lty=2)

#c. 
# install.packages("AER")
library(stargazer)
# install.packages("stargazer")
library(AER)
n=100
eu=mvrnorm(n,c(0,0),Sigma)      #generate e_i and u_I 
e=eu[,1]
u=eu[,2]
z1=rnorm(n,1,1)
z2=rgamma(n,2)
z3=rnorm(n,2,0.5)
x1=rnorm(n,1,2)
x2=z1+0.5*z2+0.01*z3+u
y=beta1+beta2*x1+beta3*x2+e

iv1=ivreg(y~x1+x2 | x1+z1)
iv2=ivreg(y~x1+x2 | x1+z2)
iv3=ivreg(y~x1+x2 | x1+z3)

#c
iv4=ivreg(y~x1+x2 | x1+z1+z2+z3)

#d
ols=lm(y~x1+x2)
# install.packages("systemfit")
library(systemfit)
t1=hausman.systemfit(iv1,ols)
t2=hausman.systemfit(iv2,ols)
t3=hausman.systemfit(iv3,ols)
t4=hausman.systemfit(iv4,ols)
t1$p.value
t2$p.value
t3$p.value
t4$p.value
stargazer(ols,iv1,iv2,iv3,iv4,type="html",out="midterm2.htm")

