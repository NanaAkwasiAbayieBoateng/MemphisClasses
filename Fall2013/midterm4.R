rm(list=ls())
#Question 4

MP.6<-c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35)

censor1<-c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)


placebo<-c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)

censor2<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

length(MP.6)
length(censor1)
length(placebo)
length(censor2)
#treatment 1=6-MP
#treatment2=placebo(control)
#censor=0,uncensored=1
time<-c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35,1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
censor<-c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
treatment<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

length(treatment)

library(survival)
gehan.surv <- survfit(Surv(time, censor) ~ treatment,
     conf.type = "log-log")

summary(gehan.surv)



 ### Plot of estimated log cumulative hazard
 plot(survfit(Surv(time, censor) ~ treatment) , fun="cloglog", lty=1:2, mark.time=FALSE, xlab="Time (months)", ylab="log (cumulative hazard)")
 legend(1,1, lty=1:2, legend=c("Control (N=21)", "6-MP (N=21)"), bty="n")

















#lines(gehan.surv,col='red')

#t1<-summary(gehan.surv)$treatment


summary(survreg(Surv(time, censor) ~ treatment))

gehan.cox <- coxph(Surv(time, censor) ~ treatment)

summary(gehan.cox)


survreg(Surv(time, censor) ~  treatment, dist = "exp")

summary(survreg(Surv(time, censor) ~ treatment, dist = "exp"))




#peto-peto
library(survival)
library(Hmisc)
library(car)
library(KMsurv)
drug.coxph <- coxph(Surv(time,censor)~treament, method="peto")
summary(drug.coxph)


