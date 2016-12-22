library(boot)


#question 8.01
x=c(55,76,65,112,158,140,145,70,67,78,91,95,196,276,162,122,101,72,125,128,111,
196,121,100,96,185,45,171,81,79,203,299,151,122,67,101,226,113,71,119)

(m=mean(x))

(s=sd(x))



co.var <- function(x,na.rm=TRUE) 100*(sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm))

(cv=co.var(x))

#mean
mymean.func=function(x,d){
#d is the index of resampled observations
mean(x[d])

} 
R=10000
mean.boot=boot(x,mymean.func,R=10000)  

(MSE=sum((mean.boot$t-mean.boot$t0)^2)/R)

(MSE=sum(mean.boot$t-mean.boot$t0)^2/R)

mean((mean.boot$t-mean.boot$t0)^2)

(sdboot=sd(mean.boot$t))

(MOE=2*sqrt(MSE))


(se=sd(mean.boot$t)/sqrt(length(x)))





#standard deviation
sd.func=function(x,d){
#d is the index of resampled observations
sd(x[d])

} 

sd.boot=boot(x,sd.func,R=10000)  

(MSE=sum(sd.boot$t-sd.boot$t0)^2/R)

(MOE=2*sqrt(MSE))

(sd=sd(sd.boot$t))

(se=sd(sd.boot$t)/sqrt(length(x)))

#coefficient of variation

cv.func=function(x,d){

co.var(x[d])

}

cv.boot=boot(x,cv.func,R=10000)  

(MSE=sum(cv.boot$t-cv.boot$t0)^2/R)

(MOE=2*sqrt(MSE))

(sd=sd(cv.boot$t))
(SE=sqrt(MSE))

#question 8.02

alpha=0.05

xbar=mean(x)

s=sd(x)

df=length(x)-1

R=1000

tb=c()

var=c()

for (i in 1:R){

boot=sample(x, size=length(x), replace = TRUE)
tb[i]=(mean(boot)-mean(x))/(sd(boot)/sqrt(length(x)))
var[i]=var(boot)
}

tb=sort(abs(tb))

lower.critical=  -tb[R*(1-alpha)]

upper.critical=tb[R*(1-alpha)]

ci=c(mean(x)+lower.critical*se,mean(x)+upper.critical*se)

se=sqrt((sum(mean.boot$t-mean(mean.boot$t))^2)/(R-1))


####confidence interval from bootstrap###
(ci=c(mean(x)-qt(c(.025, .975) ,df)[2] *(s/sqrt(length(x)))
,mean(x)-qt(c(.025, .975), df)[1] *(s/sqrt(length(x)))))


####confidence interval from student t distribution####

(ci=c(mean(x)-qt(c(.025, .975) ,df)[2] *(s/sqrt(length(x)))
,mean(x)-qt(c(.025, .975), df)[1] *(s/sqrt(length(x)))))




var.func <- function(x, d)
{
 
var(x[d])

}    
     

st.boot=boot(x,var.func , R=1000)

boot.ci(st.boot, type = c("norm", "basic", "perc", "stud"))

####BCA############

boot.ci(mean.boot)







#question 8.05
#x=moisture
x=c(335,370,380,380,380,400,400,415,415,415,415,430,440,440,470)
#y=shrew.abundance
y=c(0.5,2,2,0.5,3,0,4,0.5,2,4,4,1,2,5,7)

data=data.frame(x,y)

lm(x~y)$coef

summary(lm(x~y))



lm.fun=function(data,d){

lm(data[d,1]~data[d,-1])$coef[2]
}
slopeboot=boot(cbind(x,y),lm.fun,R=1000)


boot.ci(slopeboot,conf = 0.90)




