library(boot)
cd4

###statistic of interest,mean
cd41.f1 <- function(data,f) mean(data$baseline*f)
cd42.f2 <- function(data,f) mean(data$oneyear*f)

cd4.boot1 <- boot(cd4, cd41.f1, stype="f", R=999)
cd4.boot1$t
cd4.boot2 <- boot(cd4, cd42.f2, stype="f", R=999)
cd4.boot2$t
mean(cd4$baseline)
mean(cd4$oneyear)
boot.array(cd4.boot1,indices=T)
boot.array(cd4.boot2,indices=T)

##Estimation of bias 
R=999
mean(cd4.boot1$t)-cd4.boot1$t0
BR=(1/R)*sum(cd4.boot1$t)-mean(cd4.boot1$t0)
BR

##Estimation of variance V

var(cd4.boot1$t)

n=nrow(cd4)

tstar1=(sum(cd4.boot1$t-mean(cd4.boot1$t)))*(1/var(cd4.boot1$t))
tstar1

vstar=((n-1)/n)*(1/(n*(n-1)))*sum((cd4.boot1$t-mean(cd4.boot1$t0))^2)
vstar
Vr=(1/(R-1))*sum((cd4.boot1$t-mean(cd4.boot1$t0))^2)
Vr


#correlation coefficient

corr(cd4, w = rep(1, nrow(cd4))/nrow(cd4))

#confidence intervals
yb <- mean(cd4)
n <- nrow(cd4)
##Method 1: normal approximation

vhart=(yb^2)/n

L1=yb-qnorm(.975)*sqrt(vhart)

L1

U1=yb + qnorm(.975)*sqrt(vhart)

U1


 ##Method 2 Exact: chi^2(2n)

A <- qchisq(0.025, 2*n)
B <- qchisq(0.975, 2*n)


CI_EB <- c(2*sum(cd4$baseline)/B, 2*sum(cd4$baseline)/A)
CI_EB
CI_EO <- c(2*sum(cd4$oneyear)/B, 2*sum(cd4$oneyear)/A)
CI_EO

## method three
###  A3 variance stabilization Transformation ####

lowerlimit=log(yb)- qnorm(.975)*(1/sqrt(n))



L1=exp(lowerlimit)
L1
upperlimit=log(yb)+ qnorm(.975)*(1/sqrt(n))

U1=exp(upperlimit)
U1

#
# Nonparametric confidence intervals for mean
# of the cd4 data using boot.ci


boot.ci(cd4.boot1)
boot.ci(cd4.boot2)


#basic bootstrap without transformation

i <- order(cd4.boot1$t)
yo <- cd4.boot1$t[i]
y1 <- (yo[25])
y2 <- (yo[975])
(2*(mean(cd4.boot1$t0))-(y1))
(2*(mean(cd4.boot1$t0))-(y2))
CIB=c((2*(mean(cd4.boot1$t0))-(y2)),(2*(mean(cd4.boot1$t0))-(y1)))
CIB
i <- order(cd4.boot2$t)
yo <- cd4.boot2$t[i]
y1 <- (yo[25])
y2 <- (yo[975])
(2*(mean(cd4.boot2$t0))-(y1))
(2*(mean(cd4.boot2$t0))-(y2))
CIO=c((2*(mean(cd4.boot2$t0))-(y2)),(2*(mean(cd4.boot2$t0))-(y1)))
CIO

#basic bootstrap with transformation
i <- order(cd4.boot2$t)
yo <- cd4.boot2$t[i]
y1 <- (yo[25])
y2 <- (yo[975])
vhart=(mean(cd4$oneyear)^2)/n
z=(i-cd4.boot2$t)/sqrt(var(i))

u=mean(cd4.boot2$t)-sqrt(vhart)*z[25]

l=mean(cd4.boot2$t)-sqrt(vhart)*z[975]
c(l,u)

i <- order(cd4.boot1$t)
yo <- cd4.boot1$t[i]
y1 <- (yo[25])
y2 <- (yo[975])
vhart1=(mean(cd4$baseline)^2)/n
z=(i-cd4.boot2$t)/sqrt(var(i))

u=mean(cd4.boot1$t)-sqrt(vhart1)*z[25]

l=mean(cd4.boot1$t)-sqrt(vhart1)*z[975]
c(l,u)

par(mfrow=c(1,2)) 
hist(cd4.boot1$t)
hist(cd4$baseline)
#two sample t test
t.test(cd4.boot1$t, mu=3.28)
t.test(cd4.boot1$t, cd4.boot2$t,var.equal=T)
t.test(cd4.boot1$t, cd4.boot2$t,var.equal=T)
t.test(cd4$baseline, cd4$oneyear,var.equal=T)



#graphs

i<-c(10,20,50,100,200,300,400,500)
bias<-matrix(nrow=8,ncol=4)
#bias
variance<-matrix(nrow=8,ncol=4)
#variance

for(j in 1:4)
{
cd4.boot1 <- boot(cd4, cd41.f1, stype="f", R=1000)
for(k in 1:8)
  { bias[k,j]<-mean(cd4.boot1$t[1:i[k]])-mean(cd4$baseline)
    variance[k,j]<-var(cd4.boot1$t[1:i[k]])
  }
}

#variance[,]
#bias[,]
#ac.boot$t

###############################################
#matrix plot of  bias and variance
###############################################

par(mfrow=c(1,2))
#split.screen(c())

matplot(log(i),bias,axes=F,xlim=c(log(10),log(500)),ylim=c(-.05,.05),xlab="R",ylab="Bias",type="c",pch=20,col=1,lty=1)
#matplot(i,bias,axes=F,xlim=c(10,500),ylim=c(-50,10),xlab="R",ylab="Bias",type="c",pch=20,col=1,lty=1)
matpoints(log(i),bias,lty=1,pch=".",col=1,cex=1.5)
axis(1,at=c(log(10),log(50),log(100),log(500)),labels=paste(c(10,50,100,500)))
axis(2,at=c(-15,-10,-5,0,5,10),labels=paste(c(-15,-10,-5,0,5,10)))
abline(h=0,lty=3)
box()

matplot(log(i),variance,axes=F,xlim=c(log(10),log(500)),ylim=c(-.05,.05),xlab="R",ylab="Variance",type="c",pch=20,col=1,lty=1)
matpoints(log(i),variance,lty=1,pch=".",col=1,cex=1.5)
axis(1,at=c(log(10),log(50),log(100),log(500)),labels=paste(c(10,50,100,500)))
axis(2,at=c(600,800,1200,1600),labels=paste(c(600,800,1200,1600)))
abline(h=mean(cd4$baseline)^2/NROW(cd4$baseline),lty=3)
box()



#quantile plots


x<-qgamma((1:999)/1000,NROW(cd4$baseline),rate=NROW(cd4$baseline)/mean(cd4$baseline))
y<-qgamma((1:99)/100,NROW(cd4$baseline),rate=NROW(cd4$baseline)/mean(cd4$baseline))

par(mfrow=c(2,2))

qqnorm(cd4.boot1$t[1:99],axes=F,pch=20,main="",xlim=c(-1,2.5),ylim=c(-1,7),xlab="Quantiles of standard normal",ylab="t*",cex=0.9)
axis(1,at=c(-2,-1,0,1,2),labels=paste(c(-2,-1,0,1,2)))
axis(2,at=c(40,60,80,100,140,180),labels=paste(c(40,60,80,100,140,180)))
qqline(cd4.boot1$t[1:99],lty=3)

box()

qqnorm(cd4.boot1$t[1:999],axes=F,pch=20,main="",xlim=c(0,2.5),ylim=c(-1,7),xlab="Quantiles of standard normal",ylab="t*",cex=0.5)
axis(1,at=c(-2,0,2),labels=paste(c(-2,0,2)))
axis(2,at=c(50,100,150,200),labels=paste(c(50,100,150,200)))
qqline(cd4.boot1$t[1:999],lty=3)
box()

qqplot(y,cd4.boot1$t[1:99],axes=F,pch=20,main="",xlim=c(2,5),ylim=c(0,10),xlab="Exact gamma quantile",ylab="t*",cex=0.9)
axis(1,at=c(60,80,120,160,200),labels=paste(c(60,80,120,160,200)))
axis(2,at=c(40,60,80,100,140,180),labels=paste(c(40,60,80,100,140,180)))
abline(a=0,b=1,lty=3)
box()

qqplot(x,cd4.boot1$t[1:999],axes=F,pch=20,main="",xlim=c(2,5),ylim=c(0,10),xlab="Exact gamma quantile",ylab="t*",cex=0.5)
axis(1,at=c(50,100,150,200),labels=paste(c(50,100,150,200)))
axis(2,at=c(50,100,150,200),labels=paste(c(50,100,150,200)))
abline(a=0,b=1,lty=3) 
box()

title(sub="Figure2.2 ")



#Histogram plots

par(mfrow=c(1,2))

hist(cd4.boot1$t[,1],axes=F,probability=T,xlim=c(2.5,4),xlab="t*",ylim=c(0,2.5),ylab="",col="blue")
axis(1,at=c(0.5,1.0,1.5,2.0,2.5),labels=paste(c(0.5,1.0,1.5,2.0,2.5)))
axis(2)
box()

hist(cd4.boot1$t[1:999,3],axes=F,probability=T,xlim=c(-1,5),xlab="z*",ylim=c(0,5),ylab="",col="green")
axis(1,at=c(-8,-6,-4,-2,0,2,4),labels=paste(c(-8,-6,-4,-2,0,2,4)))
axis(2)
box()
title(sub="Figure 2.3 ")

#hist(cd4.boot1$t,col="red")
#box()