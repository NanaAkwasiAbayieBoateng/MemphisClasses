library(boot)
cd4

#####################################################
### ###############statistic of interest,mean  ######
####################################################

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

diff=cd4$oneyear-cd4$baseline


#############################################################
##Estimation of bias########################################## 
R=999
mean(cd4.boot1$t)-cd4.boot1$t0
BR=(1/R)*sum(cd4.boot1$t)-mean(cd4.boot1$t0)
BR




################################Estimation of variance V  #################

var(cd4.boot1$t)

n=nrow(cd4)

tstar1=(sum(cd4.boot1$t-mean(cd4.boot1$t)))*(1/var(cd4.boot1$t))
#tstar1

vstar=((n-1)/n)*(1/(n*(n-1)))*sum((cd4.boot1$t-mean(cd4.boot1$t0))^2)
#vstar
var=(1/(R-1))*sum((cd4.boot1$t-mean(cd4.boot1$t0))^2)




#################################################################
#######################correlation coefficient################

corr(cd4, w = rep(1, nrow(cd4))/nrow(cd4))


###########################################################
############confidence intervals ########################
##########################################################
yb <- mean(cd4)
n <- nrow(cd4)




###########################Method 1: normal approximation #################

vhart=(yb^2)/n

L1=yb-qnorm(.975)*sqrt(vhart)



U1=yb + qnorm(.975)*sqrt(vhart)



  limit= c(U1 , L1)
  
  baseline=c(limit[3],limit[1])
  oneyear=c(limit[4],limit[2])
  baseline
  oneyear



# #########Nonparametric confidence intervals for mean##########################
# #################of the cd4 data using boot.ci##############################


boot.ci(cd4.boot1, conf = 0.95, type = c("norm", "basic", "perc", "stud","bca"),var.t0=var(cd4.boot1$t),t0=mean(cd4$baseline),
,var.t =cd4.boot1$t[,i] , t = cd4.boot1$t)

boot.ci(cd4.boot2)


################################################################################
#######################################percentile method#######################


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


###############################################################################
###########################studentized bootstrap method #######################
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


##########################################################################
##################################two sample t test#######################

t.test(cd4.boot1$t, mu=3.28)
t.test(cd4.boot1$t, cd4.boot2$t,var.equal=T)
t.test(cd4.boot1$t, cd4.boot2$t,var.equal=T)
t.test(cd4$baseline, cd4$oneyear,var.equal=T)



 #####################################################################
#########################################################graphs   #############

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
title(sub="baseline ")


#####################################quantile plots ########################
################### baseline   ############################################

x<-qgamma((1:999)/1000,NROW(cd4$baseline),rate=NROW(cd4$baseline)/mean(cd4$baseline))
y<-qgamma((1:99)/100,NROW(cd4$baseline),rate=NROW(cd4$baseline)/mean(cd4$baseline))

par(mfrow=c(2,2))

qqnorm(cd4.boot1$t[1:99],axes=F,pch=20,main="",xlim=c(-1,2.5),ylim=c(-1,7),xlab="Quantiles of standard normal",ylab="t*",cex=0.9)
axis(1,at=c(0,1,2,3,4,5),labels=paste(c(0,1,2,3,4,5)))
axis(2,at=c(-1,0,2,4,6,8),labels=paste(c(-1,0,2,4,6,8)))
qqline(cd4.boot1$t[1:99],lty=3)

box()

qqnorm(cd4.boot1$t[1:999],axes=F,pch=20,main="",xlim=c(0,2.5),ylim=c(-1,7),xlab="Quantiles of standard normal",ylab="t*",cex=0.5)
axis(1,at=c(-1,1,2),labels=paste(c(-1,1,2)))
axis(2,at=c(-1,2,4,6),labels=paste(c(-1,2,4,6)))
qqline(cd4.boot1$t[1:999],lty=3)
box()

qqplot(y,cd4.boot1$t[1:99],axes=F,pch=20,main="",xlim=c(2,5),ylim=c(0,10),xlab="Exact gamma quantile",ylab="t*",cex=0.9)
axis(1,at=c(0,2,4,6),labels=paste(c(0,2,4,6)))
axis(2,at=c(0,2,4,6),labels=paste(c(0,2,4,6)))
abline(a=0,b=1,lty=3)
box()

qqplot(x,cd4.boot1$t[1:999],axes=F,pch=20,main="",xlim=c(2,5),ylim=c(0,10),xlab="Exact gamma quantile",ylab="t*",cex=0.5)
axis(1,at=c(0,1,2,3,4),labels=paste(c(0,1,2,3,4)))
axis(2,at=c(0,2,4,6),labels=paste(c(0,2,4,6)))
abline(a=0,b=1,lty=3) 
box()

title(sub=" baseline")



##############################################quantile plots  #####################
########################################oneyear ##################################


x<-qgamma((1:999)/1000,NROW(cd4$oneyear),rate=NROW(cd4$oneyear)/mean(cd4$oneyear))
y<-qgamma((1:99)/100,NROW(cd4$oneyear),rate=NROW(cd4$oneyear)/mean(cd4$oneyear))

par(mfrow=c(2,2))

qqnorm(cd4.boot2$t[1:99],axes=F,pch=20,main="",xlim=c(-1,2.5),ylim=c(-1,7),xlab="Quantiles of standard normal",ylab="t*",cex=0.9)
axis(1,at=c(0,1,2,3,4,5),labels=paste(c(0,1,2,3,4,5)))
axis(2,at=c(-1,0,2,4,6,8),labels=paste(c(-1,0,2,4,6,8)))
qqline(cd4.boot2$t[1:99],lty=3)

box()

qqnorm(cd4.boot2$t[1:999],axes=F,pch=20,main="",xlim=c(2,4),ylim=c(-1,7),xlab="Quantiles of standard normal",ylab="t*",cex=0.5)
axis(1,at=c(2,3,4),labels=paste(c(2,3,4)))
axis(2,at=c(-1,2,4,8),labels=paste(c(-1,2,4,8)))
qqline(cd4.boot2$t[1:999],lty=3)
box()

qqplot(y,cd4.boot2$t[1:99],axes=F,pch=20,main="",xlim=c(2,5),ylim=c(0,10),xlab="Exact gamma quantile",ylab="t*",cex=0.9)
axis(1,at=c(0,2,4,6),labels=paste(c(0,2,4,6)))
axis(2,at=c(0,2,4,6),labels=paste(c(0,2,4,6)))
abline(a=0,b=1,lty=3)
box()

qqplot(x,cd4.boot2$t[1:999],axes=F,pch=20,main="",xlim=c(3,5),ylim=c(3,5),xlab="Exact gamma quantile",ylab="t*",cex=0.5)
axis(1,at=c(2,3,4),labels=paste(c(2,3,4)))
axis(2,at=c(2,4,6),labels=paste(c(2,4,6)))
abline(a=0,b=1,lty=3) 
box()

title(sub="oneyear")













#################################################################################
##############################Histogram plots####################################

par(mfrow=c(2,2))

hist(cd4.boot1$t[1:99],axes=F,probability=T,xlim=c(2.5,4),xlab="t*",ylim=c(0,2.5),ylab="",breaks = 12, col="lightblue", border="pink")
box()
title(sub="Baseline ")

hist(cd4.boot1$t[1:999],axes=F,probability=T,xlim=c(2.5,4),xlab="t*",ylim=c(0,2.5),ylab="",breaks = 12, col="lightblue", border="pink")
box()

title(sub="Baseline ")


hist(cd4.boot2$t[1:99],axes=F,probability=T,xlim=c(3,5),xlab="t*",ylim=c(0,2.5),ylab="",col="gray",breaks = "Sturges")
box()
title(sub="Oneyear ")
hist(cd4.boot2$t[1:999],axes=F,probability=T,xlim=c(3,5),xlab="t*",ylim=c(0,2.5),ylab="",col="gray",breaks = "Sturges")

box()

title(sub="Oneyear ")









#####################################################################################
######################################Density function ###############################
library(boot)
cd4
cd42<-cd4[1:10,]


t1<-mean(cd42$baseline)/mean(cd42$oneyear)
t2=mean(cd4$baseline)/mean(cd4$oneyear)
t2

v1L=sum((cd42$baseline-t1*cd42$oneyear)^2)/(nrow(cd42)*mean(cd42$baseline))^2


v2L<-sum((cd4$baseline-t2*cd4$oneyear)^2)/(nrow(cd4)*mean(cd4$baseline))^2

ratio<-function(data,i)
{ tstar<-mean(data$baseline[i])/mean(data$oneyear[i])
  vstar<-sum((data$baseline[i]-tstar*data$oneyear[i])^2)/(mean(data$baseline[i])*nrow(data))^2
  c(tstar,vstar)
}

#bootstrapping,geting t* and the corresponding densities


cd42.boot<-boot(cd42,ratio,R=999)


mb1<-mean(cd42.boot$t[,1])-t1

v1<-var(cd42.boot$t[,1])

x1<-sort(cd42.boot$t[,1])

d.a1<-dnorm(x1-t1,mean=mb1,sd=sqrt(v1))

d.b1<-dnorm(x1-t1,mean=0,sd=sqrt(v1L))
d.kde1<-rep(0,999)

bw1<-density(x1-t1)$bw
for(j in 1:999)
{ for(k in 1:999)
     { d.kde1[j]<-d.kde1[j]+dnorm((x1[j]-x1[k])/bw1,mean=0,sd=1)/(999*bw1) }
}
density1<-cbind(d.a1,d.b1,d.kde1)

cd4.boot<-boot(cd4,ratio,R=999)
mb2<-mean(cd4.boot$t[,1])-t2

v2<-var(cd4.boot$t[,1])

x2<-sort(cd4.boot$t[,1])
d.a2<-dnorm(x2-t2,mean=mb2,sd=sqrt(v2))
d.b2<-dnorm(x2-t2,mean=0,sd=sqrt(v2L))
d.kde2<-rep(0,999)
bw2<-density(x2-t2)$bw
for(j in 1:999)
{ for(k in 1:999)
     { d.kde2[j]<-d.kde2[j]+dnorm((x2[j]-x2[k])/bw2,mean=0,sd=1)/(999*bw2) }
}
density2<-cbind(d.a2,d.b2,d.kde2)

#now finally plotting
par(mfrow=c(1,2))
matplot(x1-t1,density1,axes=F,main="",xlim=c(-0.2,0.2),xlab="t*-t",ylim=c(0,11),ylab="PDF",pch="",col=1:3,cex=0.1)
matlines(x1-t1,density1,lty=c(2,3,1))
axis(1,at=c(-0.2,-0.1,0,0.2),labels=paste(c(-0.2,-0.1,0,0.2)))
axis(2,at=c(0,2,4,6,8,10),labels=paste(c(0,2,4,6,8,10)))
box()

matplot(x2-t2,density2,axes=F,main="",xlim=c(-0.2,0.2),xlab="t*-t",ylim=c(0,11.5),ylab="PDF",pch="",col=1:3,cex=0.1)
matlines(x2-t2,density2,lty=c(2,3,1))
axis(1,at=c(-0.2,-0.1,0,0.1,0.2),labels=paste(c(-0.2,-0.1,0,0.1,0.2)))
axis(2,at=c(0,2,4,6,8,10,12),labels=paste(c(0,2,4,6,8,10,12)))
box()

title("Probability Density Functions plots ")