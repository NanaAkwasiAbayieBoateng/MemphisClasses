
library(boot)
cd4




 #difference in colums
diff=((cd4$oneyear-cd4$baseline)/cd4$baseline)

diff.f1 <- function(data,f) mean(diff*f)
diff.boot1 <- boot(diff, diff.f1, stype="f", R=999)
diff.boot1$t
################################################################################
#######################################percentile method#######################

i <- order(diff.boot1$t)
yo <- diff.boot1$t[i]
y1 <- (yo[25])
y2 <- (yo[975])                                                 
(2*(mean(diff.boot1$t0))-(y1))
(2*(mean(diff.boot1$t0))-(y2))
CIB=c((2*(mean(diff.boot1$t0))-(y2)),(2*(mean(diff.boot1$t0))-(y1)))
CIB






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
yb <- mean(diff)
n <- nrow(cd4)




###########################Method 1: normal approximation #################

vhart=(yb^2)/n

L1=yb-qnorm(.975)*sqrt(vhart)



U1=yb + qnorm(.975)*sqrt(vhart)





   limit= c(L1,U1)

    limit


# #########Nonparametric confidence intervals for mean##########################
# #################of the cd4 data using boot.ci##############################


#using diff of means
library(boot)
cd4
diff=cd4$oneyear-cd4$baseline
 mean.fun <- function(d, i)
{    m <- mean(d[i])
     n <- length(i)
     v <- (n-1)*var(d[i])/n^2
     c(m, v)
}
diff.boot <- boot(diff, mean.fun, R = 999)
boot.ci(diff.boot, type = c("norm", "basic", "perc", "stud"))

abc.ci(diff,diff.f1,conf=0.95)
   
#using the correlationn
#non-parametric bootstrap for cdn4
 #non-parametric bootstrap for cd4
library(boot)
cd4
corr.fun<-function(d,w=rep(1,nrow(d))/nrow(d))
{w=w/sum(w)
n=nrow(d)
m1=sum(d[,1]*w)
m2=sum(d[,2]*w)
v1=sum(d[,1]^2*w)-m1^2

v2=sum(d[,2]^2*w)-m2^2
rho=(sum(d[,1]*d[,2]*w)-m1*m2)/sqrt(v1*v2)
i=rep(1:n,round(n*w))
us=(d[i,1]-m1)/sqrt(v1)

us

xs=(d[i,2]-m1)/sqrt(v2)
L=us*xs-0.5*rho*(us^2+xs^2)
c(rho,sum(L^2)/n^2)

}

cd4.boot=boot(cd4,corr.fun,R=999,stype="w")

boot.ci(cd4.boot, type = c("norm", "basic", "perc", "stud"))

abc.ci(cd4,corr,conf=0.95)  


#fisher transformation
fisher=function(r)0.5*log((1+r)/(1-r))  
fisher.dot=function(r) 1/(1-r^2)
fisher.inv=function(z) (exp(2*z)-1)/exp((2*z)+1)
boot.ci(cd4.boot,h=fisher,hdot=fisher.dot,hinv=fisher.inv,conf=0.95)
##################################################r############################
###########################studentized bootstrap method #######################
i <- order(diff.boot1$t)
yo <- diff.boot1$t[i]
y1 <- (yo[25])
y2 <- (yo[975])
vhart=(mean(diff.boot1$t)^2)/n
z=(i-diff.boot1$t)/sqrt(var(i))

l=mean(diff.boot1$t)-sqrt(vhart)*z[25]

u=mean(diff.boot1$t)-sqrt(vhart)*z[975]
c(l,u)




##########################################################################
################################## paired t sample t test#######################
a=cd4$baseline
b=cd4$oneyear

 t.test(b,a, paired=TRUE)



 #####################################################################
#########################################################graphs   #############

 #################################################################################
##############################Histogram plots####################################
#hist( diff.boot1$t[1:99] )
par(mfrow=c(1,2))

x1=   diff.boot1$t[1:99]
x2=   diff.boot1$t[1:999]

histnorm=function(x, ...)

{
   if (length(x)>0)
   {
      hist(x,freq=FALSE, ...)
      rug(x)
      curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE, col="red", lty="dotted", xaxt="n")
      abline(v=mean(x),col="blue")
      mtext(paste("mean ", round(mean(x),1), "; sd ", round(sd(x),1), "; N ", length(x),sep=""), side=1, cex=.75)
   } # fi
} # histnorm
 histnorm(x1)
  title(sub="t*=99")   
            
 histnorm(x2)
 title(sub="t*=999")
 


#bootstrap average
library(boot)
diff
 
diff.f <- function(data,i)
{                   
   d<-data[i]
   c(mean(d),var(d))
}




diff.bootnp<-boot( diff, diff.f, R=999)




par(mfrow=c(1,2))


plot(diff.bootnp$t[1:99,1],sqrt(diff.bootnp$t[1:99,2]),axes=F,main="",xlim=c(0.2,1.5),ylim=c(0.4,1.2),xlab="Bootstrap average(t*=99)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0.2,0.4,0.8,1.4,1.5),labels=paste(c(0.2,0.4,0.8,1.4,1.5)))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.2),labels=paste(c(0,0.2,0.4,0.6,0.8,1.2)))
box()



plot(diff.bootnp$t[,1],sqrt(diff.bootnp$t[,2]),axes=F,main="",xlim=c(0.2,1.5),ylim=c(0.4,1.2),xlab="Bootstrap average(t*=999)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0.2,0.4,0.8,1.4,1.5),labels=paste(c(0.2,0.4,0.8,1.4,1.5)))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.2),labels=paste(c(0,0.2,0.4,0.6,0.8,1.2)))
box()


 #title(histogram plots of non-parametric bootstrap samples of the difference of the diff in means)
  



# Multiple Linear Regression 


library(boot)
cd4

plot(cd4$baseline,cd4$oneyear)
  cd4.lm <- glm(cd4$oneyear ~ cd4$baseline, data=cd4)
summary(cd4.lm) 
cd4.diag=glm.diag.plots(cd4.lm,ret=T)
# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(cd4.lm)



#graphs Empirical biases and variance

library(boot)
bias<-matrix(nrow=8,ncol=4)

variance<-matrix(nrow=8,ncol=4)




i<-c(10,20,50,100,200,300,400,500)
bias<-matrix(nrow=8,ncol=4)

variance<-matrix(nrow=8,ncol=4)


for(j in 1:4)
{
diff.boot1<-boot(diff,diff.f,R=1000)
for(k in 1:8)
  { bias[k,j]<-mean(diff.boot1$t[1:i[k]])-mean(diff)
    variance[k,j]<-var(diff.boot1$t[1:i[k]])
  }
}

par(mfrow=c(1,2))
matplot(log(i),bias,axes=F,xlim=c(2.2,6.3),ylim=c(-0.00004,0.08),xlab="R",ylab="Bias",type="c",pch=20,col=1,lty=1)
#matplot(i,bias,axes=F,xlim=c(10,500),ylim=c(-50,10),xlab="R",ylab="Bias",type="c",pch=20,col=1,lty=1)
matpoints(log(i),bias,lty=1,pch=".",col=1,cex=1.5)
axis(1,at=c(log(10),log(50),log(100),log(500)),labels=paste(c(10,50,100,500)))
axis(2,at=c(-0.00004,-0.004,-0.04,0.0004,0.004,0.04,0.08),labels=paste(c(-0.00004,-0.004,-0.04,0.0004,0.004,0.04,0.08)))
abline(h=0,lty=3)
#abline(h=mean(diff)^2/NROW(diff),lty=3)
box()

matplot(log(i),variance,axes=F,xlim=c(2.2,6.3),ylim=c(-0.0002,0.08),xlab="R",ylab="Variance",type="c",pch=20,col=1,lty=1)
matpoints(log(i),variance,lty=1,pch=".",col=1,cex=1.5)
axis(1,at=c(log(10),log(50),log(100),log(500)),labels=paste(c(10,50,100,500)))
axis(2,at=c(-0.0004,-0.004,-0.04,0.0004,0.004,0.04,0.08),labels=paste(c(-0.0004,-0.004,-0.04,0.0004,0.004,0.04,0.08)))
abline(h=mean(diff)^2/NROW(diff),lty=3)
box()
#title(sub=" ")





#####################################quantile plots ########################
################### baseline   ############################################

x<-qgamma((1:999)/1000,NROW(diff),rate=NROW(diff)/mean(diff))
y<-qgamma((1:99)/100,NROW(diff),rate=NROW(diff)/mean(diff))

par(mfrow=c(2,2))

qqnorm(diff.boot1$t[1:99],axes=F,pch=20,main="",xlim=c(-1,2.5),ylim=c(-1,2),xlab="Quantiles of standard normal",ylab="t*",cex=0.9)
axis(1,at=c(-1,-.5,.5,1,1.5,2,2.5),labels=paste(c(-1,-.5,.5,1,1.5,2,2.5)))
axis(2,at=c(-1,-.5,.5,1,1.5,2),labels=paste(c(-1,-.5,.5,1,1.5,2)))
qqline(diff.boot1$t[1:99],lty=3)

box()

qqnorm(diff.boot1$t[1:999],axes=F,pch=20,main="",xlim=c(0,2.5),ylim=c(-1,1.5),xlab="Quantiles of standard normal",ylab="t*",cex=0.5)
axis(1,at=c(-1,1,2),labels=paste(c(-1,1,2)))
axis(2,at=c(-1,2,4,6),labels=paste(c(-1,2,4,6)))
qqline(diff.boot1$t[1:999],lty=3)
box()

qqplot(y,diff.boot1$t[1:99],axes=F,pch=20,main="",xlim=c(0.4,1.2),ylim=c(-1,2),xlab="Theoritical  quantile",ylab="t*",cex=0.9)
axis(1,at=c(-1,-.5,.5,1,1.5,2),labels=paste(c(-1,-.5,.5,1,1.5,2)))
axis(2,at=c(-1,-.5,.5,1,2),labels=paste(c(-1,-.5,.5,1,2)))
abline(a=0,b=1,lty=3)
box()

qqplot(x,cd4.boot1$t[1:999],axes=F,pch=20,main="",xlim=c(0.3,1.5),ylim=c(3,4),xlab="Theoritical gamma quantile",ylab="t*",cex=0.5)
axis(1,at=c(0.3,0.6,0.9,1.2,1.5),labels=paste(c(0.3,0.6,0.9,1.2,1.5)))
axis(2,at=c(3,3.5,4),labels=paste(c(3,3.5,4)))
abline(a=0,b=1,lty=3) 
box()

title(sub="Theoritical quantiles,qgamma")



####plotting the  average bootstrap########################
library(boot)
diff
 
diff.f <- function(data,i)
{                   
   d<-data[i]
   c(mean(d),var(d))
}




diff.bootnp<-boot( diff, diff.f, R=999)




par(mfrow=c(1,2))


plot(diff.bootnp$t[1:99,1],sqrt(diff.bootnp$t[1:99,2]),axes=F,main="",xlim=c(0.2,1.5),ylim=c(0.4,1.2),xlab="Bootstrap average(t*=99)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0.2,0.4,0.8,1.4,1.5),labels=paste(c(0.2,0.4,0.8,1.4,1.5)))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.2),labels=paste(c(0,0.2,0.4,0.6,0.8,1.2)))
box()



plot(diff.bootnp$t[,1],sqrt(diff.bootnp$t[,2]),axes=F,main="",xlim=c(0.2,1.5),ylim=c(0.4,1.2),xlab="Bootstrap average(t*=999)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0.2,0.4,0.8,1.4,1.5),labels=paste(c(0.2,0.4,0.8,1.4,1.5)))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.2),labels=paste(c(0,0.2,0.4,0.6,0.8,1.2)))
box()

title(sub="Average bootstrap for R=99(left) and R=999(right) ")


#density function plots


library(boot)
cd4
cd42<-cd4[1:10,]
cd42<-cd4[1:20,]

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

# Define colors to be used for cars, trucks, suvs
#plot_colors <- c(rgb(r=0.0,g=0.0,b=0.9), "red", "forestgreen")

# Start PDF device driver to save output to figure.pdf
#pdf(file="C:/Desktop/graph1", height=3.5, width=5)

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

title(" Density Estimates plots ")