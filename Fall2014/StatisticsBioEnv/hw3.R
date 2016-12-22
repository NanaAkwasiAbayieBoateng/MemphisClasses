
metrop1=function(n=1000,eps=0.5) 
{
  vec=vector("numeric", n)
  x=0
  vec[1]=x
  for (i in 2:n) {
    innov=runif(1,-eps,eps)
    can=x+innov
    aprob=min(1,dnorm(can)/dnorm(x))
    u=runif(1)
    if (u < aprob) 
      x=can
    vec[i]=x
  }
  vec
}


n=1000
i=2
x=c()
u1=c()
u2=c()
r=c()
j=2
lambda=6
d=0.5
for (k in 1:n){
  
 if(i!=0){ 
   
 x[k] =j
 u1=runif(n, min = -d, max = d)
 u2=runif(n, min = -d, max = d)
  if (u1[k]>0.5){
    j=i+1
  }else{
    
    j=i-1
  }
  

}else{
  u1=runif(n,0,1)
  
  if (u1[k]<0.5){
    j=0
  }else{
    
    j=1
  }  
  
}
r[i]=(factorial(i)*lambda^(j-1))/ (factorial(j))
if(r[i]>=1){
  x[k+1]=j 
}else{
  
 if(u2[k]<r){
   x[k+1]=j  
 }else{
   x[k+1]=x[k] 
 } 
  
  
}
}
x
u1
u2
r



poisson.metropolis=function(lamda,i,n)
{
  y=seq(n)
  for(k in 1:n)
  {
    u1=runif(1)
    j =if(u1<.5)
      ifelse(i==0,i,i-1) else i+1
    r =switch(i+2-j,lamda/j,1,i/lamda)
    u2 =runif(1)
    new=if(r>=1)j else
    {if(u2<r)j else i}
    i=new
    y[k]=i
  }
  return(y)
}


poisson.metropolis(lamda=6,i=2,n)


#x=seq(n)
x[1]=i
for(h in 2:n){

if(i!=0){
 u1=runif(1,0,1) 
 if(u1>=0.5){
 j=i+1 
r= lambda/j
 }else {j=i-1 
r=i/lambda
 }
}else{
 i=0
 if(u1<0.5){
 j=0 
 r=1
 }else j=1
 
}

if(r>=1){
  x[h]=j
}else{
  u2=runif(1,0,1)
  if(u2<r){
    x[h]=j
  }else x[h]=x[h-1]
  h=h+1
}
}
x


###############13.5.6###################
###########continuous metropolis##########



y=c()
piy=c()
pix=c()
x=c()
x[1]=2
n=10000
for (t in 1:n){

  ###proposal density######
y[t]=rnorm(1,mean=x[t],sd=0.4)

## target density pi(y)=cexp(-y^2/2)
piy[t]=exp(-0.5*y[t]^2)

##target density pi(x)=cexp(-x^2/2)
pix[t]=exp(-0.5*x[t]^2)

r[i]=piy[t]/pix[t]

u=runif(n=1, min = -d, max = d)

if(u<=r[i]){
 x[t+1]=y[t] 
}else
  x[t+1]=x[t]  
}

burnin=500
plot.mcmc<-function(mcmc.out)
{
  op=par(mfrow=c(2,2))
  plot(ts(mcmc.out[burnin:n]),col=2)
  abline(h=mean(mcmc.out),col="black",lw=2)
  hist(mcmc.out,prob=TRUE,col="lightgreen"
       ,axes=TRUE,breaks = "fd",main="  target density",xlab="",ylab="")
  abline(v=mean(mcmc.out),col="red",lw=2)
  box(lty = '1373', col = 'black')
  #qqnorm(mcmc.out,col=4)
  
  acf(mcmc.out,col=2,lag.max=100)
  box(lty = '1373', col = 'black')
  par(op)
}

plot.mcmc(x)

hist(x,prob=TRUE,col="lightgreen"
     ,axes=TRUE,breaks = "fd",main=" normal target density",xlab="",ylab="")
lines(density(x),lw=2)
abline(v=mean(x),col="red",lw=2)
box(lty = '1373', col = 'black')
#box(lty = 'solid', col = 'black')


###############13.5.7###################
###########continuous metropolis##########
y=c()
piy=c()
pix=c()
x=c()
x[1]=2
n=10000
theta=2
for (t in 1:n){
  
  ###proposal density######
  y[t]=rnorm(1,mean=x[t],sd=0.4)
  
  ## target density pi(y)=cexp(-y^2/2)
  piy[t]=dexp(y[t],rate=theta)
  
  ##target density pi(x)=cexp(-x^2/2)
  pix[t]=dexp(x[t],rate=theta)
  
  r[i]=piy[t]/pix[t]
  
  u=runif(n=1, min = 0, max = 1)
  
  if(u<=r[i]){
    x[t+1]=y[t] 
  }else
    x[t+1]=x[t]  
}

plot.mcmc(x)
hist(x,prob=TRUE,col="lightgreen"
     ,axes=TRUE,breaks = "fd",main="Metroplis exponential target density",xlab="",ylab="")
lines(density(x),lw=2)
abline(v=mean(x),col="red",lw=2)
box(lty = '1373', col = 'black')
#box(lty = 'solid', col = 'black')

hist(rexp(n,2))
mean(rexp(n,2))

###############13.5.7##############################
###########continuous metropolis hastings##########
a=c()
for (t in 1:n){
  
  ###proposal density######
  
  y[t]=rnorm(1,mean=x[t],sd=0.4)
  
  ## target density pi(y)=thetaexp(-ytheta),pi(x)=thetaexp(-xtheta)
  ######## pi(y)/pi(x)=exp(theta(x-y))

 
  a[i]=min(exp(theta*(x[t]-y[t]))*(y[t]/x[t]),1)
  
  u=runif(n=1, min = 0, max = 1)
  
  if(u<=a[i]){
    x[t+1]=y[t] 
  }else
    x[t+1]=x[t]  
}
plot.mcmc(x)
hist(x,prob=TRUE,col="lightgreen"
     ,axes=TRUE,breaks = "fd",main=" M-H  target density",xlab="",ylab="")
lines(density(x),lw=2)
abline(v=mean(x),col="red",lw=2)
box(lty = '1373', col = 'black')

###############13.5.8##############################
###########continuous metropolis hastings##########

a=c()
#x[1]=runif(n=1, min = 0, max =1)
x[1]=-0.09
(x[1]^34)*((1-x[1])^38)*(2+x[1])^125
for (t in 1:n){
  
  ###proposal density######
  y[t]=runif(n=1, min = x[t]-1, max = x[t]+1)
 
  ## ####target density pi(y)=ay34(1-y)38(2+y)125,pi(x)=ax34(1-x)38(2+x)125#######
  ######## #######################################################################
  
  piy[t]=(y[t]^34)*((1-y[t])^38)*(2+y[t])^125
  pix[t]=(x[t]^34)*((1-x[t])^38)*(2+x[t])^125
  a[i]=min(( piy[t]* y[t])/(pix[t]*x[t]),1)
  
 
  
  u=runif(n=1, min = 0, max = 1)
  
  if(u<=a[i]){
    x[t+1]=y[t] 
  }else
    x[t+1]=x[t]  
}


plot.mcmc(x)
hist(x,prob=TRUE,col="lightgreen"
     ,axes=TRUE,breaks = "fd",main=" M-H exponential target density",xlab="",ylab="")
lines(density(x),lw=2)
abline(v=mean(x),col="red",lw=2)
box(lty = '1373', col = 'black')

x=seq(-1,1,length.out=100)
y=(x^34)*((1-x)^38)*(2+x)^125
plot(x,y,type="l")

###############13.5.3##############################
###########continuous metropolis hastings##########
a=c()
scale=2
shape=2
x=rep(1,n)

for (t in 1:n){
  
  ###proposal density######

  y[t]=rgamma(n=1, round(shape), round(shape)/shape )
  
  ## target density ###################
 
  
  piy[t]=dgamma(y[t], shape, scale, log = FALSE)
  pix[t]=dgamma(x[t], shape, scale, log = FALSE)
 
  
  a[i]=min(((piy[t]*y[t])/(pix[t]*x[t])),1)
  
  u=runif(n=1, min = -d, max = d)
  
  if(u<=a[i]){
    x[t+1]=y[t] 
  }else
    x[t+1]=x[t]  
}
plot.mcmc(x)
hist(x,prob=TRUE,col="lightgreen"
     ,axes=TRUE,breaks = "fd",main=" M-H  target density",xlab="",ylab="")
lines(density(x),lw=2)
abline(v=mean(x),col="red",lw=2)
box(lty = '1373', col = 'black')
plot(ts(x),col=2)
box(lty = '1373', col = 'black')

###############13.5.9##############################
###########gibbs smapling##########################
rm(list=ls())


x=c()
betabinomial<-function(k,yp){

  y=c()
  x=c()
  y[1]=yp
  x[1]=rbinom(n=1, size=15, prob=y[1])
  alpha=1
  beta=2
  for (i in 2:k){
  
    x[i]=rbinom(n=1, size=15, prob=y[i-1])
    y[i]=rbeta(n=1, shape1=x[i]+alpha, shape2=15-x[i]+beta, ncp = 0)
    
  }

return(list(y=y[(k-20):k],x=x[(k-20):k]))
}

betabinomial(1000,1/3)[]
betabinomial(1000,1/2)
betabinomial(1000,2/3)

gibbsplot=function(x){
  burn=500
  mat[(N-10):N,]
  par(mfrow=c(2,1))
  plot(1:N,x, type="l", col="purple")
  abline(h=mean(x),col="black",lw=2)
  hist(x,prob=TRUE,col="lightgreen"
       ,axes=TRUE,breaks = "fd",xlab="",ylab="")
  lines(density(x),main="x2",lw=2)
}
###############13.5.10################################
########### gibbs smapling ##########################


bivariate= function(k,y1,x1){

  y=c()
  x=c()
  y[1]=y1
  x[1]=x1

  for (i in 2:k){
    
    x[i]=exp(-(x[i-1]+4*x[i-1]*y[i-1]))/(1+4*y[i-1])
    
    y[i]=exp(-(y[i-1]+4*x[i]*y[i-1]))/(1+4*x[i])
    
  }
  
 
  return(list(y=y,x=x))
}

bivariate(25,2,4)


###############13.5.11################################
########### gibbs smapling ##########################
rm(list=ls())
library(msm)      # For truncted normal
library(tmvtnorm)		# Multivariate trunctaed normal
library(mvtnorm)# Multivariate normal

N=1000
x1=rep_len(x=12, length.out=N)
x2=rep_len(x=12, length.out=N)
mean=c(0,0)
sigma1=1
sigma2=1
rho=0.7
gibbsampling=function(N,thin)
{
  mat=matrix(0,ncol=3,nrow=N)
  mat[,1]=1:N
  x=0
  y=0
  for (i in 1:N) {
    for (j in 1:thin) {
      x1=rnorm(n=1, mean = mean[1]+(sigma1/sigma2)*rho*(x2-mean[2]), sd = (1-rho^2)*sigma1^2)
      y2=rnorm(n=1, mean = mean[2]+(sigma2/sigma1)*rho*(x1-mean[1]), sd = (1-rho^2)*sigma2^2)
    }
    mat[i,2:3]=c(x,y)
  }
  mat=data.frame(mat)
  names(mat)=c("Iter","x","y")
  mat
}
gibbsampling(1000,500)

gibbsplot=function(x){
  burn=500
  mat[(N-10):N,]
  par(mfrow=c(2,1))
  plot(1:N,x, type="l", col="purple")
  abline(h=mean(x),col="black",lw=2)
  hist(x,prob=TRUE,col="lightgreen"
       ,axes=TRUE,breaks = "fd",xlab="",ylab="")
  lines(density(x),main="x2",lw=2)
}
gibbsplot(mat[,1])
gibbsplot(mat[,2])



##############13.5.12################################
########### gibbs smapling ##########################
rm(list=ls())

N=1000
x1=rep_len(x=12, length.out=N)
x2=rep_len(x=12, length.out=N)
mean=c(0,0)
sigma1=2
sigma2=1
rho=1
mat=matrix(0,N,2)

for (i in 2:N){
  x1=rnorm(n=1, mean = mean[1]+(sigma1/sigma2)*rho*(x2-mean[2]), sd = (1-rho^2)*sigma1^2)
  
  x2=rnorm(n=1, mean = mean[2]+(sigma2/sigma1)*rho*(x1-mean[1]), sd = (1-rho^2)*sigma2^2)
  
 
  mat[i,]=c(x1,x2)  

}

gibbsampling=function(N,thin)
{
  mat=matrix(0,ncol=3,nrow=N)
  mat[,1]=1:N
  x=0
  y=0
  for (i in 1:N) {
    for (j in 1:thin) {
      x1=rnorm(n=1, mean = mean[1]+(sigma1/sigma2)*rho*(x2-mean[2]), sd = (1-rho^2)*sigma1^2)
      y2=rnorm(n=1, mean = mean[2]+(sigma2/sigma1)*rho*(x1-mean[1]), sd = (1-rho^2)*sigma2^2)
    }
    mat[i,2:3]=c(x,y)
  }
  mat=data.frame(mat)
  names(mat)=c("Iter","x","y")
  mat
}

mat=gibbsampling(10000,500)


gibbsplot=function(x){
  burn=500
  mat[(N-10):N,]
  par(mfrow=c(2,1))
  plot(1:N,x, type="l", col="purple")
  abline(h=mean(x),col="black",lw=2)
  hist(x,prob=TRUE,col="lightgreen"
       ,axes=TRUE,breaks = "fd",xlab="",ylab="")
  lines(density(x),main="x2",lw=2)
}
gibbsplot(mat[,2])