#############################################################
###### n is the number of independent exponential sample#####
#### x is generated from a random standard normal distribution###
#### y is generated from x with mean=1/lambda
##### alpha0 is the initial value  for alpha
######  beta0 is the initial seed for beta
#####max.iter is the maximum allowed number of iterations
#####if the number iterations exceed max.iter,algortihm 
######failed to converge is displayed
#############################################################

rm(list=ls())

exponential.nr<-function(x,y,alpha0,beta0,tol=10^-6){


tol=10^-6
theta<-c(alpha0,beta0)

score<-c(3,3)
hess<-matrix(rep(NA,4),nrow=2)
max.iter=10000
i=0
#for(i in 1:maxiter){
 while(max(abs((score)))>tol){
score[1]<-sum(1/(theta[1]+theta[2]*x))-sum(y)
score[2]<-sum(x/(theta[1]+theta[2]*x))-sum(x*y)
hess[1,1]<- -sum(1/(theta[1]+theta[2]*x)^2)
hess[1,2]<- -sum(x/(theta[1]+theta[2]*x)^2)
hess[2,1]<-hess[1,2]
hess[2,2]<- -sum(x^2/(theta[1]+theta[2]*x)^2)
newtheta<-theta-solve(hess)%*%score
theta<-newtheta
alpha=theta[1]
beta=theta[2]
s<-max(abs((score)))
i=i+1
cat("The alpha is",alpha,"\n")
cat("The beta is",beta,"\n")
}
out.list<-list(alpha=theta[1],beta=theta[2],score,hess,number.iterations=i)
if (i<=max.iter){
return(out.list)

}else{
cat("Algorithm failed to converge")
}
}

n=10
x=abs(rnorm(n, mean = 0, sd = 1))

lambda<-1+3*x
y<-NULL
 for(i in 1:10){

y[i]<-rexp(1,lambda[i])
}

exponential.nr(x,y,alpha0=0.9,beta0=2.9,tol=10^-6)

system.time(exponential.nr(x,y,alpha0=0.9,beta0=2.8,tol=10^-6))

 