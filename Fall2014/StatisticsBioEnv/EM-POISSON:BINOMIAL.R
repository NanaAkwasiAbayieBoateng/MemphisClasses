rm(list=ls())
options(digits = 10)
n=1000
lambda1=10
theta1=0.8
x=rpois(n,lambda1)




y=c()
theta=c()
lambda=rep(0,n)
theta[1]=3
lambda[1]=3
diff=c()
for (i in 1:n){
  
  
  y[i]=rbinom(size=x[i], n=1, prob=theta1)
  
   
  
  
}





for (i in 2:n){
  
lambda[i] =lambda[i-1]*(1-theta[i-1])+mean(y) 
  
theta[i]=sum(y)/(sum(y)+n*lambda[i]*(1-theta[i-1]))

diff[i]=abs(lambda[i]-lambda[i-1])
  
}

lambda[(n-10):n]
theta[(n-10):n]
diff[(n-10):n]








theta=3
lambda=3
tol=10^(-9)
iter<-0
maxiter=10000

diff=c(1,1)
#while((iter<maxiter)){  
while((iter<maxiter)&&((diff)>tol)){ 
    newlambda =lambda*(1-theta)+mean(y) 
    
    newtheta=sum(y)/(sum(y)+n*newlambda*(1-theta))
    
    
    diff1=abs(newlambda-lambda)
    diff2=abs(newlambda-lambda)
    diff=c(diff1,diff2)
    lambda=newlambda
    theta=newtheta
   
    iter=iter+1
  
    
  }

newlambda
newtheta
diff

