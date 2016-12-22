rm(list=ls())
#install.packages("psych")
library(psych)

n=100
m=75
mu1=5
sigma21=5
z=rnorm(n,mu1,sigma21)

beta1=3
beta2=4
sigma=7
yz=c()
for (i in 1:n){
  
  yz[i]=rnorm(1,beta1+beta2*z[i],sigma)
}

z1=matrix(z[1:m],m,1)
y1=matrix(yz[1:m],m,1)
y2=matrix(yz[(m+1):n],n-m,1)


N=1000  #number of iterations
m1=rep(3,N)  ## mu 1  mean of z
s=rep(8,N)  # variance of y|z
s[1]=8
#s=c()
s1=rep(4,N)  #variance of z
s1[1]=4
#s1=c()
b1=rep(1,N)  #beta1
b1[1]=1
#b1=c()
b2=rep(5,N) 

#b2=c()
b2[1]=5

A=matrix(0,N,n-m)   #mean of z2|y2

d=matrix(0,N)
B=matrix(0,N)  # variance of z2|y2

d[1]=(s1[1]*s[1])/(s1[1]*b2[1]^2+s[1])
A[1,]=(1/(s1[1]*b2[1]^2))*(m1[1]*s[1]+s1[1]*y2-b1[1]*b2[1]*s1[1])
B[1]=tr(diag(d[1],n-m,n-m))

for (i in 1:N){
  
  A[i,]=(1/(s1[i]*b2[i]^2))*(m1[i]*s[i]+s1[i]*y2-b1[i]*b2[i]*s1[i])
  B[i]=tr(diag(d[i],n-m,n-m))
  
}



for(i in 2:N){
  
  d[i]=(s1[i-1]*s[i-1])/(s1[i-1]*b2[i-1]^2+s[i-1]) 
  
  #B[i]=tr(diag(d[i],n-m,n-m)) 
  
  s[i]=(sum(y1^2)-2*sum(y1)*b1[i-1]-2*sum(y1*z1)*b2[i-1]+m*b1[i-1]^2+2*b1[i-1]*b2[i-1]*sum(z1))/n
        +(sum(y2^2)-2*sum(y2)*b2[i-1]-2*t(y2)%*%A[i,]*b2[i]+(n-m)*b1[i-1]^2+
         2*b1[i-1]*b2[i-1]*sum(A[i,])+(b2[i-1]^2)+(b2[i-1]^2)*(B[i-1]+t(A[i,])%*%A[i,]))/n
  
    
  s1[i]=(B[i]+t(A[i-1,])%*%A[i-1,]-2*m1[i]*sum(A[i-1,])+(n-m)*m1[i]^2)/(n-m)
  
 # mu1  
  m1[i]=sum(A[i-1,])/(n-m)
 
 b1[i]=(sum(y1)-b2[i-1]*sum(z1)+sum(y2)-b2[i-1]*sum(A[i-1,]))/n
 
 
 b2[i]=(1/(sum(z1^2)+t(A[i,])%*%A[i,]+B[i]))*(sum(z1*y1)-b1[i]*sum(z1)+
                                                sum(y2*A[i,])-b1[i]*sum(A[i,]))
  
}



b1[(N-10):N]
b2[(N-10):N]



#warnings() 