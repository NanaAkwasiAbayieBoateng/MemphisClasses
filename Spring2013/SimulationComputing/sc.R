k=0.05
n=500
f1<-((2+k)/2)^125

f2<-(1-k)^38

f3<-k^34

 for (i in 1:n){

z1<-runif(n, min=0, max=f1)

z2<-runif(n, min=0, max=f2)

z3<-runif(n, min=0, max=f3)

k1<-(2*z1^(1/125))-2

k2<-1-z2^(1/38)

k3<-z3^(1/34)


A1<-matrix(c(k1,k3),nrow=n)

A<-apply(A1,1,max)

B<-1-z2^(1/38)

knew<-runif(n, A, max=B)

x<-seq(1,n,by=1)



knew<-cumsum(knew) / seq_along(knew)


k=knew
f1<-((2+k)/2)^125

f2<-(1-k)^38

f3<-k^34
  
  
 }

plot(x,knew,lty = 1, type = "o"k=0.05
n=500
f1<-((2+k)/2)^125

f2<-(1-k)^38

f3<-k^34

 for (i in 1:n){

z1<-runif(n, min=0, max=f1)

z2<-runif(n, min=0, max=f2)

z3<-runif(n, min=0, max=f3)

k1<-(2*z1^(1/125))-2

k2<-1-z2^(1/38)

k3<-z3^(1/34)


A1<-matrix(c(k1,k3),nrow=n)

A<-apply(A1,1,max)

B<-1-z2^(1/38)

knew<-runif(n, A, max=B)

x<-seq(1,n,by=1)



knew<-cumsum(knew) / seq_along(knew)


k=knew
f1<-((2+k)/2)^125

f2<-(1-k)^38

f3<-k^34
  
  
 }

plot(x,knew,lty = 1, type = "o"k=0.05
n=500
f1<-((2+k)/2)^125

f2<-(1-k)^38

f3<-k^34

 for (i in 1:n){

z1<-runif(n, min=0, max=f1)

z2<-runif(n, min=0, max=f2)

z3<-runif(n, min=0, max=f3)

k1<-(2*z1^(1/125))-2

k2<-1-z2^(1/38)

k3<-z3^(1/34)


A1<-matrix(c(k1,k3),nrow=n)

A<-apply(A1,1,max)

B<-1-z2^(1/38)

knew<-runif(n, A, max=B)

x<-seq(1,n,by=1)



knew<-cumsum(knew) / seq_along(knew)


k=knew
f1<-((2+k)/2)^125

f2<-(1-k)^38

f3<-k^34
  
  
 }

plot(x,knew,ylim = range(0.00:1))
abline(0.622,0)
title(main = list(paste("k=0.05"),col="black", cex = 1.0))























