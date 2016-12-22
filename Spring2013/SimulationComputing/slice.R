
slice<- function(k,n)
{

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
return(knew)


}





par(mfrow=c(2,2))
y1<-slice(0.05,5000)
y2<-slice(0.5,5000)
y3<-slice(0.95,5000)
x<-seq(1,5000,by=1)
plot(x,y1,ylim = range(0:1),type="l")
abline(0.622,0)
title(main = list(paste("k=0.05"),col="black", cex = 1.0)) 
plot(x,y2,ylim = range(0:1),type="l")
title(main = list(paste("k=0.5"),col="black", cex = 1.0)) 
abline(0.622,0)
plot(x,y3,ylim = range(0:1),type="l")
title(main = list(paste("k=0.95"),col="black", cex = 1.0)) 
abline(0.622,0) 
matplot(x, cbind(y1,y2,y3),ylim = range(0:1), pch=20,ylab = "y1 y2 y3 ",,type="l")
abline(0.622,0)
#legend(y1,y2,y3,c("k=0.05", "k=0.5", "k=0.95"), pch = c(1,2,3), lty = c(1,2,3))
