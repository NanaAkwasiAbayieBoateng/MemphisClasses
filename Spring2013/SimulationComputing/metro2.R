metro=function(a,b,c,n)
{
#n=5000
#a=56
#b=34
#c=0.5
x=c
for (i in 2:n){

y=runif(1)
rho=dbeta(y,a,b)/dbeta(x[i-1],a,b)
x[i]=x[i-1]+(y-x[i-1])*(runif(1)<rho)
}
avex=cumsum(x)/(1:n)
}
n=5000
time=1:n


par(mfrow=c(2,2))
y1=metro(56,34,0.5,5000)
plot(time,y1,type="l")
abline(0.622,0)
title(main = list(paste("beta(56,34)"),col="black", cex = 1.0))
y2=metro(30,30,0.5,5000)
plot(time,y2,type="l")
abline(0.622,0)
title(main = list(paste("beta(30,30)"),col="black", cex = 1.0))
y3=metro(55,30,0.5,5000)
plot(time,y3,type="l")
abline(0.622,0)
title(main = list(paste("beta(55,30)"),col="black", cex = 1.0))
matplot(time, cbind(y1,y2,y3),ylim = range(0:1),ylab = "y1 y2 y3 ",,type="l")
abline(0.622,0)
title(main = list(paste("All three"),col="black", cex = 1.0))



par(mfrow=c(2,2))
y1=metro(56,34,0.95,5000)
plot(time,y1,type="l")
abline(0.622,0)
title(main = list(paste("beta(56,34),y0=0.95"),col="black", cex = 1.0))
y2=metro(56,34,0.5,5000)
plot(time,y2,type="l")
abline(0.622,0)
title(main = list(paste("beta(56,34),y0=0.5"),col="black", cex = 1.0))
y3=metro(55,30,0.05,5000)
plot(time,y3,type="l")
abline(0.622,0)
title(main = list(paste("beta(56,34),y0=0.05"),col="black", cex = 1.0))
matplot(time, cbind(y1,y2,y3),ylim = range(0:1),ylab = "y1 y2 y3 ",,type="l")
abline(0.622,0)
title(main = list(paste("All three"),col="black", cex = 1.0))
