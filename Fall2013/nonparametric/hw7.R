# 10.1
x=c(43.8,40.1,49.2,41.8,34,49.1,47.8,48.1,37.6,42,43.7,47.1,47.7,46.9,36.5,
     45,48,37.6,42.2,38.7,45.2,42.5,43.1,36,47.4,48.5,47.1,43.2,43.8,45.7)



h=1.06*sd(x)/(length(x)^(1/5))
par(mfrow=c(3,2))
out=hist(x,probability = TRUE,nclass =10)
z=(out$breaks[-1]+out$breaks[-9])/2
lines(out$density~z,col="red")
 # In stead of frequency
   #breaks = "sturges",      # For more breaks than the default
  # col = "darkslategray4", border = "seashell3",main= "March Temperaturesity for Kansas ",ylab="Density Estimation",xlab="Temperature")
#lines(density(x ),   # Add the kernel density estimate (fix for the bins)
   #col = "firebrick2", lwd = 3)
#n=length(x)
#hist(x,prob=TRUE,breaks = "sturges",col = "darkslategray4", border = "seashell3"
#,main= "March Temperaturesity for Kansas ",ylab="Density Estimation",xlab="Temperature")


#Estimating the density using histogram and kernel estimation



#kernel density estimation
d1=density(x,kernel="gaussian",bw=h)
plot(d1,main="Gaussian kernel,h=rule of thumb")

d2=density(x,kernel="gaussian",bw=1)
plot(d2,main="Gaussian kernel,h=1")


d3=density(x,kernel="triangular",bw=h)
plot(d3,main="Triangular Kernel,h=rule of thumb")


d4=density(x,kernel="triangular",bw=1)
plot(d4,main="Triangular Kernel,h=1")


#duration = x  #faithful$eruptions
#n=length(duration)
#estimate the density using histogram and kernel estimation
#par(mfrow=c(2,2))
#histogram
#out=hist(duration, nclass=15, prob=TRUE, main="Histogram, h=0.2")
#z = (out$breaks[-1] + out$breaks[-19])/2
#lines(out$density~z, col="blue")
#kernel density estimation
#d1 = density(duration, kernel="gaussian", bw=0.2)
#plot(d1, main="Gaussian kernel, h=0.2")
#d2=density(duration, kernel="gaussian", bw=0.05)
#plot(d2, main="Gaussian kernel, h=0.05")
#d3=density(duration, kernel="triangular", bw=0.2)
#plot(d3,main="Triangular kernel, h=0.2")





#10.3
mpg=c(65.4,56,55.9,49,46.5,46.2,45.4,59.2,53.3,43.4,41.1,40.9,40.9,40.4,39.6,
       39.3,38.9,38.8,38.2,42.2,40.9,40.7,40,39.3,38.8,38.4,38.4,38.4,29.5,
        46.9,36.3,36.1,35.4,35.3,35.1,35.1,35,33.2,32.9,32.3,32.2,32.2,32.2,32.2
         ,31.5,31.5,31.4,31.4,31.2,33.7,32.6,31.3,31.3,30.4,28.9,28,28,28,28,28,
          27.7,25.6,25.3,23.9,23.6,23.6,23.6,23.6,23.6,23.5,23.4,23.4,23.1,22.9,
            22.9,19.5,18.1,17.2,17,16.7,13.2)

sp=c(96,97,97,105,96,105,97,98,98,107,103,113,113,103,100,103,106,113,106,
       109,110,101,111,105,111,110,110,110,109,90,112,103,103,111,111,102,106
     ,106,109,109,120,106,106,109,106,105,108,108,107,120,109,109,109,109,133,
      125,115,102,109,104,105,120,107,114,114,117,122,122,122,122,148,160,121,
      121,110,121,165,140,147,157,130)

par(mfrow=c(2,2))
x=mpg
y=sp
plot(y~x,main="scatter plot")

 lo=loess(y~x,span=0.7)
plot(y~x,main="span =0.75",pch=19)
newx=seq(min(x),max(x),length=length(sp))
pred=predict(lo,data.frame(x=newx))
lines(pred~newx,col=2)



lo=loess(y~x,span=0.5)
plot(y~x,main="span =0.5",pch=19)
newx=seq(min(x),max(x),length=length(sp))
pred=predict(lo,data.frame(x=newx))
lines(pred~newx,col=2)



lo=loess(y~x,span=0.2)
plot(y~x,main="span =0.2",,pch=19)
newx=seq(min(x),max(x),length=length(sp))
pred=predict(lo,data.frame(x=newx))
lines(pred~newx,col=2)



lo=loess(y~x,span=1)
plot(y~x,main="span =0.1",pch=19)
newx=seq(min(x),max(x),length=length(sp))
pred=predict(lo,data.frame(x=newx))
lines(pred~newx,col=2)



##loess####
par(mfrow=c(2,2))
y.loess <- loess(sp ~ mpg, span=0.75, data.frame(x=mpg, y=sp))
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.75")
lines(mpg,y.predict,col=2)



y.loess <- loess(sp ~ mpg, span=0.5, data.frame(x=mpg, y=sp))
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.5")
lines(mpg,y.predict,col=2,lwd=5)




y.loess <- loess(sp ~ mpg, span=0.4, data.frame(x=mpg, y=sp))
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.4")
lines(mpg,y.predict,col=2)



y.loess <- loess(sp ~ mpg, span=1, data.frame(x=mpg, y=sp))
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.1")
lines(mpg,y.predict,col=2)





#kernel regression
par(mfrow=c(3,2))
plot(sp~mpg,main="ks,h=0.5",pch=19,cex=0.1)

lines(ksmooth(mpg,sp,"normal",bandwidth=0.5),col=2,lwd=2)


plot(sp~mpg,main="ks,h=0.2")
lines(ksmooth(mpg,sp,"normal",bandwidth=2),col=2,lwd=2)


plot(sp~mpg,main="ks,h=5",pch=19,cex=0.1)
lines(ksmooth(mpg,sp,"normal",bandwidth=5),col=2,lwd=2)


plot(sp~mpg,main="ks,h=10",pch=19,cex=0.1)
lines(ksmooth(mpg,sp,"normal",bandwidth=10),col=2,lwd=2)



plot(sp~mpg,main="ks,h=12",pch=19,cex=0.1)
lines(ksmooth(mpg,sp,"normal",bandwidth=12),col=2,lwd=2)

plot(sp~mpg,main="ks,h=20",pch=19,cex=0.1)
lines(ksmooth(mpg,sp,"normal",bandwidth=20),col=2,lwd=2)

#loess  regression
par(mfrow=c(2,2))
data=data.frame(x=mpg, y=sp)
y.loess <- loess(sp ~ mpg, span=0.75, data=data)
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.75",pch=19,cex=0.1)
j=order(data$x)
lines(data$x[j],y.loess$fitted[j],col=2,lwd=2)




y.loess <- loess(sp ~ mpg, span=0.5, data=data)
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.5",pch=19,cex=0.1)
j=order(data$x)
lines(data$x[j],y.loess$fitted[j],col=2,lwd=2)


y.loess <- loess(sp ~ mpg, span=0.2, data=data)
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =0.2",pch=19,cex=0.1)
j=order(data$x)
lines(data$x[j],y.loess$fitted[j],col=2,lwd=2)


y.loess <- loess(sp ~ mpg, span=1, data=data)
y.predict <- predict(y.loess, data.frame(x=mpg))
plot(mpg,sp,main="span =1",pch=19,cex=0.1)
j=order(data$x)
lines(data$x[j],y.loess$fitted[j],col=2,lwd=2)