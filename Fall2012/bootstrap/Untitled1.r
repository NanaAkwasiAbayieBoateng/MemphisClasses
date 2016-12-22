library(boot)
ac <- c(3,5,7,18,43,85,91,98,100,130,230,487)

ac.s<-function(data)
{ c(mean(data),var(data))
}

ac.sim<-function(data,mle)
{   out<-rexp(NROW(data),1/mle)
    out
}

ac.f <- function(data,i)
{
   d<-data[i]
   c(mean(d),var(d))
}

ac.boot<-boot(ac,ac.s,R=999,sim="parametric",ran.gen=ac.sim,mle=mean(ac))
ac.bootnp<-boot(ac, ac.f, R=999)

par(mfrow=c(2,2))

plot(ac.boot$t[1:99,1],sqrt(ac.boot$t[1:99,2]),axes=F,main="",xlim=c(0,300),ylim=c(0,300),xlab="Bootstrap average",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
axis(2,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
box()

plot(ac.bootnp$t[1:99,1],sqrt(ac.bootnp$t[1:99,2]),axes=F,main="",xlim=c(0,300),ylim=c(0,300),xlab="Bootstrap average",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
axis(2,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
box()

plot(ac.boot$t[,1],sqrt(ac.boot$t[,2]),axes=F,main="",xlim=c(0,300),ylim=c(0,300),xlab="Bootstrap average",ylab="Bootstrap SD",pch=20,cex=0.8)
axis(1,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
axis(2,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
box()

plot(ac.bootnp$t[,1],sqrt(ac.bootnp$t[,2]),axes=F,main="",xlim=c(0,300),ylim=c(0,300),xlab="Bootstrap average",ylab="Bootstrap SD",pch=20,cex=0.8)
axis(1,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
axis(2,at=c(0,50,100,150,200,250,300),labels=paste(c(0,50,100,150,200,250,300)))
box()

title(sub="Figure 2.9 ")