library(boot)

 cd4$baseline
 cd4$oneyear



#####plotting average bootstrap########################

cd4.f <- function(data,i)
{
   d<-data[i]
   c(mean(d),var(d))
}


cd41.bootnp<-boot( cd4$baseline, cd4.f, R=999)
cd42.bootnp<-boot( cd4$oneyear, cd4.f, R=999)



par(mfrow=c(2,2))


plot(cd41.bootnp$t[1:99,1],sqrt(cd41.bootnp$t[1:99,2]),axes=F,main="",xlim=c(2,4),ylim=c(0.4,1.0),xlab="Bootstrap average(baseline)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0,2,3,4),labels=paste(c(0,2,3,4)))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1),labels=paste(c(0,0.2,0.4,0.6,0.8,1)))
box()






plot(cd41.bootnp$t[,1],sqrt(cd41.bootnp$t[,2]),axes=F,main="",xlim=c(2,4),ylim=c(0.4,1),xlab="Bootstrap average(baseline)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0,2,3,4),labels=paste(c(0,2,3,4)))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1),labels=paste(c(0,0.2,0.4,0.6,0.8,1)))
box()



plot(cd42.bootnp$t[1:99,1],sqrt(cd42.bootnp$t[1:99,2]),axes=F,main="",xlim=c(3,5),ylim=c(0.4,1.9),xlab="Bootstrap average(oneyear)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0,2,3,4,6),labels=paste(c(0,2,3,4,6)))
axis(2,at=c(0.4,0.6,0.8,1,1.2,1.8,2),labels=paste(c(0.4,0.6,0.8,1,1.2,1.8,2)))
box()



plot(cd42.bootnp$t[,1],sqrt(cd42.bootnp$t[,2]),axes=F,main="",xlim=c(3,5),ylim=c(0.4,1.9),xlab="Bootstrap average(oneyear)",ylab="Bootstrap SD",pch=20,cex=1.5)
axis(1,at=c(0,2,3,4,6),labels=paste(c(0,2,3,4,6)))
axis(2,at=c(0.4,0.6,0.8,1,1.2,1.8,2),labels=paste(c(0.4,0.6,0.8,1,1.2,1.8,2)))
box()


title(sub="Figure 2.9 ")