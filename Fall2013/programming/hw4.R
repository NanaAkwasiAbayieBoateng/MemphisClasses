
rm(list = ls(all = TRUE))


click2<-function(){
 x<-runif(1);y<-runif(1)
 plot(x=x,y=y,xlim=c(0,1),main="please click on the circle",xlab="",ylab="",axes=FALSE,frame.plot=TRUE)
 
 clicktime<-system.time(xyclick<-locator(1))
 d=sqrt(sum((x-xyclick$x)^2)+sum((y-xyclick$y)^2))
 out=list(timestamp=Sys.time(),x=x,y=y,xclick=xyclick$x,yclick=xyclick$y,tclick=clicktime[3])
 
 dx=as.data.frame(out)
 p=rbind(dx,data.frame(out))

 out1=list(p=p)
 return(out1)
 }













clickright=function(n){
h=NULL

for (i in 1:n){
x=as.data.frame(print(click2()$p[1,]))
h=rbind(h,x)
write.table(h,file = "rightclick.txt",eol = "\n", row.names = TRUE,
            col.names = TRUE)
	
}

return(h)
}


r=as.matrix(clickright(50))

# the time from right click
tclick.right=as.numeric(r[,6])

#computing the distance from right click
dright=sqrt((as.numeric(r[,2])-as.numeric(r[,4]))^2+(as.numeric(r[,3])-as.numeric(r[,5]))^2)




clickleft=function(n){
h=NULL

for (i in 1:n){
x=as.data.frame(print(click2()$p[1,]))
h=rbind(h,x)
write.table(h,file = "leftclick.txt",eol = "\n", row.names = TRUE,
            col.names = TRUE)
	
}

return(h)
}


l=as.matrix(clickleft(50))

################################ the time from left click###############################

tclick.left=as.numeric(l[,6])


#########################computing the distance from left click##########################

dleft=sqrt((as.numeric(l[,2])-as.numeric(l[,4]))^2+(as.numeric(l[,3])-as.numeric(l[,5]))^2)








#########################exploratory data analysis####################################


exploratory=function(x,y){
center.x <- mean(x); 
var.x <- var(x)
med.x<-median(x)
range.x=max(x)-min(x) 
center.y <- mean(y); 
var.y <- var(y)
med.y<-median(y) 
range.y=max(y)-min(y)
correlation<-cor(x,y)
summary(x)
summary(y)
out=list(variance.x=var.x,median.x=med.x,center.x=center.x,
          variance.y=var.y,median.y=med.y,center.y=center.y,
           range.x=range.x,range.y=range.y,tclick.right=summary(x),tclick.left=summary(y))

return(out)

}


exploratory.time= exploratory(tclick.right,tclick.left)

exploratory.time


exploratory.distance= exploratory(dright,dleft)

exploratory.distance



############################descriptive plots of histogram and boxplot###########################
par(mfrow=c(2,2))

hist(tclick.right,breaks="sturges",col="blue")
d <- density(tclick.right,na.rm=T)
lines(d,col=2,lwd=2,lty=2)
boxplot(tclick.right,main="tclick.right")
stem(tclick.right)



par(mfrow=c(2,2))
hist(tclick.left,breaks="sturges",col="blue")
d <- density(tclick.left,na.rm=T)
lines(d,col=2,lwd=2,lty=2)
boxplot(tclick.left,main="tclick.left")
stem(tclick.left) 

par(mfrow=c(2,2))
hist(dright,breaks="sturges",,col="blue")
d <- density(dright,na.rm=T)
lines(d,col=2,lwd=2,lty=2)
boxplot(dright,main="dright")
stem(dright) 

par(mfrow=c(2,2))
hist(dleft,breaks="sturges",col="blue")
d <- density(dleft,na.rm=T)
lines(d,col=2,lwd=2,lty=2)
boxplot(dleft,main="dleft")
stem(dleft)
###########test for normality##############
#################normality plot and shapiro test #################

par(mfrow=c(2,2))
shapiro.test(tclick.right)
qqnorm(tclick.right,main="tclick.right")
qqline(tclick.right)
###the tclick time from the right click appears not to be normal from
####wilks shapiro test

shapiro.test(tclick.left)
qqnorm(tclick.left,main="tclick.left")
qqline(tclick.left)
###the tclick time from the leftt click appears not to be normal from
####wilks shapiro test


shapiro.test(dright)
qqnorm(dright,main="dright")
qqline(dright)
###the distance  from the right click appears  to be normal from
####wilks shapiro test


shapiro.test(dleft)
qqnorm(dleft,main="dleft")
qqline(dleft)
###the distance  from the left click appears  to be normal from
####wilks shapiro test


####test for difference between left and right data#########
############## t-test################
#H0:tclick.right=tclick.left
#H0:tclick.right<tclick.left

#variance obtained from EDA for both sample is the not same
#$variance.x
#[1] 0.3956694
#$variance.y
#[1] 0.3097682



t.test(tclick.right,tclick.left,
       alternative =  "less",
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)



#H0:dright=dleft
#H0:dright<dleft

#variance obtained from EDA for both sample is the NOT  same
#$variance.x
#[1] 0.0006663789
#$variance.y
#[1] 6.299877e-06

t.test(dright,dleft,
       alternative =  "less",
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)


###since the wilk shapiro proved that the time from right 
###and left click is not normal,the t-test is not appraopriate
##test here.



####permutation ##############################################
######################## two sample permutation test#######################

perm.test=function(x,y){
# POOL DATA
pooledData <- c(x, y)
# SET THE NUMBER OF ITERATIONS
nIter <- 9999
# SET UP A CONTAINER FOR PERMUTED DIFFERENCES. ADD IN A SLOT FOR THE OBSERVED VALUE
meanDiff <- numeric(nIter+1)
# CALCULATE THE OBSERVED MEAN DIFFERENCE
meanDiff[1] <- mean(x) - mean(y)
# RUN THE ITERATION IN A FOR() LOOP
for(i in 2:length(meanDiff)){ 
 index <- sample(1:50, size=25, replace=F) # Sample numbers 1-50 25 times and store in an index
 xperm <- pooledData[index] # Assign the sampled values to xperm
 yperm <- pooledData[-index] # Assign everything else to yperm
 meanDiff[i] <- mean(yperm) - mean(xperm) # Calcualte and store the difference in means
}
 

# CALCULATE THE P-VALUE FOR LOWER TAILED TEST

p.value=mean(meanDiff <= meanDiff[1])

return(p.value=p.value)

}



######permutation test of times#####
perm.test(tclick.right,tclick.left)

####[1] 1e-04
##reject the null hypothesis that there is no
##difference between the mean of right click times and
###left click time at a significance level of 0.05



######permutation test of distance#####

perm.test(dright,dleft)

###[1] 1
##we fail to reject the null hypothesis that there is no
##difference between the mean of right click distances and
###left click distances at a significance level of 0.05




par(mfrow=c(1,2))


hist(dright)
hist(dleft)







################wilcoxon signed rank#######################


wilcox.test(tclick.right,tclick.left,
            alternative =  "less",
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = TRUE, conf.level = 0.95)



wilcox.test(dright,dleft,
            alternative =  "less",
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = TRUE, conf.level = 0.95)





