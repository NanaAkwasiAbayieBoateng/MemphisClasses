

curve(pnorm(x),-3,3)
arrows(-1,0,-1,pnorm(-1),col="red")
arrows(-1,pnorm(-1),-3,pnorm(-1),col="green")

pnorm(-1)

curve(dnorm(x),-3,3)

#test for normality
#gaphical 
qqnorm(rnorm(100))
qqline(rnorm(100))

qqnorm(runif(100))

qqline(runif(100))

#shapiro -wilks test
#H0; data is normally dist
#Ha:data not normally dist
shapiro.test(rnorm(100))

#reject the null hypothesis if the p-value is small

shapiro.test(runif(100))


hist(runif(10000)*10,main="")

#now let's look at the distribution of the sample means
#based on means of just 5 uniform random variables

means<-numeric(10000)

for (i in 1:10000){
means[i]=mean(runif(5)*10)


}

hist(means,ylim=c(0,1600))



m<-mean(means)
s<-sd(means)

#compare to a normal distribution with this mean and sd

xv<-seq(0,10,0.1)

yv<-dnorm(xv,mean=m,sd=s)
lines(xv,yv)


#dnorm takes argument z ,options mean and sd,default is standard normal

par(mfrow=c(2,2))
curve(dnorm(x),-3,3,xlab="z",ylab="probability density",main="density")


#pnorm takes argument z,options mean and standard deviation,default is standard normal
#shows cdf p(Z<=z)
curve(pnorm(x),-3,3,xlab="z",ylab="probablity",main="probablity")

#qnorm has cummulative prob for argument and returns the associated
#z value

curve(qnorm(x),0,1,xlab="p",ylab="quantiles",main="quantiles")


ydevs<-rnorm(100)

ydevs<-(ydevs-mean(ydevs))/sd(ydevs)

sd(ydevs)

yvals<-24+4*ydevs

mean(yvals)
sd(yvals)


par(mfrow=c(1,1))

#chi-square distribution


#rnorm produces random numbers from the normal distribution with 
#specified mean and sd,first argument is the number of random numbers
# to generate

y<-rnorm(1000)
hist(y,xlab="z",ylab="frequency",main="random numbers")



#chi-square distribution
par(mfrow=c(1,2))
x<-seq(0,30,.25)
plot(x,dchisq(x,3),type="l",xlab="df=3")
plot(x,dchisq(x,20),type="l",xlab="df=20")


par(mfrow=c(1,1))
x=seq(0.01,3,0.01)
plot(x,df(x,1,10),type="l",ylim=c(0,1))
lines(x,df(x,2,10),lty=6)
lines(x,df(x,5,10),lty=2)
lines(x,df(x,30,10),lty=3)



x=seq(-4,4,0.01)

plot(x,dnorm(x),type="l",col="red")
lines(x,dt(x,df=5),lty=6)

lines(x,dt(x,df=1),lty=2)


phi<-function(x){
return(exp(-x^2/2)/sqrt(2*pi))
}

trapezium=function(ftn,a,b,n=300){

h=(b-a)/n
x.vec=seq(a,b,by=h)
f.vec=sapply(x.vec,ftn)
T=h*(f.vec[1]/2+sum(f.vec[2:n])+f.vec[n+1]/2)

return(T)
}

ppoint<-function(p,pdf=phi,z.min=-10,tol=1e-9){

#calculate z alpha
#p is assumed between 0 and 1
#pdf is a valid pdf
#let F(x) be the integral of pdf from -inf to x
#we apply the N-R ALGORITHM to find z alpha(alpha=p)
#such that F(z alpha)=p
#derivative of F(x) =pdf
#we use trapezium method to integrate pdf
#approximate -inf by -10

x<-0
f.x<-trapezium(pdf,z.min,x)-p
#continue iterating until stopping criteria is met
while(abs(f.x)>tol){

x<-x-f.x/pdf(x)
f.x<-trapezium(pdf,z.min,x)-p
}
return(x)
}
ppoint(.975)

