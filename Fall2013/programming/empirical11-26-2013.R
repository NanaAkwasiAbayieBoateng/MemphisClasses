#caveendish measurements on density of earth(1798)
cavendish<-c(5.5,5.57,5.42,5.61,5.53,5.47,4.88,5.62,5.63,4.07,
5.29,5.34,5.26,5.44,5.46,5.55,5.34,5.3,5.36,5.79,5.75,5.29,5.1,
5.86,5.58,5.27,5.85,5.65,5.39)

#empirical distribution function ecdf()
plot(ecdf(cavendish),xlab="density  of the earth",ylab="cummulative curve",main="")


#binomial distribution
#dbinom(x,n,p)
barplot(pbinom(0:4,4,0.1),names=as.character(0:4),xlab="x",ylab="F(x)")

barplot(dbinom(0:4,4,0.1),names=as.character(0:4),xlab="x",ylab="f(x)")


#geometric
fx<-dgeom(0:20,0.2)

barplot(fx,names=as.character(0:20),xlab="x",ylab="f(x)")

table(rgeom(100,0.1))

#hypergeometric
#dhyper(x,K,N-K,n)

barplot(dhyper(0:5,6,14,5),names=as.character(0:5))

dhyper(5,6,14,5)


#multinomial 3 mutually exclusive outcomes from n independent trials
# p1 is the probability of type a outcome
#p2 is the probablity of type b outcome
multi<-function(n,a,b,p1,p2){
factorial(n)/(factorial(a)*factorial(b)*factorial(n-a-b))*p1^a*p2^b*(1-p1-p2)^(n-a-b)}

#suppose n=24,a=4,b=0:20,pl=0.2,p2=.25

psuc<-numeric(21)

#for (i in 0:20) psuc[i+1]<-multi(24,4,i,0.2,0.25)

psuc=multi(24,4,0:20,0.2,0.25)

barplot(psuc,names=as.character(0:20))

negbin<-function(x,u,k)
(1+u/k)^(-k)*(u/(u+k))^x*gamma(k+x)/(factorial(x)*gamma(k))


xf<-sapply(0:10,function(i) negbin(i,0.8,0.2))
xf

barplot(xf,names=as.character(0:10))


#dnbinom(x,size=r,p)

plot(5:100,dnbinom(5:100,5,0.1),type="s",xlab="x",ylab="f(x)")


count<-rnbinom(100,1,0.6)
table(count)
mean(count)
k<-mean(count)^2/(var(count)-mean(count))
k

x<-0:12
freq<-c(131,55,21,14,6,6,2,0,0,0,0,2,1)

barplot(freq,names=as.character(x))

y<-rep(x,freq)
sum(freq)
length(y)




