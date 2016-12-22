kfit<-function(x){  ##x is frequency count
	lhs<-numeric()
	rhs<-numeric()
	y<-0:(length(x)-1)
	j<-0:(length(x)-2)
	m<-sum(x*y)/sum(x)  #mean
	s2<-(sum(x*y^2)-sum(x*y)^2/sum(x))/(sum(x)-1) #sample variance
	k1<-m^2/(s2-m)  #sample estimate of k
	a<-numeric(length(x)-1)
	for (i in 1:(length(x)-1)) a[i]<-sum(x[-c(1:i)])
	#a(x) = number of x's >x
	i<-0
	for (k in seq(k1/1.2, 2*k1,.001)){ #range of k1's
		i<-i+1
		lhs[i]<-sum(x)*log(1+m/k)
		rhs[i]<-sum(a/(k+j))
	}
	k<-seq(k1/1.2,2*k1,.001)
	plot(k,abs(lhs-rhs),type="l")
	d<-min(abs(lhs-rhs))
	sdd<-which(abs(lhs-rhs)==d)
	k[sdd]
}
#sample mean=1.0042
#k=.583
#n=sum(freq) = 238
nb<-238*(1+1.0042/.583)^(-.583)*factorial(.583+(0:12)-1)/
(factorial(0:12)*factorial(0.583-1))*(1.0042/(1.0042+.583))^(0:12)
# compare visually
#plot both on the same chart
both<-numeric(26)
both[1:26%%2!=0]<-freq
both[1:26%%2==0]<-nb
barplot(both,col=rep(c(1,0),13))
legend (locator(1),c("observed","expected"),fill=c("black","white"))

#compare with a goodness of fit test
t<-sum(((freq-nb)^2/nb)[nb>5])
df<-sum(nb>5)-2-1
#p-value
1-pchisq(t,df)		