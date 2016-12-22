library(boot)
x=cd4[,1]
y=cd4[,2]
d=y-x
#####classical method,paired test##########


t.test(y, x,
       alternative ="two.sided", paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(y, x,
       alternative ="greater", paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(d,
       alternative ="greater", paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

#############paired-comparison permutation test################
n=length(y)
d=y-x
R=1000
Dobsbar=mean(y-x)

getbinvecs(n)



dstarbar=perm.approx.dbar(d,R)



p.value=length(abs(dstarbar)[abs(dstarbar)>= abs(Dobsbar)])/R
p.value


############Wilcoxon-Signed Rank Test#############



wilcox.test(x, y, paired = TRUE, alternative = "two.sided",conf.int = TRUE, conf.level = 0.95)


wilcox.test(y, x, paired = TRUE, alternative = "two.sided",conf.int = TRUE, conf.level = 0.95)         
             



########Binomial Test sign test##########
#Ho:theta=0
#Ha:theta>0


s=length(d[d>0])
cval=1
 n=length(d)
p=0.5



(binom.test(s, n, p = 0.5,
           alternative = "greater",
           conf.level = 0.95))

length(x[x>y])
length(y[y>x])
(binom.test(length(y[y>x]), n, p = 0.5,
           alternative = "greater",
           conf.level = 0.95))






#####################################################
##  Bootstrap statistic of interest,mean  ######
####################################################





mymean.func=function(x,d){
#d is the index of resampled observations
mean(x[d])

} 
R=10000
mean.boot=boot(d,mymean.func,R=10000)  

boot.ci(mean.boot) 



par(mfrow=c(2,2))
qqnorm(d,main="Normal Q-Q Plot : Difference between x and y")
qqline(d)

hist(d,prob=TRUE,col="blue",breaks = "freedman-diaconis",xlab="")
title(sub="Histogram of difference")
qqnorm(mean.boot$t[1:R],main="Normal Q-Q Plot : Bootstrap sample of mean difference")
qqline(mean.boot$t[1:R])

hist(mean.boot$t[1:R],prob=TRUE,col="blue",breaks = "fd",main="Histogram of Bootstrap sample of mean difference",xlab="")
lines(density(mean.boot$t[1:R]),lw=2)
title(sub="Histogram of Bootstrap sample of mean difference")


