set.seed (83748)

#3
p=c(9243,9671,11792,13357,9055,6290,12412,18806)
a=c(17649,12013,19979,21816,13850,9806,17208,29044)
n=c(16449,14614,17274,23798,12560,10157,16570,26325)
(x=a-p)
(y=n-a)
(mu=mean(x))
(v=mean(y))
(rho=v/mu)

library(boot)
R=1000
alpha=0.05



rho.fun=function(data,d){

mean(y[d])/mean(x[d])
}
boot=boot(cbind(x,y),rho.fun,R=1000)


boot.ci(boot,conf = 0.90,type = "perc")





#question 1
s=c(4.5,6.3,5.7,5.3,5.8,4.9)
m=c(3.5,4.3,3.3,5.0,4.6,4.8)
r=c(1,1.3,1,1.7,1.9,1.2)
mean(s)
x=c(4.5,3.5,1,6.3,4.3,1.3,5.7,3.3,1,5.3,5,1.7,5.8,4.6,1.9,4.9,4.8,1.2)

blocks=rep(1:6,c(3,3,3,3,3,3))

grps=rep(1:3,6)

#a)

summary(aov(x~factor(grps)))


x=c(s,m,r)
grps=rep(1:3,each=6)
summary(aov(x~factor(grps)))

#b)


x1=c(4.5,6.3,5.7,5.3,5.8,4.9,3.5,4.3,3.3,5.0,4.6,4.8,1,1.3,1,1.7,1.9,1.2)
grp=rep(1:3,each=6)
perm.F=perm.approx.F(x,grp,R=1000)
Fobs=getF(x,grp)
perm.pval=mean(perm.F>=Fobs)
perm.pval

#d)
alpha=0.05
#Bonf.adj(x, grps, k, alpha, R=1000, test= "ttest")


s=s=c(4.5,6.3,5.7,5.3,5.8,4.9)
m=m=c(3.5,4.3,3.3,5.0,4.6,4.8)
r=r=c(1,1.3,1,1.7,1.9,1.2)

x1=s=c(4.5,6.3,5.7,5.3,5.8,4.9)
x2=m=c(3.5,4.3,3.3,5.0,4.6,4.8)
x3=r=c(1,1.3,1,1.7,1.9,1.2)

(newalpha=alpha/3)

t.test(x1,x2,var.equal=TRUE)
t.test(x1,x3,var.equal=TRUE)
t.test(x3,x2,var.equal=TRUE)



#e)
x=c(r,m,s)
k=3
grps=rep(1:3,each=6)
Tukey.HSD(x, grps, k, alpha=0.05, R=1000)

#f)
x=c(x1,x2,x3)
k=3
grps=rep(1:3,each=6)
Fisher.LSD(x, grps, k, alpha=0.05, R=1000)


#g)
(x=c(r,m,s))

k=3
grps=rep(1:3,each=6)
JT.MW(x2, grps, k, R=1000)



#question 2
data <- read.table("C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/NONPARAMETRIC/father.r", header=FALSE 
  	 )


#a)
y=data[,1]
x=data[,2]
plot(y~x,pch=19)


#b)
(spearman=cor(rank(x),rank(y)))


#c)


#spearman correlation

(spearman.obs=cor(rank(x),rank(y)))

spearman.perm=perm.approx.r(rank(x),rank(y),1000)
(p.value=length(abs(spearman.perm)[abs(spearman.perm)>=abs(spearman.obs)])/R)



mean(spearman.perm>=spearman.obs)


mean(abs(spearman.perm)>=abs(spearman.obs))



d)
spearman.fun=function(data,d){

rho=cor(rank(x[d]),rank(y[d]))
}
boot=boot(cbind(x,y),spearman.fun,R=1000)


boot.ci(boot,conf = 0.90,type = "norm")




e)

summary(lm(y~x))




data=data.frame(x,y)

lm.fun=function(data,d){

lm(data[d,1]~data[d,-1])$coef 
}
slope.boot=boot(cbind(x,y),lm.fun,R=1000)

bhat=slope.boot$t0

bhat.boot=slope.boot$t

apply(bhat.boot,2,sd)


t1=bhat[2]
t1.star=bhat.boot[,2]-bhat[2]
(pval=mean(abs(t1.star)>=abs(t1)))


\
