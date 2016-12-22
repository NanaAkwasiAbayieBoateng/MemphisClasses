#question 5.01
height=c(68,70,74)
weight=c(145,155,160)

R=1000 
(rho.star=perm.approx.r(height,weight,R))

(rho.obs=cor(height,weight))

(p.value=length(abs(rho.star)[abs(rho.star)>=abs(rho.obs)])/R)



mean((rho.star)>=(rho.obs))

#question 5.02

#spearman permutation test

spearman.perm=perm.approx.r(rank(height),rank(weight),R)
(spearman.obs=cor(rank(height),rank(weight)))
(p.value=length(abs(spearman.obs)[abs(spearman.obs)>=abs(spearman.obs)])/R)
#kendall tau
(KendallsTau.obs=getTau.notie(age,strength))
kendall.tau.perm=perm.approx.tau(height,weight,R)
(p.value=length(abs(kendall.tau.perm)[abs(kendall.tau.perm)>=abs(KendallsTau.obs)])/R)


#question 5.03
age=c(3,7,15,24,85,180,360)
strength=c(2500,3200,4300,5300,5900,6700,6900)

plot(age,strength)

#plot(strength,age)

#pearson correlation
(pearson.obs=cor(age,strength))
pearson.perm=perm.approx.r(age,strength,1000)
(p.value=length(abs(pearson.perm)[abs(pearson.perm)>=abs(pearson.obs)])/R)

#spearman correlation

(spearman.obs=cor(rank(age),rank(strength)))

spearman.perm=perm.approx.r(rank(age),rank(strength),1000)
(p.value=length(abs(spearman.perm)[abs(spearman.perm)>=abs(spearman.obs)])/R)


#####  Kendall's Tau
##
(KendallsTau.obs=getTau.notie(age,strength))

spearman.perm=perm.approx.r(rank(age),rank(strength),1000)
(p.value=length(abs(spearman.perm)[abs(spearman.perm)>=abs(KendallsTau.obs)])/R)




