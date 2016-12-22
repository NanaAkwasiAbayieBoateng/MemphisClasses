#3.01
x1=c(2.9736,0.9448,1.6394,0.0389,1.2958,0.7681,0.8027,0.2156,0.074,1.5076,4.8249,2.2516,1.5609,2.0452,1.0959)
grps=rep(1:3,each=5)



summary(aov(x1~factor(grps)))

#permutation F test
perm.F=perm.approx.F(x1,grps,R=1000)
Fobs=getF(x1,grps)

(perm.pval=mean(perm.F>=Fobs))

table(grps)


#3.02
#grps1=1700lb,grps2=2300lb,grps3=2800lb,grps4=3200lb grps5=3700

x2=c(574,976,789,805,361,529,791,1146,394,767,1385,1021,2073,803,1263,1016,1101,945,139,865,
775,729,1721,1113,820,1613,1404,1201,205,1380,580,1803,998,1049,736,782,730,742,1219,705,
1260,611,1350,1657,1143,1154,541,406,1529,1132,767,1224,314,1728)
grps=c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3
,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5)


hist(x2, prob=TRUE)
lines(density(x2))


summary(aov(x2~factor(grps)))

#permutation F test
perm.F=perm.approx.F(x2,grps,R=1000)
(Fobs=getF(x2,grps))

(perm.pval=mean(perm.F>=Fobs))

#3.03
grps=rep(1:3,each=5)
kruskal.test(x1,grps)


#3.06
C=c(791,846,1024,1007,399,1279,407,1656,1036,1226)
H=c(423,541,517,1328,471,533,863,786,551,1068)
L=c(551,1068,757,1114,920,1809,1238,918,1339,603)
M=c(712,435,298,733,1200,1701,707,790,800,480)
MPV=c(1345,1269,1477,758,996,1306,968,943,1026,1564)
P=c(985,1074,742,985,1342,1184,977,1465,892,1074)
V=c(805,2613,903,949,1183,1051,1387,1320,1434,1603)

x6=c(C,H,L,M,MPV,P,V)
grps=rep(1:7,each=10)
length(x6)
length(grps)

kruskal.test(x6,grps)

#kruskal walis with permuttation-based approx p-value
rank.x6=rank(x6)
summary(aov(rank.x6~factor(grps)))

(SST=summary(aov(rank.x6~factor(grps)))[[1]][1,2])


(SR2=var(rank.x6))
(SST/SR2)
Fobs=getF(rank.x6,grps)
permFs=perm.approx.F(rank.x6,grps,R=1000)
mean(permFs>=Fobs)

k=7
Fisher.LSD.rank(x6, grps, k, alpha=0.05, R=1000)
Tukey.HSD.rank(x6, grps, k, alpha=0.05, R=1000)


###################3.08
Tukey.HSD(x6, grps, k, alpha=0.05, R=1000)
Tukey.HSD(x6, grps, k, alpha=0.1, R=1000)
k=7
trtmeans=getmeans(x6,grps)
grps=rep(1:7,each=10)
MSE=summary(aov(x6~factor(grps)))[[1]][2,3]
n=table(factor(grps))
Tijs<-matrix(NA,7,7)
for (i in 2:k){
        for (j in 1:(i-1)){
             Tijs[i,j] <- abs(trtmeans[i] - trtmeans[j])/sqrt(MSE * (1/n[i] + 1/n[j]))
            }}

 perm.maxTij <- perm.approx.maxTij(x6, grps, MSE, R=1000)
quantile(perm.maxTij,c(0.9,0.95))


#############3.09
grps=c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3
,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5)
k=5
JT.MW(x2, grps, k, R=1000)

