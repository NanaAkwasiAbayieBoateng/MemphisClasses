#question 4.2
#Before=x
#After =y
x=c(2,3,4,4,3,1,3,4,4,5,3,4,2,2,4,3,4,2,2)
y=c(2,4,4,4,4,4,3,5,4,4,4,5,4,2,5,5,4,1,2)
n=length(y)
d=y-x
R=1000
Dobs=mean(y-x)
Dobs
getbinvecs(n)



dstar=perm.approx.dbar(d,R)

p.value=length(abs(dstar)[abs(dstar)>= abs(Dobs)])/R
p.value


#question 4.3
Treatment1=c(100,250,50,80)
Treatment2=c(112,240,58,82)
d=Treatment2-Treatment1

d=d[d!=0]

n=length(d)
R=2^n

wilcox.test(d)

getbinvecs(n)

perm.srplus <- function(d)
{
    # exact permutation distribution of SR_i
    
    n <- length(d)
    junk <- getbinvecs(n)
    signs <- junk - !junk  ### permuted signs (+/-)
     t(rank(abs(d)) * t(signs)) 

}


dstar1= perm.srplus(d)

si=matrix(0,nrow(dstar1),ncol(dstar1))
 for (i in 1:nrow(dstar1)){

    for (j in 1:ncol(dstar1)){

       if (dstar1[i,j]>0){

          si[i,j]=dstar1[i,j]}else
                    
               si[i,j]=0
}
}
si







SRPLUS=apply( si , 1, sum)
SRPLUS









#question 4.7
#blocks=rep(1:6,3)
#grps=rep(1:3, each = 6)

#rep(1:4, 2)
#R=factorial(3)^6
#sept1=c(1.5,2.1,1.9,2.8,1.4,1.8)
#sept2=c(1.8,2.0,2.0,2.7,1.6,2.3)
#sept30=c(1.9,2.5,2.5,2.6,2.1,2.4)

#x=c(sept1,sept2,sept30)
#summary(aov(x~grps))

x=c(1.5,1.8,1.9,2.1,2.0,2.5,1.9,2.0,2.5,2.8,2.7,2.6,1.4,1.6,2.1,1.8,2.3,2.4) 
#																																								R=factorial(3)^6
R=1000
blocks=rep(1:6, c(3,3,3,3,3,3))

grps=rep(1:3,6)

getmeans(x,grps)

grandmean=mean(getmeans(x,grps))
j=6
k=3
#R=factorial(3)^6

v=matrix(x,3)
xbar.i=apply(v,1,mean)
xbar.j=apply(v,2,mean)
SST=j*sum((xbar.i-grandmean)^2)

SSE=sum((x-xbar.i-xbar.j+grandmean)^2)

Fobs=(SST/(k-1))/(SSE/((k-1)*(j-1)))

SST=j*(sum(xbar.i^2)-k*grandmean^2)

SSTMobs=(sum(xbar.i^2)-k*grandmean^2)



SSTMOBS=getSSTM(x,grps)


SSTM.STAR=perm.approx.RCBD(x,grps,blocks,R)

p.value=length(abs(SSTM.STAR)[abs(SSTM.STAR)>=abs(SSTMOBS)])/R
p.value
mean(SSTM.STAR>=SSTMobs)


#b
summary(aov(x~factor(grps)+factor(blocks)))






Friedman=rankinblock(x, blocks)  

 friedman.test(x,grps,blocks)

#b
x=c(1.5,1.8,1.9,2.1,2.0,2.5,1.9,2.0,2.5,2.8,2.7,2.6,1.4,1.6,2.1,1.8,2.3,2.4) 
grps=rep(1:3,6)
k=3  #number of treatment levels
n=6   #number of control blocks
f=c("sept1","sept15","sept30")  #treatment levels
tm=gl(k,1,n*k,factor(f))

blk=gl(n,k,k*n)  #blocking factor


summary(aov(x~tm+blk))

summary(aov(x~factor(grps)+factor(blocks)))


