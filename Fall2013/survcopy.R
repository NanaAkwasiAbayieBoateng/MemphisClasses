rm(list=ls())

#year after diagnosis end points
yad<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
z<-rep(0,n)
n<-length(yad)
tss=2418


#midpoint of year after diagnosis end points

tm<-numeric(n)
tm[n]<-NA
 for (i in 1:n-1)
tm[i]<-0.5*(yad[i+1]+yad[i])
tm

#width

b=c(diff(yad),NA)

# number lost to follow-up
l=c(0,39,22,23,24,107,133,102,68,64,45,53,33,27,23,0)

#number withdrawn alive 
w=numeric(n)

#number dying
d=c(456,226,152,171,135,125,83,74,51,42,43,34,18,9,6,0)

#number entering interval,nprime
#tss is total sample size

nprime<-numeric(n)
nprime[1]=tss
 for (i in 2:n)
nprime[i]<-nprime[i-1]-l[i-1]-w[i-1]-d[i-1]                     
nprime

#number exposed to risk,ner

ner<-nprime-0.5*(l+w)                    
ner

#conditional proportion dying,q                    
q<-d/ner
q

#conditional proportion surviving,p
p=1-q
p

#cummulative proportion surviving,s[ti]
st[1]=1
st<-numeric(n)
st[1]=1
 for (i in 2:n)
st[i]<-p[i-1]*st[i-1]
st
#Estimated probability density function pdf ,ft
ft<-numeric(n)
ft[n]=NA
 for (i in 1:n-1)
ft[i]<-(st[i]*q[i])/b[i]
ft

#Hazard function,ht
ht<-numeric(n)
ht[n]=NA
 for (i in 1:n-1)
ht[i]<- (2*q[i])/(b[i]*(1+p[i]))    
ht

# variance of st,varst
varst<-numeric(n)
varst[1]=NA




for (i in 2:n){
varst[i]=(st[i]^2)*cumsum(q/(ner*p))[i-1]

}
sqrt(varst)


#variance of pdf,varft
varft<-numeric(n)
varft[1]=(((st[1]*q[1])^2)/b[1])*(p[1]/(q[1]*ner[1]))
varft[n]=NA



for (i in 2:n){
varft[i]=(((st[i]*q[i])^2)/b[i])*((p[i]/(ner[i]*q[i]))+cumsum(q/(ner*p))[i-1])

}
sqrt(varft)


#variance of Hazard function,varht
varht<-numeric(n)

 for (i in 1:n)
varht[i]<- ((ht[i]^2)/(ner[i]*q[i]))*(1-(0.5*ht[i]*b[i])^2)    
sqrt(varht)

# median time,tmed
tmed<-numeric(n)
tmed[n]=NA
for(i in 1:n){
if (st[i]>=0.5){

tmed[i]=yad[i]+(st[i]-0.5)/ft[i]
tmed}else{

}
}
tm<-numeric(n)
tm[n]<-NA
 for (i in 1:n-1)
tm[i]<-0.5*(yad[i+1]+yad[i])
tm




yad=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
st=c(1.0000000, 0.8114144, 0.7170105 ,0.6523689, 0.5785647, 0.5192585, 0.4611239,
  0.4172073, 0.3711964, 0.3342218, 0.2986843, 0.2556577, 0.2135639, 0.1838794,
 0.1635737, 0.1429117)
stt=0.5*st
stt
st

v=matrix(c(yad,st, stt), nrow = length(st), ncol = 3)
u=matrix(c(st, stt), nrow = length(st), ncol = 2)

apply(u, 1:2, function(x) x/2)
#which.min(u)
#A1<-matrix(c(k1,k3),nrow=n)
v1=matrix(c(st), nrow = length(st), ncol = 1)
#A<-apply(stt,st,max)

st[st>stt]
st[st<=stt]
i=1
for (i in 1:n){
if ((stt[i]>st)&(sst[i]<st)){
y=c(st)

}
}


data.frame(yad,Midpoint=tm,width=b,nprime,no.exposedtorisk=ner,q,p,st,pdf=ft,hazard=ht,sqrt(varst),sqrt(varft),sqrt(varht))
 
