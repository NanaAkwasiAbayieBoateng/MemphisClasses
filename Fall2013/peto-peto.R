
#Peto-Peto
rm(list=ls())
# group1 23,16+,18+,20+,24+
#group2  15,18,19,19,20
#status, censored =0, uncensored=1
 
T1=c(23,16,18,20,24)
status1=c(1,0,0,0,0)
#status1=c(0,1,1,1,1)
group1=rep(1,length(T1))
x1=cbind(T1,status1,group1)

T2=c(15,18,19,19,20)
status2=c(1,1,1,1,1)
#status2=c(0,0,0,0,0) 
group2=rep(2,length(T2))
y1=cbind(T2,status2,group2)

A=rbind(x1,y1)

A=A[order(A[,1],-A[,2]),]

mi=c()
for (i in 1:dim(A)[1]){

if(A[i,2]==0){

mi[i]=0

}else{

mi[i]=1

}

}
mi

A=cbind(A,mi)
mi=A[,2]
#A[,4][which(is.na(A[,4]))]=0
  
n1i=c()
n2i=c()
for (i in 1:dim(A)[1]){
if(A[i,4]==1){
n1i[i]=length(mi[mi>0])-i
n2i[i]=length(mi[mi>0])-i
}else{
n1i[i]=0
n2i[i]=0
}

}



A=cbind(A,n1i,n2i)

ri=A[,5]+A[,6]

A=cbind(A,ri,m)
#mi/ri=mr
mr=c()
for (i in 1:dim(A)[1]){
if (ri[i]==0){
mr[i]=0

}else{
mr[i]=mi[i]/ri[i]
}
}

mr
A=cbind(A,ri,mr)

et=c()
for (i in 1:(dim(A)[1])){
if (mr[i]==0){
et[i]=0

}else{
et[i]=cumsum(abs(mr))[i]
}
}


A=cbind(A,et)

wi=c()
for (i in 1:(dim(A)[1])){
if (et[i]==0){
wi[i]=-et[i-1]

}else{
wi[i]=1-et[i]
}
}

A=cbind(A,wi)


nw=c()
for (i in 1:(dim(A)[1])){
if (A[i,3]==2&&A[i,2]==1){
nw[i]=wi[i]

}else{
nw[i]=0
}
}
nw
vi=c()
for (i in 1:(dim(A)[1])){
if (et[i]==0){
vi[i]=0

}else{
vi[i]=mi[i]*(ri[i]-mi[i])/ri[i]
}
}
vi
## intersect <- function(x, y) y[match(x, y, nomatch = 0)]
 #seq(1,10,1)[1:10 %in% c(1,3,5,9)]
#intersect((A[,3]==1), (A[,4]==1))