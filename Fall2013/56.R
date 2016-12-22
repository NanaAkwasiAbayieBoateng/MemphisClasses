rm(list=ls())

x=c(16,18,20,23,24)
status1=c(0,0,0,1,0)
x1=cbind(x,status1)
xp=apply(x1,1,)





y=c(15,18,19,19,20)
status2=c(1,1,1,1,1)
y1=cbind(y,status2)
y1[,2]
yp=y*status2
yp=c(yp[yp==0])
ynp=c(status2[status2!=0])


u=matrix(rep(0,length(x)*length(x)),nrow=length(x))
for (i in 1:length(x)){
j=i+1
 for (j in 1:(length(x))){
if ((x[i]>=y[j]|xp>=y)){
u[i,j]=1

}else if (x[i]<=y[j]){

u[i,j]=0

}else{
u[i,j]=-1

}
}

}
u
(W<- sum(apply(u, 1, sum)))