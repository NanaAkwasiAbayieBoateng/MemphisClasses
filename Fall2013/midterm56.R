rm(list=ls())

x=c(16,18,20,23,24)
status1=c(0,0,0,1,0)

x2=cbind(x,status1)

#selecting the censored observations in first group
h=c()
for (i in 1:nrow(x2)){

if (x2[,2][i]==0){
h[i]=x2[,1][i]

}else{
h[i]=0
}


}








#selecting the censored observations in second group
y=c(15,18,19,19,20)
status2=c(1,1,1,1,1)

y2=cbind(y,status2)

p=c()
for (i in 1:nrow(y2)){

if (y2[,2][i]==0){
p[i]=y2[,1][i]

}else{
p[i]=0
}


}

#Gehan's Generalized Wilcoxon Test

u=matrix(0,length(x),length(x))

for (i in 1:length(x)){
 for (j in 1:(length(x))){
if ((x[i]>=y[j])||(h[i]>=y[j])){
u[i,j]=1

}else if (x[i]<=y[j]||h[i]<y[j]||p[i]<x[i]){

u[i,j]=0

}else if(x[i]<y[j]||x[i]<=p[i]){
u[i,j]=-1

}
}

}
u
(W<- sum(apply(u, 1, sum)))