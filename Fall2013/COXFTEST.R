
x=c(8,8,10,12,12,13)
x1=rep(1,length(x))
status.x=c(1,1,1,1,1,1)
x.n=cbind(x,x1,status.x)

y=c(9,12,15,20,30,30)
status.y=c(1,1,1,1,0,0)
y1=rep(2,length(y))
y.n=cbind(y,y1,status.y)

t=rbind(x.n,y.n)
h=data.frame(x.n,y.n)
 


#h=data.frame(xy,status)


t=t[order(t[,1]),]

trn=c()
for (i in 1:nrow(t)){

trn[i]=1/(nrow(t)-i+1)


}
trn=cumsum(trn)

t=cbind(t,trn)
t

n=length(cumsum(trn))
####which(t[,1]==t[,1][i]) gives index of  elements in t[,1] which are equal
#### replaces those elements with their mean
n=length(cumsum(trn))
for (i in 1:(n-1)){
if ((t[,3][i]==0)&&(t[,4][which(t[,1]==t[,1][i])])){
t[,4][i+1]=t[,4][i]
}else
t[,4][i] =mean(t[,4][which(t[,1]==t[,1][i])])



}
t

#sample.1=c()
#sample.2=c()
#for (i in 1:nrow(t)){
#if(t[,2][i]==1){
#sample.1[i]=t[,4][i]
#sample.1[is.na(sample.1)] <- 0
#}else 
#sample.2[i]=t[,4][i]
#sample.2[is.na(sample.2)] <- 0
#}

#####################replacing NA with 0################

#sample.1[!is.na(sample.1)]  #one way
#sample.1[is.na(sample.1)] <- 0  #another way

#tbar.1=mean(sample.1[sample.1>0])
#tbar.2=mean(sample.2[sample.2>0])






sample.1=c()
sample.2=c()
for (i in 1:nrow(t)){
if(t[,2][i]==1){
sample.1[i]=t[,4][i]

}else 
sample.2[i]=t[,4][i]

}

tbar.1=mean(na.omit(sample.1))
tbar.2=mean(na.omit(sample.2))
F.value=tbar.1/tbar.2
df1=2*length(status.x[status.x>0])
df2=2*length(status.y[status.y>0])

pvalue=pf(F.value, df1, df2, ncp=0, lower.tail = TRUE, log.p = FALSE)
pvalue

pf(0.248, df1=12, df2=8, ncp=0, lower.tail = TRUE, log.p = FALSE)
 




n=length(cumsum(trn))
h=c()

####which(t[,1]==t[,1][i]) gives index of  elements in t[,1] which are equal
#### replaces those elements with their mean
n=length(cumsum(trn))
for (i in 1:(n-1)){
if ((t[,3][i]==0)&&(t[,4][which(t[,1]==t[,1][i])])){
t[,4][i+1]=t[,4][i]
}else
#t[,4][i] =mean(t[,4][which(t[,1]==t[,1][i])])
h=which(t[,1]==t[,1][i])
t[,4][h]=mean(t[,4][h])
#t[,4][i]=mean(t[,4][h])
#t[,4][h]=sum(t[,4][h])/length(t[,4][h])
}
t
h









