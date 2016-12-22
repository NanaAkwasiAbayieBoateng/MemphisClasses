coxftest<-function(x,status.x,y,status.y){


rm(list=ls())

x1=rep(1,length(x))
##censored=0,uncensored =0



x.n=cbind(x,x1,status.x)


y1=rep(2,length(y))
y.n=cbind(y,y1,status.y)

t=rbind(x.n,y.n)

 




#sorting the columns by the  first column t[,1]

t=t[order(t[,1]),]

trn=c()
for (i in 1:nrow(t)){

trn[i]=1/(nrow(t)-i+1)


}
trn=cumsum(trn)

t=cbind(t,trn)
t


n=length(cumsum(trn))
h=c()

####which(t[,1]==t[,1][i]) gives index of  elements in t[,1] which are equal
#### replaces those elements with their mean
n=length(cumsum(trn))
for (i in 1:(n-1)){

#####if an element in t[,1] is censored,replicate its t[,4] value to the
########## next  censored element in t[,1] which is the same 
if ((t[i,3]==0)&&(t[,4][which(t[,1]==t[i,1])])){
t[i+1,4]=t[i,4]
}else

h=which(t[,1]==t[i,1])
t[h,4]=mean(t[h,4])


}
t



sample.1=c()
sample.2=c()
for (i in 1:nrow(t)){
if(t[i,2]==1){
sample.1[i]=t[i,4]

}else 
sample.2[i]=t[i,4]

}



tbar.1=sum(na.omit(sample.1))/length(status.x[status.x>0])
tbar.2=sum(na.omit(sample.2))/length(status.y[status.y>0])

F.value=tbar.1/tbar.2
df1=2*length(status.x[status.x>0])
df2=2*length(status.y[status.y>0])


pvalue=pf(F.value, df1, df2, ncp=0, lower.tail = TRUE, log.p = FALSE)
pvalue




return(p.vaue =pvalue)
}








x=c(8,8,10,12,12,13)
status.x=c(1,1,1,1,1,1)
y=c(9,12,15,20,30,30)
status.y=c(1,1,1,1,0,0)
coxftest(x,status.x,y,status.y)








#n=length(cumsum(trn))
#h=c()

####which(t[,1]==t[,1][i]) gives index of  elements in t[,1] which are equal
#### replaces those elements with their mean
#n=length(cumsum(trn))
#for (i in 1:(n-1)){
#if ((t[,3][i]==0)&&(t[,4][which(t[,1]==t[,1][i])])){
#t[,4][i+1]=t[,4][i]
#}else
#t[,4][i] =mean(t[,4][which(t[,1]==t[,1][i])])
#h=which(t[,1]==t[,1][i])
#t[,4][h]=mean(t[,4][h])
#t[,4][i]=mean(t[,4][h])
#t[,4][h]=sum(t[,4][h])/length(t[,4][h])
#}
#t