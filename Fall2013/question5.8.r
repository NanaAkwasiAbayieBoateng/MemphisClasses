
> coxftest<-function(x,status.x,y,status.y){
+ 
+ 
+ rm(list=ls())
+ 
+ x1=rep(1,length(x))
+ 
+ ##censored=0,uncensored =0
+ 
+ 
+ length(status.x)
+ 
+ x.n=cbind(x,x1,status.x)
+ 
+ 
+ y1=rep(2,length(y))
+ length(y1)
+ y.n=cbind(y,y1,status.y)
+ 
+ t=rbind(x.n,y.n)
+ 
+  
+ 
+ 
+ 
+ 
+ #sorting the columns by the  first column t[,1]
+ 
+ t=t[order(t[,1]),]
+ 
+ trn=c()
+ for (i in 1:nrow(t)){
+ 
+ trn[i]=1/(nrow(t)-i+1)
+ 
+ 
+ }
+ trn=cumsum(trn)
+ 
+ t=cbind(t,trn)
+ t
+ 
+ 
+ n=length(cumsum(trn))
+ h=c()
+ 
+ ####which(t[,1]==t[,1][i]) gives index of  elements in t[,1] which are equal
+ #### replaces those elements with their mean
+ n=length(cumsum(trn))
+ for (i in 1:(n-1)){
+ 
+ #####if an element in t[,1] is censored,replicate its t[,4] value to the
+ ########## next  censored element in t[,1] which is the same 
+ if ((t[i,3]==0)&&(t[,4][which(t[,1]==t[i,1])])){
+ t[i+1,4]=t[i,4]
+ }else
+ 
+ h=which(t[,1]==t[i,1])
+ t[h,4]=mean(t[h,4])
+ 
+ 
+ }
+ t
+ 
+ 
+ 
+ sample.1=c()
+ sample.2=c()
+ for (i in 1:nrow(t)){
+ if(t[i,2]==1){
+ sample.1[i]=t[i,4]
+ 
+ }else 
+ sample.2[i]=t[i,4]
+ 
+ }
+ 
+ 
+ 
+ tbar.1=sum(na.omit(sample.1))/length(status.x[status.x>0])
+ tbar.2=sum(na.omit(sample.2))/length(status.y[status.y>0])
+ 
+ F.value=tbar.1/tbar.2
+ df1=2*length(status.x[status.x>0])
+ df2=2*length(status.y[status.y>0])
+ 
+ 
+ pvalue=pf(F.value, df1, df2, ncp=0, lower.tail = TRUE, log.p = FALSE)
+ 
+ 
+ 
+ 
+ out.list<-list(pvalue=pvalue,F.value=F.value)
+ return(out.list)
+ 
+ 
+ }
> 
> 
> 
> 
> 
> time1<-c(1.8,2.3,3.8,5.5,6.3,6.4,16.6,17.1,23.8,33.7,33.7)
> status1<-c(1,1,1,1,1,1,0,0,0,0,0)
> time2<-c(2.8,3.0,4.3,4.5,5.8,6.8,7.8,8.2,8.2,9.2,9.2,10.8,11.0,15.9,18.1,21.4,22.1,23.0,26.9)
> status2<-c(1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0)
> 
> 
> coxftest(x=time1,status.x=status1,y=time2,status.y=status2)
$pvalue
[1] 0.5779886

$F.value
[1] 1.08351

