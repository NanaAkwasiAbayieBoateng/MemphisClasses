(ma1=matrix(1:6,nrow=2))
>>
>>      [,1] [,2] [,3]
>>[1,]    1    3    5
>>[2,]    2    4    6
>>
>>>vr1=c(2,3)
>>>rma1=NULL
>>>for(i in 1:length(vr1)){for(j in 1:vr1[i]){rma1=rbind(rma1,ma1[i,])}}
>>>rma1





v=1:10
t(replicate(7, v))

x=1:10

# Method 1
rep(x,each=3)

# Method 2

x=matrix(c(1,2,3,4,5,6,7,8,9),3,3)
x
k=matrix(0,6,6)
for (i in 1:2){
for(j in 1:2){
k[i,j]=matrix(t(matrix(x[i,j],nrow(x),3)))
}
}
k


#final
ma <- matrix(1:6, nrow  = 2)
rma <- ma[rep(1:2, c(2,3)),]


x=matrix(c(1,2,3,4,5,6,7,8,9),3,3)
xnew <- x[rep(1:3, c(5,5,5)),]



#another way

a<-matrix(c(1,7,8,3,1,2),3,2)

rep(a[,1],times=a[,2])

rep(a[,2],times=a[,2])

cbind(rep(a[,1],times=a[,2]),rep(a[,2],times=a[,2]))

