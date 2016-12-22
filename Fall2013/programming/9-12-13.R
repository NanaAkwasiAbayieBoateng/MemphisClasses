#example 2.3.1 pg 47

rm(list=ls())
a<-1
b<-4
c<-1
#calculate the discriminant
discrim<-b^2+-4*a*c

#calculate the roots of the quadratic function on the discriminant

if (discrim > 0){

roots<-c((-b+sqrt(discrim))/(2*a),(-b-sqrt(discrim))/(2*a))
} else if (discrim==0){
roots<--b/(2*a)
}else{
roots<-c()
}

#output
show(c(discrim,roots))



#summing
n<-100
sum((1:n)^2)
sum<-0
for (i in 1:n){
sum<-sum+i^2
}
sum


#computtational time
x<-runif(10000000)
system.time(max(x))

pc<-proc.time()
cmax<-x[1]
for (i in 2:10000000){
if (x[i]>cmax) cmax<-x[i]

}

proc.time()-pc




#finding Fibonacci numbers

Fibonacci<-numeric(12)  # creates a vector of length 12 to store output
Fibonacci[1]<-Fibonacci[2]<-1

for (i in 3:12) Fibonacci[i]<-Fibonacci[i-1]+Fibonacci[i-2]

Fibonacci

(x_list<-seq(1,9,by=2))

 sum_x<- 0
for (x in x_list){

sum_x <- sum_x +x
cat("The loop element is",x,"\n")
cat("The cummulative is",sum_x,"\n")
}


rm(list=ls())
#set the tolerance level
tol<-0.1
i<-0.006

count<-0
while (tol>0.0001){
newi<-(1-(1+i)^(-20))/19
tol<-abs(newi-i)
i<-newi
count<-count+1
}
i
count


# ifelse
y<-log(rpois(20,1.5))
y
y<-ifelse(y<0,NA,y)
y
#ex 3.9
#1
rm(list=ls())
x.values<-seq(-2,2,by=0.1)
n<-length(x.values)
y.values<-rep(0,n)
 
for (i in 1:n){
x<-x.values[i]

#if (x<=0){y=-x^3}
#else if ((x>0)&(x<=1)){y=x^2}
#else {y=sqrt(x)} 

y.values<-ifelse(x.values<=0,-x.values^3,ifelse(x.values<=1,x.values^2,sqrt(x.values))

#y.values[i]<-y
}
#output
plot(x.values,y.values,type="l")

#doing the same thing vectorising
#y.values[i]=y
#y.values<-ifelse(x.values<=0,-x.values^3,ifelse(x.values<=1,x.values^2,sqrt(x.values))
#output
#plot(x.values,y.values,type="l")
 
