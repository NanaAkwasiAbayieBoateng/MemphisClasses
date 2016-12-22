#compare times for different methods of summing columns in large matrix
#make a big matrix
big.matrix<-matrix(1:1e+06,nrow=1000)
columns<-rep(NA,dim(big.matrix)[2])

#compute column sum using a double loop of summations
system.time(
for (i in 1:dim(big.matrix)[2]){
s<-0
for (j in (1:dim(big.matrix)[1])){

s<-s+big.matrix[j,i]
}
columns[i]<-s
}
)


#compute column sums by using apply
system.time(colsums<-apply(big.matrix,2,sum))
 

#compute column sums using a single loop
system.time(for(i in 1:dim(big.matrix)[2]){
colsums[i]<-sum(big.matrix[,i])

}
)

#using the dedicated R functions

system.time(colsums<-colSums(big.matrix))


#Numerical Optimization
#givena function  f(x)   values of x  makes f() as large as possible
#example -maximizing a likelihood function
#it is efficient to minimize since if we want to maximize we can
#minimize -f()
#the golden section search method
#single variable function which has a single minimum on interval [a,b]

#example -minimize f(x) =|x-3.5|+(x-2)^2 on [0,5]
#define a function f(x)

f<-function(x){
abs(x-3.5)+(x-2)^2
}
 
#to check for a single minimum by graphing
curve(f,from=0,to=5)

#the Golden section algorithm
#1.start with [a,b] known to contain the minimum
#2. repeatedly shrink this interval into smaller and smaller intervals known
#to contain the minimum
#3. stop when the difference in endpoints is small enough
#the midpoint of the interval is the estimated minimum

golden<-function(f,a,b,tol=0.0000001){
#f is the function to be minimized
#a is the lower bound
#b is the upper bound
#tol is the stopping criteria
#initialize with endpoint value
ratio<-2/(sqrt(5)+1)
x1<-b-(b-a)*ratio
x2<-a+(b-a)*ratio
#evaluate the function at x1 and x2
f1<-f(x1)
f2<-f(x2)
#fo   01:2){
while(abs(b-a)>tol){
if (f2>f1){
b<-x2  #replace upper endpoint
x2<-x1   #property of golden ratio
f2<-f1  #dont have to recompute new x1,new f1
x1<-b-ratio*(b-a)
f1<-f(x1)

}else{
a<-x1 # #replace lower endpoint 
x1<-x2 # property of golden ratio(u can also compute
f1<-f2 #dont have to recompute
x2<-a+ratio*(b-a) #new x2
f2<-f(x2) #new f2
}

 
}
return((a+b)/2)
}

golden(f,0,5)












































