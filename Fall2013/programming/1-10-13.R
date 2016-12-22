#we want to w
eval_polynomial<-function(x,a){
n<-length(a)-1

sum<-a[n+1]
for (i in 1:n){
sum<-sum+a[i]*x^(n-i+1)


}
return(sum)

}

eval_polynomial(1,c(1,-3,3,1))


inversetransform<-function(lambda,n){
if ((lambda| n)<=0){
cat("enter a  number  greater than zero")


}else{
x<-rep(0,n)
for (i in 1:n){
x[i]<--log(1-runif(1))/lambda
}
return(x)
}


}

#?set.seed(32789)

#mean(inversetransform(2,100000))

inversetransform(6,0)

gen_exp<-function(n,lambda){
if (lambda<=0){
return()

}else{
x=-log(1-runif(n))/lambda

}
return(x)

}
set.seed(32789)
mean(gen_exp(1000,3))