#generate independent chi -square random variables
#with v degrees of freedom
#if z_1,.....2_v are independent identically distributed
#standard normal random variables
#the the sum of their squares is a chi-square random variable with v degrees of freedom 
#
#1 fill an n times matrix with and random standard normal  varaibles 
#2 square each entry in the matrix
#3 compute the row sums,each will be chi-square with v degrees of freedom






####################################################
generate_chisquare<-function(n,v){
x<-matrix(rnorm(n*v),nrow=n)^2
y<-apply(x,1,sum)
return(y)




}
y<-generate_chisquare(100,4)
mean(y)

#variance of chi-square with v df=2v
mean(y^2)-mean(y)^2
var(y)*99/100




#suppose we have a mixture of gamma distributions
#f(x)=sum from 1 to 5 theta_i fix_(x)

#where f_i is the gamma (3,lambda_j) density

#theta<_(0.1,0.2,.2,.3,.2)

#lambda<-(1,1.5,2,2.5,3)
#we want to plot f(x) and f_i(x) on the same plot

theta<-c(0.1,0.2,.2,.3,.2)
lambda<-c(1,1.5,2,2.5,3)

### function to evaluate f(x) at a given point x
f<-function(x,lambda,theta){
sum(dgamma(x,3,lambda)*theta)

}

x<-seq(0,8,length=200)
y<-sapply(x,f,lambda=lambda,theta=theta)

plot(x,y,type="l",ylim=c(0,.85),lwd=3,ylab="Density")


for (j in 1:5){
#add the jth gamma density to the plot
y<-sapply(x,dgamma,shape=3,rate=lambda[j])
lines(x,y)


}

nfact<-function(n){
if (n==1){
cat("called nfact(1) \n")
return(1)
}else{
cat("called nfact(",n,") \n",sep="")

return(n*nfact(n-1))

}



}
 nfact(100)


my_fun<-function(x){
browser()
y<-x*z
return(y)

}
z<-2
my_fun(c(1,2,3))

#hit n after enter
rm(z)
my_fun(c(1,2,3))


############################
#write a function to find the euclidean length of a vector
##########################

euclidean<-function(v){
y=sqrt(sum(v^2))

return(y)

}
v=seq(1:10)

euclidean(v)

