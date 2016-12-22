#deriv takes derivatives

Df<-deriv(z~sin(x^2/2-y^2/4)*cos(2*x-exp(y)),c('x','y'),func=TRUE,hess=TRUE)

f3<-function(x){
Dfx<-Df(x[1],x[2])
f<-Dfx[1]
gradf<-attr(Dfx,'gradient')[1,]
hessf<-attr(Dfx,'hessian')[1,,]
return(list(f,gradf,hessf))

}

f3(c(pi/3,pi/4))



#optimum
#u can choose the method of optimization you want
#melder mead simplex method is the default
# wild function global min at about -15.8

fw<-function(x){
10*sin(0.3*x)*sin(1.3*x^2)+0.0001*x^4+0.2*x+80}

plot(fw,-50,50,n=1000,main='wild function')


res<-optim(50,fw,method="SANN",control=list(maxit=20000,parscale=20))
res


fr<-function(x){
x1=x[1]
x2<-x[2]
100*(x2-x1*x1)^2+(1-x1)^2

}


optim(c(1.2,1),fr)

using

#numerical Integration

trapezium<-function(ftn,a,b,n=100){

#numerical integration of ftn from a to b
#using the trapezoidal rule with n intervals

h=(b-a)/n
x.vec<-seq(a,b,by=h)
f.vec<-sapply(x.vec,ftn)
T<-h*(f.vec[1]/2+sum(f.vec[2:n])+f.vec[n+1]/2)
return(T)
}

# try on integral from 0 to 1 of 4x^3=1
ftn1<-function(x) return(4*x^3)
trapezium(ftn1,0,1,n=20)

trapezium(ftn1,0,1,n=40)

trapezium(ftn1,0,1)

ftn2<-function(x) return(1/sqrt(2*pi)*exp(-x^2/2))

trapezium(ftn2,0,1,1000)

pnorm(1)-pnorm(0)

#monte carlo integration
#uses simulation to approximate deifnite integrals
#relies on the law of large numbers-says that a sample mean
#from a large random sample will tend to the expected value
#of the distribution being sampled

#if we can express an integral  apas an expected value 
#we can approximate anit with the sample mean

u<-runif(100000)
mean(u^4)

#integral 2-5 of sin(x)dx
u<-runif(10000,min=2,max=5)
mean(sin(u))*(5-2)


#integral from 2 to 4e^(-x) dx =e^(-2)-e^(-4)

u<-runif(10000,min=2,max=4)

mean(exp(-u))*(4-2)

exp(-2)-exp(-4)