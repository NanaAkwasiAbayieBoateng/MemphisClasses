#Newton Raphson method
#for optimization f(d) must have first and second derivatives
#then tan the N0R can converge faster than golden section method
#we want the minimum of f(x) 
#to be in the interval [a,b]
#assume the minimum is not at a or b
#then the derivative of f() at the minimum is 0
#if the second derivative >0 the the minimum
#keep iterating until the derivative is close enough to 0


#example
#find the minimum of e^(-x)+x^4 from [-1,4]

#f<-function(x) {exp(-x)+x^4}
#with brackets is the same as without bracket

f<-function(x) {exp(-x)+x^4}

curve(f,from=-1,to=1)

fprime<-function(x) -exp(-x)+4*x^3

fprimeprime<-function(x) exp(-x)+12*x^2

#x<-0.5
x<-c(0.5,rep(NA,6))

fval<-rep(NA,7)

fprimeval<-rep(NA,7)

fprimeprimeval<-rep(NA,7)

for (i in 1:6){
fval[i]<-f(x[i])
fprimeval[i]<-fprime(x[i])
fprimeprimeval[i]<-fprimeprime(x[i])
x[i+1]<-x[i]-fprimeval[i]/fprimeprimeval[i]


}
data.frame(x,fval,fprimeval,fprimeprimeval)


#we can use the N-R  method to find the zeros(roots) of the function

#a function to find the root using N-R method

newtonraphson<-function(ftn,xo,tol=1e-9,maxiter=100){

#ftn is function of a single variable 
#returns both function value and first derivative
#xo is the initial guess
#function terminates if |f(x)| <tol or
#iter exceeds maxiter
#initialize
x<-xo
fx<-ftn(x)
iter<-0
#continue until stopping conditions are set
while((abs(fx[1])>tol)&&(iter<maxiter)){
x<-x-fx[1]/fx[2]
fx<-ftn(x)
iter=iter+1
cat("At iteration",iter,"value of x is:",x,"\n")


}

#output
if (abs(fx[1])>tol){
cat("Algorithm failed to converge\n")

return(NULL)
}else{
cat("Algorithm converged \n")
#out=list(data.frame(x,fx))

#return(out)
return(x)
}



ftn4<-function(x){
fx<-log(x)-exp(-x)
dfx<-1/x+exp(-x)
return(c(fx,dfx))


}
}
newtonraphson(ftn4,2,1e-6)
