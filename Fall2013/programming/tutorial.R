x=seq(-4,4,by=0.01)
y1=sqrt(3*(x^2-1))
y2=-sqrt(3*(x^2-1))
y.upper<-2*sqrt(x)
y.lower<- -2*sqrt(x)
plot(x,y1,x,y2)
n=5
prod(1:n)

factorial(n)


x=c(1,2,3,4,5)
sapply(x, x/2)

k=3
#sum(x>2)
x[1:k]
x[k+1]

#winsorised mean

x <- sort(c( 8.244, 51.421, 39.020, 90.574, 44.697,
83.600, 73.760, 81.106, 38.811, 68.517))
k=2
n=length(x)
#(sum((k+1)*x[k+1] + (k+1)*x[n-k]+ x[(k+2):(n-k+1)]))/n

sum(c((k+1)*x[k+1] , (k+1)*x[n-k], x[(k+2):(n-k-1)]))/n

mean(x)
sum(x[(k+1):(n-k)])/(n-2*k)

wmean <- function(x, k) {
# calculate the k-th Windsorised mean of the vector x
x <- sort(x)
n <- length(x)
x[1:k] <- x[k+1]
x[(n-k+1):n] <- x[n-k]
return(mean(x))
}

wmean(x, 2)

 x.err <- x
x.err[1] <- 1000
 mean(x.err)
wmean(x.err, 2)


 nfact2 <- function(n) {
# calculate n factorial
 if (n == 1) {
cat("called nfact2(1)\n")
 return(1)
} else {
 cat("called nfact2(", n, ")\n", sep = "")
 return(n*nfact2(n-1))
 }
 }
nfact2(6)
