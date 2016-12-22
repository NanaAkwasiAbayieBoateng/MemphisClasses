rm(list=ls())

n=10





 if (n<2) {cat("Enter a Number Greater than or Equal to 2")
	
} else if (n==2) {
		cat("2  is the first prime number")
		} else {
x<-c(2:n)
x<-rep(TRUE,n)
for (i in 2:n^0.5){

if (x[i]==TRUE){

for (j in i^2:n){
x[j]=FALSE

}

}


}
x

}



#The Prime Function 1

primegen=function(v){
 return(sapply(v,function(z){sum(z/1:z==z%/%1:z)})==2)
}

primegen(c(1,2,3,4,5))
print(which(primegen(c(1,2,3,4,5))))

#2
sieve <- function(n)
{
   n <- as.integer(n)
   if(n > 1e6) stop("n too large")
   primes <- rep(TRUE, n)
   primes[1] <- FALSE
   last.prime <- 2L
   for(i in last.prime:floor(sqrt(n)))
   {
      primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
      last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
   }
   which(primes)
}

 sieve(100)

#3
sieve <- function(n)
{
   n <- as.integer(n)
   if(n > 1e8) stop("n too large")
   primes <- rep(TRUE, n)
   primes[1] <- FALSE
   last.prime <- 2L
   fsqr <- floor(sqrt(n))
   while (last.prime <= fsqr)
   {
      primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
      sel <- which(primes[(last.prime+1):(fsqr+1)])
      if(any(sel)){
        last.prime <- last.prime + min(sel)
      }else last.prime <- fsqr+1
   }
   which(primes)
}

sieve(20)


#4
primes <- function(n){
    primesR <- function(p, i = 1){
        f <- p %% p[i] == 0 & p != p[i]
        if (any(f)){
            p <- primesR(p[!f], i+1)
        }
        p
    }
    primesR(2:n)
}
primes(10)



#5
primest <- function(n){
    p <- 2:n
    i <- 1
    while (p[i] <= sqrt(n)) {
        p <-  p[p %% p[i] != 0 | p==p[i]]
        i <- i+1
    }
    p
}

primest(1)


#6
 n = as.integer(n)
   if(n > 1e8) stop("n too large")
   primes = rep(TRUE, n)
   primes[1] = FALSE
   last.prime = 2L
   fsqr = floor(sqrt(n))
   while (last.prime <= fsqr)
   {
      primes[seq.int(2L*last.prime, n, last.prime)] = FALSE
      sel = which(primes[(last.prime+1):(fsqr+1)])
      if(any(sel)){
        last.prime = last.prime + min(sel)
      }else last.prime = fsqr+1
   }
   which(primes)


#checks one number is prime
isPrime <- function(x){
    div <- 2:floor(sqrt(x))
    !any(x %% div == 0)
}

isPrime(10)