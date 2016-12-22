rm(list=ls())
n=50

if (n<2) {cat("Enter a Number Greater than or Equal to 2")
	
} else if (n==2) {
		cat("2  is the first prime number")
		} else {

#creates a vector from 2 to n
#if n is not an integer,it is rounded to the nearest integer
#round(n) rounds  any number to the nearest integer
round(n)  
p <- 2:n
    i <- 1
    while (p[i] <= sqrt(n)) {

# takes out multiples of numbers from 2
#takes out multiples of remaining numbers except the number itself

        p <-  p[p %% p[i] != 0 | p==p[i]]
#updates the numbers
        i <- i+1
    }
    p



}