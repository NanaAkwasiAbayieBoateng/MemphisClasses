# a holds a sequence of numbers from 2 to n
n<-50
a <- c(2:n)
# we start from 2 since it is the beginning of prime numbers,
# it is also the loop varibale
l <- 2
# r this vector holds the results
r <- c()
#while the square of loop variable is less than n
while (l*l < n) {
# the prime number is the first element in vector a
# for e.g. in first iteration it will be 2
r <- c(r,a[1])
 
# remove elements from a which are multiples of l
# for e.g. in first iteration it will remove 2,4,6,8...
a <- a[-(which(a %% l ==0))]
 
# the loop varibale if the first variable in remaining a
# for e.g after first iteration, it will be 3, then 5 (since 4 has been removed)...
l <- a[1]
}
# the result is r and all the remaining elements in a
c(r,a)