rm(list=ls())
#FUNCTION TO FIND THE MEDIAN OF A VECTOR
med<-function(x){
odd.even<-length(x)%%2  #x odd or even
if (odd.even==0)(sort(x)[length(x)/2]+sort(x)[1+length(x)/2])/2

else sort(x)[ceiling(length(x)/2)]
}

y=c(5,3,4,5,3)

med(y)
med(y[-1])


#FUNCTION TO "ROLL" N FAIR DICE AND RETURN THE SUM
sumdice<-function(n){

k<-sample(1:6,size=n,replace=T)

return(sum(k))


}

#print result to the console

sumdice(2)

#to store the result
a<-sumdice(5)
a
#we want to update sumdice function to "roll" n fair "k" sided dice


fix(sumdice)
sumdice


function(n,sides=6){

if (sides<1)return(0)


k<-sample(1:sides,size=n,replace=T)

return(sum(k))


}

sumdice(n=5,sides=4)

sumdice(5,4)





quad <- function(a, b, c) {
# find the zeros of a2*x^2 + a1*x + a0 = 0
if (a == 0 && b == 0 && c == 0) {
roots <- NA
} else if (a == 0 && b == 0) {
roots <- NULL
} else if (a == 0) {
roots <- -c/b
} else {
# calculate the discriminant
discrim <- b^2 - 4*a*c
# calculate the roots depending on the value of the discriminant
if (discrim > 0) {
roots <- (-b + c(1,-1) * sqrt(discrim))/(2*a) #c(1,-1) +or -
} else if (discrim == 0) {
roots <- -b/(2*a)
} else {
roots <- NULL  #when discrim <0,complex so return null

}
}
return(roots)
}


f<-function(){
x<-1
g()
return(x)
}

g<-function(){
x<-2
}

f()

test<-function(x){
y<-x+1
return(y)

}
test(1)
y=10
test(1)
test2<-function(x){
y<-x+z
return(y)
}

z<--1
test2(1)

#apply is for matices
(x<-matrix(1:24,nrow=4))

apply(x,1,sum)#1 mean rows  
apply(x,2,sum)#1 mean columns

apply(x,1,sqrt) #sqrt of transpose matrix

apply(x,2,sqrt) #sqrt of  matrix

apply(x,2,sample)

apply(x,1,function(x) x^2+x)

# sapply is for vectors
#generates a sequence of sequences from 1:3 to 1:7
sapply(3:7,seq)
