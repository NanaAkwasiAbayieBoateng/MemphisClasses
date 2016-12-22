rm(list=ls())

#given a vector x calculate its geometric mean using a loop
#using a vector operation
# ex 3.6

#harmonic mean
x<-c(1,2,3,4,5,6)
sum=0
for (i in 1:length(x)){
sum=sum+(1/x[i])

}
1/sum

1/(sum(1/x))


#geometric mean
s=1
for (i in 1:length(x)){
 s=s*   x[i]^(1/length(x))

}
s

prod(x)^(1/length(x))


#7
#find the sum of every third element of x
rm(list=ls())
x<-rpois(25,4)

sum=0
for (i in 1:length(x)){
if(i%%3==0){
sum=sum+x[i]
}
}
sum

sum(x[seq(3,25,by=3)])

sum(x[1:(length(x)%/%3)*3])


#DISPLAYS POWERS OF 1 TO N OF X
#input
x<-7
n<-5
#display powers
cat("powers",x,"\n")
cat("exponent  result\n\n")


result<-1
for (i in 1:n){
result<-result*x
cat(format(i,width=8),
format(result,width=10),"\n",sep="")
}


cat(paste(format(1:n,width=8),format(x^(1:n),width=10),"\n"),sep="")

data()
try(data(package="MASS"))
dir(path=".")
getwd()



