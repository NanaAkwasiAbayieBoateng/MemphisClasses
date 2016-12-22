rm(list=ls())

#working directory
setwd("H:/7608")

getwd()
dir(path=".")

dir(path="C:/Users/Owner/Documents/memphisclassesbooks/FALL2013")

list.files(path = "C:/Users/Owner/Documents/memphisclassesbooks/FALL2013")


scan(file = "C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/scan.txt")

#scan(file = "C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/johnprime.r", what = 0, n = -1, sep = "", skip = 0, quiet = FALSE)

#input data from a file
file_name="C:/Users/Owner/Desktop/data.txt"

#read from file

data<-scan(file=file_name)

#quartiles calculations

n<-length(data)
data.sort<-sort(data)

data.1qrt<-data.sort[ceiling(n/4)]
data.med<-data.sort[ceiling(n/2)]
data.3qrt<-data.sort[ceiling(3*n/4)]

#output
cat("1st quartile:",data.1qrt,"\n")
cat("median :",data.med,"\n")
cat("3rd quartile:",data.3qrt,"\n")

#inbuilt function
quantile(scan("C:/Users/Owner/Desktop/data.txt"),(0:4)/4)


#writing a matrix to a file
(x<-matrix(1:24,nrow=4,ncol=6))
write(t(x),file="C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/newmatrix.txt",ncol=6)


usefuldata<-matrix(1:25,nrow=5)
usefuldata

dump("usefuldata","useful.r")

rm(usefuldata)

source("C:/Users/Owner/Desktop/useful.r")
usefuldata

dump(c("usefuldata","x"),"useful.r")

rm(usefuldata)
rm(x)
x

dump(list=objects(),"all.r")
rm(list=ls())
x

usefuldata
source



#usetext editor to create a file containing vector 64 38 97 88 24 14 104 83
#save it as random vector.dat
#read it into a vector called random_data using scan
#input data from a file
rm(list=ls())
#file_name="C:/Users/Owner/Desktop/random vector.txt"
file_name="C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/random vector.txt"

#read from file

random_data<-scan(file=file_name)
random_data

x<-rnorm(100)
y<-rpois(100,30)
plot(x,y,main="poisson vs normal")
plot(x,y,main="poisson vs normal",pch=16)
plot(x,y,main="poisson vs normal",type='l')

plot(sort(x),sort(y),main="poisson vs normal",type='l')

plot(sort(x),sort(y),main="poisson vs normal",type='l',xlab="normal",ylab="poisson")

plot(sort(x),sort(y),ylim=c(15,45),main="poisson vs normal",type='l',xlab="normal",ylab="poisson")
title("poisson vs normal")



#example at page 57
rm(list=ls())
x<-seq(0,5,by=.01)
y.upper<-2*sqrt(x)
y.lower<- -2*sqrt(x)
y.max<-max(y.upper)
y.min<-min(y.upper)
y.min<-min(y.lower)
plot(c(-2,5),c(y.min,y.max),type="n",xlab="x",ylab="y")
lines(x,y.upper)
lines(x,y.lower)
abline(v=-1)
points(1,0)
text(1,0,"focus(1,0)",pos=4)
text(-1,y.min,"directrix x=1",pos=4)
title("The parabola y^2=4*x")

par(mfrow=c(2,2))
curve(x*sin(x),from=0,to=100,n=1001)
curve(x*sin(x),from=0,to=10,n=1001)
curve(x*sin(x),from=0,to=1,n=1001)
curve(x*sin(x),from=0,to=.1,n=1001)

par(mfrow=c(1,1))

usefuldata<-matrix(1:25,nrow=5)
usefuldata
dump("usefuldata","useful.r")
dir(path=".")
rm(usefuldata)
usefuldata
source("useful.r")
usefuldata
x=rnorm(500)
x
dump(c("x","usefuldata"),"useful.r")
rm(x)
rm(usefuldata)
x
usefuldata
source("useful.r")
usefuldata









