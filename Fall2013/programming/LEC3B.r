
 #naming elements within a vector
 counts<-c(25,12,7,4,6,2,1,0,2)

#get the length of counts
 length(counts)

names(counts)<-0:8
counts

as.vector(counts)


counts<-as.vector(counts)

counts


##arithmetic with vectors
 x<-c(1,2,3,4,5)
 y<-c(-1,0,4,8,20)

x*3

z<-y-4

z

x*y

x^y

w<-c(2,3)

 x+w

 max(counts)


#find the range of counts
range(counts)

quantile(counts)

sum(counts)

mean(counts)

sum(counts)/length(counts)

median(counts)

cor(x,y)

var(counts)

sort(counts)

sort(counts, decreasing = TRUE)

rank(counts)
rank(counts, na.last = FALSE)

###example 2.4.1
x<-c(1.2,0.9,0.8,1,1.2)

x.mean<-sum(x)/length(x)

x.mean-mean(x)

x.var<-sum((x-x.mean)^2)/(length(x)-1)

x.var-var(x)


###example 2.4.3
x<-seq(10,200,by=10)
x

y<-(1+1/x)^x
y

exp(1)-y

plot(x,y)

a<-c(11,NA,13)
is.na(a)

mean(a)

mean(a,na.rm=T)

which(is.na(a))

c(0,0,1,1)|c(0,1,0,1)

xor(c(0,0,1,1),c(0,1,0,1))

x<-0:10


#summing and indexing

sum(x)

x<5
x[x<5]

sum(x<5)

sum(x[x<5])

## the number of elements in x<5

sum(x<5)


## the sum of elements in x<5

sum(x[x<5])

#### suppose we want to work out the sum of the three largest #s in a vector
#1 sort the vector into descending order
# add the first three elements

y<-c(8,3,5,7,6,6,8,9,2,3,9,4,10,4,11)
sort(y)

t=(sort(y,decreasing=TRUE))
t1=t[!t%%7==0]

rev(sort(y))

rev(sort(y))[1:3]

sum(rev(sort(y))[1:3])

# which values of y are >5
 y[y>5]

 # which indexes of y>5
 which(y>5)

# find how many elements of y>5
sum(y>5)

length(y[y>5])

## suppose we want to calculate a trimmed mean of a vector that ignores the largest and smallest values # sort x
# remove first and last values
# use mean function on the remaining values

x<-c(5,8,6,7,1,5,3)

sort(x)

sort(x)[-c(1,length(x))]

mean(sort(x)[-c(1,length(x))])


### suppose we need to produce a vector containing the numbers 1 to 50 but omitting all the multiples of 7
# first make vector 1 to 50
# see how many multiples of 7 are between 1 and 50
# create a second vector of the multiples
# remove that vector from the first vector
vec<-1:50
multiples<-floor(50/7)
subscripts<-7*(1:multiples)
subscripts

vec[-subscripts]

vec[!vec%%7==0]

x<-1:20
y<-x[!x%%4==0]
y

sqrt(2)*sqrt(2)==2

sqrt(2)*sqrt(2)-2
