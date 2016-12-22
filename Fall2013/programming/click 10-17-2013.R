

my.list<-list("one",TRUE,3,c("F","O","U","R"))
my.list

my.list[[2]]

mode(my.list[2])
my.list[[4]][2]



my.list<-list(first="one",second=TRUE,third=3,fourth=c("f","o","u","r"))
names(my.list)

my.list$first

names(my.list)<-c("First element","Second element","Third element","Forth element")

names(my.list)

my.list$"Second element"

my.list$'Second element'

names(lm)

x<-list(1,c(2,3),c(4,5,6))

mode(x)


y<-unlist(x)


mode(y)


unlist(my.list)

y<-c(1.1,1.2,2.4,2.3,1.8,1.9)

x<-c(1,1,2,2,3,3)

lm.xy<-lm(y~x)

lm.xy

mode(lm.xy)

names(lm.xy)

lm.xy$coefficients

lm.xy$residuals


lm.xy$model


#wilcoxon rank sum test-nonparametric test for difference in means
(w<-wilcox.test(rnorm(10),rnorm(10,2)))

mode(w)

names(w)

w$statistic

w$null.value


####use list to assign row and column names to a matrix
(a<-matrix(runif(8),4,2))

(dimnames(a)<-list(NULL,c("x","y")))

####use list to assign row and column names to a matrix

(dimnames(a)<-list(letters[1:4],c("y","z")))


click1<-function(){
x<-runif(1);y<-runif(1)
plot(x=x,y=y,xlim=c(0,1),main="please click on the circle",xlab="",ylab="",axes=FALSE,frame.plot=TRUE)

clicktime<-system.time(xyclick<-locator(1))

list(timestamp=Sys.time(),x=x,y=y,xclick=xyclick$x,yclick=xyclick$y,tclick=clicktime[3])

}
click1()

(x<-matrix(1:24,nrow=4))
# to sum across rows
apply(x,1,sum)

#to  get the square root of each element
apply(x,1,sqrt)

apply(x,2,sqrt)

library(MASS)
#summing all placebo where y=n in first column
with(bacteria,tapply((y=="n"),trt,sum))


a<-c("a","b","c","d")
b<-c(1,2,3,4,4,3,2,1)
c<-c(T,T,F)
list.object<-list(a,b,c)
class(list.object)

list.object

# to determine the length of each object in the list

lapply(list.object,class)

#to find the mean of each object in the list
lapply(list.object,mean)


head(bacteria)



