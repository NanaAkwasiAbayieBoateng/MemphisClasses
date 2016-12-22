##three-d scatterplots using iris

attach(iris)
head(iris)
library(lattice)
print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species))

print(cloud(Sepal.Length~Petal.Length*Petal.Width,data=iris,groups=Species,main="1",pch=1:3, scales=list(draw=FALSE),zlab="SL",screen=list(z=30,x=-75,y=0)),
split=c(1,1,2,2),move = TRUE,newpage=FALSE)

print(cloud(Sepal.Width~Petal.Length*Petal.Width,data=iris,groups=Species,main = "2",pch=1:3,scales=list(draw=FALSE),zlab="SW",screen=list(z=30,x=-75,y=0)),
split=c(2,1,2,2),move=TRUE, newpage=FALSE)

print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species,main = "3",pch=1:3,scales=list(draw=FALSE),zlab="PL",screen=list(z=30,x=-55,y=0)),
split=c(1,2,2,2),move=TRUE, newpage=FALSE)

print(cloud(Petal.Width~Sepal.Length*Sepal.Width,data=iris,groups=Species,main = "4",pch=1:3,scales=list(draw=FALSE),zlab="PW",screen=list(z=30,x=-55,y=0)),
split=c(2,2,2,2),move=TRUE, newpage=FALSE)


?split

## contour plot using volcano
data(volcano)
contour(volcano,asp=1,labcex=1)
head(volcano)
dim(volcano)
contourplot(volcano)
# now a filled contour plot that provide color background
image(volcano,col=terrain.colors(100),axes=FALSE)
contour(volcano,levels=seq(100,200,by=10),add=TREU)
filled.contour(volcano,color=terrain.colors,asp=1)
levelplot(volcano,scales=list(draw=FALSE),xlab="",ylab="")
# now if you want to save your work
setwd("C:/Users/yjhang/Desktop")##remember to change the direction of slashes
# to create a pdf file
data(Loblolly)
pdf(file="grahic.pdf",width=4,height=3)
plot(Loblolly$age,Loblolly$height)
dev.off()

##second half
##find out what packages are loaded
sessionInfo()

##check available pacakges
available.packages() ##now select your location

##look at a generic function
mean
methods(mean)
methods(var)

##maximum size of integer that we can store
.Machine$integer.max

#let's see how long I have spent on this current R session
proc.time() ##in seconds
proc.time()/60 ## in minutes

##another round of practice on "math"
eps<-1e-12
x<-0.5

n<-0
log1x<-0
while(n==0 ||abs(last.term)>eps) {
	n<-n+1
	last.term<-(-1)^(n+1)*x^n/n
	log1x<-log1x+last.term
}

log1x
log(1.5)













