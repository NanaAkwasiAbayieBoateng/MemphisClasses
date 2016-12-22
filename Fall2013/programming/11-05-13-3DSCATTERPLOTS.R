attach(iris)
#three-d scatter plots
head(iris)
library(lattice)
print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species))
#3-D scatterplots with different response varaibles on the same device
print(cloud(Sepal.Length~Petal.Length*Petal.Width,data=iris,
groups=Species,main="1",pch=1:3,scales=list(draw=FALSE),zlab="SL",screen=list(z=30,x=75,y=0))
,split=c(1,1,2,2),move=TRUE,newpage=FALSE)

print(cloud(Sepal.Length~Petal.Length*Petal.Width,data=iris,
groups=Species,main="2",pch=1:3,scales=list(draw=FALSE),zlab="PL",screen=list(z=30,x=75,y=0))
,split=c(2,1,2,2),move=TRUE,newpage=FALSE)

print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species,main="3",
pch=1:3,scales=list(draw=FALSE),zlab="FL",screen=list(z=30,x=55,y=0),split=c(1,2,2,2),move=TRUE,newpage=FALSE)


print(cloud(Petal.Width~Sepal.Length*Sepal.Length,main="4",pch=1:3,scale=list(draw=FALSE)  screen=list(z=30,x=-55,y=0),split=c(2,2,2,2),move=FALSE)

data(volcano)
contour(volcano,asp=1,labcex=1)

contourplot(volcano)
#a filled contour plot
#provide color background
image(volcano,col=terrain.colors(100),axes=FALSE)
contour(volcano,levels=seq(100,200,by=10),add=TRUE)
filled.contour(volcano,color=terrain.colors,asp=1)
levelplot(volcano,scales=list(draw=FALSE),xlab="",ylab="")

#create a pdf file
data(Loblolly)
pdf(file="graphic.pdf",width=4,height=3)
dev.off()
plot(Loblolly$age,Loblolly$height)

png(file="graphic.png", width = 800, height = 600)
dev.off()

plot(Loblolly$age,Loblolly$height)


#find  out what packages are installed
help.start()
#find out what packages are loaded
sessionInfo()

#look at a generic function
mean

methods(mean)

methods(var)

#maximum size of an integer
.Machine$integer.max

#see how long i have spent on this current R session
p=proc.time()
proc.time()/60
 
p[3]


esp<-1e-12
x<-0.5
n<-0
loglx<-0
while(n==0||abs(last.term)>esp){
n<-n+1
last.term<-(-1)^(n+1)*x^n/n
loglx<-loglx+last.term

}
loglx
log(1.5)



