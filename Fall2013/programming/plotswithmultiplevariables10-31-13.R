data(iris)
pairs(iris[101:150,1:4])

panel.d<-function(x,...){
usr<-par("usr")
on.exit(par(usr))
par(usr=c(usr[1:2],0,0.5))
lines(density(x))
}

x<-scale(iris[101:150,1:4])

r<-range(x)

pairs(x,diag.panel=panel.d,xlim=r,ylim=r)

library(lattice)
#does the same as pair but with color
splom(iris[101:150,1:4])

splom(iris[,1:4],groups=iris$Species)

#all 3 at once in black and white with different symbols
splom(~iris[,1:4],groups=iris$Species,col=1,pch=c(1,2,3),cex=c(.5,.5,.5)) in

####other graphs in lattice###

data(Loblolly)
densityplot(height~age|Seed,data=Loblolly)


bwplot(height~age|Seed,data=Loblolly)


attach(iris)
#shows different  relationship at different intervals
coplot(Sepal.Length~Sepal.Width|Petal.Length,panel=panel.smooth)


#interaction plots
data(CO2)
interaction.plot(CO2$Type,CO2$Treatment,CO2$uptake)

#plot a standard bivariate normal distribution
f<-function(x,y){

z<-(1/(2*pi))*exp(-0.5*(x^2+y^2))

}

y<-x<-seq(-3,3,length=50)
z<-outer(x,y,f ) #applies the function f to the (x,y) grid

persp(x,y,z,theta=45,phi=30,expand=0.6,ltheta=120,shade=0.75,ticktype="detailed",,xlab="x",ylab="y",col="lightpink")

M<-persp(x,y,z,theta=45,phi=30,expand=0.4,box=FALSE)
M
#  add some points along a circle in the same coordinate system
a<-seq(-pi,pi,pi/16)
newpts<-cbind(cos(a),sin(a))*2
newpts<-cbind(newpts,0,1)
N<-newpts%*%M
points(N[,1]/N[,4],N[,2]/N[,4],col=2)

#add lines
x2<-seq(-3,3,.1)
y2<- -x2^2/3
z2=dnorm(x2)*dnorm(y2)
N<-cbind(x2,y2,z2,1)%*%M
lines(N[,1]/N[,4],N[,2]/N[,4],col=4)


#add text
x3<-c(0,3,1)
y3<-c(0,-3,1)
z3<-dnorm(x3)*dnorm(y3)*1.1
N<-cbind(x3,y3,z3,1)%*%M
text(N[1,1]/N[1,4],N[1,2]/N[1,4],"f(x,y)",col="lightpink")

x<-y<-seq(-3,3,length=50)
xy<-expand.grid(x,y)
z<-(1/(2*pi))*exp(-0.5*(xy[,1]^2+xy[,2]^2))
wireframe(z~xy[,1]*xy[,2])