data(Loblolly)
attach(Loblolly)
names(Loblolly)
str(Loblolly)


plot(age,height,col=as.numeric(Seed),pch=as.numeric(Seed))


#use text function to label each point on the plot with a value of a variable

plot(age,height)

text(age,height,labels=Seed,pos=1,offset=0.5,cex=0.7)

#change the colors for logical condition

plot(age,height,pch=16,col=ifelse(height>median(height),"red","black"))


#add text to a scatter plot at a point(x,y)
text(10,35,"(b)")

#draw a smooth curve
curve(x^3-3*x,-2,2)

x=seq(-2,2,0.01)
y=x^3-3*x
plot(x,y,type="l")

plot(0:10,0:10,xlab="",ylab="",xaxt="n",type="n")

#this just made a big box
#to add a rectangle rect(xleft,ybottom,xright,ytop)
rect(6,6,9,9)


#to draw an arrow from (x0,y0) to (x1,y1) with default arrowhead at (x1,y1)
#arrows(x0,y0,x1,y1)
arrows(1,1,3,8)

#a double headed arrow from (x0,y0) to (x1,y1) add option code =3
arrows(1,9,5,9,code=3)

# a vertical line with 2 square ends use option angle =90
arrows(4,1,4,6,code=3,angle=90)


#to draw a polygon and shade it we specify the endpoint
x<-c(1.19,3.09,5.56,6.02,4.85,1.51)
y<-c(8.71,9.54,8.06,4.38,1.24,2.54)
length(x)
length(y)
polygon(x,y,col="lavender")

#shade area corresponding to a p-value
xv<-seq(-3,3,0.01)
yv<-dnorm(xv)

plot(xv,yv,type="l")

#shade everything to the left of negative 1
polygon(c(xv[xv<=-1],-1),c(yv[xv<=-1],yv[xv==-3]),col="red")



#smooth curves you need about 100 straight line sections between the 
#min and max values on your x-axis

xv<-0:100
ya<-482*xv*exp(-0.045*xv)
yb<-518*xv*exp(-0.055*xv)

#concern is plotting both graphs on same plot so that the ylimits
#fit both plots
#find limits of ya and yb and set with ylim
#use type $n$ to draw axes without data then use lines to add the smooth "n"


plot(c(xv,xv),c(ya,yb),xlab="stock",ylab="recruits",type="n")
lines(xv,ya,lty=1,col="blue")
lines(xv,yb,lty=2,col="navy")

data<-data.frame(cars)
names(cars)
attach(cars)
par(mfrow=c(2,2)) #2 * 2 square of 4  plots in the same graphic device
#lowers-a non -parametric curve fitter
plot(speed,dist,pch=16)
text(20,10,"lowess",pos=2)
lines(lowess(speed,dist))

#loess non parametric modeling tool
plot(speed,dist,pch=16)
text(20,10,"loess',pos=)
