click1<-function(){
	x<-runif(1);y<-runif(1)
	plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),
	main="Please click on the circle",xlab="", ylab="",
	axes=FALSE,frame.plot=TRUE)
	clicktime<-system.time(xyclick<-locator(1))
	list(timestamp=Sys.time(),
		x=x,y=y,
		xclick=xyclick$x,yclick=xyclick$y,
		tclick=clicktime[3])
}

