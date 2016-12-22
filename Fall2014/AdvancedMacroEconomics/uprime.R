rm(list=ls())

t=seq(0,100,0.1)
p=5*exp(0.2*t)
up1=1*10+p
up2=(100*10+p)/100


par(mfrow=c(1,2))

plot(t,up1,type='l',main="path of v(t) over time,a=1",
     col=4,ylab="v(t)=up(e)/a",xlab="time")

plot(t,up2,type='l',main=" path of v(t) over time,a=100",col=2)






plot(t,up1,type='l',main="path of v(t) over time",
     col=4,ylab="v(t)=up(e)/a",xlab="time")

lines(t,up2,type='l',main="a=100",col=2)

legend("topright", inset=.05, title="Dynamics of a in energy demand",
       c("up1=low a","up2=high a"), fill=c("4","2"), horiz=TRUE)

plot(t,up2,type='l',main=" Dynamics of v(t) at a=100",col=2)




library(ggplot2)
df <- data.frame(t, up1, up2)
ggplot(df, aes(t, y = up(e1+e2)/a, color = variable)) + 
  geom_point(aes(y = up1, col = "up1")) + 
  geom_point(aes(y = up2, col = "up2"))



