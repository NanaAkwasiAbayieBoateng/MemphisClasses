complete.cases(my.data) #check missing data
na.omit(my.data)#check NA  in data
my.data[-(3:6),] #drop colums 3 -6
my.data[perm >=mean(perm),]

#Graphics
attach(Loblolly)
head(Loblolly)
str(Loblolly)

plot(age,height)

plot(Seed,height)

plot(Seed,age)

barplot(height)

means<-tapply(height,Seed,mean)
barplot(means)


plot(height~age)
plot(height~age,col="red",xlab="explanatory variable",ylab="response variable")

abline(lm(height~age))


plot(0:10,0:10,type="n",xlab="",ylab="")
k<-1
for (i in c(2,5,8)){

for (j in 1:9){
k=k+1
points(i,j,pch=k,cex=2)}}
