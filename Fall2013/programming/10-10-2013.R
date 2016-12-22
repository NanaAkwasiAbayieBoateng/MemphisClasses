#create data  frame from vectors
x<-runif(10)

y<-letters[1:10]
z<-sample(c(rep(T,5),rep(F,5)))
new<-data.frame(y,z,x)
new

#create data frame from a table
y<-rpois(1500,1.5)
table(y)

as.data.frame(table(y))


short.frame<-as.data.frame(table(y))

long<-as.data.frame(lapply(short.frame,function(x) rep(x,short.frame$Freq)))

head(long)
tail(long)

var1<-c(1,1,3,4,3,6,1)
var2<-c(2,2,2,4,2,1,2)
var3<-c(3,2,1,2,1,2,3)
var4<-c(1,1,1,1,1,5,2)
dups<- data.frame(var1,var2,var3,var4)
dups

#row 5 is an exact duplicate of row 3
unique(dups)


dups[duplicated(dups),]


#add margins to a dataframe
name<-c("Jane.Smith","Robert.jones","Dick.Rogers","Williams.Edwards","Janet.Jones")


spring<-c(14,17,12,15,11)
summer<-c(18,18,16,14,17)
autumn<-c(11,10,9,11,11)
winter<-c(12,13,14,10,16)
frame<-data.frame(name,spring,summer,autumn,winter)
frame

people<-rowMeans(frame[,2:5])
people<-people-mean(people)
(new.frame<-cbind(frame,people))


new.row<-new.frame[1,]

new.row[1]<-"seasonal effect"

(seasons<-colMeans(frame[,2:5]))

(seasons<-seasons-mean(seasons))

new.row[2:5]<-seasons

new.row[6]<-0
(new.frame<-rbind(new.frame,new.row))

(gm<-mean(unlist(new.frame[1:5,2:5])))

(gm<-rep(gm,4))


(new.frame[1:5,2:5]<-sweep(new.frame[1:5,2:5],2,gm))
new.frame

#calculate the number of "n"= no=not infected cases in the
#bacteria data frame according to trt=treatment levels
library(MASS)
with(bacteria,tapply((y=="n"),trt,sum))

with(mammals,plot(body,brain,log="xy"))


#Fisher iris 4 measurements from 3 species of iris
#150 cases in rows
head(iris)

names(iris)
summary(iris)

table(iris$Species)

w<-iris[[2]]
mean(w)
w<-iris[,2]
mean(w)

with(iris,summary(Petal.Length[51:100]))

head(iris[,c(1,5)])

head(iris[,c(1,5)])

iris[,sapply(iris,is.numeric)]

with(iris,by(iris[,1:4],Species,mean))
