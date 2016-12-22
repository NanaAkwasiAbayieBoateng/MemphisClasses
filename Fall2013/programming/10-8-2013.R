#C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/dataframe.txt

data<-read.table("C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/dataframe.txt",header=T,sep="")

summary(data)

data()
 x<-data$Response
x
names(data)
#print numbers
y<-data[[1]]
y
z<-data[["Response"]]
z
attach(data)
Response

detach(data)
Response

colors<-c("red","yellow","blue")
numbers<-c(1,2,3)
colors.and.numbers<-data.frame(colors,numbers,more.numbers=c(4,5,6))
colors.and.numbers

head(data)
tail(data)

#mean of the groups
by(data[,1],data$Treatment,mean)
data



data[1,2]


data[4:7,2]

class(data[1,])

class(data[,1])
class(data[,2])

attach(data)
#sort data by response,ascending order
data[order(Response),]
 


#sort data by response,descending order
data[rev(order(Response)),]

#sort data by Response within each treatment
data[order(Treatment,Response),]





#select only control rows
data[Treatment=="control",]

#select rows withresponse greater than median response
data[Response>median(Response),]

#check which columns are numeric
data[,sapply(data,is.numeric)]


data[,sapply(data,is.factor)]


#check which columns are character variables
data[,sapply(data,is.character)]


#drop rows from data frame
data[-(2:4),]


#drop all rows not preheated
data[!Treatment=="preheated",]

data[-which(Treatment=="preheated"),]

#check for missing values
complete.cases(data)

#you want to keep only a single record say the largest response for each treatment
#first order all the rows in a data frame by Treatment

new<-data[rev(order(Treatment,Response)),]
new

#select the subset that is not duplicated
new[!duplicated(new$Treatment),]


#sort multiple variables in opposite directions
data[order(Treatment,-Response),]


#to reverse the direction of the factor use rank
data[order(-rank(Treatment),Response),]


data[order(rev(rank(Treatment)),Response),]


