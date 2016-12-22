#2a
(y<-c(1,2,3,4,5,6,7,8,7,6,5,4,3,2,1))
y=c(1:8,7:1)

#2b

(b<-c(rep(1:5,1:5)))

#2c
y<-c(rep(1,9))
y1=matrix(y,nrow=3)

y1[1,1]=0
y1[2,2]=0
y1[3,3]=0
y1

matrix(rep(1,9),nrow=3)-diag(c(1,1,1))

#4
y=seq(1:100)
y<-y[(!y%%2==0) & (!y%%3 ==0) & (!y%%7 ==0) ]
v<-y[!(y%%2==0 | y%%3 ==0 | y%%7 ==0) ]


#5
queue <- c("Steve", "Russell", "Alison", "Liam")

queue<-c(queue,"Barry")

queue[length(queue)+1)]<-"Barry"

queue<-c(queue[-1])

queue<-c("pam",queue)


queue<-queue[-length(queue)]

queue <- c("Steve", "Russell", "Alison", "Liam")
queue<-c(queue!=="Alison")
queue

queue<-queue[-which(queue=="Alison")]

