all.equall(sqrt(2)*sqrt(2),2)

? all.equal

#create a matrix x 3 by 3
x<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
x
atttributes(x)
vector<-c(1,2,3,4,4,3,2,1)
vector
class(vector)
attributes(vector)
v<-matrix(vector,nrow=2,byrow=T)
v


#### changes the vector into 4 by 2 matrix
dim(vector)<-c(4,2)


### to name rows and columns in a matrix
rownames(vector)<-rownames(x,do.NULL=F,prefix="Test.")
rownames(vector)<-rownames(vector,do.NULL=F,prefix="Test.")


names=c("A","B","C","D")
rownames(vector)=names
vector

names<-c("rose","lilly")
colnames(vector)<-names
vector

vector[3,]
vector[,2]

### adding rows to matrix
t<-c(1,2,3)

vector<-rbind(vector,t)

### adding columns to matrix
t<-c(1,2,3)

vector<-cbind(vector,t)

#making a diagonal matrix

diag(c(1,2,3))

#practice matrix functions
A=matrix(c(3,2,5,3),nrow=2,byrow=T)
B =matrix(c(1,0,1,1),nrow=2,byrow=T)

A%*%B
#### inverse of A
solve(A)%*%A

###crossprod(A,B)=t(A)%*%B but more efficient

###determinant

det(A)
## nrow
nrow(A)

#ncol

ncol(A)

rowsums(A)

colMeans(A)


### eigenvalues and eigenvectors

eigen(A)


eval<-eigen(A)$values

eval<-eigen(A)
eval$values

eval$vectors


####arrays
array <-1:25

is.matrix(array)
dim(array)<-c(5,5)
is.matrix(array)

A<-letters[1:24]
dim(A)<-c(4,2,3)
A

A[3,2,2]

B<-1:24
dim(B)<-c(4,2,3)
B

A[3,,, drop=F]

A[3,,]

##suppose   we have a 4 by 5 matrix X and want to extract
## elements x[1,3] x[2,2] and x[3,1] and change them to 0's
x<-array(1:20,dim=c(4,5))#generate a 4 by 5 matrix
x
i<-array(c(1:3,3:1) ,dim=c(3,2)) #i is a 3 by 2 index array
i
x[i]

x[i]<-0
x

##### character vectors
a<-"abc"
b<-"123"
as.numeric(a)
as.numeric(b)


colors <-c("red","yellow","blue")
z<-c("red","green",1)
z

length(colors)
nchar(colors)

substr(colors,1,2)
paste(a,b,sep="")
 paste(a,b,sep=",")
paste(colors,"flowers")
paste("several ",colors,"s",sep="")
paste("I like",colors,collapse=",")


#program to solve quadratic equation  ax^2+bx+c=0
#clear workspace
rm(list=ls())

#input
a<-1
b<-4
c<-2
#calculation
root1<-(-b+sqrt(b^2-4*a*c))/(2*a)
root2<-(-b-sqrt(b^2-4*a*c))/(2*a)

#output
show(c(root1,root2))

root1;root2









