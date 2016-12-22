 y.fun<-function (x)  
{y<-(log(x))^2-x*exp(-x^3) 
}
 root.fun<- function () 
{      x<-seq(0.2,2,0.001) 
        y<-y.fun(x) 
        win.graph() 
        plot(x,y,type="l") 
        abline(h=0) 
        r1 <- uniroot(y.fun,lower=0.2,upper=1)$root 
        r2 <- uniroot(y.fun,lower=1,upper=2)$root 
        cat("Root1 : ", round(r1,4), "  ","\n","Root2 : ", round(r2,4),"\n") 
}
 root.fun()

Make<-c("Honda","Chevrolet","Ford","Eagle","Volkswagen","Buick","Mitsbusihi","Dodge","Chrysler","Acura") 
 
Model<-c("Civic","Beretta","Escort","Summit","Jetta","Le Sabre","Galant","Grand Caravan","New Yorker","Legend") 
 

Cylinder<-c(rep("V4",5),"V6","V4",rep("V6",3)) 
 
 Weight<-c(2170,2655,2345,2560,2330,3325,2745,3735,3450,3265) 
Mileage<-c(33,26,33,33,26,23,25,18,22,20) 
 Type<-c("Sporty","Compact",rep("Small",3),"Large","Compact","Van",rep("Medium",2))
 Car<-data.frame(Make,Model,Cylinder,Weight,Mileage,Type) 
Car
Car$Mileage 
mean(Car$Mileage)    
min(Car$Weight) 

 
standardize=function(x){
m=mean(x)
std=sqrt(var(x))
result=(x-m)/std
return(result)
}
standardize=function(x,y){
m=mean(x)
std=sqrt(var(x))
ped=0.5*y
result=(x-m)/std
result1=result+ped
return(result1)
#return(x)
#return(y)
}
standardize(1:6,1:6)
squarecu=function(x){
# inputs a vector x
# outputs the square and cube of x
res1=x^2;res2=x^3
return(list("square"=res1,"cube"=res2))
}

jsum=function(x){
jsum=0
for(i in 1:length(x))
{jsum=jsum+x[i]}
return(jsum)
}
x=c(1,2,3)
 jsum(x)
x1=cumsum(x)
x2=matrix(c(x1),nrow=1)
x2[1,3]l


jsum=function(x){
jsum=0
for(i in 1:length(x))
{jsum=jsum+x[i]}
return(jsum)
}
x=c(1,2,3)
 jsum(x)
x1=mean(cumsum(x))
x1
