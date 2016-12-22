
poisson.nr<-function(y,x,alpha0,beta0,tol=0.00001){

theta<-c(alpha0,beta0)
score<-c(3,3)
hess<-matrix(rep(NA,4),nrow=2)

#for(i in 1:10){
 while(max(abs((score)))>tol){
score[1]<-sum(y/(theta[1]+theta[2]*x))-length(y)
score[2]<-sum(y*x/(theta[1]+theta[2]*x))-sum(x)
hess[1,1]<--sum(y/(theta[1]+theta[2]*x)^2)
hess[1,2]<--sum(y*x/(theta[1]+theta[2]*x)^2)
hess[2,1]<-hess[1,2]
hess[2,2]<--sum(y*x^2/(theta[1]+theta[2]*x)^2)
newtheta<-theta-solve(hess)%*%score
theta<-newtheta
s<-max(abs((score)))
cat(s,"")
}
out.list<-list(theta,score,hess)
return(out.list)

}

x<-abs(rpois(10,2))
lambda<-1+30*x

y<-NULL
 for(i in 1:10){

y[i]<-rpois(1,lambda[i])
}
poisson.nr(y,x,.9,29)
