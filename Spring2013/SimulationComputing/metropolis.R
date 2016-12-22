n=1000

y<-0.683
x<-y
a<-56
b<-34


for (i in  2:n)
{

u<-runif(1)


px<-((2+x)^125)*((1-x)^38)*(x^34)
py<-((2+y)^125)*((1-y)^38)*(y^34)

rho=(py*dbeta(y,a,b))/(px[i-1]*dbeta(x[i-1],a,b))

one=rep(1,n)
 r1=matrix(c(one,rho))

#y<-dbeta(y,a,b)





}

