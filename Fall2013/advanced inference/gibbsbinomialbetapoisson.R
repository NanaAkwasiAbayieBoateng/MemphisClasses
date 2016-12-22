
#gibbs(100,2,4,20,16)
#dbeta(x, shape1, shape2, ncp = 0, log = FALSE)
#pbeta(q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
#qbeta(p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
#rbeta(n, shape1, shape2, ncp = 0)

#dbinom(x, size, prob, log = FALSE)
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
#rbinom(n, size, prob)

#dpois(x, lambda, log = FALSE)
#ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
#qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
#rpois(n, lambda)



n=1000
alpha=2
beta=4

lambda=16
x=3
p=c()
N=c()

      
        
        
      
       p[1]= 0.5
       N[1]<-20
         
        
        for (i in 2:n) {
                
   #x=rep(3,100000)
if (N[i-1]>=x[i]) {
    p[i]<-rbeta(1, x+alpha, beta+N[i-1]-x, ncp = 0)               
    N[i]<-rpois(1, lambda*(1-p[i]))
             
     
}else{
N[i-1]=N[i]

}                    
                  

                 
        }
               
 v=  cbind(p,N)
par(mfrow=c(2,2))
plot(v[,1],v[,2])

hist(v[,1])
hist(v[,2])
      




n=30000
alpha=2
beta=4

lambda=16
x=3
p=c()
N=c()

      
        
        
      
       p[1]= 0.5
       N[1]<-20
         
        
        for (i in 2:n) {
                
 
    p[i]<-rbeta(1, x+alpha, beta+N[i-1]-x, ncp = 0)               
    N[i]<-rpois(1, lambda*(1-p[i]))
             
     
}                   
                  

                 
        
               
 v=  cbind(p,N)
p=v[,1]
N=v[,2]
par(mfrow=c(3,2))

# burnin and thinning
burnin = 1000
thinning = 25

# Gibbs sample
sim = cbind(p[seq(from=burnin,to=n,by=thinning)],N[seq(from=burnin,to=n,by=thinning)])

# A diagnosis tool to assess the convergence of the sampler
par(mfrow=c(3,2))
plot(ts(sim[,1]),main="p")
plot(ts(sim[,2]),main="N")


hist(p)
lines(density(p))

hist(N)
   
plot(sim,col=1:1000)

plot(sim,type="l")











