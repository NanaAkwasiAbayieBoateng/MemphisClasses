###############################
# MVN Means Gibbs.r           #
# Gibbs Sampler for MVN Model #
# March 5, 2013               #
###############################
library(mvtnorm)  # rmvnorm function
library(MCMCpack) # rwish and riwish function

################# 
# Generate Data #
#################
set.seed(030513)
n<-1000
theta<-c(-1,2)
Sigma<-matrix(c(1,.5,.5,2),2,2)
Y<-rmvnorm(n,theta,Sigma)

##########
# Priors #
##########
theta0<-c(0,0)
Sigma0<-S0<-diag(2)  # S0 is IW prior scale matrix for Simga^{-1}. Or, equivalently, T0=S0^{-1} is the Wishart scale for Tau = Sigma^{-1}
nu0<-3

#########
# Inits #
#########
theta<-c(0,0)
Tau<-diag(2)  # Sigma^{-1}

#########
# Store #
#########
nsim<-1000
Theta<-matrix(0,nsim,2);  Theta[1,]<-theta
Sigmas<-matrix(0,nsim,4); Sigmas[1,]<-c(diag(2))

#########
# Gibbs #
#########
for (i in 2: nsim){
  # Update theta
  v<-solve(solve(Sigma0)+n*Tau)
  m<-v%*%(solve(Sigma0)%*%theta0+Tau%*%apply(Y,2,sum))
  Theta[i,]<-theta<-c(rmvnorm(1,m,v))
  
  # Update Sigma
  tmp<-t(Y)-theta
  Tau<-rwish(nu0+n,solve(S0+tmp%*%t(tmp)))  # Recall T0=S0^{-1} is prior Wishart scale matrix; hence we have T0^{-1}=S0 in the posterior
  Sigmas[i,]<-c(solve(Tau))                 # Or Sigmas[i,]<-Sigma<-riwish(nu0+n,S0+tmp%*%t(tmp)); Tau<-solve(Sigma)
  if (i%%10==0) print(i)
}

# Posterior Means
apply(Theta[101:nsim,],2,mean)
apply(Sigmas[101:nsim,],2,mean)
