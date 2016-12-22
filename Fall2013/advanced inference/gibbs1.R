 # MH Algorithm for simple normal model with 
 #    unknown mean and unknown variance
 # Use symmetric t(3) proposal 
 # True mu = -10, True tau=0.50
 # Oct 18, 2008
 #############################

 set.seed(250)

 ########
 # Data #
 ########

 n<-100					# Sample size
 mu<--10	  			# True value of mu
 sigma2<-2				# True value of sigma^2
 tau<-1/sigma2  			# Tau 0.50
 y<-rnorm(n,mu,sqrt(sigma2))		# Data
 
 a<-.001				# Gamma hyperprior parms for tau
 b<-.001 
 
 ###########
 # Priors  #
 ###########
  mu0<-0
  v0<-10		# Non-informative variance
  prec0<-1/v0
  mtau<-a+n/2		# Posterior location for tau

 #########
 # Inits #
 #########

  mu<-5		
  tau<-3
  nsim<-5000		# Number of iterations

 #########
 # Store #
 #########
  Mu<-Tau<-rep(0,nsim)	# Store Results
  A<-0			# Acceptance Rate	

############
#   MCMC   #
############  
for (j in 1:nsim){
 mus<-mu+.3*rt(1,3)  # t with 3 df as proposal	
			# Note: Symmetric Proposal -- will cancel out of acceptance
			# So this is technically Metropolis not Metrop-Hastings
			# Scaled by 1/3 to improve acceptance rate
 
# Acceptance prob on log scale
 r<- (sum(dnorm(y,mus,1/sqrt(tau),log=T) - dnorm(y,mu,1/sqrt(tau),log=T)) +
	dnorm(mus,mu0,sqrt(v0),log=T) -  dnorm(mu,mu0,sqrt(v0),log=T))

if (log(runif(1))<r) {   
   mu<-mus		# Update mu if accepted
   A<-A+1		# Acceptance rate
  }
  Mu[j]<-mu
  tau<-Tau[j]<-rgamma(1,mtau,b+sum((y-mu)^2)/2)
}

#######################
# Posterior Summaries #
#######################
mean(Mu[1001:nsim])	# Posterior Mean of mu
mean(1/Tau[1001:nsim])	# Posterior mean of sigma^2
Acc<-A/nsim

# Iteration Plots
par(mfrow=c(1,1),font=3)

plot(seq(1001,nsim) ,Mu[1001:nsim],type="l",xlab="Iteration", ylab="mu",
main="Fig. 1: IP for Posterior of Mu",col=2,lty=1)
abline(h=mean(Mu[1001:nsim]))

