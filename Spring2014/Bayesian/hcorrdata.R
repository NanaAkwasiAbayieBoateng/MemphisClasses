# Data Augmentation and Gibbs Sampling for Binary Data
# using logit link function
# part  with non informative prior

##########libraries ##########
#######################################################################  
#libraries
library(R2WinBUGS)
library("R2WinBUGS")
library(coda)
# Libraries
library(MASS)
library(tseries)     # for time series 
library(boot)        # for bootstrapping 
library(vars)        # VAR models
library(MSBVAR)      # for Bayesian VAR
# library
library(BRugs)       # OpenBugs
library(denstrip)    # For density strips    # install.packages(denstrip)
set.seed(1)
library(forecast)     # for  forecast
library(R2OpenBUGS)

library(msm)      # For truncted normal
library(tmvtnorm)		# Multivariate trunctaed normal
library(mvtnorm)# Multivariate normal
#######################################################################   
#######################################################################  
########## Data Reading and Estimation##########
##############################################################
setwd("C:/Users/nboateng/Documents/spring2014/BAYESIAN/highlycorrelateddata")  #Change the directory

####################################################################### 

# Simulate data
set.seed(250)
n<-1000  				# Sample Size
x1<-sample(rnorm(1000,0,1),n,replace=T)# Covariates
x2<-sample(rnorm(1000,0,1),n,replace=T)
x3<-sample(rnorm(1000,0,1),n,replace=T)
x4<-sample(rnorm(1000,0,1),n,replace=T)
x5<-sample(rnorm(1000,0,1),n,replace=T)
x6<-sample(rnorm(1000,0,1),n,replace=T)
x7<-sample(rnorm(1000,0,1),n,replace=T)
x8<-sample(rnorm(1000,0,1),n,replace=T)
x9<-sample(rnorm(1000,0,1),n,replace=T)
x10<-sample(rnorm(1000,0,1),n,replace=T)
x11<-sample(rnorm(1000,0,1),n,replace=T)
x12<-sample(rnorm(1000,0,1),n,replace=T)
x13<-sample(rnorm(1000,0,1),n,replace=T)
x14<-sample(rnorm(1000,0,1),n,replace=T)
x15<-sample(rnorm(1000,0,1),n,replace=T)

X<-matrix(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15), ncol=15)   	# Design matrix
tbeta<-c(-.75,.75)            	# True beta
tbeta=rep(tbeta,15)
tbeta=tbeta[1:15]
p<-pnorm(X%*%tbeta) 	        	# Prob. Vector
y<-matrix(rbinom(n,1,p)	,n)			# Observations
k<-dim(X)[2]						# Number of parms
n1<-sum(y)                  		# Number of successes
n0<-n-n1
# Initial Values
fit<-glm(y~X,family=binomial(link=probit))

beta=fit$coefficient[-1]

length(beta)


data=list("n","X","k")

parameters=c("beta")

inits=function() {list(beta=fit$coefficient[-1],eta=0,phi=0.35,sigma=1)}


hcd.sim <- bugs (data, inits, parameters,"highcorrdata.odc", n.chains=3, n.iter=1000,codaPkg=TRUE,debug=TRUE)
hcd.coda = read.bugs(hcd.sim)

summary(hcd.coda)

xyplot(hcd.coda)

acfplot(hcd.coda)

densityplot(hcd.coda,col="black")


