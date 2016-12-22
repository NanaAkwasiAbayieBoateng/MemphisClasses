# Data Augmentation and Gibbs Sampling for Binary Data
# using logit link function
# part  with non informative prior

library(msm)      # For truncted normal
library(tmvtnorm)		# Multivariate trunctaed normal
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
library(mvtnorm)# Multivariate normal



# Simulate data
set.seed(250)
n<-1000  				# Sample Size
x<-sample(0:4,n,replace=T)		# Covariates

X<-matrix(c(rep(1,n),x), ncol=2)   	# Design matrix
tbeta<-c(-.75,.75)            	# True beta

p<-pnorm(X%*%tbeta) 	        	# Prob. Vector
y<-matrix(rbinom(n,1,p)	,n)			# Observations
k<-2 						# Number of parms
n1<-sum(y)                  		# Number of successes
n0<-n-n1
# Initial Values
fit<-glm(y~x,family=binomial(link=probit))

# Read and prepare data   for TEs
#DrK.file<-"C:/Users/dfofana/Desktop/Dr. Kemme/PaperKemme/czech_pol_hung.txt"
#DrK.data<-read.table(DrK.file,sep="\t",as.is=T,header=T,quote="",comment.char="")
#data.dir<-"C:/Users/dfofana/Desktop/Dr. Kemme/PaperKemme/"
##############################################################
##############################################################
#setwd("F:/spring2014/BAYESIAN/")  #Change the directory

setwd("C:/Users/nboateng/Documents/spring2014/")  #Change the directory
#######################################################################  
#########Resampling  
#Czech_DrK.data<-DrK.data[DrK.data$X=="CR",]    
#Czech_DrK.data<-DrK.data[DrK.data$X=="PO",] 
#Czech_DrK.data<-DrK.data[DrK.data$X=="HU",]
#head( Czech_DrK.data)
#n<-dim(Czech_DrK.data)[1]
#y<-sample(rnorm(1000,0,1), 1000, replace = TRUE)
#z<-sample(rnorm(1000,0,1), 1000, replace = TRUE)
#r<-sample(rnorm(1000,0,1), 1000, replace = TRUE)
#e<-sample(rnorm(1000,0,1), 1000, replace = TRUE)
#n<-length(y)

#Analysisdata<-list("n","y","z","r","e")
Analysisdata<-X
X[,2]
#parameters<-c("a0y","a1yy","a1yp","a1yr","a1ye","a0p","a1py","a1pp","a1pr","a1pe","a0r","a1ry",
             # "a1rp","a1rr","a1re","a0e","a1ey","a1ep","a1er","a1ee","sigma")

parameters<-c("beta[1]","beta[2]")
#inits<-list(list(a0y = -6483.571, a1yy =-6483.571,a1yp = -6483.571, a1yr = -6483.571, a1ye=0.1,
# a0p=-6483.571, a1py=-6483.571,a1pp=-6483.571,a1pr=-6483.571, a1pe=0.1,
# a0r=-6483.571, a1ry=-6483.571,a1rp=-6483,a1rr=-6483.571,a1re=0.1,
# a0e=-6483.571,a1ey=-6483.571, a1ep=-6483,a1er=-6483,a1ee=0.1, sigma = 10)) 
####################################################################                                                
#data_czech_pol_hung<-dget("czech_pol_hungSAS.txt")
inits<-NULL
#network.fit<-bugs#(Analysisdata,inits,parameters,"albertchib1.txt",      #Change the directory
                  #n.chains=1,n.burnin=1000, n.iter=2000, save.history=TRUE,n.thin=1, codaPkg=TRUE,
                  
                 # DIC=TRUE,debug=TRUE,program="OpenBUGS",bugs.directory="C:/Program Files (x86)/OpenBUGS")


network.fit<-bugs(data=Analysisdata,inits,parameters.to.save=c("beta[1]","beta[2]"),model="C:/Users/nboateng/Documents/spring2014/BAYESIAN/albertchib1.txt",      #Change the directory
                  n.chains=1,n.burnin=1000, n.iter=2000, save.history=TRUE,n.thin=1, codaPkg=TRUE,
                  DIC=TRUE,debug=TRUE
                  , over.relax = TRUE,summary.only=TRUE)

network.fit
summary(network.fit)



#update.packages(checkBuilt=TRUE, ask=FALSE)
#install.packages("R2WinBUGS")
#install.packages("R2openBUGS")
#install.packages("coda")
#install.packages("MASS")
#install.packages("tseries")
#install.packages("boot")
#install.packages("vars")
#install.packages("MSBVAR")
#install.packages("BRugs")
#install.packages("denstrip")
#install.packages("forecast")
#install.packages("R2openBUGS")

  

