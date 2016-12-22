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
#######################################################################   
#######################################################################  
########## Data Reading and Estimation##########
#######################################################################  
# Read and prepare data   for TEs
#DrK.file<-"C:/Users/dfofana/Desktop/Dr. Kemme/PaperKemme/czech_pol_hung.txt"
#DrK.data<-read.table(DrK.file,sep="\t",as.is=T,header=T,quote="",comment.char="")
#data.dir<-"C:/Users/dfofana/Desktop/Dr. Kemme/PaperKemme/"
##############################################################
##############################################################
setwd("C:/Users/nboateng/Documents/spring2014/BAYESIAN/")  #Change the directory
#C:\Users\nboateng\Documents\spring2014
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
#parameters<-c("a0y","a1yy","a1yp","a1yr","a1ye","a0p","a1py","a1pp","a1pr","a1pe","a0r","a1ry",
              #"a1rp","a1rr","a1re","a0e","a1ey","a1ep","a1er","a1ee","sigma")
#inits<-list(list(a0y = -6483.571, a1yy =-6483.571,a1yp = -6483.571, a1yr = -6483.571, a1ye=0.1,
# a0p=-6483.571, a1py=-6483.571,a1pp=-6483.571,a1pr=-6483.571, a1pe=0.1,
# a0r=-6483.571, a1ry=-6483.571,a1rp=-6483,a1rr=-6483.571,a1re=0.1,
# a0e=-6483.571,a1ey=-6483.571, a1ep=-6483,a1er=-6483,a1ee=0.1, sigma = 10)) 
####################################################################                                                
#data_czech_pol_hung<-dget("czech_pol_hungSAS.txt")
#inits<-NULL
#network.fit<-bugs(data=Analysisdata,inits,parameters,model="C:/Users/nboateng/Documents/spring2014/BAYESIAN/MyProgramTEs_Demba.txt",      #Change the directory
                 # n.chains=1,n.burnin=1000, n.iter=2000, save.history=TRUE,n.thin=1, codaPkg=TRUE,
                 # DIC=TRUE,debug=TRUE
                 # , over.relax = TRUE,summary.only=TRUE)

#network.fit
#summary(network.fit)



data1=list(y = 7, n = 50, alpha = 0.5, beta = 0.5)
parameters=c("p")

inits=list(p = 0.1)
t1=bugs(data=data1,inits,parameters,model="C:/Users/nboateng/Documents/spring2014/BAYESIAN/tutorial1.txt",      #Change the directory
        n.chains=1,n.burnin=1000, n.iter=2000, save.history=TRUE,n.thin=1, codaPkg=TRUE,
        DIC=TRUE,debug=TRUE
        , over.relax = TRUE,summary.only=TRUE)

summary(t1)
t1
