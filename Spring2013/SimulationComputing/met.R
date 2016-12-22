###### R code and output for hw3
###### Stat 494 -- computational statistics, fall 2008

#### Problem 1)
#### Exercise 7.2 on page 213
#### Target distribution: 0.7*N(7,0.5^2)+0.3*N(10,0.5^2)
f <- function(x) {
  0.7*dnorm(x, mean=7, sd=0.5) + 0.3*dnorm(x, mean=10, sd=0.5);
}
num.its = 10000         # Number of iterations

### Part (a)
## Proposal distribution: N(x^(t), 0.01^2)
sdnum <- 0.01

# starting value: x^(0) = 0
set.seed(3)             # Set random seed
x <- rep(0, num.its)    # MCMC output: vector of u realizations
x[1] <- 0               # Starting value
# This is the code for the Metropolis Hastings algorithm, random walk chain
j <- num.its - 1        # counting acceptance
for (i in 1:(num.its-1)) {
   # Generate proposal (random walk)
   x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);

   # Compute Metropolis-Hastings ratio
   R <- f(x[i+1])/f(x[i]);

   # Reject or accept proposal
   if (R<1) {
      if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
   }
}
x.rw.1 <- x             # Save the output to examine later
cat("\nAcceptance Ratio:", round(j/num.its*100,2),"%\n")
#Acceptance Ratio:  95.06 %

# starting value: x^(0) = 7
set.seed(3)             # Set random seed
x <- rep(0, num.its)    # MCMC output: vector of u realizations
x[1] <- 7               # Starting value
# This is the code for the Metropolis Hastings algorithm, random walk chain
j <- num.its - 1        # counting acceptance
for (i in 1:(num.its-1)) {
   # Generate proposal (random walk)
   x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);

   # Compute Metropolis-Hastings ratio
   R <- f(x[i+1])/f(x[i]);

   # Reject or accept proposal
   if (R<1) {
      if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
   }
}
x.rw.2 <- x             # Save the output to examine later
cat("\nAcceptance Ratio:", round(j/num.its*100,2),"%\n")
#Acceptance Ratio:  99.3 %

# starting value: x^(0) = 15
set.seed(3)             # Set random seed
x <- rep(0, num.its)    # MCMC output: vector of u realizations
x[1] <- 15              # Starting value
# This is the code for the Metropolis Hastings algorithm, random walk chain
j <- num.its - 1        # counting acceptance
for (i in 1:(num.its-1)) {
   # Generate proposal (random walk)
   x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);

   # Compute Metropolis-Hastings ratio
   R <- f(x[i+1])/f(x[i]);

   # Reject or accept proposal
   if (R<1) {
      if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
   }
}
x.rw.3 <- x             # Save the output to examine later
cat("\nAcceptance Ratio:", round(j/num.its*100,2),"%\n")
#Acceptance Ratio:  96.1 %

# Sample paths
par(mfrow=c(3,1))
plot(x.rw.1,type="l",ylab="x",xlab="Iteration",ylim=c(0,15))
plot(x.rw.2,type="l",ylab="x",xlab="Iteration",ylim=c(0,15))
plot(x.rw.3,type="l",ylab="x",xlab="Iteration",ylim=c(0,15))

# save R graph of sample paths
postscript("samplepath_hw3_7_2a.ps",paper="special",width=5,height=7,horizontal=F)
par(mfrow=c(3,1))
temp <- seq(from=1, to=num.its, by=5)
plot(temp,x.rw.1[temp],type="l",ylab="x",xlab="Iteration", main="Sample Path with x0=0, N(*,0.01^2)", ylim=c(0,15))
plot(temp,x.rw.2[temp],type="l",ylab="x",xlab="Iteration", main="Sample Path with x0=7, N(*,0.01^2)", ylim=c(0,15))
plot(temp,x.rw.3[temp],type="l",ylab="x",xlab="Iteration", main="Sample Path with x0=15, N(*,0.01^2)", ylim=c(0,15))
dev.off()

# Summary statistics
burn.in=1:1000
summary.x1 <- summary(x.rw.1[-burn.in])
summary.x2 <- summary(x.rw.2[-burn.in])
summary.x3 <- summary(x.rw.3[-burn.in])
round(rbind(summary.x1,summary.x2,summary.x3),2)
#            Min. 1st Qu. Median  Mean 3rd Qu.  Max.
#summary.x1  0.84    2.47   4.42  3.93    5.51  6.15
#summary.x2  6.72    7.10   7.37  7.41    7.69  8.27
#summary.x3 11.41   11.87  12.09 12.47   13.04 14.65

# Histograms
par(mfrow=c(3,1))
hist(x.rw.1[-burn.in], xlim=c(0,15), ylim=c(0,1.2), nclass=30, xlab="x", main="Histogram with x0=0", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.2[-burn.in], xlim=c(0,15), ylim=c(0,1.2), nclass=10, xlab="x", main="Histogram with x0=7", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.3[-burn.in], xlim=c(0,15), ylim=c(0,1.2), nclass=20, xlab="x", main="Histogram with x0=15", freq=F)
curve(f, from=0, to=15, add=T)

# save R graph of histograms
postscript("histogram_hw3_7_2a.ps",paper="special",width=5,height=7,horizontal=F)
par(mfrow=c(3,1))
hist(x.rw.1[-burn.in], xlim=c(0,15), ylim=c(0,1.2), nclass=30, xlab="x", main="Histogram with x0=0, N(*,0.01^2)", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.2[-burn.in], xlim=c(0,15), ylim=c(0,1.2), nclass=10, xlab="x", main="Histogram with x0=7, N(*,0.01^2)", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.3[-burn.in], xlim=c(0,15), ylim=c(0,1.2), nclass=20, xlab="x", main="Histogram with x0=15, N(*,0.01^2)", freq=F)
curve(f, from=0, to=15, add=T)
dev.off()

### Part (b)
## Proposal distribution: N(x^(t), sdnum^2)
fchain <- function(sdnum, x0, inum) {  
  # function to generate M-H chain using proposal N(x^(t), sdnum^2)
  # starting at x0, length = inum
  # return vector "x" and acceptance ratio "acratio"
  num.its <- inum;
  x <- rep(0, num.its);    # MCMC output: vector of u realizations
  x[1] <- x0;              # Starting value
  j <- num.its - 1;        # counting acceptance
  for (i in 1:(num.its-1)) {
     # Generate proposal (random walk)
     x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);
     # Compute Metropolis-Hastings ratio
     R <- f(x[i+1])/f(x[i]);
     # Reject or accept proposal
     if (R<1) {
        if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
     }
  }
  list(x=x, acratio=j/num.its);
}

# use cusum plot to choose proposal distribution (not required)
sdvec <- c(1,2,3,5)
x0 <- 0
inum <- 10000
burn.in <- 1:1000
par(mfrow=c(length(sdvec),1))
for(i in 1:length(sdvec)) {
  sdnum<-sdvec[i];
  temp<-fchain(sdnum,x0,inum);
  cat("\nsd=",sdnum," Acceptance Ratio:", round(temp$acratio*100,2),"%\n"); 
  x<-temp$x;
  temp <- x[-burn.in];
  ntemp <- length(temp);
  mtemp <- mean(temp);
  temp <- cumsum(temp-mtemp);
  plot(temp, type="l",xlab=sdnum);
}
#sd= 1  Acceptance Ratio: 51.83 %
#sd= 2  Acceptance Ratio: 37.39 %
#sd= 3  Acceptance Ratio: 30.7 %
#sd= 5  Acceptance Ratio: 20.99 %

# use actocorrelation function to choose proposal distribution (not required)
sdvec <- c(0.01,0.5,1,2,3,5)
par(mfrow=c(3,2))
for(i in 1:length(sdvec)) {
  sdnum<-sdvec[i];
  x<-fchain(sdnum,x0,inum)$x;
  acf(x, lag.max=50, type="correlation",xlab=sdnum);
}

## repeat (a) with proposal distribution: N(x^(t), 2^2)
sdnum <- 2

# starting value: x^(0) = 0
set.seed(3)             # Set random seed
x <- rep(0, num.its)    # MCMC output: vector of u realizations
x[1] <- 0               # Starting value
# This is the code for the Metropolis Hastings algorithm, random walk chain
j <- num.its - 1        # counting acceptance
for (i in 1:(num.its-1)) {
   # Generate proposal (random walk)
   x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);

   # Compute Metropolis-Hastings ratio
   R <- f(x[i+1])/f(x[i]);

   # Reject or accept proposal
   if (R<1) {
      if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
   }
}
x.rw.1 <- x             # Save the output to examine later
cat("\nAcceptance Ratio:", round(j/num.its*100,2),"%\n")
#Acceptance Ratio:  38.39 %

# starting value: x^(0) = 7
set.seed(3)             # Set random seed
x <- rep(0, num.its)    # MCMC output: vector of u realizations
x[1] <- 7               # Starting value
# This is the code for the Metropolis Hastings algorithm, random walk chain
j <- num.its - 1        # counting acceptance
for (i in 1:(num.its-1)) {
   # Generate proposal (random walk)
   x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);

   # Compute Metropolis-Hastings ratio
   R <- f(x[i+1])/f(x[i]);

   # Reject or accept proposal
   if (R<1) {
      if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
   }
}
x.rw.2 <- x             # Save the output to examine later
cat("\nAcceptance Ratio:", round(j/num.its*100,2),"%\n")
#Acceptance Ratio:  38.83 %

# starting value: x^(0) = 15
set.seed(3)             # Set random seed
x <- rep(0, num.its)    # MCMC output: vector of u realizations
x[1] <- 15              # Starting value
# This is the code for the Metropolis Hastings algorithm, random walk chain
j <- num.its - 1        # counting acceptance
for (i in 1:(num.its-1)) {
   # Generate proposal (random walk)
   x[i+1] <- x[i] + rnorm(1, mean=0, sd=sdnum);

   # Compute Metropolis-Hastings ratio
   R <- f(x[i+1])/f(x[i]);

   # Reject or accept proposal
   if (R<1) {
      if(rbinom(1,1,R)==0)  { x[i+1]<-x[i]; j<-j-1; }
   }
}
x.rw.3 <- x             # Save the output to examine later
cat("\nAcceptance Ratio:", round(j/num.its*100,2),"%\n")
#Acceptance Ratio:  38.41 %

# Sample paths
par(mfrow=c(3,1))
plot(x.rw.1,type="l",ylab="x",xlab="Iteration",ylim=c(0,15))
plot(x.rw.2,type="l",ylab="x",xlab="Iteration",ylim=c(0,15))
plot(x.rw.3,type="l",ylab="x",xlab="Iteration",ylim=c(0,15))

# save R graph of sample paths
postscript("samplepath_hw3_7_2b.ps",paper="special",width=5,height=7,horizontal=F)
par(mfrow=c(3,1))
temp <- seq(from=1, to=num.its, by=5)
plot(temp,x.rw.1[temp],type="l",ylab="x",xlab="Iteration", main="Sample Path with x0=0, N(*,2^2)", ylim=c(0,15))
plot(temp,x.rw.2[temp],type="l",ylab="x",xlab="Iteration", main="Sample Path with x0=7, N(*,2^2)", ylim=c(0,15))
plot(temp,x.rw.3[temp],type="l",ylab="x",xlab="Iteration", main="Sample Path with x0=15, N(*,2^2)", ylim=c(0,15))
dev.off()

# Summary statistics
burn.in=1:1000
summary.x1 <- summary(x.rw.1[-burn.in])
summary.x2 <- summary(x.rw.2[-burn.in])
summary.x3 <- summary(x.rw.3[-burn.in])
round(rbind(summary.x1,summary.x2,summary.x3),2)
#           Min. 1st Qu. Median Mean 3rd Qu.  Max.
#summary.x1 4.91    6.79   7.25 7.83    9.40 11.69
#summary.x2 4.85    6.82   7.27 7.87    9.49 11.69
#summary.x3 4.86    6.81   7.26 7.85    9.40 11.69 

# Histograms
par(mfrow=c(3,1))
hist(x.rw.1[-burn.in], xlim=c(0,15), ylim=c(0,0.8), nclass=30, xlab="x", main="Histogram with x0=0", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.2[-burn.in], xlim=c(0,15), ylim=c(0,0.8), nclass=30, xlab="x", main="Histogram with x0=7", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.3[-burn.in], xlim=c(0,15), ylim=c(0,0.8), nclass=30, xlab="x", main="Histogram with x0=15", freq=F)
curve(f, from=0, to=15, add=T)

# save R graph of histograms
postscript("histogram_hw3_7_2b.ps",paper="special",width=5,height=7,horizontal=F)
par(mfrow=c(3,1))
hist(x.rw.1[-burn.in], xlim=c(0,15), ylim=c(0,0.8), nclass=30, xlab="x", main="Histogram with x0=0, N(*,2^2)", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.2[-burn.in], xlim=c(0,15), ylim=c(0,0.8), nclass=30, xlab="x", main="Histogram with x0=7, N(*,2^2)", freq=F)
curve(f, from=0, to=15, add=T)
hist(x.rw.3[-burn.in], xlim=c(0,15), ylim=c(0,0.8), nclass=30, xlab="x", main="Histogram with x0=15, N(*,2^2)", freq=F)
curve(f, from=0, to=15, add=T)
dev.off()


#### Problem 2)
#### Example 7.4 (Stream ecology monitoring) on pages 196~197
alpha <- c(3,1,1)
lambda <- 50
num.its <- 10000

### Part (a)
N <- rep(0, num.its)          # iterations of N
P <- rep(0, num.its*3)        # iterations of (P1, P2, P3)
dim(P) <- c(num.its, 3)
Y <- rep(0, num.its*3)        # iterations of (Y1, Y2, Y3)
dim(Y) <- c(num.its, 3)

# sample initial values
set.seed(3)
N[1] <- rpois(1, lambda=lambda)
library(MCMCpack)             # need to install packages "MCMCpack" and "coda" first
P[1,] <- rdirichlet(1, alpha=alpha)
Y[1,] <- rmultinom(1, size=N[1], prob=P[1,])

# Gibbs iterations begin
for (i in 2:num.its) {
  Y[i,1] <- rbinom(1, size=N[i-1]-Y[i-1,2], prob=P[i-1,1]/(1-P[i-1,2]));
  Y[i,2] <- rbinom(1, size=N[i-1]-Y[i,1], prob=P[i-1,2]/(1-P[i-1,1]));
  P[i,1] <- (1-P[i-1,2])*rbeta(1, shape1=Y[i,1]+alpha[1], shape2=N[i-1]-Y[i,1]-Y[i,2]+alpha[3]);
  P[i,2] <- (1-P[i,1])*rbeta(1, shape1=Y[i,2]+alpha[2], shape2=N[i-1]-Y[i,1]-Y[i,2]+alpha[3]);
  P[i,3] <- 1 - P[i,1] - P[i,2];
  Y[i,3] <- rpois(1, lambda=lambda*P[i,3]);
  N[i] <- sum(Y[i,]);
}

### Part (b)
# Sample paths
par(mfrow=c(5,1))
plot(N,type="l",ylab="N",xlab="Iteration")
plot(Y[,1],type="l",ylab="Y1",xlab="Iteration")
plot(Y[,2],type="l",ylab="Y2",xlab="Iteration")
plot(P[,1],type="l",ylab="P1",xlab="Iteration")
plot(P[,2],type="l",ylab="P2",xlab="Iteration")

# save R graph of sample paths
postscript("samplepath_hw3_p2b.ps",paper="special",width=5,height=8,horizontal=F)
par(mfrow=c(5,1))
temp <- seq(from=1, to=num.its, by=10)
plot(temp,N[temp],type="l",ylab="N",xlab="Iteration", main="Sample Path of N")
plot(temp,Y[temp,1],type="l",ylab="Y1",xlab="Iteration", main="Sample Path of Y1")
plot(temp,Y[temp,2],type="l",ylab="Y2",xlab="Iteration", main="Sample Path of Y2")
plot(temp,P[temp,1],type="l",ylab="P1",xlab="Iteration", main="Sample Path of P1")
plot(temp,P[temp,2],type="l",ylab="P2",xlab="Iteration", main="Sample Path of P2")
dev.off()

### Part (c)
# Summary statistics
burn.in=1:1000
summary.N <- summary(N[-burn.in])
summary.Y1 <- summary(Y[-burn.in,1])
summary.Y2 <- summary(Y[-burn.in,2])
summary.P1 <- summary(P[-burn.in,1])
summary.P2 <- summary(P[-burn.in,2])
round(rbind(summary.N,summary.Y1,summary.Y2,summary.P1,summary.P2),2)
#            Min. 1st Qu. Median  Mean 3rd Qu.  Max.
#summary.N  28.00   45.00  50.00 49.96   55.00 76.00
#summary.Y1  0.00   22.00  30.00 30.24   38.00 71.00
#summary.Y2  0.00    3.00   8.00  9.56   14.00 47.00
#summary.P1  0.02    0.47   0.63  0.60    0.74  0.99
#summary.P2  0.00    0.07   0.16  0.19    0.27  0.88
