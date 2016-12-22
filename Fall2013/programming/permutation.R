# SET RANDOM NUMBER GENERATOR SEED
set.seed(2001)
 
# SIMULATE NORMAL DATA
PA <- rnorm(15, 4.5, 2)
PP <- rnorm(15, 7.5, 2)
 
# PLOT HISTOGRAMS
mf <- par(mfrow=c(1,2))
hist(PA, breaks=5)
hist(PP, breaks=5)
par(mf)

# POOL DATA
pooledData <- c(PA, PP)
# SET THE NUMBER OF ITERATIONS
nIter <- 9999
# SET UP A CONTAINER FOR PERMUTED DIFFERENCES. ADD IN A SLOT FOR THE OBSERVED VALUE
meanDiff <- numeric(nIter+1)
# CALCULATE THE OBSERVED MEAN DIFFERENCE
meanDiff[1] <- mean(PP) - mean(PA)
# RUN THE ITERATION IN A FOR() LOOP
for(i in 2:length(meanDiff)){ # start from 2 to avoid overwriting the observed difference
 index <- sample(1:30, size=15, replace=F) # Sample numbers 1-30 15 times and store in an index
 PAperm <- pooledData[index] # Assign the sampled values to PA
 PPperm <- pooledData[-index] # Assign everything else to PP
 meanDiff[i] <- mean(PPperm) - mean(PAperm) # Calcualte and store the difference in means
}
 
# PLOT HISTORGRAM OF DIFFERENCES IN MEANS
hist(meanDiff, xlab='Difference in PP and PA means', prob=T, main='')
# ADD IN A LINE FOR OUR OBSERVED VALUE
abline(v=meanDiff[1], lty=2, col='red')
 
# CALCULATE THE P-VALUE. USE THE ABSOLUTE VALUE FOR A TWO-TAILED TEST
mean(abs(meanDiff) >= abs(meanDiff[1]))


# SIMULATE LOG-NORMAL DATA
PAlnorm <- rlnorm(n=15, mean=log(4.5), sd=log(2))
PPlnorm <- rlnorm(n=15, mean=log(7.5), sd=log(2))
 
mf <- par(mfrow=c(1,2))
hist(PAlnorm);hist(PPlnorm)
par(mf)
 
# t-Test
t.test(PAlnorm, PPlnorm)
 
# POOL DATA
pooledData <- c(PAlnorm, PPlnorm)
# SET THE NUMBER OF ITERATIONS
nIter <- 9999
# SET UP A CONTAINER FOR PERMUTED DIFFERENCES. ADD IN A SLOT FOR THE OBSERVED VALUE
meanDiff <- numeric(nIter+1)
# CALCULATE THE OBSERVED MEAN DIFFERENCE
meanDiff[1] <- mean(PPlnorm) - mean(PAlnorm)
# RUN THE ITERATION IN A FOR() LOOP
for(i in 2:length(meanDiff)){ # start from 2 to avoid overwriting the observed difference
 index <- sample(1:30, size=15, replace=F) # Sample numbers 1-30 15 times and store in an index
 PAperm <- pooledData[index] # Assign the sampled values to PA
 PPperm <- pooledData[-index] # Assign everything else to PP
 meanDiff[i] <- mean(PPperm) - mean(PAperm) # Calcualte and store the difference in means
}
 
# PLOT HISTORGRAM OF DIFFERENCES IN MEANS
hist(meanDiff, xlab='Difference in PP and PA means', prob=T, main='')
# ADD IN A LINE FOR OUR OBSERVED VALUE
abline(v=meanDiff[1], lty=2, col='red')
 
mean(abs(meanDiff) >= abs(meanDiff[1]))