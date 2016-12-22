# Example x, fx
x<-c("a","b","c","d","e","f","g","h","i","j")
fx<-0.01*(2^(0:9))   # Note this is unnormalized!

# i is the "current" index
# j is the "proposal" index
# y is the output stream of samples from x

# Initialization / allocation
y <- rep(0, 10000)
i <- sample(1:10,1)

# MCMC (Metropolis-Hastings)
u01 <- runif(length(y))
for (k in 1:length(y)) {
  j <- sample(1:10,1)  # Independence sampler
  alpha <- fx[j] / fx[i]
  if (u01[k] <= alpha) {
    y[k] <- x[j]
    i <- j
  } else {
    y[k] <- x[i]
  }
}

# Compare sample frequencies to true probabilities
SampleFreq <- table(y)/length(y)
df <- data.frame(Value=names(SampleFreq), 
                 Sample.Freq=as.numeric(SampleFreq), 
                 True.Prob=fx/sum(fx))
df





poisson.metro=function(lamda,i,n)
{
  y=seq(n)
  for(k in 1:n)
  {
    u1=runif(1)
    j =if(u1<.5)
      ifelse(i==0,i,i-1) else i+1
    r =switch(i+2-j,lamda/j,1,i/lamda)
    u2 =runif(1)
    new=if(r>=1)j else
    {if(u2<r)j else i}
    i=new
    y[k]=i
  }
  return(y)
}


poisson.metro(lamda=5,i=2,n=20)


seq(10)
?switch