gamm<-function (n, a, b) 
{
        mu <- a/b
        sig <- sqrt(a/(b * b))
        vec <- vector("numeric", n)
        x <- a/b
        vec[1] <- x
        for (i in 2:n) {
                can <- rnorm(1, mu, sig)
                aprob <- min(1, (dgamma(can, a, b)/dgamma(x, 
                        a, b))/(dnorm(can, mu, sig)/dnorm(x, 
                        mu, sig)))
                u <- runif(1)
                if (u < aprob) 
                        x <- can
                vec[i] <- x
        }
        vec
}
vec<-gamm(10000,2.3,2.7)
par(mfrow=c(2,1))
plot(ts(vec))
hist(vec,30)




norm<-function (n, alpha) 
{
        vec <- vector("numeric", n)
        x <- 0
        vec[1] <- x
        for (i in 2:n) {
                innov <- runif(1, -alpha, alpha)
                can <- x + innov
                aprob <- min(1, dnorm(can)/dnorm(x))
                u <- runif(1)
                if (u < aprob) 
                        x <- can
                vec[i] <- x
        }
        vec
}
normvec<-norm(10000,1)
par(mfrow=c(2,1))
plot(ts(normvec))
hist(normvec,30)
par(mfrow=c(1,1))







Metropolis <- function(F_sample # distribution we want to sample
                      , F_prop  # proposal distribution 
                      , I=1e5   # iterations
               ){
  y = rep(NA,T)
  y[1] = 0    # starting location for random walk
  accepted = c(1)

  for(t in 2:I)    {
    #y.prop <- rnorm(1, y[t-1], sqrt(sigma2) ) # random walk proposal
    y.prop <- F_prop(y[t-1]) # implementation assumes a random walk. 
                             # discard this input for a fixed proposal distribution

    # We work with the log-likelihoods for numeric stability.
    logR = sum(log(F_sample(y.prop))) -
           sum(log(F_sample(y[t-1])))    

    R = exp(logR)

    u <- runif(1)        ## uniform variable to determine acceptance
    if(u < R){           ## accept the new value
      y[t] = y.prop
      accepted = c(accepted,1)
    }    
    else{
      y[t] = y[t-1]      ## reject the new value
      accepted = c(accepted,0)
    }    
  }
  return(list(y, accepted))
}



set.seed(100)

test = function(x){dnorm(x,-5,1)+dnorm(x,7,3)}

# random walk
response1 <- Metropolis(F_sample = test
                       ,F_prop = function(x){rnorm(1, x, sqrt(0.5) )}
                      ,I=1e5
                       )

y_trace1 = response1[[1]]; accpt_1 = response1[[2]]
mean(accpt_1) # acceptance rate without considering burn-in
# 0.85585   not bad

# looks about how we'd expect
plot(density(y_trace1))
abline(v=-5);abline(v=7) # Highlight the approximate modes of the true distribution



response2 <- Metropolis(F_sample = test ,F_prop = function(x){rnorm(1, -5, sqrt(0.5) )} ,I=1e5 )
y_trace2 = response2[[1]]; accpt_2 = response2[[2]]
mean(accpt_2) # .871, not bad



plot(density(y_trace2))


response2b <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, 7, sqrt(10) )}
                        ,I=1e5
)

plot(density(response2b[[1]]))




response3 <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, -2, sqrt(10) )}
                        ,I=1e5
)
y_trace3 = response3[[1]]; accpt_3 = response3[[2]]
mean(accpt_3) # .3958! 


plot(density(y_trace3))

plot(y_trace3) # we really need to set the variance pretty high to catch 
               # the mode at +7. We're still just barely exploring it





response4 <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, 1, sqrt(10) )}
                        ,I=1e5
)
y_trace4 = response4[[1]]; accpt_4 = response4[[2]]

plot(density(y_trace1))
lines(density(y_trace4), col='red')

