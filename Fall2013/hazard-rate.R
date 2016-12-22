h_exp <- function(t, rate)
{
dexp(t, rate)/(1-pexp(t, rate))
}

x <- (1:500)*.001
y <- h_exp(x, 2)
plot(x,y, type="l")


h_gamma <- function(t, shape, rate)
{
dgamma(t, shape, rate)/(1-pgamma(t, shape, rate))
}

x <- (1:1000)*.001
y <- h_gamma(x, 2, 5)
plot(x,y, type="l")


x <- (1:1000)*.001
y1 <- h_gamma(x, 1.25, 5)
#plot(x,y1, type="l")


x <- (1:1000)*.001
y2 <- h_gamma(x, 1, 5)
#plot(x,y2, type="l")


x <- (1:1000)*.001
y3 <- h_gamma(x, 0.75, 5)
#plot(x,y3, type="l")

Y <- cbind(y1, y2, y3)
matplot(x, Y, type="l")


h_beta <- function(t, A, B)
{
dbeta(t, A, B)/(1-pbeta(t, A, B))
}

x <- (1:999)*.001
y <- h_beta(x, 0.5, 0.5)
plot(x,y, type="l")


x <- (1:999)*.001
y <- h_beta(x, 1, 1)
plot(x,y, type="l")

#bathtub shape hazard function

x <- (1:999)*.001
y <- h_beta(x, 0.1, 0.1)
plot(x,y, type="l")


x <- (1:999)*.001
y <- h_beta(x, 2.1, 2.1)
plot(x,y, type="l")


#mountain shape hazard function

h_lnorm <- function(t, m, s)
{
dlnorm(t, m, s)/(1-plnorm(t, m, s))
}

x <- (1:4999)*.001
y <- h_lnorm(x, 0.25, 1)
plot(x,y, type="l")

x <- (1:4999)*.001
y <- h_lnorm(x, 0, 1)
plot(x,y, type="l")


##more advanced feature in R function...

h <- function(t, rate, f_pdf, F_cdf)
{
f_pdf(t, rate)/(1-F_cdf(t, rate))
}

x <- (1:500)*.001
y <- h(x, 2, dexp, pexp)
plot(x,y, type="l")


