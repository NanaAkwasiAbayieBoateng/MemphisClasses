library(boot)
city

###non-parametric bootstrap using city data
city[i,]

###example of using stype="f"
city.f <- function(data,f) mean(data$x*f)/mean(data$u*f)

m1= mean(city$x*f)/mean(city$u*f)

city.boot_NP <- boot(city, city.f, stype="f", R=9)
names(city.boot_NP)


#city[1:12,2]

city.boot_NP$t0
boot.array(city.boot_NP,indices=T)
city.boot_NP$t
##mean(city.boot_NP$t)

##Estimation of bias v
mean(city.boot_NP$t)-city.boot_NP$t0

##Estimation of variance V
var(city.boot_NP$t)

##How to compute v_L using delta method ?
n <- nrow(city)
#n1=ncol(city)

T <- mean(city$x)/mean(city$u)
vL <- sum((city$x-T*city$u)^2)/(n^2*mean(city$u)^2)
vL
###example of using stype="i" (default)
city.f <- function(data,i)
{
ub <- data$u[i]
xb <- data$x[i]
mean(xb)/mean(ub)
}


city.boot_NP_i <- boot(city, city.f, R=9) #stype="i" is default
city.boot_NP_i$t
##output both mean and variance
city.f <- function(data,i)
{
us <- data$u[i]
xs <- data$x[i]
tstar <- mean(xs)/mean(us)

n <- nrow(data)
vstar <- sum((xs-tstar*us)^2)/((mean(us)*n)^2)
c(tstar,vstar)
}
city.boot_NP_i <- boot(city, city.f, R=9)
city.boot_NP_i$t



