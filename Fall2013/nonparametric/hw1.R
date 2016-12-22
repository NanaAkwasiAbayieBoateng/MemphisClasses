rm(list=ls())
#1
x=c(4.20 ,0.50 ,1.25 ,3.45 ,2.00 ,0.35 ,0.15 ,0.35 ,1.50 ,0.85,
2.50 ,3.80 ,0.50 ,1.50 ,0.05 ,1.35 ,1.15 ,1.00 ,0.95 ,0.25)


mean(x)
var(x)

 library(TeachingDemos)
 length(x)
 t.test(x,mu=1,alternative="greater")

#2
x1=c(75 ,70 ,88 ,80 ,80, 66, 65 ,68, 85, 80, 78 ,72, 69, 75, 86, 96, 77, 81 ,76, 80,
66 ,65 ,78, 87, 88 ,75, 85 ,84, 66 ,78, 76, 74, 85, 64, 76, 75, 76, 74, 81 ,76)

 t.test(x1,mu=70,alternative="greater")
t.test(x1,mu=70,conf.level=.90)

#3
x=c(126, 142, 156 ,228 ,245 ,246 ,370 ,419 ,433 ,454 ,478 ,503)
mean(x)
var(x)
sqrt(var(x))
pnorm(1.9572)
pnorm(1.9572 ,lower.tail = TRUE, log.p = FALSE)
pnorm(-1.07387)


#4
x=c(79, 74, 88, 80, 80, 66, 65, 86, 84 ,80 ,78 ,72 ,71, 74, 86, 96 ,77, 81, 76 ,80,76, 75, 78 ,87 ,87, 74, 85, 84 ,76 ,77, 76, 74, 85, 74, 76 ,77, 76, 74, 81 ,76)
theta=70
(sobs=length(x[x>theta]))
(pvalue=dbinom(38,40,.5)+dbinom(39,40,.5)+dbinom(40,40,.5))
#or
(pvalue=1-pbinom(sobs-1,40,.5))

#5
x=c(21.3, 28.8, 17.6, 23.0, 27.2, 28.5, 32.8, 28.2, 25.9, 22.5, 27.2, 33.1, 28.7, 24.8, 24.3, 27.1, 30.6,
26.8, 18.9, 36.3, 28.0, 17.9, 25.0, 27.5, 27.7, 32.1, 28.0, 30.9, 20.0, 20.2, 33.5, 26.4, 30.9, 33.2)
sort(x)

sobs=sum(x>18)
sobs
(n=length(x))
p=0.2
#a
1-pbinom(sobs-1,n,1-p)
#c
n=length(x)
p=0.2
alpha=0.1

l=n*p+qnorm(alpha/2)*sqrt(n*p*(1-p))
u=1+n*p+qnorm(1-alpha/2)*sqrt(n*p*(1-p))
u
l
pbinom(u,n,p)-pbinom(l,n,p)
sort(x)[12]
sort(x)[3]

#b
p=0.5
u=n*p+qnorm(0.975)*sqrt(n*p*(1-p))
l=1+n*p+qnorm(1-0.975)*sqrt(n*p*(1-p))
u
l
pbinom(22,n,p)-pbinom(11,n,p)
sort(x)[23]
sort(x)[12]

"conf.med"<-function(x, alpha)
{
        v <- sort(x, na.last = NA)
        n <- length(x)
        if(n > 0) {
                m <- median(x)
                l <- qbinom(alpha/2, n, 0.5)
                if(l > 0) 
                		{
                			u = n-l+1
                        r <- c(m, v[l], v[u])
                       }
                else r <- c(m, NA, NA)
        }
        else r <- c(NA, NA, NA)
        r <- as.data.frame(list(median = r[1], lower = r[2], upper = r[3]))
        class(r) <- "table"
        r
}
x=c(21.3, 28.8, 17.6, 23.0, 27.2, 28.5, 32.8, 28.2, 25.9, 22.5, 27.2, 33.1, 28.7, 24.8, 24.3, 27.1, 30.6,
26.8, 18.9, 36.3, 28.0, 17.9, 25.0, 27.5, 27.7, 32.1, 28.0, 30.9, 20.0, 20.2, 33.5, 26.4, 30.9, 33.2)

conf.med(x,0.95)


#6
D=c(-13,11,-3,-12,-15,-1,-4)
wilcox.test(D,alternative="less")
wilcox.test(D,alternative="two.sided")
D=c(8,5,3.5,-1.5,1,1.5)
wilcox.test(D,alternative="greater")

2*pnorm(18.5, mean = 18, sd = 5.679, lower.tail = FALSE, log.p = FALSE)

2*pnorm(6.5, mean = 14, sd = 5.906, lower.tail = FALSE, log.p = FALSE)
pnorm(6.5, mean = 14, sd = 5.906, lower.tail = FALSE, log.p = FALSE)
