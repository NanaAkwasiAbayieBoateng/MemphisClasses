rm(list=ls())
#logrank test
library(survival)

time1<-c(1.8,2.3,3.8,5.5,6.3,6.4,16.6,17.1,23.8,33.7,33.7)
status1<-c(1,1,1,1,1,1,0,0,0,0,0)
time2<-c(2.8,3.0,4.3,4.5,5.8,6.8,7.8,8.2,8.2,9.2,9.2,10.8,11.0,15.9,18.1,21.4,22.1,23.0,26.9)
status2<-c(1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0)

length(time1)
length(status1)
length(time2)
length(status2)

time<-c(1.8,2.3,3.8,5.5,6.3,6.4,16.6,17.1,23.8,33.7,33.7,2.8,3.0,4.3,4.5,5.8,6.8,7.8,8.2,8.2,9.2,9.2,10.8,11.0,15.9,18.1,21.4,22.1,23.0,26.9)
status<-c(1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0)
treatment<-c(1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

length(time)
length(status)
length(treatment)

(fit <- survdiff(Surv(time, status) ~ treatment))

 #time1 <- c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35) 
#status1 <- c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0) 
 
# time2 <-c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23) 
#status2 <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
 
 (fit1 <- survfit(Surv(time1, status1) ~ 1) )
 (fit2 <- survfit(Surv(time2, status2) ~ 1) )
 
 plot(fit1,conf.int=0, col = 'blue', xlab = 'Time (months)', ylab = 'Survival Probability') 
lines(fit2, col = 'red') 
legend(21,1,c('Group 1 (C.parvum)', 'Group 2 (BCG)'), col = c('blue','red'), lty = 1) 
 title(main='KM-Curves for Remission Data') 












 library("coin")
(surv_test(Surv(time, status) ~ treatment, distribution = "exact"))


rm(list=ls())
#logrank test
library(survival)

time1<-c(1.8,2.3,3.8,5.5,6.3,6.4,16.6,17.1,23.8,33.7,33.7)
status1<-c(1,1,1,1,1,1,0,0,0,0,0)
time2<-c(2.8,3.0,4.3,4.5,5.8,6.8,7.8,8.2,8.2,9.2,9.2,10.8,11.0,15.9,18.1,21.4,22.1,23.0,26.9)
status2<-c(1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0)

length(time1)
length(status1)
length(time2)
length(status2)

time<-c(1.8,2.3,3.8,5.5,6.3,6.4,16.6,17.1,23.8,33.7,33.7,2.8,3.0,4.3,4.5,5.8,6.8,7.8,8.2,8.2,9.2,9.2,10.8,11.0,15.9,18.1,21.4,22.1,23.0,26.9)
status<-c(1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0)
treatment<-c(1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

length(time)
length(status)
length(treatment)

(fit <- survdiff(Surv(time, status) ~ treatment))

 #time1 <- c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35) 
#status1 <- c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0) 
 
# time2 <-c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23) 
#status2 <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
 
 (fit1 <- survfit(Surv(time1, status1) ~ 1) )
 (fit2 <- survfit(Surv(time2, status2) ~ 1) )
 
 plot(fit1,conf.int=0, col = 'blue', xlab = 'Time (months)', ylab = 'Survival Probability') 
lines(fit2, col = 'red') 
legend(21,1,c('Group 1 (C.parvum)', 'Group 2 (BCG)'), col = c('blue','red'), lty = 1) 
 title(main='KM-Curves for Remission Data') 













