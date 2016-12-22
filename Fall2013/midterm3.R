rm(list=ls())
#kruskal-wallis test 
#h0:g1=g2=g3  H0: the means of the 3 groups are statistically equal.
#ha:at least one different
low.fat<-c(140,177,50,65,86,153,181,191,77,84,87,56,66,73,199)
s1<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
saturated.fat<-c(124,58,56,68,79,89,107,86,142,110,96,142,86,
75,117,98,105,126,43,46,81,133,165)
s2<-c(rep(1,length(saturated.fat)))
unsaturated.fat<-c(112,68,84,109,153,143,60,70,98,164,63,63,77,91,91,66,70,
77,63,66,66,94,101,105,108,112,115,126,161,178)
s3<-c(rep(1,length(unsaturated.fat)))
x<-c(low.fat,saturated.fat,unsaturated.fat)
y<-c(s1,s2,s3)
length(x);length(y)

tumor.free<-list(g1=low.fat,g2=saturated.fat,g3=unsaturated.fat)

kruskal.test(tumor.free)


 
 (fit1 <- survfit(Surv(low.fat, s1) ~ 1) )
 (fit2 <- survfit(Surv(saturated.fat, s2) ~ 1) )
 (fit3 <- survfit(Surv(unsaturated.fat, s3) ~ 1) )
 
 plot(fit1,conf.int=0, col = 'blue', xlab = 'Time (Days)', ylab = 'Survival Probability') 
lines(fit1,col='blue')
lines(fit2, col = 'red') 
lines(fit3,col='green')


legend(121, 1,c('low.fat', 'saturated.fat','unsaturated.fat'),
 pch=c(1,2,3) ,col=c("blue","red","green"))
 
 
title(main='KM-Curves for Tumor-Free Time of 
90 Rats on Three Different Diets ') 
