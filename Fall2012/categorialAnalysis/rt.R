data <- matrix(c(9,8,27,8,47,236,23,39,88,49,179,706,28,48,89,19,104,293),
ncol=6,byrow=TRUE)
 chisq.test(data)
chisq.test(data)$stdres
M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
dimnames(M) <- list(gender=c("M","F"),
                    party=c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M) 
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
M
yes <- c(54,25)
 n <- c(10379,51815)
 x <- c(1,0)
fit <-glm(x,yes)
summary(fit)
confint(fit)

tea <- matrix(c(3,1,1,3),ncol=2,byrow=TRUE)
 fisher.test(tea)
fisher.test(table, simulate.p.value=TRUE, B=10000)