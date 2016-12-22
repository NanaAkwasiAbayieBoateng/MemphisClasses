library(boot)
cd4
var.fun <- function(d, i,k)
{
  m <-colMeans(d[i,k])
     n <- length(i)
     v <- (n-1)*var(cd4[i,k])/n^2
     c(m, v)
}




#baseline

boot.ci(cd4.boot1, type = c("norm", "basic", "perc", "stud"))

cd4.boot1 <- boot(cd4,var.fun , R=999)
#after oneyear

cd4.boot2 <- boot(cd4,var.fun , R=999)
boot.ci(cd4.boot2, type = c("norm", "basic", "perc", "stud"))