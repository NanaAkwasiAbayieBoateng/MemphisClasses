

ifactor <- function(n) 
{ 
    LL <- NULL; 
    f <- 2; 
    while(f <= n^0.5) 
    { 
        found <- F; 
        c <- 0; 
        while(n%%f == 0) 
        { 
            c <- c+1; 
            found <- T; 
            n <- n/f; 
        } 
        if(found) 
            LL <- rbind(LL, c(f,c)); 
        if(f==2) {f <- f+1;} else {f <- f+2;} 
    } 
    if(n >1) 
        { LL <- rbind(LL, c(n,1)); } 
LL
} 



    

phi <- function(m)
{
    mm <- m;
    LL <- ifactor(m); 
    nf <- nrow(LL);  
    for (i in 1:nf) 
    { 
        q <- LL[i,1];  
        mm <- mm/q*(q-1); 
    } 
    mm
}



is_even <- function(n) 
{ if(n%%2 == 0) return (TRUE) else return (FALSE) } 

  is_even(123)

 is_even(1234)






#version 2
is_prime <- function(n) 
{ 
    LL <- ifactor(n) 
    return (nrow(LL)==1 && LL[1,2]==1); 
} 

###NOTE: may cause overflow, if m is too large (e.g m > W^0.5=2^16) 
###computes  a^(m-1)/q using the reccurence relation#######
power_mod <- function(a,n,m) 
{ 
    if (n==0)  {return (1);} 
    if (n==1) {return (a%%m);} 
    if(is_even(n)) 
        { aa <- power_mod(a,n/2,m); aa <- aa^2%%m; return(aa);} 
    else 
        { aa <- power_mod(a,(n-1)/2,m); aa <- aa^2%%m;  aa <- (a*aa)%%m; return(aa);} 
} 




##computing a*b%m, b< m^0.5 can avoid overflow
mulmod <- function(a,b,m)
{
R <- m%%a
Q <- (m-R)/a
if(Q >0) 
  {
    B <- b%%Q
    A <-  (b-B)/Q
  }
else
    {
    B <- b
    A <- 0
    }
return(a*B-R*A)   
}


igcd <- function(m, n) 
{ 
    L <- max(abs(m),abs(n)); 
    S <- min(abs(m),abs(n)); 
    while(S >0) 
        { 
            R <- L%%S; if(R == 0) {return(S)}; 
            L <- S; 
            S <- R; 
        } 
}



rel_prime <- function(m,n) 
{ return( igcd(m,n)==1); } 

 

###Note: may fail if m is too large.....
is_primitive <- function(a,m) 
{ 
    if(!(is_prime(m))) {return(FALSE);} 
    if(!(rel_prime(a,m))) {return(FALSE);} 
    LL <- ifactor(m-1); nf <- nrow(LL);  
    for (i in 1:nf) 
    { 
        q <- LL[i,1];  
        qq <- power_mod(a, (m-1)/q,m);  
        if (qq == 1) {return(FALSE);} 
    } 
    return(TRUE); 
} 

 
#find the first primitive root >= a
primroot <- function(a,m) 
{ 
    if(!(is_prime(m))) {return(FALSE);} 
    i <- a; 
    while (!is_primitive(i,m)) {i <- i+1;} 
    return(i); 
}




primroot_all <- function(m) 
{ 
    if(!(is_prime(m))) {return(FALSE);} 
    a <- primroot(2,m); # a is the smallest primitive root
    L <- NULL; 
    for (c in 1:(m-1))
        {
        if(rel_prime(c, m-1))
            {
                B <- power_mod(a, c, m); 
                L <- append(L, B);
            }
        }
L
}

 primroot_all(127)
 

 PRIM127 <- primroot_all(127)
length(PRIM127)



LCG <- function(x, B, m)
{
(x*B)%%m
}

LCG_m <- function(x, B, m)
{
mulmod(B, x, m)
}


LCG_seq  <- function(x0, B, m, n)
{
    x <- x0
    L <- NULL
    for (i in 1:n)
    {
        x <- LCG(x, B, m)
        L <- append(L, x)
    }
    L
}



LCG_seq(3, 9, 31, 31)


########################################
###BAYS-DURHAM SHIFTING GENERATOR




Bays_Durham<-function(k,x0, B, m, n)
{

LL <- LCG_seq(x0, B, m, n)

   L1<-LL
   L2<-L1[k+1]


   L3<-L2

  for (i in 1:length(LL))

   {


   t<-L2%%k+1

  
   L2<-L1[t]

  
    L3<-append(L3,L2)
  
   L1[t]<-L1[k+i+1]
   }
       L3
               
            }



y=Bays_Durham(8,9, 3, 31, 310)



plot_Bays<-function(k,x0, B, m, n)
{
L3<-Bays_Durham(k,x0, B, m, n)

LLx <- L3[1:length(L3)-1]
LLy <- L3[2:length(L3)]
plot(LLx, LLy, type="p")

}

par(mfrow=c(2,2))
plot_Bays(2,9, 3, 127, 5000)
title(main = list(paste("k=2"),col="black", cex = 1.0)) 
plot_Bays(8,9, 3, 127, 5000)
title(main = list(paste("k=8"),col="black", cex = 1.0)) 
plot_Bays(16,9, 3, 127, 5000)
title(main = list(paste("k=16"),col="black", cex = 1.0)) 
plot_Bays(32,9, 3, 127, 5000)
title(main = list(paste("k=32"),col="black", cex = 1.0))
 
par(mfrow=c(2,2))
plot_Bays(8,9, 3, 127, 50)
title(main = list(paste("n=50"),col="black", cex = 1.0)) 
plot_Bays(8,9, 3, 127, 500)
title(main = list(paste("n=500"),col="black", cex = 1.0)) 
plot_Bays(8,9, 3, 127, 5000)
title(main = list(paste("n=5000"),col="black", cex = 1.0)) 
plot_Bays(8,9, 3, 127, 10000)
title(main = list(paste("n=10000"),col="black", cex = 1.0))






#random U(0,1) sequence
LCG_u_seq  <- function(x0, B, m, n)
{
    x <- x0
    L <- NULL
    for (i in 1:n)
    {
        x <- LCG(x, B, m)
        L <- append(L, x/m)
    }
    L
}

LL <- LCG_u_seq(1, 3, 127, 127)


U<-function(k,x0, B, m, n)
{
for (i in 1:(n-7))
{
X1<- Bays_Durham(k,x0, B, m, n)
X2<- (1/m)*X1

X3<-X2
}
 
X3
  }
U(8,9, 3, 31, 50)
length(U)
plot_U<-function(k,x0, B, m, n)
{
L3<-U(k,x0, B, m, n)

LLx <- L3[1:length(LL)-1]
LLy <- L3[2:length(LL)]
plot(LLx, LLy, type="p")

}
plot_U(8,9, 3, 31, 50)

par(mfrow=c(2,2))
plot_U(2,9, 3, 127, 5000)
title(main = list(paste("k=2"),col="black", cex = 1.0)) 
plot_U(8,9, 3, 127, 5000)
title(main = list(paste("k=8"),col="black", cex = 1.0)) 
plot_U(16,9, 3, 127, 5000)
title(main = list(paste("k=16"),col="black", cex = 1.0)) 
plot_U(32,9, 3, 127, 5000)
title(main = list(paste("k=32"),col="black", cex = 1.0))

par(mfrow=c(2,2))
plot_U(8,9, 3, 127, 50)
title(main = list(paste("n=50"),col="black", cex = 1.0)) 
plot_U(8,9, 3, 127, 500)
title(main = list(paste("n=500"),col="black", cex = 1.0)) 
plot_U(8,9, 3, 127, 5000)
title(main = list(paste("n=5000"),col="black", cex = 1.0)) 
plot_U(8,9, 3, 127, 10000)
title(main = list(paste("n=10000"),col="black", cex = 1.0))
