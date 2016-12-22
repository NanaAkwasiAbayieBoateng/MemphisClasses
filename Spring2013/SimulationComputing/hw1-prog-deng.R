#version 1: 
ifactor <- function(n) 
{ 
    L <- NULL; 
    f <- 2; 
    while(f <= n^0.5) 
    { 
        while(n%%f == 0) 
        { 
            L <- append(L, f); 
            n <- n/f; 
        } 
    f <- f+1; 
    } 
    if(n >1) 
        L <- append(L, n); 
L; 
} 

 ifactor(126)
[1] 2 3 3 7

#version 2: 

ifactor <- function(n) 
{ 
    Lf <- NULL; 
    Lc <- NULL; 
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
        {
            Lf <- append(Lf, f); 
            Lc <- append(Lc, c);
        } 
        f <- f+1; 
    } 
    if(n >1) 
        { Lf <- append(Lf, n); Lc <- append(Lc, 1); } 
    cbind(Lf, Lc); 
} 

ifactor(49)
     Lf Lc
[1,]  7  2
 ifactor(2^31-2)
      Lf Lc
[1,]   2  1
[2,]   3  2
[3,]   7  1
[4,]  11  1
[5,]  31  1
[6,] 151  1
[7,] 331  1
 ifactor(126)
     Lf Lc
[1,]  2  1
[2,]  3  2
[3,]  7  1

#version 3: 

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


 ifactor(49)
     [,1] [,2]
[1,]    7    2
 ifactor(2^31-2)
     [,1] [,2]
[1,]    2    1
[2,]    3    2
[3,]    7    1
[4,]   11    1
[5,]   31    1
[6,]  151    1
[7,]  331    1
 ifactor(127^2-1)
     [,1] [,2]
[1,]    2    8
[2,]    3    2
[3,]    7    1


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

 phi(126);
Lf 
36 
 phi(2^31-2);
       Lf 
534600000 

is_even <- function(n) 
{ if(n%%2 == 0) return (TRUE) else return (FALSE) } 

 is_even(123)
[1] FALSE
 is_even(1234)
[1] TRUE

#version 1
is_prime <- function(n) 
{ 
    if(n%%2 == 0) return(FALSE) 
    f <- 3; 
    while(f <= n^0.5) 
    { 
        if(n%%f == 0) return (FALSE) 
        f <- f+2; 
    } 
    return (TRUE); 
} 

 is_prime(2^31-1)
[1] TRUE
 is_prime(126)
[1] FALSE


#version 2
is_prime <- function(n) 
{ 
    LL <- ifactor(n) 
    return (nrow(LL)==1 && LL[1,2]==1); 
} 

###NOTE: may cause overflow, if m is too large (e.g m > W^0.5=2^16) 

power_mod <- function(a,n,m) 
{ 
    if (n==0)  {return (1);} 
    if (n==1) {return (a%%m);} 
    if(is_even(n)) 
        { aa <- power_mod(a,n/2,m); aa <- aa^2%%m; return(aa);} 
    else 
        { aa <- power_mod(a,(n-1)/2,m); aa <- aa^2%%m;  aa <- (a*aa)%%m; return(aa);} 
} 

 power_mod(123, 567, 1000000)
[1] 204947
 power_mod(123, 56789, 1000000)
[1] 498763
 power_mod(123, 56789, 10000000)
[1] 3498763
 power_mod(123, 56789, 100000000) ##error due to overflow
[1] 17799168
 power_mod(13, 56789, 100000000) ###error due to overflow
[1] 73579008

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

 igcd(12345, 55555)
[1] 5
 igcd(12345, 555555)
[1] 15

rel_prime <- function(m,n) 
{ return( igcd(m,n)==1); } 

 rel_prime(134, 246);
[1] FALSE
 rel_prime(134, 2467);
[1] TRUE

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

 is_primitive(2, 127)
[1] FALSE
 is_primitive(3, 127)
[1] TRUE
 is_primitive(5, 127)
[1] FALSE
 is_primitive(6, 127)
[1] TRUE

#find the first primitive root >= a
primroot <- function(a,m) 
{ 
    if(!(is_prime(m))) {return(FALSE);} 
    i <- a; 
    while (!is_primitive(i,m)) {i <- i+1;} 
    return(i); 
}

 primroot(2, 127)
[1] 3
 primroot(3, 127)
[1] 3
primroot(4, 127)
[1] 6

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
 [1]   3 116 109  92  86  12  83 112  55 114  48  78  67  93 106  65  58  14 118
[20]  46  43   6  56  91  57  45  39  97 110 101  53  96  29   7  23  85

PRIM127 <- primroot_all(127)
 length(PRIM127)
[1] 36



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



 LL <- LCG_seq(1, 3, 127, 127)
 LL
  [1]   3   9  27  81 116  94  28  84 125 121 109  73  92  22  66  71  86   4
 [19]  12  36 108  70  83 122 112  82 119 103  55  38 114  88  10  30  90  16
 [37]  48  17  51  26  78 107  67  74  95  31  93  25  75  98  40 120 106  64
 [55]  65  68  77 104  58  47  14  42 126 124 118 100  46  11  33  99  43   2
 [73]   6  18  54  35 105  61  56  41 123 115  91  19  57  44   5  15  45   8
 [91]  24  72  89  13  39 117  97  37 111  79 110  76 101  49  20  60  53  32
[109]  96  34 102  52  29  87   7  21  63  62  59  50  23  69  80 113  85   1
[127]   3

LLx <- LL[1:length(LL)-1]
LLy <- LL[2:length(LL)]
plot(LLx, LLy, type="p")


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


LLx <- LL[1:length(LL)-1]
LLy <- LL[2:length(LL)]
plot(LLx, LLy, type="p")


#plotting LCG in 2-dim
plot_LCG_u <- function(x0, B, m, n)
{
    LL <- LCG_u_seq(x0,  B, m, n)
    LLx <- LL[1:length(LL)-1]
    LLy <- LL[2:length(LL)]
    plot(LLx, LLy, type="p")
}
par(mfrow=c(2,2))
plot_LCG_u(1, 3, 127, 127)
title(main = list(paste("a=3,m=127,c=0"),col="black", cex = 1.0))
plot_LCG_u(1, 6, 127, 127)
title(main = list(paste("a=6,m=127,c=0"),col="black", cex = 1.0))
 plot_LCG_u(1, 1108, 12007, 1000)
title(main = list(paste("a=1101,m=12007,c=0"),col="black", cex = 1.0))
 plot_LCG_u(1, 1101, 12007, 1000)
 title(main = list(paste("a=1101,m=12007,c=0"),col="black", cex = 1.0))


plot_LCG_u(1, 6, 127, 127)

plot_LCG_u(1, 3, 127, 127)
 plot_LCG_u(1, 3, 128, 128)

#version 2: plotting LCG in 2-dim
plot_LCG_u <- function(x0, B, m, n)
{
    LL <- LCG_u_seq(x0,  B, m, n)
    LLx <- LL[1:length(LL)-1]
    LLy <- LL[2:length(LL)]
    plot(LLx, LLy, type="p", xlab="U[i]", ylab="U[i+1]")
}

#plotting all 2-dim alttice points
lat2D <- function(m)
{
    LL <- NULL
    for (i in 0:(m-1))
    {
        for (j in 0:(m-1))
        {
            LL <- rbind(LL, c(i/m,j/m))
        }
    }
    LL
}

 L2D <- lat2D(31)
 plot_LCG_u(1, 3, 31, 31)
 points(L2D, col="red", cex=0.3)

##too many points  plotted for m=127

 L2D <- lat2D(127)
 plot_LCG_u(1, 3, 127, 127)
points(L2D, col="red", cex=0.3)

