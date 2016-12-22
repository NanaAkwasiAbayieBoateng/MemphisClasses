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


##computing a*b%m, b< m^0.5 can avoid overflow
mulmod <- function(a,b,m,c)
{
R <- m%%a
Q <- (m-R)/a
if(Q >0) 
  {
    B <- (b+c)%%Q
    A <-  ((b+c)-B)/Q
  }
else
    {
    B <- b+c
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



rel_prime <- function(m,n) 
{ return( igcd(m,n)==1); } 

rel_prime(134, 246);

 rel_prime(134, 2467);


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

is_primitive(3, 127)

 is_primitive(5, 127)

is_primitive(6, 127)


#find the first primitive root >= a
primroot <- function(a,m) 
{ 
    if(!(is_prime(m))) {return(FALSE);} 
    i <- a; 
    while (!is_primitive(i,m)) {i <- i+1;} 
    return(i); 
}

primroot(2, 127)

 primroot(3, 127)

primroot(4, 127)


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




LCG <- function(x, B, m,c)
{
(x*B+c)%%m
}

LCG_m <- function(x, B, m,c)
{
mulmod(B, x, m,c)
}


LCG_seq  <- function(x0, B, m, n,c)
{
    x <- x0
    L <- NULL
    for (i in 1:n)
    {
        x <- LCG(x, B, m,c)
        L <- append(L, x)
    }
    L
}



 LL <- LCG_seq(1, 3, 127, 127,2)
LL
 
#random U(0,1) sequence
LCG_u_seq  <- function(x0, B, m, n,c)
{
    x <- x0
    L <- NULL
    for (i in 1:n)
    {
        x <- LCG(x, B, m,c)
        L <- append(L, x/m)
    }
    L
}







boxmuller=function(x0,a,m,n,c){



x1=rep(0,n)
x2=rep(0,n)




L=LCG_u_seq(x0,a,m,n,c)

u=rep(0,n)
v=rep(0,n)

for (i in 1:n){
u[i]=L[2*i-1]
v[i]=L[2*i]
}
  

for (i in 1:n){

x1[i]=sqrt(-2*log(u[i]))*cos(2*pi*v[i])

x2[i]=sqrt(-2*log(u[i]))*sin(2*pi*v[i])
}

plot(x1,x2)

}


par(mfrow=c(1,2))
boxmuller(101,1221, 2048, 2000,1)
title(main = list(paste("a=1221,m=2048,c=1"),col="black", cex = 1.0))

boxmuller(101,1229, 2048, 2000,1)
title(main = list(paste("a=1229,m=2048,c=1"),col="black", cex = 1.0))


par(mfrow=c(1,2))
boxmuller(101,61, 2048, 2000,1)
title(main = list(paste("a=61,m=2048,c=1"),col="black", cex = 1.0))

boxmuller(101,65, 2048, 2000,1)
title(main = list(paste("a=65,m=2048,c=1"),col="black", cex = 1.0))


par(mfrow=c(1,2))
boxmuller(101,7, 2^31-1, 2000,0)
title(main = list(paste("a=61,m=2048,c=0"),col="black", cex = 1.0))

boxmuller(101,16807, 2^31-1, 2000,0)
title(main = list(paste("a=65,m=2048,c=0"),col="black", cex = 1.0))


#The polar method

polar=function(x0,a,m,n,c){

x=rep(0,n)
y=rep(0,n)




L=LCG_u_seq(x0,a,m,2*n,c)

u1=rep(0,n)
u2=rep(0,n)

for (i in 1:n){
u1[i]=L[2*i-1]
u2[i]=L[2*i]
}
  
v1=2*u1-1
v2=2*u2-1
r2=v1^2+v2^2


for (i in 1:n){

if( (r2[i]>1.0) ){

v1=2*u1-1
v2=2*u2-1
r2=v1^2+v2^2   
  }

else{

  
   x[i] =v1[i]*sqrt((-2*log(r2[i]))/r2[i])
  y[i] = v2[i]*sqrt((-2*log(r2[i]))/r2[i])

}

}
plot(x,y)

}


par(mfrow=c(1,2))
polar(101,1221, 2048, 1000,1)
title(main = list(paste("a=1221,m=2048,c=1"),col="black", cex = 1.0))
polar(101,1229, 2048, 1000,1)
title(main = list(paste("a=1229,m=2048,c=1"),col="black", cex = 1.0))
par(mfrow=c(1,2))
polar(101,61, 2048, 1000,1)
title(main = list(paste("a=61,m=2048,c=1"),col="black", cex = 1.0))
polar(101,65, 2048, 1000,1)
title(main = list(paste("a=65,m=2048,c=1"),col="black", cex = 1.0))

par(mfrow=c(1,2))
polar(101,7, 2^31-1, 1000,0)
title(main = list(paste("a=7,m=2^31-1,c=0"),col="black", cex = 1.0))
polar(101,16807, 2^31-1, 1000,0)
title(main = list(paste("a=16807,m=2^31-1,c=0"),col="black", cex = 1.0))

