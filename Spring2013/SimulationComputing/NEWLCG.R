#version 1:

##function to factor numbers####
################################
 
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
  
 ifactor(2^31-2)
  
 ifactor(126)


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
   
 ifactor(2^31-2)
  
 ifactor(127^2-1)
    

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

 is_even(1234)


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

is_prime(126)



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

power_mod(123, 567, 1000000)

 power_mod(123, 56789, 1000000)

 power_mod(123, 56789, 10000000)

 power_mod(123, 56789, 100000000) ##error due to overflow

 power_mod(13, 56789, 100000000) ###error due to overflow


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

 igcd(12345, 555555)


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


########################################
###BAYS-DURHAM SHIFTING GENERATOR


LL <- LCG_seq(9, 3, 31, 31)

#Bays_Durham<-function(k)

k=8
 for (i in 1:k)
{

LL <- LCG_seq(9, 3, 31, 31)
T<-LL[1:k]

y<-LL[k+1]
y[1]<-LL[k+1]
p=y[1]%%k+1
p[1]=y[1]%%k+1

T[p[1]]<-LL[k+1]
T


y[2]<-LL[p[1]]
p[2]<-y[2]%%k+1
T[p[2]]<-LL[k+3]
T

for (i in 3:length(LL))
{
y[i]<-LL[k+i]
 p[i]<-y[i-1]%%k+1
T
T[p[i]]<-LL[k+i+1]
#T[p[i]]<-replace(LL,p[i],LL[k+i+1])
y[i]<-LL[p[i]]

}
}




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

 plot_LCG_u(1, 3, 127, 127)
 plot_LCG_u(1, 5, 127, 127)
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

