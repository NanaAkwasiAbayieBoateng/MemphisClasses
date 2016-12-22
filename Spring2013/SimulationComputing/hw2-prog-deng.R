#HW2: R code for the implementation of Bays-Durham Shuffle Generator

LCG <- function(x, B, m)
{
(x*B)%%m
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



> LL <- LCG_seq(1, 3, 127, 127)
> LL
  [1]   3   9  27  81 116  94  28  84 125 121 109  73  92  22  66  71  86   4
 [19]  12  36 108  70  83 122 112  82 119 103  55  38 114  88  10  30  90  16
 [37]  48  17  51  26  78 107  67  74  95  31  93  25  75  98  40 120 106  64
 [55]  65  68  77 104  58  47  14  42 126 124 118 100  46  11  33  99  43   2
 [73]   6  18  54  35 105  61  56  41 123 115  91  19  57  44   5  15  45   8
 [91]  24  72  89  13  39 117  97  37 111  79 110  76 101  49  20  60  53  32
[109]  96  34 102  52  29  87   7  21  63  62  59  50  23  69  80 113  85   1
[127]   3

LL <- LCG_seq(1, 3, 127, 127)
LLx <- LL[1:length(LL)-1]
LLy <- LL[2:length(LL)]
plot(LLx, LLy, type="p",xlab="X[i]",ylab="X[i+1]")




##initialization of shuffle T-table of size K
m <- 31
x0 <- 9
B <- 3
K <- 8

T <- LCG_seq(x0, B, m, K)
T

# Bayes-Durham Shuffle Generator
Bays_Durham <- function(T, B, m, n)
{
K <- length(T) 
x0 <- LCG(T[K], B, m)
Y_list <- c(x0)
y <- x0
for (i in 1:(n-1))
{
    j <- y%%K+1
    y <- T[j]
    Y_list <- append(Y_list, y)
    x0 <- LCG(x0, B, m)
    T[j] <- x0
}
Y_list
}

BD <- Bays_Durham(T, B, m, 1000)

> BD
   [1] 13 20 17 19 16 27 30 29  8 28 24  5 12 15 25 10 26 23  6  4  2  7 21 11
  [25] 22  1 18  9 17 29 19 26 20  3  8 14 16 28 10 24  4 12 14 22  2  5 13 23
  [49] 27 30  6  3  1 25 19 27 16 15 21  7 29 25 26 18 10 30  9 24 20 11 17  4
  [73]  5  8 12 11 15 13  2 28 18  1 14 22 19 23  7 17 27 16  6 26  9 29 21 30
  [97]  8 13 28  3 25 10 24  4  5 12  6 22  7 20 23  1 14 21 18 11 15  9 27 17
 [121] 29 26 16  2 24 10 30 19 25 13  8 28  3 12 11  2 22  4  6  7 20 21 15  3
 [145] 18 23 19 26 16 14  1  5 27 20  9  8 25 28 30 13 24 22  5 15 17  4 12 23
 [169]  6 11 10 29  2  9 18 19  3 17 26 16 14  1 29 27 20  7 21 10 25 24 13  4
 [193] 28 11 30  8 15 22 18 12  2  1  5 14 21 19  6 26  9 27 17 25  8 23  7 30
 [217] 20  3 13 16 10 29  5 11 12  4 18 14 22 21  2  7 28 23 27  6  1 24 15 26
 [241]  9 20 19 16 29  3 10 13 28 24 30 17  8 15 25 11 22 14  7  6 21 12  5  9
 [265] 18  4 27 23  1 26 16  2 13 19 20 17 25 22  3 30 12 28 14 15 29 10 24  8
 [289]  7  6  2 23  1  4 11  5 18  9 19 16 21 17 29  8 13 10 20 26 22  3 25 24
 [313] 30  5 28  4 18 12 23 27 15  1 14  2  7  9 27  3 20 21  6 19 29 13 10 26
 [337] 28 25 17 12  4 15 16 11 24  2 22  8 18 23 11  6  7  3  9  5 30 27 26  1
 [361] 16 21 17 13 24  8 28 14 20  4  5 30 12 15 19 29 14 11 18 25 10  1  3 21
 [385] 23  6  7 16 22 17 27 19  8 29 26  9 13 30 25 22 12  2 28 14 15 20  2 11
 [409] 24 10  7 18  3 21  4 23  9  5 26 27 19 13 29 24  1 20 16 30  6 12 22  5
 [433] 10 25 28 15 17  6 14 21 11  8  4 18  2 26 16 27  9  7 23 13  3 29 24 20
 [457] 19 10 17 25  5 30  1 15  8 28 22 11  4 23  6  7  3 21 14  9  2 12  1 17
 [481] 25 13 26 20 29 24 18 10  4 30 16 22 15 27 19  6 11 18 12  5 28  1  8 14
 [505] 23  2 21  3  7 16 19 29 20  9 27  8 13 24 22 26 17 30  5  4 10 15 25 14
 [529] 11 28  6 21  2 18 19  1  7 23 20  3 16 12 25 17 10 26 28 24  8 12  4 15
 [553] 29 27 13  2 22  9 30  7 11  6  1 21 18 23  3  9 19 20 14 27 25 29 26 16
 [577]  5 30  8 22 12 13  4 14 15 17 10 28  2  7 18  1 23  3 24  5 11 26  9 27
 [601] 20 21 17 25 10 29 24 16  4  8 12  5 22  6  2 28 14  6  7 19 13 11  3 27
 [625] 19 26 18 17 30 21  9 29 13 24 15  1  8 30 25 22 12 23 28 14 15 11 16  4
 [649]  2 20 21 10  1  5  9 19 23 18 27 17 16  7 20  3 25 13 26 29 22  6  5 12
 [673] 10  4 11 30 15 24  8 21 14 23  7 27 18  2 16  1 28  6  9 29  3 26 17  8
 [697] 20 25 28  4  5 24 22 13 14  2 30 18 23 19 10 21  6  7  1 12 15 26  9 16
 [721] 11  3  8 13 27 24 10 29 30 19 28 17 25 11 15 20 14  5 12 23 18  4  1  2
 [745]  9 19  6  7  3 17 16 22 20 27 25 13 21 22 24  8  5  4 10 26  2  6 12 11
 [769] 30 23 29 14  1 28  7  3 21  9 19 17 29 20 26 18 10 30 27 25 13  8 15 16
 [793] 15 14 22  6 18 28 24 11  4 21  5 27  3 26  7  2 17 12  9 25  8  1 24 10
 [817] 29 19 16 28 13  4 15 20 11 12  6 23  2 22  7 21 14  3 18  1 30 19 26 16
 [841]  5 27 29  8 13 10 25 17  4 23  9 12  5 28 11 24 30 20  6  7 15  3 18 22
 [865]  1 14 26 19 27 29  2 20 21 13 10  8 23  9 16 22 17 12 24  5 30 15  4 11
 [889] 25 14 18 28  7 23 19 21  6  3 16  2  9  1  8 25 24 10 13 17 30 20 27 29
 [913]  4 15 26 22  5 11 14 23  6  1 12  2 18 26 16 28 19 21  7  3 25 27 24 20
 [937] 29 13  4 22  9 10 17 14 15  8 28  5 12  7 18 11 30  6 19 27 16 23  3 17
 [961]  2  9 13 21 10  8 20  1 24 22 26 28  4 11 25 12  2 14 15 29 30 21  3  6
 [985]  9 18  7  1 16  5 27 19  8 25 29 13 28 23 20  4

> T
[1] 27 19 26 16 17 20 29 25

> #Note T content was not changed after calling Bays_Durham

#plotting all 2-dim alttice points (integer points)
lat2D_ij <- function(m)
{
    LL <- NULL
    for (i in 0:(m-1))
    {
        for (j in 0:(m-1))
        {
            LL <- rbind(LL, c(i,j))
        }
    }
    LL
}

SIM <- length(BD)
plot(BD[1:SIM-1], BD[2:SIM],xlab="X[i]",ylab="X[i+1]")

L2D_ij <- lat2D_ij(31)
points(L2D_ij, col="red", cex=0.4)

##HW2: larger example with m=127
m <- 127
x0 <- 9
B <- 3
K <- 8

T <- LCG_seq(x0, B, m, K)

BD <- Bays_Durham(T, B, m, 5000)

SIM <- length(BD)
plot(BD[1:SIM-1], BD[2:SIM],xlab="X[i]",ylab="X[i+1]")

L2D_ij <- lat2D_ij(m)
points(L2D_ij, col="red", cex=0.3)


> T
[1]  27  81 116  94  28  84 125 121
> SIM <- length(BD)
> L2D_ij <- lat2D_ij(m)
> plot(BD[1:SIM-1], BD[2:SIM])

> points(L2D_ij, col="red", cex=0.3)
> BD[1:50]
 [1] 109  84  28  92  22 125  73  81   4  66 116  36  70  71 121  12  83  94 122
[20] 108 119 112  27 103  88  10  38  55  90  48  16  78  17  82  26  95  51  30
[39]  67  25  74  31  93  86  75  98 120 107  68 114


##HW2: larger example with m=127
m <- 127
x0 <- 9
B <- 3
K <- 8
N <- 5000 

#plotting all 2-dim alttice points (integer points)
lat2D_ij <- function(m)
{
    LL <- NULL
    for (i in 0:(m-1))
    {
        for (j in 0:(m-1))
        {
            LL <- rbind(LL, c(i,j))
        }
    }
    LL
}


# Bayes-Durham Shuffle Generator
Bays_Durham <- function(T, B, m, n)
{
K <- length(T) 
x0 <- LCG(T[K], B, m)
Y_list <- c(x0)
y <- x0
for (i in 1:(n-1))
{
    j <- y%%K+1
    y <- T[j]
    Y_list <- append(Y_list, y)
    x0 <- LCG(x0, B, m)
    T[j] <- x0
}
Y_list
}

Bays_Durham_Plot <- function(x0, B, m, K, N)
{
    T <- LCG_seq(x0, B, m, K)
    BD <- Bays_Durham(T, B, m, N)
    plot(BD[1:N-1], BD[2:N],xlab="X[i]",ylab="X[i+1]", cex=0.5)
    #L2D_ij <- lat2D_ij(m)
    #points(L2D_ij, col="red", cex=0.3)
}

m <- 31
x0 <- 9
B <- 3
K <- 8
N <- 5000 

#Bays_Durham_Plot(x0, B, m, K, N)


par(mfrow=c(1,2))
plot_LCG_u(1, 3, 31, 1000)
title(main = list(paste("a=3,m=127,c=0"),col="black", cex = 1.0))
N <- 1000; Bays_Durham_Plot(x0, B, m, K, N)
title(main = list(paste("Bays-Durham a=3,m=31,c=0"),col="black", cex = 1.0))

N <- 2000; Bays_Durham_Plot(x0, B, m, K, N)
N <- 4000; Bays_Durham_Plot(x0, B, m, K, N)
N <- 8000; Bays_Durham_Plot(x0, B, m, K, N)


m <- 31
x0 <- 9
B <- 3
K <- 8
N <- 5000 

#Bays_Durham_Plot(x0, B, m, K, N)

par(mfrow=c(2,2))
K <- 2; Bays_Durham_Plot(x0, B, m, K, N)
K <- 4; Bays_Durham_Plot(x0, B, m, K, N)
K <- 8; Bays_Durham_Plot(x0, B, m, K, N)
K <- 16; Bays_Durham_Plot(x0, B, m, K, N)



m <- 127
x0 <- 9
B <- 3
K <- 8
N <- 5000 

#Bays_Durham_Plot(x0, B, m, K, N)

par(mfrow=c(2,2))
N <- 1000; Bays_Durham_Plot(x0, B, m, K, N)
N <- 2000; Bays_Durham_Plot(x0, B, m, K, N)
N <- 4000; Bays_Durham_Plot(x0, B, m, K, N)
N <- 8000; Bays_Durham_Plot(x0, B, m, K, N)


m <- 127
x0 <- 9
B <- 3
K <- 8
N <- 5000 

#Bays_Durham_Plot(x0, B, m, K, N)

par(mfrow=c(2,2))
K <- 2; Bays_Durham_Plot(x0, B, m, K, N)
K <- 4; Bays_Durham_Plot(x0, B, m, K, N)
K <- 8; Bays_Durham_Plot(x0, B, m, K, N)
K <- 16; Bays_Durham_Plot(x0, B, m, K, N)

