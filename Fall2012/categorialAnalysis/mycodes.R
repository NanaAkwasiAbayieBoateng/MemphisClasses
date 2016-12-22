# function to compute Correlation coefficient
#simplified form of rho2

rho2=function(lambda1,lambda2){
rho=(1/(lambda1-lambda1^2))*(lambda2-lambda1^2)

return(rho)

}

x=c(.579,.5,.591,.875) #CEF treated group
y=c(.477,.444,.455,.750) #CEF treated group
 rho2(x,y)

x1=c(.486,.2,.611,.857)  #AMO treated group
y1=c(.419,.133,.556,.857)  #AMO treated group
rho2(x1,y1)






# computing rho 2

# function for combination



CC=function(n,r){
C1=ncol(combn(n,r))

return(C1)

}

#CC(5,1)
#k=2
l=c(1,.579,.477)
#l[2]^(3-1)
rho22=function(k,l){
rho=0
for(j in 1:(k+1))
{rho=rho + ((((-1)^((k+1)-j))*CC(k,(j-1))*(l[2]^(k-j))*l[j])/((l[2]-(l[2])^2)^(k/2)))
}
return(rho)
}
l=c(1,.579,.477)



rho22(2,l)



n=3
r=1
CC(n,r)

########################################
#function to compute lambdas

####All
A=c(14,9,21)


lam=function(L,m,A,n){
lam1=0
for(j in 0:(n-L))
{lam1=lam1 + CC((n-j),L)*A[j]
lam2=1/(m*CC(n,L))

lam3=lam2*lam1}
return(lam3)
}

lam(2,44,A,2)

#computing lambdas for the CEF group
# All ages,m=44
A4=c(14,9,21)

la=function(m,A4){


{la1=(1/(2*m))*(2*A4[3]+A4[2]);
la2=(1/m)*A4[3]}
return(list("la1"=la1,"la2"=la2))
}
la(44,A4)

# < 2,m=18
A=c(8,2,8)

la=function(m,A){


{la1=(1/(2*m))*(2*A[3]+A[2]);
la2=(1/m)*A[3]}
return(list("la1"=la1,"la2"=la2))
}
la(18,A)


#  2-5,m=22
A=c(6,6,10)

la=function(m,A){


{la1=(1/(2*m))*(2*A[3]+A[2]);
la2=(1/m)*A[3]}
return(list("la1"=la1,"la2"=la2))
}
la(22,A)


# >=6,m=4
A=c(0,1,3)

la=function(m,A){


{la1=(1/(2*m))*(2*A[3]+A[2]);
la2=(1/m)*A[3]}
return(list("la1"=la1,"la2"=la2))
}
la(4,A)



#############################################
#computing lambdas for the AMO group
# All ages,m=31
A4=c(15,3,13)

la=function(m,A4){


{la1=(1/(2*m))*(2*A4[3]+A4[2]);
la2=(1/m)*A4[3]}
return(list("la1"=la1,"la2"=la2))
}
la(31,A4)

# < 2,m=15
A=c(11,2,2)

la=function(m,A){


{la1=(1/(2*m))*(2*A[3]+A[2]);
la2=(1/m)*A[3]}
return(list("la1"=la1,"la2"=la2))
}
la(15,A)


#  2-5,m=9
A=c(3,1,5)

la=function(m,A){


{la1=(1/(2*m))*(2*A[3]+A[2]);
la2=(1/m)*A[3]}
return(list("la1"=la1,"la2"=la2))
}
la(9,A)


# >=6,m=7
A=c(1,0,6)

la=function(m,A){


{la1=(1/(2*m))*(2*A[3]+A[2]);
la2=(1/m)*A[3]}
return(list("la1"=la1,"la2"=la2))
}
la(7,A)


A=c(14,9,21)


lam=function(L,m,A){
lam1=0
for(j in 1:(2-L))
{lam1=lam1 + A[j]
lam2=1/(m*CC(2,L))

lam3=lam2*lam1}
return(lam3)
}

lam(l,44,A)

