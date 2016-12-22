Tablex=matrix(c(8,2,0,10,2,0,12,3,0,14,3,0,16,3,0,18,1,1,20,3,2,22,2,1,24
,1,0,26,1,1,28,1,1,32,1,0,34,1,1,38,3,2),ncol=3,byrow=T)

#dimnames(Tablex)=list(c(1:14),cat("LI","Number of  Cases","Number of  Remissions"))

Tablex
chisq.test(Tablex)
chisq.test(Tablex)$stdres

#h=c("LI","Number of Caes","Number of Remissions")


LI=c(8,10,12,14,16,18,20,22,24,26,28,32,34,38)
#Number of Cases=c(2,2,3,3,3,1,3,2,1,1,1,1,1,3)
#Number of Remissions=c(0,0,0,0,0,1,2,1,0,1,1,0,1,2)
#data1=data.frame(cbind(LI,Number of Cases,Number of Remissions))

Nc=c(2,2,3,3,3,1,3,2,1,1,1,1,1,3)
Nr=c(0,0,0,0,0,1,2,1,0,1,1,0,1,2)
data1=data.frame(cbind(LI,Nc,Nr))

data1$LI.bin<-ifelse(data1$LI>0,c(1,2,2,3,1,1,3,3,5,6,7,7,8,3),0)#adds a new column
data1

#(data1.fit.logit<-glm(LI.bin~LI, family=binomial, data=data1))
 data1

left=c(3,3,4)
yes=c(2,4,5)
feet=c(1,2,1)
tab=data.frame(left,yes,feet)
row.names(tab)=c("A","B","C")
tab


y60<-c(316.27, 316.81, 317.42, 318.87, 319.87, 319.43, 318.01, 315.74, 314.00, 313.68, 314.84, 316.03) 
 y70<-c(324.89, 325.82, 326.77, 327.97, 327.91, 327.50, 326.18, 324.53, 322.93, 322.90, 323.85, 324.96) 
y80<-c(337.84, 338.19, 339.91, 340.60, 341.29, 341.00, 339.39, 337.43, 335.72, 335.84, 336.93, 338.04) 
y90<-c(353.50, 354.55, 355.23, 356.04, 357.00, 356.07, 354.67, 352.76, 350.82, 351.04, 352.69, 354.07)  
y97<-c(363.23, 364.06, 364.61, 366.40, 366.84, 365.68, 364.52, 362.57, 360.24, 360.83, 362.49, 364.34) 
CO2<-data.frame(y60, y70, y80, y90, y97) 
row.names(CO2)<-c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") 
 CO2 




y60<-c(316.27, 316.81, 317.42, 318.87, 319.87, 319.43, 318.01, 315.74, 314.00, 313.68, 314.84, 316.03) 
 y70<-c(324.89, 325.82, 326.77, 327.97, 327.91, 327.50, 326.18, 324.53, 322.93, 322.90, 323.85, 324.96) 
y80<-c(337.84, 338.19, 339.91, 340.60, 341.29, 341.00, 339.39, 337.43, 335.72, 335.84, 336.93, 338.04) 
y90<-c(353.50, 354.55, 355.23, 356.04, 357.00, 356.07, 354.67, 352.76, 350.82, 351.04, 352.69, 354.07)  
y97<-c(363.23, 364.06, 364.61, 366.40, 366.84, 365.68, 364.52, 362.57, 360.24, 360.83, 362.49, 364.34) 
CO2<-data.frame(y60, y70, y80, y90, y97) 
row.names(CO2)<-c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") 
 CO2 


#c1=chisq.test(CO2)
#c2=chisq.test(CO2)$residuals
#n1=margin.table( c1,1)

#n2=margin.table( CO2,1)

#prop.table(CO2, 1)

#pag.tab <- xtabs(data=CO2)
y60<-c(316.27, 316.81, 317.42, 318.87, 319.87, 319.43, 318.01, 315.74, 314.00, 313.68, 314.84, 316.03) 
 y70<-c(324.89, 325.82, 326.77, 327.97, 327.91, 327.50, 326.18, 324.53, 322.93, 322.90, 323.85, 324.96) 
y80<-c(337.84, 338.19, 339.91, 340.60, 341.29, 341.00, 339.39, 337.43, 335.72, 335.84, 336.93, 338.04) 
y90<-c(353.50, 354.55, 355.23, 356.04, 357.00, 356.07, 354.67, 352.76, 350.82, 351.04, 352.69, 354.07)  
y97<-c(363.23, 364.06, 364.61, 366.40, 366.84, 365.68, 364.52, 362.57, 360.24, 360.83, 362.49, 364.34) 
CO2<-data.frame(y60, y70, y80, y90, y97) 
row.names(CO2)<-c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") 
 CO2 


CO2 <- matrix(c(y60, y70, y80, y90, y97) , nrow=12)

dimnames(CO2) =list(months=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
columns=c("y60", "y70", "y80", "y90", "y97"))
#row.names(r1)<-c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#column.names(r1.tab)=c("y60", "y70", "y80", "y90", "y97"))
CO2 <- as.table(CO2)
#CO2
#names(CO2)
margin.table(CO2, 1)
margin.table(CO2, 2)
addmargins(CO2)
prop.table(CO2, 1)
prop.table(CO2, 2)
round(prop.table(CO2, 1), 3)
chisq.test(CO2)
R1.chisq <- chisq.test(CO2)
names(R1.chisq)
R1.chisq$expected
with(R1.chisq, sum((observed - expected)^2/expected))
R1.chisq$residuals
with(R1.chisq, (observed - expected)/sqrt(expected))
n <- sum(CO2)
 n1 <- margin.table(CO2, 1)
n2 <- margin.table(CO2, 2)
n
n1
n2
(n1 %o% n2)/n  #expected chi sq values
R1.chisq$residual/sqrt((1-n1/n) %o% (1-n2/n))



myadjresids <- function(x) {
n <- sum(x)
n1 <- margin.table(x,1)
n2 <- margin.table(x,2)
expected <- (n1 %o% n2) / n
adj <- (1 - n1/n) %o% (1 - n2/n)
(x - expected) / sqrt(expected * adj)
}


myadjresids(CO2)



OME <-matrix(c(18,22,4,44,8,6,0,14,2,6,1,9,8,10,3,21), nrow=4
,dimnames=list(Age=c("<2","2-5",">6","All ages"), CEFGROUP=c("Total","0","1","2")))
 OME<- as.table(OME)
OME

