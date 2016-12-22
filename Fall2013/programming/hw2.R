rm(list=ls())

#reading the survival data into R

Control.and.Treatment<-read.table("C:/Users/Owner/Documents/memphisclassesbooks/FALL2013/R PROGRAMMING/book2.txt",header=T,sep="\t")

Control.and.Treatment[,2]


#selecting  only   Control and Treatment group rows



x<-Control.and.Treatment[Control.and.Treatment[,2]=="ControlGoup",]

y<-Control.and.Treatment[Control.and.Treatment[,2]=="TreatmentGroup",]


#x= Control Group
#y=Treatment Group
Control.Group<-x[,1]
Treatment.Group<-y[,1]


Normal.plot<-function(x,y){
par(mfrow=c(3,1))
 hist(Control.Group,breaks = "Sturges")
hist(Treatment.Group,breaks = "Sturges")
}

Normal.plot(Control.Group,Treatment.Group)



######function to compute t test#########################

x=x[,1]
y=y[,1]
my.t.test<-function(x,y,alternative = c("Two.sided", "less", "greater"),df){

############finding mean of control and treatment#############

xbar=mean(x)
ybar=mean(y)
n=length(x)
m=length(y)
df=m+n-2
critical.value=0.05
##############finding pooled variance and test statistic##################

sp2=((n-1)*var(x)+(m-1)*var(y))/(n+m-2)
T1=(xbar-ybar)/sqrt(sp2*(1/m+1/n))




if(alternative== "Two.sided"){
cat('H0:mean.x=mean.y','\n')
cat('Ha:mean.x != meany','\n')

####################finding  p-value for two tailed test######################

p.value=2*(1-pt(abs(T1),df))

if (p.value<=critical.value){

cat('Reject H0','\n')

}else{

cat('Fail to reject H0','\n')

}



####################finding  p-value for rigth tailed test######################

}else if(alternative=="greater"){
cat('H0:mean.x=mean.y','\n')
cat('Ha:mean.x > meany','\n')
p.value=1-pt(T1,df)


if (p.value<=critical.value){

cat('Reject H0','\n')

}else{

cat('Fail to reject H0','\n')

}

}else{


####################finding  p-value for left tailed test######################

alternative=="less"
cat('H0:mean.x=mean.y','\n')
cat('Ha:mean.x <meany','\n')
p.value=pt(T1,df)}

if (p.value<=critical.value){

cat('Reject H0','\n')

}else{

cat('Fail to reject H0','\n')

}



  return(list(pvalue=p.value,Test.statistic=T1 ))
}


############function of my t test###################
my.t.test(x,y,alternative="Two.sided",15)


####################2 sample t test in R##########################
t.test(x, y,alternative = "two.sided",mu = 0, paired = FALSE, var.equal = TRUE,conf.level = 0.95)








######################## two sample permutation test#######################
my.permutation.dist<-function(x,y){


n<-length(x)
m<-length(y)
N<-n+m
numperm<-choose(N,n)
num.iterations<-numperm/2

################# the observed mean difference###################


T<-mean(x)-mean(y)


##################combined sample of control and treatment################

xy=c(x,y)

if(numperm>numperm){ 
cat("Number of permutations is too large,compute a smaller number of permutations as a sample of the total number of permutations")

}else{


mean.difference <- as.numeric(num.iterations)

for(i in 1:num.iterations){

# Sample numbers 1-N ,n times and store in perm

perm<-sample(1:N, n, replace = FALSE, prob = NULL)

# Assign the sampled values to control.perm

control.perm <- xy[perm]
 
#Assign remainder to treatment.perm

treatment.perm <- xy[-perm]

mean.difference[i] <- mean(control.perm) - mean(treatment.perm)
 
}


}
return(mean.difference)


}

my.permutation.dist(x,y)





############################## Plot of hitogram of difference in means###########################


hist(my.permutation.dist(x,y),breaks = "Sturges", xlab='Difference in Control and Traetment means', prob=T, main='')



#################Adding a line to indicate the observed value########

T<-(mean(x)-mean(y))


abline(v =T, untf = FALSE, col ='blue' , lty = 2, lwd = 2)


###########p-value#################

permutation.p.value=function(x,y){

T<-(mean(x)-mean(y))
p.value=mean(abs(my.permutation.dist(x,y)) >= abs(T))

return(p.value)
}


permutation.p.value(x,y)
