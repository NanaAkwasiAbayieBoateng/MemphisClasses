nchooser<-function(n,r){

#|| or use | and bracket#

if (n==r||r==0){
cat("called nchooser(n,n) or nchooser(n,0) \n")
return(1)

}else{
cat("called nchooser(",n,r,") \n",sep="")
return(nchooser(n-1,r-1)+nchooser(n-1,r))
}

}

nchooser(5,2)



grp<-c("control","treatment","treatment","control")
is.character(grp)
is.factor(grp)

grp<-factor(grp)
#grp is a factor with 4 elements and 2 levels
grp

grp<-factor(grp ,levels=c("control","treatment","baseline"))
grp
as.integer(grp)
levels(grp)
levels(grp)[as.integer(grp)]


phys.act<-c("L","H","H","L","M","M")
phys.act<-factor(phys.act,levels=c("L","M","H"),ordered=TRUE,labels=c("aLow","Medium","High"))
is.ordered(phys.act)
phys.act[2]>phys.act[1]
table(phys.act)
which(phys.act=="High")

hair<-c("blond","black","brown","black","gray","none")
hair<-factor(hair,levels=c("black","gray","brown","blond","white","none"))
table(hair)
as.numeric(hair)
is.factor(hair)
c(hair,5)
table(hair[hair=="gray" |hair=="none"])
table(hair[hair=="gray" |hair=="none"],drop=TRUE)
table(hair[hair=="gray" |hair=="none"],drop=TRUE)

y<-c(1.1,1.2,2.4,2.3,1.8,1.9)
x<-c(1,1,2,2,3,3)
reg<-lm(y~x)
summary(reg)
x<-factor(x)
anov<-lm(y ~x)
summary(anov)