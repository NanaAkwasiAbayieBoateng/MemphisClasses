
#1
#a
x=c(10,15,50)
y=c(12,17,19)




#b
 perm.2sample(x,y,alternative ="greater" ,stat="meandiff")


#c
perm.2sample(x,y,alternative ="greater" ,stat="sumX")

#
perm.2sample(x,y,alternative ="greater" ,stat="mediandiff")


#2

R=c(3, 2, 1, 1, 2, 1,3, 2, 2 ,2 ,2 ,5, 1, 4 ,1 ,1, 1, 1 ,6 ,2 ,2 ,2 ,1, 1)
u=c( 1, 0, 1, 1, 0, 0, 1, 1 ,1 ,8, 1, 1 , 1 , 0 , 1 , 1 ,2)

#a
uR=c(R,u)
sort(rank(uR))
(m=length(R))
(n=length(u))
(rank.x=rank(uR)[1:m])
(w=sum(rank.x))


#b
t.test(R,u,"two.sided")
t.test(x,y,var.equal=T,alternative="two.sided")


#c


#d
Urban=u
Rural=R
par(mfrow=c(1,3))
hist(Rural)
hist(Urban)
boxplot(R,u,names=c("Rural","Urban"))


#3
#
Section_1 =c(5 ,11 ,16 ,8 ,12)
Section_2 =c(17 ,14 ,15, 21, 19 ,13)
length(Section_1)
length(Section_2)
#pwd=outer(Section_2,Section_1,"-")
pwd=outer(Section_1,Section_2,"-")
(pwd=sort(pwd))
median(pwd)
length(pwd)
pwd[6]
pwd[25]



wilcox.test(Section_2 ,Section_1,paired=FALSE)
wilcox.test(Section_1,Section_2 ,paired=FALSE)




wilcox.test(Section_2 ,Section_1,paired=TRUE)

k=c(77,78,70,72,65,74)
k1=c(60,62,70,76,68,72,70)
sort(c(k,k1))
#pwd=outer(k,k1,"-")
pwd=outer(k1,k,"-")
(pwd=sort(pwd))
median(pwd)




?outer

#4
Species_A =c(5.1 ,9.4, 7.2, 8.1, 8.8)
Species_B =c(2.5, 4.2 ,6.9, 5.5 ,5.3)

ks.test(Species_A,Species_B)



#5
Treatment_1 =c(21.9 ,20.2 ,19.4 ,20.3, 19.6, 20.4, 18.4 ,20.1, 22.0 ,18.9)
Treatment_2 =c(20.2, 13.8, 21.8, 19.2, 19.6, 25.5, 17.0, 17.6, 19.5 ,22.2)
sort(Treatment_1)
sort(Treatment_2)
length(Treatment_1)
length(Treatment_2)

sqrt(var(Treatment_1))
sqrt(var(Treatment_2))

TT=c(Treatment_1,Treatment_2)
y=sort(TT)
y
rank(y)
x=c(16.55,15.36,15.94,16.43,16.01)
yy=c(16.05,15.98,16.1,15.88,15.91)
sort(c(x,yy))


#5

# Loading the function
source("http://www.r-statistics.com/wp-content/uploads/2012/01/source_https.r.txt") # Making sure we can source code from github
source_https("https://raw.github.com/talgalili/R-code-snippets/master/siegel.tukey.r")
# Using the function
x =c(21.9 ,20.2 ,19.4 ,20.3, 19.6, 20.4, 18.4 ,20.1, 22.0 ,18.9)
y =c(20.2, 13.8, 21.8, 19.2, 19.6, 25.5, 17.0, 17.6, 19.5 ,22.2)
sd(x)
sd(y)
xy = c(x,y)
sort(x,y)
siegel.tukey(x,y)
