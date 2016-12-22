> 
> rm(list = ls(all = TRUE))
> 
> 
> click2<-function(){
+  x<-runif(1);y<-runif(1)
+  plot(x=x,y=y,xlim=c(0,1),main="please click on the circle",xlab="",ylab="",axes=FALSE,frame.plot=TRUE)
+  
+  clicktime<-system.time(xyclick<-locator(1))
+  d=sqrt(sum((x-xyclick$x)^2)+sum((y-xyclick$y)^2))
+  out=list(timestamp=Sys.time(),x=x,y=y,xclick=xyclick$x,yclick=xyclick$y,tclick=clicktime[3])
+  
+  dx=as.data.frame(out)
+  p=rbind(dx,data.frame(out))
+ 
+  out1=list(p=p)
+  return(out1)
+  }
> 
> 
> 
> 
> 
> 
> 
> 
> 
> 
> 
> 
> 
> clickright=function(n){
+ h=NULL
+ 
+ for (i in 1:n){
+ x=as.data.frame(print(click2()$p[1,]))
+ h=rbind(h,x)
+ write.table(h,file = "rightclick.txt",eol = "\n", row.names = TRUE,
+             col.names = TRUE)
+ 
+ }
+ 
+ return(h)
+ }
> 
> 
> r=as.matrix(clickright(50))
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:19 0.4980101 0.9438981 0.5032353 0.9512851   3.62
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 20:57:20 0.2621935 0.09394978 0.2692967 0.09382002   1.49
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:22 0.9260507 0.7720189 0.9312932 0.7727297   2.16
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:24 0.1477449 0.8933175 0.1548161 0.8982524   1.67
                  timestamp          x         y     xclick    yclick tclick
elapsed 2013-12-06 20:57:25 0.09861329 0.6974454 0.09757581 0.6980875   1.14
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 20:57:27 0.3483134 0.1109437 0.3514241 0.111812   1.83
                  timestamp         x        y    xclick    yclick tclick
elapsed 2013-12-06 20:57:29 0.8092918 0.401127 0.8118352 0.4033429   1.56
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:31 0.5100853 0.1621962 0.5206563 0.1615988   1.75
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:32 0.7694957 0.9503406 0.7720159 0.9555906   1.66
                  timestamp         x         y   xclick    yclick tclick
elapsed 2013-12-06 20:57:34 0.2829775 0.1335116 0.284229 0.1327126   1.57
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:36 0.8566777 0.7097882 0.8640981 0.7153431   2.17
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:38 0.8226172 0.5102596 0.8267674 0.5107294   1.36
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:40 0.3454531 0.1623421 0.3539128 0.1636126   1.95
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:42 0.8026066 0.9258705 0.8093465 0.9267229   2.39
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:46 0.3403733 0.9791959 0.3464467 0.9733357   3.87
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 20:57:48 0.9025265 0.04899577 0.9064061 0.04949199   1.63
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:50 0.4828655 0.3627932 0.4907918 0.3631272   2.52
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:54 0.2744896 0.3448661 0.2742741 0.3467712   3.73
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 20:57:56 0.8242665 0.07202592 0.8267674 0.07242382   1.73
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:57:58 0.9729834 0.3968953 0.9711125 0.3990879   2.18
                  timestamp          x         y     xclick    yclick tclick
elapsed 2013-12-06 20:58:01 0.06976771 0.1831736 0.07268873 0.1829206   2.64
                  timestamp         x          y    xclick    yclick tclick
elapsed 2013-12-06 20:58:02 0.7543568 0.07279559 0.5704305 0.0733653   0.76
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:03 0.5682769 0.7899997 0.5803853 0.7925455   1.17
                  timestamp         x        y   xclick    yclick tclick
elapsed 2013-12-06 20:58:05 0.7720491 0.766477 0.779482 0.7724755   2.08
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 20:58:07 0.1494682 0.7915942 0.1498387 0.790501   1.72
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:08 0.3365066 0.9610561 0.3364918 0.9685774   1.67
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:10 0.4673312 0.9752734 0.4758595 0.9761713   1.34
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 20:58:12 0.5989275 0.03512833 0.6052724 0.03516067   1.67
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 20:58:13 0.7767088 0.9975184 0.7819707 1.000733    1.9
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 20:58:15 0.1002589 0.05840565 0.1050419 0.05872831   1.89
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:17 0.7292191 0.5545094 0.7321965 0.5575727   1.51
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:18 0.7726805 0.5518872 0.7297078 0.5536657   0.68
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:20 0.3138963 0.4211254 0.3190709 0.4205438   1.74
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:21 0.5826022 0.7975665 0.5878514 0.7983008   1.86
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:24 0.1187916 0.1270157 0.1274403 0.1277174   2.37
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 20:58:25 0.8238206 0.05773491 0.8292562 0.05738938   1.51
                  timestamp        x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:27 0.255996 0.6209878 0.2568532 0.6244184   1.46
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:29 0.9701709 0.1840128 0.9736013 0.1854529   2.43
                  timestamp         x          y    xclick    yclick tclick
elapsed 2013-12-06 20:58:31 0.8073458 0.07306411 0.8093465 0.0729632   1.63
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:33 0.4096274 0.1911583 0.4111531 0.1917743   1.81
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:34 0.2554135 0.8432175 0.2643193 0.8439938   1.33
                  timestamp        x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:36 0.555425 0.5408648 0.5554982 0.5438527   1.58
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:39 0.2520394 0.6235642 0.2593419 0.6255736    2.7
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:41 0.9003938 0.2205272 0.9039174 0.2202227   1.85
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:42 0.2809395 0.2786392 0.2892064 0.2788958   1.57
                  timestamp         x        y    xclick    yclick tclick
elapsed 2013-12-06 20:58:44 0.8443996 0.801265 0.8516545 0.8038471   1.62
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:46 0.1969998 0.4758633 0.2021016 0.4784921   1.89
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:48 0.9457616 0.4040825 0.9487142 0.4063148   1.62
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:50 0.6413882 0.5720231 0.6450917 0.5712331   2.17
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 20:58:52 0.9479059 0.2664063 0.9512029 0.2666516   1.92
> 

> tclick.right=as.numeric(r[,6])
> dright=sqrt((as.numeric(r[,2])-as.numeric(r[,4]))^2+(as.numeric(r[,3])-as.numeric(r[,5]))^2)
> 
> 
> tclick.right=as.numeric(r[,6])
> dright=sqrt((as.numeric(r[,2])-as.numeric(r[,4]))^2+(as.numeric(r[,3])-as.numeric(r[,5]))^2)
> 
> clickleft=function(n){
+ h=NULL
+ 
+ for (i in 1:n){
+ x=as.data.frame(print(click2()$p[1,]))
+ h=rbind(h,x)
+ write.table(h,file = "leftclick.txt",eol = "\n", row.names = TRUE,
+             col.names = TRUE)
+ 
+ }
+ 
+ return(h)
+ }
> 
> 
> l=as.matrix(clickleft(50))
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:33 0.2475203 0.1042656 0.2468983 0.1046016   4.46
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:35 0.7933838 0.2068398 0.7969029 0.2060781   2.23
                  timestamp         x        y    xclick    yclick tclick
elapsed 2013-12-06 21:00:37 0.7313383 0.835549 0.7321965 0.8382415   1.46
                  timestamp         x         y   xclick    yclick tclick
elapsed 2013-12-06 21:00:39 0.9177694 0.2719634 0.916361 0.2722138   2.03
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 21:00:40 0.7037798 0.04822732 0.7048207 0.04827173   1.64
                  timestamp          x         y     xclick    yclick tclick
elapsed 2013-12-06 21:00:43 0.00688029 0.9143802 0.01295972 0.9173268   2.34
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:46 0.8362142 0.3567992 0.8342336 0.3587703   2.56
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:47 0.6948892 0.3037639 0.6973546 0.3040436   1.53
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 21:00:49 0.7524379 0.5061843 0.7521062 0.512476   1.77
                  timestamp         x        y    xclick    yclick tclick
elapsed 2013-12-06 21:00:52 0.4218983 0.149521 0.4235966 0.1493145   2.53
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:54 0.2083322 0.3986381 0.2095677 0.3980875   2.23
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:57 0.9724536 0.4663121 0.9785787 0.4613747   2.85
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:00:59 0.2730688 0.5692793 0.2767628 0.5724241   2.37
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:01 0.2013393 0.2305097 0.2045903 0.2312525    1.7
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:03 0.2864633 0.7842829 0.2892064 0.7886155   1.72
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:05 0.5177512 0.8984046 0.5206563 0.9054356   2.12
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:08 0.2708355 0.2655865 0.2742741 0.2646084   2.65
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 21:01:11 0.5829043 0.1670094 0.5803853 0.167932      3
                  timestamp         x         y   xclick    yclick tclick
elapsed 2013-12-06 21:01:14 0.2601861 0.1613589 0.266808 0.1615074   3.09
                  timestamp        x         y  xclick    yclick tclick
elapsed 2013-12-06 21:01:16 0.817742 0.2613117 0.82179 0.2615522   2.29
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:18 0.5098978 0.6844946 0.5181676 0.6835492   2.07
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:20 0.7989567 0.2632227 0.7969029 0.2646768    1.7
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:23 0.3917216 0.3510612 0.3937321 0.3497683   2.47
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:25 0.7769217 0.8057382 0.7819707 0.8101894   2.42
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 21:01:27 0.6140898 0.0596408 0.6152272 0.059833   1.75
                  timestamp       x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:28 0.65978 0.8941296 0.6650014 0.8970109   1.55
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:30 0.8781433 0.6764235 0.8790303 0.6817172    1.8
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:32 0.6563193 0.6168877 0.6625127 0.6188756   1.53
                  timestamp          x         y     xclick    yclick tclick
elapsed 2013-12-06 21:01:35 0.04056883 0.7596041 0.04531293 0.7655488   2.96
                  timestamp         x         y    xclick   yclick tclick
elapsed 2013-12-06 21:01:37 0.3149391 0.1154001 0.3190709 0.115772   1.76
                  timestamp           x         y     xclick    yclick tclick
elapsed 2013-12-06 21:01:39 0.007203894 0.3626729 0.01295972 0.3638416   1.95
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:41 0.6045784 0.1271433 0.6077611 0.1278456   2.29
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:43 0.4992762 0.7775407 0.5007466 0.7818361   1.52
                  timestamp          x         y     xclick    yclick tclick
elapsed 2013-12-06 21:01:45 0.03911892 0.7785573 0.04033551 0.7828582   2.62
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:48 0.7193226 0.5136806 0.7222417 0.5141536   2.45
                  timestamp           x         y     xclick    yclick tclick
elapsed 2013-12-06 21:01:49 0.008614203 0.2369287 0.01793714 0.2387829   1.59
                  timestamp         x         y   xclick   yclick tclick
elapsed 2013-12-06 21:01:52 0.9359382 0.8195939 0.941248 0.822235    2.8
                  timestamp        x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:54 0.562577 0.1260446 0.5704305 0.1264507   2.15
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:01:57 0.9427093 0.9769481 0.9437367 0.9823451   2.51
                  timestamp         x         y   xclick    yclick tclick
elapsed 2013-12-06 21:01:59 0.3789296 0.2782599 0.386266 0.2797971   2.06
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:02:01 0.7720619 0.9806785 0.7745046 0.9860961   1.87
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:02:03 0.1373958 0.8392246 0.1448613 0.8399973      2
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:02:05 0.5727409 0.3139446 0.5778966 0.3164015   2.23
                  timestamp         x        y    xclick    yclick tclick
elapsed 2013-12-06 21:02:07 0.8116772 0.375901 0.8168126 0.3771123   1.82
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:02:09 0.1905357 0.8206637 0.1946355 0.8251973   1.87
                  timestamp         x         y    xclick    yclick tclick
elapsed 2013-12-06 21:02:11 0.4356991 0.8666479 0.4360402 0.8714355   1.54
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 21:02:13 0.3595214 0.01605997 0.3713338 0.01611172   1.64
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 21:02:14 0.3769789 0.01771875 0.3738225 0.01777585   1.44
                  timestamp          x         y     xclick    yclick tclick
elapsed 2013-12-06 21:02:16 0.04228536 0.5660285 0.05029035 0.5678525   1.84
                  timestamp         x          y    xclick     yclick tclick
elapsed 2013-12-06 21:02:18 0.6399514 0.06981668 0.6401143 0.07020237      2
> 
> 

> 
> tclick.left=as.numeric(l[,6])
> dleft=sqrt((as.numeric(l[,2])-as.numeric(l[,4]))^2+(as.numeric(l[,3])-as.numeric(l[,5]))^2)
> 



> 
> tclick.left=as.numeric(l[,6])
> dleft=sqrt((as.numeric(l[,2])-as.numeric(l[,4]))^2+(as.numeric(l[,3])-as.numeric(l[,5]))^2)
> exploratory=function(x,y){
+ center.x <- mean(x); 
+ var.x <- var(x)
+ med.x<-median(x)
+ range.x=max(x)-min(x) 
+ center.y <- mean(y); 
+ var.y <- var(y)
+ med.y<-median(y) 
+ range.y=max(y)-min(y)
+ correlation<-cor(x,y)
+ 
+ out=list(variance.x=var.x,median.x=med.x,center.x=center.x,
+           variance.y=var.y,median.y=med.y,center.y=center.y,
+            range.x=range.x,range.y=range.y)
+ 
+ return(out)
+ 
+ }
> 
> 
> exploratory.time= exploratory(tclick.right,tclick.left)
> 
> exploratory.time
$variance.x
[1] 0.3956694

$median.x
[1] 1.735

$center.x
[1] 1.8814

$variance.y
[1] 0.3097682

$median.y
[1] 2.045

$center.y
[1] 2.1354

$range.x
[1] 3.19

$range.y
[1] 3.02

> 


> 
> exploratory.distance= exploratory(dright,dleft)
> 
> exploratory.distance
$variance.x
[1] 0.0006663789

$median.x
[1] 0.005373384

$center.x
[1] 0.00987794

$variance.y
[1] 6.299877e-06

$median.y
[1] 0.004989666

$center.y
[1] 0.004875333

$range.x
[1] 0.1827729

$range.y
[1] 0.01139378

> 
> 



> t.test(tclick.right,tclick.left,
+        alternative =  "less",
+        mu = 0, paired = FALSE, var.equal = FALSE,
+        conf.level = 0.95)

        Welch Two Sample t-test

data:  tclick.right and tclick.left
t = -2.1384, df = 96.568, p-value = 0.0175
alternative hypothesis: true difference in means is less than 0
95 percent confidence interval:
        -Inf -0.05673136
sample estimates:
mean of x mean of y 
   1.8814    2.1354 

>

> 
> #H0:dright=dleft
> #H0:dright<dleft
> 
> #variance obtained from EDA for both sample is the NOT  same
> #$variance.x
> #[1] 0.0006663789
> #$variance.y
> #[1] 6.299877e-06
> 
> t.test(dright,dleft,
+        alternative =  "less",
+        mu = 0, paired = FALSE, var.equal = FALSE,
+        conf.level = 0.95)

        Welch Two Sample t-test

data:  dright and dleft
t = 1.3639, df = 49.926, p-value = 0.9106
alternative hypothesis: true difference in means is less than 0
95 percent confidence interval:
       -Inf 0.01114985
sample estimates:
  mean of x   mean of y 
0.009877940 0.004875333 

> 
> 



> par(mfrow=c(2,2))
> shapiro.test(tclick.right)

        Shapiro-Wilk normality test

data:  tclick.right
W = 0.8719, p-value = 6.406e-05

> qqnorm(tclick.right,main="tclick.right")
> qqline(tclick.right)
> 
> 
> shapiro.test(tclick.left)

        Shapiro-Wilk normality test

data:  tclick.left
W = 0.8796, p-value = 0.0001088

> qqnorm(tclick.left,main="tclick.left")
> qqline(tclick.left)
> 
> 
> shapiro.test(dright)

        Shapiro-Wilk normality test

data:  dright
W = 0.2434, p-value = 1.294e-14

> qqnorm(dright,main="dright")
> qqline(dright)
> 
> shapiro.test(dleft)

        Shapiro-Wilk normality test

data:  dleft
W = 0.9783, p-value = 0.4837

> qqnorm(dleft,main="dleft")
> qqline(dleft)
> 



> ################wilcoxon signed rank#######################
> 
> 
> wilcox.test(tclick.right,tclick.left,
+             alternative =  "less",
+             mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
+             conf.int = FALSE, conf.level = 0.95)

        Wilcoxon rank sum test with continuity correction

data:  tclick.right and tclick.left
W = 863.5, p-value = 0.003893
alternative hypothesis: true location shift is less than 0

> 
> 
> 
> wilcox.test(dright,dleft,
+             alternative =  "less",
+             mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
+             conf.int = FALSE, conf.level = 0.95)

        Wilcoxon rank sum test with continuity correction

data:  dright and dleft
W = 1479, p-value = 0.9432
alternative hypothesis: true location shift is less than 0

> 

