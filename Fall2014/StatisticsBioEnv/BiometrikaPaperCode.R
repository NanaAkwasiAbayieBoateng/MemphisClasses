library(lattice)
library(CorrBin)

#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code

########
# Basic descriptives
########

# mean cluster size
by(edge, edge["Trt"], function(x)sum(x$ClusterSize*x$Freq)/sum(x$Freq))
fisher.test(xtabs(Freq~ClusterSize+Trt, data=edge))

# prob(affected)
by(edge, edge["Trt"], function(x)sum(x$NResp*x$Freq)/sum(x$ClusterSize*x$Freq))

# prob(>0 affected in cluster)
by(edge, edge["Trt"], function(x)sum((x$NResp>0)*x$Freq)/sum(x$Freq))
# prob(>0 affected) by clustersize
paff <- by(edge, edge[c("Trt","ClusterSize")], function(x)sum((x$NResp>0)*x$Freq)/sum(x$Freq))[,]
sstab <- xtabs(Freq~ClusterSize, data=edge, subset=ClusterSize>4 & ClusterSize<14)
ssprob <- sstab/sum(sstab)
apply(paff[,as.character(5:13)], 1, function(x) x %*% ssprob) 


# test of MC
mc.test.chisq(edge)

##################
# survival curves
##################

panel.cumsum <- function(x,y,...){
  x.ord <- order(x)
  panel.xyplot(x[x.ord], cumsum(y[x.ord]), ...)}

uncond.surv.fun <- function(cbdata, pool.n = TRUE){
	#calculates P(R>=r)=\sum_n P(R>=r|N=n)*P(N=n)
	#for each Trt group
	#if pool.n=T, then P(N=n) is computed over all treatment groups
	#   otherwise it is different for each group
	
	usf <- function(td){
		surv.r <- lapply(0:max(td$ClusterSize), function(r){sum(td$Freq[td$NResp>=r])})
		surv.r <- unlist(surv.r)/sum(td$Freq)
		data.frame(NResp=0:max(td$ClusterSize), Surv=surv.r, Trt=td$Trt[1])
	}
	usf.pool <- function(td){     #uses the globally defined prob.n for P(N=n)
	  M <- length(prob.n)  #max cluster size
	  freq.tab <- xtabs(Freq~factor(NResp, levels=0:M) +
		                        factor(ClusterSize,levels=1:M), data=td)
    prob.tab <- sweep(freq.tab,2,colSums(freq.tab), "/")
		surv.rn <- apply(prob.tab, 2, function(x)rev(cumsum(rev(x))))
		surv.rn[is.na(surv.rn)] <- 0  #NA means no clusters of size n in this trt group
		surv.r <- surv.rn %*% prob.n
		surv.r <- surv.r/surv.r[1]  #surv.r[1] is not 1 because of the missing cluster sizes
		data.frame(NResp=0:M, Surv=surv.r[,1], Trt=td$Trt[1])
	}

	prob.n <- xtabs(Freq~factor(ClusterSize,levels=1:max(ClusterSize)), data=cbdata)
  prob.n <- prob.n/sum(prob.n)

  res <- by(cbdata, list(Trt=cbdata$Trt), if (pool.n) usf.pool else usf)
  do.call(rbind, res)
}   

us.edge <- uncond.surv.fun(edge, pool.n=FALSE)

#PDF plot
trellis.device(device="pdf", file="EDGErawsurvBW.pdf", color=F)
trellis.par.set(list(superpose.line=list(col=c("black",gray.colors(7)), lwd=2)))

xyplot(Surv~NResp, data=us.edge, groups=Trt, 
       panel=function(...){ref <- trellis.par.get("reference.line")                           panel.abline(h=c(0,1), col=ref$col, lty=ref$lty, lwd=ref$lwd)
                           panel.superpose(...)},
       type="s",ylim=c(-0.05,1.05), xlim=c(0,20),
       as.table=T, scales=list(x=list(alternating=1,tck=c(1,0), at=seq(0,16,by=4)),
       y=list(alternating=3)), xlab="Number affected r", ylab="Survival function P(R>=r)",
       auto.key=list(columns=4, lines=T, points=F, title="Dose, mg/kg", cex.title=1))
dev.off()
#EPS plot for Biometrika - no in-plot legend, no gray
trellis.device(device="postscript", file="art/EDGErawsurvBW.eps", color=F,width=6, height=4, horizontal=F)
trellis.par.set(list(superpose.line=list(col=c("black"), lwd=1.4)))

xyplot(Surv~NResp, data=us.edge, groups=Trt, 
       panel=function(...){ref <- trellis.par.get("reference.line") 
                           panel.abline(h=c(0,1), col=ref$col, lty=ref$lty, lwd=ref$lwd)
                           panel.superpose(...)},
       type="s",ylim=c(-0.05,1.05), xlim=c(0,16),
       as.table=T, scales=list(alternating=1,tck=c(1,0), 
         x=list(at=seq(0,16,by=4))), 
       xlab="Number affected r", ylab=expression(P(R>=r)),
       auto.key=F)
dev.off()

edge.reprod <- mc.est(edge)

prob.n <- xtabs(Freq~factor(ClusterSize,levels=0:max(ClusterSize))+Trt, data=edge)
prob.n <- sweep(prob.n,2,colSums(prob.n),FUN="/")
edge.reprod$Freq <- edge.reprod$Prob * prob.n[cbind(edge.reprod$ClusterSize+1, unclass(edge.reprod$Trt))]
us.edge.reprod <- uncond.surv.fun(edge.reprod)

us.edge <- merge(us.edge, us.edge.reprod, by=c("NResp","Trt"), suff=c(".raw",".reprod"))
 #add stoch ordered (Ha) and H0 estimates 

edge.so <- SO.mc.est(edge, control=soControl(eps=0.05, max.iter=5000, verbose=F, method="ISDM", max.directions=500))  
edge.so$Freq <- edge.so$Prob * prob.n[cbind(edge.so$ClusterSize, unclass(edge.so$Trt))]
us.edge.so <- uncond.surv.fun(edge.so)
us.edge <- merge(us.edge, us.edge.so, by=c("NResp","Trt"))
names(us.edge)[names(us.edge)=="Surv"] <- "Surv.so"


edge0 <- edge
edge0$Trt <- factor(rep("Eq", nrow(edge0)))
edge0.reprod <- mc.est(edge0)
edge0.reprod <- subset(edge0.reprod, ClusterSize>0)
edge0.reprod$Freq <- edge0.reprod$Prob * prob.n[cbind(edge0.reprod$ClusterSize, unclass(edge0.reprod$Trt))]
us.edge0 <- uncond.surv.fun(edge0.reprod)
names(us.edge0)[names(us.edge0)=="Surv"] <- "Surv.H0"
us.edge <- merge(us.edge, subset(us.edge0, select=-Trt), by="NResp")

#PDF plot
trellis.device(device="pdf", file="EDGE4surv.pdf", color=FALSE, width=10, height=4)
trellis.par.set(list(superpose.line=list(col=c("black",gray.colors(7)))))

xyplot(Surv.raw+Surv.reprod+Surv.so+Surv.H0~NResp|Trt, data=us.edge,  outer=F, allow.multiple=T,
       panel=function(...){ref <- trellis.par.get("reference.line") 
                           panel.abline(h=c(0,1), col=ref$col, lty=ref$lty, lwd=1)
                           panel.superpose(...)},
       type="s",ylim=c(-0.05,1.05), xlim=c(0,16), layout=c(4,1),   lwd=2,
       as.table=T, scales=list(x=list(alternating=1,tck=c(1,0), at=seq(0,16,by=4)),
       y=list(alternating=3)), xlab="Number affected r", ylab="Survival function P(R>=r)",
       key=list(text=list(c("Raw","Marginal\nCompatibility","Ha","H0")), columns=4, 
                lines=list(col=trellis.par.get("superpose.line")$col[1:4],
                           lty=trellis.par.get("superpose.line")$lty[1:4],
                           lwd=2), 
                title="Estimator", cex.title=1))
dev.off()
#EPS plot for Biometrika - no in-plot legend, no gray
trellis.device(device="postscript", file="art/EDGE4surv.eps", color=F,width=6, height=2.5, horizontal=F)
trellis.par.set(list(superpose.line=list(col=c("black"), lwd=1.4)))

xyplot(Surv.raw+Surv.reprod+Surv.so+Surv.H0~NResp|Trt, data=us.edge,  outer=F, allow.multiple=T,
       panel=function(...){ref <- trellis.par.get("reference.line") 
                           panel.abline(h=c(0,1), col=ref$col, lty=ref$lty, lwd=1)
                           panel.superpose(...)},
       type="s",ylim=c(-0.05,1.05), xlim=c(0,16), layout=c(4,1),   
       as.table=T, scales=list(alternating=1,tck=c(1,0), x=list(at=seq(0,12,by=4))), 
       xlab="Number affected r", ylab=expression(P(R>=r)))
dev.off()
                                       
#estimates for risk assessment

#MC
 p.mc <- subset(edge.reprod, ClusterSize==1 & NResp==1)$Prob
 p.aff.mc <-  by(edge.reprod, edge.reprod[c("Trt","ClusterSize")], function(x)sum((x$NResp>0)*x$Prob))[,]
 p.aff.mc <-  rowSums(p.aff.mc * t(prob.n), na.rm=T)
#Ha
 p.Ha <- subset(edge.so, ClusterSize==1 & NResp==1)$Prob
 p.aff.Ha <-  by(edge.so, edge.so[c("Trt","ClusterSize")], function(x)sum((x$NResp>0)*x$Prob))[,]
  p.aff.Ha <- rowSums(p.aff.Ha * t(prob.n[-1,]), na.rm=T)

library(xtable)
xtable(signif(cbind(p.mc, p.Ha, p.aff.mc, p.aff.Ha),3),file="")
 
#NOSTASOT
n.edge <- NOSTASOT(edge, R=100, control=soControl(eps=0.01, max.directions=250))


#log-likelihoods
#raw
edge2 <- by(edge, list(edge$Trt, edge$ClusterSize), function(x)cbind(x, Prob=x$Freq/sum(x$Freq)))
edge2 <- do.call(rbind, edge2)
with(edge2, sum(Freq*log(Prob)))
#Ha
edge.so.MLE <- SO.mc.est(edge, control=soControl(eps=0.01, max.iter=5000, verbose=F, 
                                   method="ISDM", max.directions=500))
attr(edge.so.MLE, "loglik")
#MC
edge.reprod$Freq <- NULL
edge.reprod2 <- merge(edge, edge.reprod)
with(edge.reprod2, sum(Freq*log(Prob)))

#########
# Simulation study
# #######

set.seed(346361)
cs.distr <- xtabs(Freq~ClusterSize, data=edge)
cs.distr <- cs.distr/sum(cs.distr)
N <- 30
ss <- sample(as.numeric(names(cs.distr)), size=N, prob=cs.distr, replace=TRUE)
ss.tab <- as.data.frame(table(ss))
names(ss.tab)[1] <- "ClusterSize"
ss.sim <- rbind(data.frame(Trt="Control", ss.tab),
                data.frame(Trt="Treated", ss.tab))
ss.sim$ClusterSize <- as.numeric(as.character(ss.sim$ClusterSize))

power.simul <- function(sample.sizes,p.gen.fun,rho.gen.fun,
                        test=c("RS","GEE","GEEtrend","GEEall","SO"), exact=test=="SO",
                        pdf.fun=qpower.pdf, R=101, R.exact=100, control=list()){
 test <- match.arg(test)
  run.one <- function(){
    rd <- ran.CBData(sample.sizes, p.gen.fun=p.gen.fun, rho.gen.fun=rho.gen.fun, pdf.fun=pdf.fun)
    res <- trend.test(rd, test=test, exact=exact, R=R.exact, control=control)
    p.val <- res$p.val
    p.val
  }
  
  pvals <- numeric(R)
  for (i in 1:R) pvals[i] <- run.one()
  pow <- mean(pvals<=0.05)
  list(power=pow, p.vals=pvals)
}

run.power.sim <- function(test.vec, p.fun.list, rho.fun.list,...){
  res <- list()
  for (i in seq(along=test.vec)){
    res[[i]] <- matrix(NA, nr=length(p.fun.list), nc=length(rho.fun.list))
    for (pidx in seq(along=p.fun.list)){
      for (ridx in seq(along=rho.fun.list)){
         res[[i]][pidx,ridx] <- power.simul(test=test.vec[[i]],
                                            p.gen.fun=p.fun.list[[pidx]],
                                            rho.gen.fun=rho.fun.list[[ridx]],
                                            ...)$power
      }
    }
  }
  names(res) <- test.vec
  res
 }

lin.fun <- function(val.min, by)
   {function(g){val.min + by*(g-1)}}

set.seed(34631)
p.fun.list <- list(lin.fun(0.1,0), lin.fun(0.1,0.2), lin.fun(0.7,0.2))
rho.fun.list <- list(lin.fun(0.2,-0.2), lin.fun(0.2,0), lin.fun(0.2,0.2))
esim1 <- run.power.sim(test.vec=c("RS","GEE","SO"), 
              p.fun.list=p.fun.list, rho.fun.list=rho.fun.list,
              sample.sizes=ss.sim, R=1001, exact=TRUE, R.exact=100, pdf.fun=qpower.pdf, 
              control=mixControl(eps=0.1, max.directions=250))

# plot SF of the q-power distributions

plot.q2 <- function(p.idx, rho.idx, cols){
  p <- p.fun.list[[p.idx]](1:2)                                             
  rho <- rho.fun.list[[rho.idx]](1:2)
  plot(0:13, 1-cumsum(qpower.pdf(p[1], rho[1], 13)), col=cols[1], lty=1, type="l", ylim=c(0,1),
       xaxt="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", lwd=2)
  lines(0:13, 1-cumsum(qpower.pdf(p[2], rho[2], 13)), col=cols[2], lty=2, lwd=2)
}

cls <- c("black","black")
#pdf(file="QPowerCurves%01d.pdf",onefile=FALSE)
postscript(file="art/QPowerCurves%01d.eps",onefile=FALSE, horizontal=FALSE, width=2, height=2, paper="special")
  par(mar=c(0,0,0,0),pty="s",bty="l")
  for (i in 1:3){
    for (j in 1:3){
      plot.q2(i,j,cls)
    }
  }
dev.off()

#asymptotic GEE and RS tests
set.seed(34631)
p.fun.list <- list(lin.fun(0.1,0), lin.fun(0.1,0.2), lin.fun(0.7,0.2))
rho.fun.list <- list(lin.fun(0.2,-0.2), lin.fun(0.2,0), lin.fun(0.2,0.2))
esim2 <- run.power.sim(test.vec=c("RS","GEE"), 
              p.fun.list=p.fun.list, rho.fun.list=rho.fun.list,
              sample.sizes=ss.sim, R=1001, exact=FALSE, R.exact=100, pdf.fun=qpower.pdf, 
              control=mixControl(eps=0.1, max.directions=250))

# heteroskedastic GEE

set.seed(34631)
p.fun.list <- list(lin.fun(0.1,0), lin.fun(0.1,0.2), lin.fun(0.7,0.2))
rho.fun.list <- list(lin.fun(0.2,-0.2), lin.fun(0.2,0), lin.fun(0.2,0.2))
esim3 <- run.power.sim(test.vec=c("GEEall"), 
              p.fun.list=p.fun.list, rho.fun.list=rho.fun.list,
              sample.sizes=ss.sim, R=1001, exact=TRUE, R.exact=100, pdf.fun=qpower.pdf)


 
      
