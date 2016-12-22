################# ToxRes2014 Examples
mm <- data.frame(
  BUN = c(15.0, 15.2, 14.9, 15.4, 16.8, 14.1, 14.9, 14.5, 13.7, 12.9, 15.8, 16.6, 12.7, 15.4, 14.0, 
          20.8, 19.1, 18.7, 18.8, 17.5, 21.9, 17.5, 21.3, 20.4, 18.2, 15.4, 15.2, 13.0, 14.7, 15.7,
          15.7, 13.4, 15.1, 14.2, 15.2, 17.9, 18.2, 21.8, 18.2, 15.6, 22.3, 18.6, 22.0, 19.9, 19.6,
          21.0, 21.7, 19.7, 18.8, 18.6, 14.9, 15.2, 15.2, 12.4, 13.7, 14.2, 15.0, 14.5, 16.3, 17.1),
  Dose = rep(c(0, 62.5, 125, 250, 500, 1000),each=10)
)
mm$Dose<-as.factor(mm$Dose)
library(plyr)
library(ggplot2)
sumL <- ddply(mm, .(Dose), summarize, mean=mean(BUN), sd=sd(BUN), n=paste("n=", length(BUN), sep=""))
ggplot(mm, aes(x=Dose, y=BUN)) +
  geom_boxplot(outlier.size=0) +
  geom_point(position=position_jitter(h=0, w=0.2)) +
  geom_errorbar(data=sumL, aes(x=as.numeric(Dose)+0.45, y=NULL, ymin=mean-sd, ymax=mean+sd),
                width=0.2, position=position_identity(width=0.5), colour="red") +
  geom_point(data=sumL, aes(x=as.numeric(Dose)+0.45, y=mean), shape=3, colour="red") +
  geom_text(data=sumL, aes(y=min(mm$BUN), label=n), vjust=2) +
  xlab("Dose in mg/kg") +
  ylab("Blood urinary nirogen") +
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12),
        legend.position="none")


mbu<-lm(BUN~Dose, data=mm); anova(mbu)
# define contrast matrix for DuWi
duwil <- rbind("Du1" = c(-1, 0, 0, 0, 0, 1),
               "Du2" = c(-1, 0, 0, 0, 1, 0),
               "Du3" = c(-1, 0,0, 1, 0, 0),
               "Du4" = c(-1, 0,1, 0, 0, 0),
               "Du5" = c(-1, 1,0, 0, 0, 0),
               "Wi2" = c(-1, 0,0, 0, 0.5, 0.5),
               "Wi3" = c(-1, 0,0, 1/3, 1/3, 1/3),
               "Wi4" = c(-1, 0,1/4, 1/4, 1/4, 1/4),
               "Wi5" = c(-1, 1/5,1/5, 1/5, 1/5, 1/5),
               "U1" = c(-1, 0,0, 0.5, 0.5, 0),
               "U2" = c(-1, 0, 1/3, 1/3, 1/3, 0),
               "U3" = c(-1, 1/4, 1/4, 1/4, 1/4,0),
               "U4" = c(-1, 0, 0.5, 0.5,0, 0),
               "U5" = c(-1, 1/3, 1/3, 1/3, 0,0),
               "U6" = c(-1, 0.5, 0.5,0,0, 0))


library(multcomp)
## Dunnett test
aa<-summary(glht(mbu, linfct=mcp(Dose=duwil), alternative="greater"))
aa$test$pvalues

bb<-summary(glht(mbu, linfct=mcp(Dose="Dunnett"), alternative="greater"))
bb$test$pvalues


################################## example CP
library(mratios)
data(Mutagenicity)

Mutagenicity$Treat<-factor(Mutagenicity$Treatment, levels=c("Vehicle",
                                                            "Hydro30","Hydro50","Hydro75","Hydro100","Cyclo25"))

library(plyr)
library(ggplot2)
sumL <- ddply(Mutagenicity, .(Treat), summarize, mean=mean(MN), sd=sd(MN), n=paste("n=", length(MN), sep=""))
ggplot(Mutagenicity,aes(x=Treat, y=MN)) +
  geom_boxplot(outlier.size=0) +
  geom_point(position=position_jitter(h=0, w=0.2)) +
  geom_errorbar(data=sumL, aes(x=as.numeric(Treat)+0.45, y=NULL, ymin=mean-sd, ymax=mean+sd),
                width=0.2, position=position_identity(width=0.5), colour="red") +
  geom_point(data=sumL, aes(x=as.numeric(Treat)+0.45, y=mean), shape=3, colour="red") +
  geom_text(data=sumL, aes(y=min(Mutagenicity$MN), label=n), vjust=2) +
  xlab("Dose in mg/kg") +
  ylab("Number of Micronuclei") +
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12),
        legend.position="none")

library(pairwiseCI)
pairwiseCI(MN ~ Treat, data=Mutagenicity, method="Param.ratio", var.equal=FALSE, conf.level=0.90)
pairwiseCI(MN ~ Treat, data=Mutagenicity, method="Param.ratio", var.equal=FALSE, 
           conf.level=0.90, control="Cyclo25")
