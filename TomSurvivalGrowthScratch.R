#POSP: Surv= NFrag!!, size^2 (AIC), size (BIC); Growth = NFrag!! size^3 (AIC) size (BIC)
#POCS: Surv= size^2 (AIC), size (BIC); Growth = size
#MOSP: Surv= size^2, N_Frag (but Tom's doesn't like the size^2); Growth = size^2





#Scratch
#surv_dat
library(jtools)
head(PatchLevel)

POSPsurv=subset(surv_dat,Genus_Code=="POSP")
POCSsurv=subset(surv_dat,Genus_Code=="POCS")
MOSPsurv=subset(surv_dat,Genus_Code=="MOSP")

Sdata=na.omit(MOSPsurv)
head(Sdata)
dim(Sdata)

modS     <- glm(survival ~ size, family = "binomial" , data = Sdata)
summ(modS)

modS2     <- glm(survival ~ size+I(size^2), family = "binomial" ,  data = Sdata)
summ(modS2)

modSf     <- glm(survival ~ size+N_t0, family = "binomial" ,  data = Sdata)#
summ(modSf)

modSf2     <- glm(survival ~ size+I(size^2)+N_t0, family = "binomial" ,  data = Sdata)#
summ(modSf2)


AIC(modS,modS2,modSf,modSf2)
BIC(modS,modS2,modSf,modSf2)

Sdata$ModPred=predict(modSf,newdata=Sdata,type="response")
Sdata$Nbin=cut(Sdata$N_t0,breaks = c(0,1,2,3,5,10,1000),include.lowest = T)
#Numerical Assessment
Nsizebins=100
Sdata$sizebin=round(as.numeric(as.vector(cut(Sdata$size,
                                             breaks = quantile(Sdata$size,seq(0,1,length.out = Nsizebins)),
                                             labels = rollmean(quantile(Sdata$size,seq(0,1,length.out = Nsizebins)),2),
                                             include.lowest = T))),3)
Pmort=table(subset(Sdata,survival==1)$sizebin)/table(Sdata$sizebin)
Pm_df=data.frame(size=as.numeric(as.vector(names(Pmort))),Pm=as.vector(Pmort))

ggplot()+
  geom_jitter(data=Sdata,aes(x=size,shape=Nbin,y=survival),width=0,height=.1,color="black",alpha=1,size=.1)+
  geom_point(data=Sdata,aes(x=size,shape=Nbin,y=ModPred),color="blue",size=.1)+
  geom_point(data=Pm_df,aes(size,Pm),color="red")+
  facet_wrap("Nbin")


#Growth

allGdata=subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK")
#allG_pm_data=subset(ColonyLevel, TransitionTypeSimple == "SHRINK")# |TransitionTypeSimple == "SHRINK")
POSPgrow=subset(subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK"),Genus_Code=="POSP")
POCSgrow=subset(subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK"),Genus_Code=="POCS")
MOSPgrow=subset(subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK"),Genus_Code=="MOSP")

Gdata=na.omit(MOSPgrow)
head(Gdata)
dim(Gdata)

modG <- lm(log10_ESc ~ log10_SS, Gdata)
summ(modG)


modG2 <- lm(log10_ESc ~ log10_SS+I(log10_SS^2), Gdata)
summ(modG2)

modG3 <- lm(log10_ESc ~ log10_SS+I(log10_SS^2)+I(log10_SS^3), Gdata)
summ(modG3)

modGf <- lm(log10_ESc ~ log10_SS+N_t0, Gdata)
summ(modGf)

modG2f <- lm(log10_ESc ~ log10_SS+I(log10_SS^2)+N_t0, Gdata)
summ(modG2f)

modG3f <- lm(log10_ESc ~ log10_SS+I(log10_SS^2)+I(log10_SS^3)+N_t0, Gdata)
summ(modG3f)

#AIC(modG,modG2,modG3)
AIC(modG,modG2,modG3,modGf,modG2f,modG3f)
BIC(modG,modG2,modG3,modGf,modG2f,modG3f)

modGsimp=step(modG3f)
summ(modGsimp)


Gdata$ModPred=predict(modG,newdata=Gdata,type="response")
Gdata$Nbin=cut(Gdata$N_t0,breaks = c(0,1,2,3,5,10,1000),include.lowest = T)

# #Numerical Assessment
# Nsizebins=30
# Sdata$sizebin=round(as.numeric(as.vector(cut(Sdata$size,
#                                              breaks = quantile(Sdata$size,seq(0,1,length.out = Nsizebins)),
#                                              labels = rollmean(quantile(Sdata$size,seq(0,1,length.out = Nsizebins)),2),
#                                              include.lowest = T))),3)
# Pmort=table(subset(Sdata,survival==1)$sizebin)/table(Sdata$sizebin)
# Pm_df=data.frame(size=as.numeric(as.vector(names(Pmort))),Pm=as.vector(Pmort))
# 
ggplot()+
  geom_point(data=Gdata,aes(x=log10_SS,shape=Nbin,y=log10_ESc),width=0,height=.1,color="black",alpha=1,size=.1)+
  geom_point(data=Gdata,aes(x=log10_SS,shape=Nbin,y=ModPred),color="blue",size=.1)+geom_abline(color="red")
#  geom_point(data=Pm_df,aes(size,Pm),color="red")+
  #facet_wrap("Nbin")


