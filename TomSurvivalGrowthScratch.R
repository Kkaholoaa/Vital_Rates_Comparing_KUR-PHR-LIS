#POSP: Surv= NFrag!!, size^2 (AIC), size (BIC); Growth = NFrag!! size^3 (AIC) size (BIC)
#POCS: Surv= size^2 (AIC), size (BIC); Growth = size
#MOSP: Surv= size^2, N_Frag (but Tom's doesn't like the size^2); Growth = size^2


#Scratch
#surv_dat
library(jtools)
library(zoo)

load("data/Patch_And_Colony_Data_20210531.rdata") #load to get PatchLevel
load("data/Colony_Data_20210622_edited_survival.rdata") #load to get surv_dat
load("data/Colony_Data_20210622_edited.rdata") #load to update ColonyLevel (41 variables, incl log_10Esc)
head(PatchLevel)

POSPsurv=subset(surv_dat,Genus_Code=="POSP")
POCSsurv=subset(surv_dat,Genus_Code=="POCS")
MOSPsurv=subset(surv_dat,Genus_Code=="MOSP")

Sdata=na.omit(MOSPsurv)
head(Sdata)
dim(Sdata)

modS     <- glm(survival ~ size, family = "binomial" , data = Sdata) #simplest survival model
summ(modS)

modS2     <- glm(survival ~ size+I(size^2), family = "binomial" ,  data = Sdata) # +size^2
summ(modS2)

modSf     <- glm(survival ~ size+N_t0, family = "binomial" ,  data = Sdata)# +numb fragments
summ(modSf)

modSf2     <- glm(survival ~ size+I(size^2)+N_t0, family = "binomial" ,  data = Sdata)# +size^2 + numb fragments
summ(modSf2)

modSf3     <- glm(survival ~ size+I(size^2)+I(size^3)+N_t0, family = "binomial" ,  data = Sdata)# +size^2 +size^3 +numb fragments
summ(modSf3)

AIC(modS,modS2,modSf,modSf2,modSf3)
BIC(modS,modS2,modSf,modSf2,modSf3)


modSsimp=step(modSf3)
summ(modSsimp)

Sdata$ModPred=predict(modSf2,newdata=Sdata,type="response") #change the model type here
Sdata$Nbin=cut(Sdata$N_t0,breaks = c(0,1,2,3,5,10,1000),include.lowest = T)
#Numerical Assessment
Nsizebins=100
Sdata$sizebin=round(as.numeric(as.vector(cut(Sdata$size,
                                             breaks = quantile(Sdata$size,seq(0,1,length.out = Nsizebins)),
                                             labels = rollmean(quantile(Sdata$size,seq(0,1,length.out = Nsizebins)),2),
                                             include.lowest = T))),3)
Pmort=table(subset(Sdata,survival==1)$sizebin)/table(Sdata$sizebin)
Pm_df=data.frame(size=as.numeric(as.vector(names(Pmort))),Pm=as.vector(Pmort))

#plot data, model predictions, and prob of mortality
#want red dots to fit around blue line as close as possible
ggplot()+
  geom_jitter(data=Sdata,aes(x=size,shape=Nbin,y=survival),width=0,height=.1,color="black",alpha=1,size=.1)+ #black = data points
  geom_point(data=Sdata,aes(x=size,shape=Nbin,y=ModPred),color="blue",size=.1)+ #blue = model prediction
  geom_point(data=Pm_df,aes(size,Pm),color="red")+ #red = prob of mortality (what we're trying to approximate)
  facet_wrap("Nbin")

#POSP: best model fit according to AIC and BIC & stepwise regression is size+size^2+size^3+numb fragments, 
#but size^3 isn't better for MOSP or POCS (so drop size^3)
#POCS: best model fit segun AIC y BIC & stepwise regression es size or size+size^2 (doesn't care about fragments)
#MOSP: best model fit segun AIC y BIC & stepwise regression es size+size^2+numb fragments



#################
#Growth

allGdata=subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK")
#allG_pm_data=subset(ColonyLevel, TransitionTypeSimple == "SHRINK")# |TransitionTypeSimple == "SHRINK")
POSPgrow=subset(subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK"),Genus_Code=="POSP")
POCSgrow=subset(subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK"),Genus_Code=="POCS")
MOSPgrow=subset(subset(ColonyLevel, TransitionTypeSimple == "GROWTH" |TransitionTypeSimple == "SHRINK"),Genus_Code=="MOSP")

Gdata=na.omit(POSPgrow)
head(Gdata)
dim(Gdata)

modG <- lm(log10_ESc ~ log10_SS, Gdata) #normal growth model
summ(modG)

modG2 <- lm(log10_ESc ~ log10_SS+I(log10_SS^2), Gdata) #growth model + size^2
summ(modG2)

modG3 <- lm(log10_ESc ~ log10_SS+I(log10_SS^2)+I(log10_SS^3), Gdata) #growth model + size^2 + size^3
summ(modG3)

modGf <- lm(log10_ESc ~ log10_SS+N_t0, Gdata) #groth model + # frag
summ(modGf)

modG2f <- lm(log10_ESc ~ log10_SS+I(log10_SS^2)+N_t0, Gdata) #growth model+ size^2+ frag
summ(modG2f)

modG3f <- lm(log10_ESc ~ log10_SS+I(log10_SS^2)+I(log10_SS^3)+N_t0, Gdata) #growth model+ size^2+ size^3+ frag
summ(modG3f)

#AIC(modG,modG2,modG3)
AIC(modG,modG2,modG3,modGf,modG2f,modG3f)
BIC(modG,modG2,modG3,modGf,modG2f,modG3f)

#perform stepwise regression with backward elimination (adding or removing potential explanatory variables in succession and testing for stat sig
#after each iteration)
modGsimp=step(modG2f)
summ(modGsimp)


#POSP growth: size^2+ frag or size^2+ size^3+ frag (AIC), numb frag (BIC), size^2,size^3, numb frag (stepwise regression)

Gdata$ModPred=predict(modG2f,newdata=Gdata,type="response")
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


