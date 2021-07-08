#Tom_Plotting
library(ggplot2)
library(scales)
library(patchwork)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
head(PatchLevel)

#############################
# Rec and Cover
#############################

#Get to clean Area Surveyed 
Asurv=read.csv("./data/Area surveyed_N_Circrats.csv")
Asurv$StartingDate=mdy(Asurv$Date)
Asurv_l=Asurv[,c("Site","StartingDate","POCS","POSP","MOSP")]%>%pivot_longer(cols=c("POSP","MOSP","POCS"),#,"dSE.POSP","dSE.MOSP","dSE.POCS"
                     names_to=c("Genus_Code"),values_to=c("Ncircrats"))
Rec.S_T=ddply(ColonyLevel,.(Site,StartingDate,Genus_Code),summarize,Nrec=length(which(TransitionTypeSimple=="RECR")))
Rec.S_T.A=left_join(Rec.S_T,Asurv_l)
Rec.S_T.A$A.Surv.m2=Rec.S_T.A$Ncircrats*2.5
Rec.S_T.A$A.Surv.cm2=Rec.S_T.A$Ncircrats*25000
Atax=ddply(ColonyLevel,.(Site,StartingDate,Genus_Code),summarise,A.adult_cm2=sum(StartingSize))
Rec.S_T.A=left_join(Rec.S_T.A,Atax)
Rec.S_T.A$Rec.m2=Rec.S_T.A$Nrec/Rec.S_T.A$A.Surv.m2
Rec.S_T.A$Rec.Ad_cm2=Rec.S_T.A$Nrec/Rec.S_T.A$A.adult_cm2

hist(Rec.S_T.A$Rec.Ad_cm2)
FIX=ggplot(Rec.S_T.A,aes(Rec.Ad_cm2))+geom_histogram()+facet_grid("Genus_Code")+xlim(c(0,.3))

#Juv Data, Cover Data
juv <- read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS.csv")
juv_site=read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_SITE.csv")
cov=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier3_SECTOR.csv")
cov_site=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE.csv")
jmhi=subset(juv,REGION=="MHI"&GENUS_CODE%in%c("POCS","MOSP","POSP"))
jmhi_site=subset(juv_site,REGION=="MHI"&GENUS_CODE%in%c("POCS","MOSP","POSP"))
cmhi=subset(cov,Mean.REGION=="MHI")
cmhi_site=subset(cov_site,REGION=="MHI")

#Get Sector Level Cover, POSP, MOSP, POCS # 5 cm diameter.
TaxSec_i=which(substr(names(cmhi),1,5)=="Mean."&!substr(names(cmhi),6,50)%in%c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR","TOT_AREA_WT","N"))
names(cmhi)[TaxSec_i]
rowSums(cmhi[,TaxSec_i])
cmhi$Mean.MOSP=rowSums(cmhi[,c("Mean.MOEN","Mean.MOBR","Mean.MOFO")])#"Mean.MONE",
cmhi$PooledSE.MOSP=sqrt(rowSums((sqrt(cmhi$Mean.N)*cmhi[,c("PooledSE.MOEN","PooledSE.MOBR","PooledSE.MOFO")])^2))/sqrt(cmhi$Mean.N)#"Mean.MONE",
cmhi$Mean.POSP=rowSums(cmhi[,c("Mean.POMA","Mean.POEN")])
cmhi$PooledSE.POSP=sqrt(rowSums((sqrt(cmhi$Mean.N)*cmhi[,c("PooledSE.POMA","PooledSE.POEN")])^2))/sqrt(cmhi$Mean.N)
metacol_sec=c("Mean.REGION","Mean.ISLAND","Mean.ANALYSIS_SEC","Mean.ANALYSIS_YEAR","Mean.N")
cov_sec_w=cmhi[,c(metacol_sec,"Mean.POSP","Mean.MOSP","Mean.POCS","PooledSE.POSP","PooledSE.MOSP","PooledSE.POCS")]
names(cov_sec_w)=substr(names(cov_sec_w),6,99)
cov_long=cov_sec_w %>% 
  pivot_longer(cols=c("POSP","MOSP","POCS"),#,"dSE.POSP","dSE.MOSP","dSE.POCS"
               names_to=c("GENUS_CODE"),values_to=c("cover.mn"))
cov_sec=cov_long %>% 
  pivot_longer(cols=c("dSE.POSP","dSE.MOSP","dSE.POCS"),
               names_to=c("dSE.Genus"),values_to=c("cover.se"))
cov_sec=select(cov_sec,-dSE.Genus)

names(jmhi)[which(names(jmhi)=="Sector")]="ANALYSIS_SEC"
juv_sec=jmhi[,c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR","GENUS_CODE","n","Mean_JuvColDen","SE_JuvColDen")]
names(juv_sec)[names(juv_sec)=="n"]="N.juv"
names(cov_sec)[names(cov_sec)=="N"]="N.cov"
cov_sec$ANALYSIS_YEAR[cov_sec$ANALYSIS_YEAR=="2013-15"]="2013"
cov_sec=subset(cov_sec,ANALYSIS_YEAR%in%c("2013","2016","2019"))
cov_sec$ANALYSIS_YEAR=as.numeric(as.vector(cov_sec$ANALYSIS_YEAR))
sec=na.omit(left_join(juv_sec,cov_sec))
sec$coverP.mn=sec$cover.mn/100
sec$coverP.se=sec$cover.se/100
sec$coverP.var=(sqrt(sec$N.cov)*sec$coverP.se)^2
sec$juv.var=(sqrt(sec$N.juv)*sec$SE_JuvColDen)^2
sec$JuvColDen.mn_cm2=sec$Mean_JuvColDen/10000
sec$juv_mod.mn.pcm2=sec$JuvColDen.mn_cm2/(sec$coverP.mn)
sec$juv_mod.var.pcm2=sec$juv_mod.mn.pcm2*sqrt((sec$juv.var/sec$Mean_JuvColDen)^2+(sec$coverP.var/sec$coverP.mn)^2)
sec$juv_mod.se.pcm2=sqrt(sec$juv_mod.var)/sqrt(sec$N.juv)


REA=ggplot(sec,aes(x = juv_mod.mn.pcm2))+
  geom_histogram()+
  #geom_errorbar()+
  facet_grid(GENUS_CODE~.)+
  xlab("Juvenile Density per Adult cm^2 area")+xlim(c(0,.3))

FIX+REA



ggplot(sec,aes(x = ANALYSIS_SEC,y =juv_mod.mn.pcm2,ymin =juv_mod.mn.pcm2-juv_mod.se.pcm2,ymax =juv_mod.mn.pcm2+juv_mod.se.pcm2,fill=ISLAND))+
  geom_point()+
  #geom_errorbar()+
  facet_grid(.~GENUS_CODE)+
  theme(axis.text.x = element_text(angle=90))+ylab("Juvenile Density per Adult cm^2 area")+ylim(c(0,.3))

ggplot(sec,aes(mean = juv_mod.mn.pcm2, sd = sqrt(juv_mod.var.pcm2),fill=ISLAND))+
  stat_function(fun = dnorm, geom = "area", 
                          fill = "orange", alpha = 0.25)+
  facet_grid("GENUS_CODE")

#scaled juvcolden
sec$Genus=GeneraLU[sec$GENUS_CODE]
sec$Genus=factor(sec$Genus,levels=c("Porites sp.","Montipora sp.","Pocillopora sp." ))
ggplot(sec,aes(juv_mod))+geom_histogram()+facet_grid("GENUS_CODE")+scale_x_log10()

#raw juvcolden, sector
ggplot(jmhi,aes(x=GENUS_CODE,y=Mean_JuvColDen))+geom_violin()+
  #facet_grid("GENUS_CODE")+
  scale_y_log10()
#raw juvcolden, site
ggplot(jmhi_site,aes(JuvColDen ))+geom_histogram()+facet_grid(GENUS_CODE~ISLAND)+scale_x_log10()


#Get Site Level Cover, POSP, MOSP, POCS
TaxSite_i=22:126#which(substr(names(cmhi_site),1,5)=="Mean."&substr(names(cmhi),6,16)!="TOT_AREA_WT")
names(cmhi_site)[TaxSite_i]
rowSums(cmhi_site[,TaxSite_i])
cmhi_site$MOSP=rowSums(cmhi_site[,c("MOEN","MOBR","MOFO")])#"Mean.MONE",
cmhi_site$POSP=rowSums(cmhi_site[,c("POMA","POEN")])
metacol_site=c("SITEVISITID","REGION","ISLAND","SEC_NAME","SITE","LATITUDE","LONGITUDE","ANALYSIS_YEAR","OBS_YEAR","DATE_","DEPTH_BIN")
site_cov=cmhi_site[,c(metacol_site,"POSP","MOSP","POCS")]

apply(cmhi[,5:ncol(cmhi)],2,mean)

head(cmhi)

ddply(jmhi,.(GENUS_CODE),summarize,
      q05=quantile(113*JuvColDen,.05),
      q25=quantile(113*JuvColDen,.25),
      q50=quantile(113*JuvColDen,.50),
      q75=quantile(113*JuvColDen,.75),
      q95=quantile(113*JuvColDen,.95))

ggplot(jmhi,aes(x=JuvColDen))+
  geom_histogram()+
  scale_x_log10()+
  facet_grid(GENUS_CODE~OBS_YEAR)+
  geom_vline(xintercept = 35.75*.1,color="red")+
  geom_vline(xintercept = 30.75*.1,color="red")


#############################
# Data Transitions
#############################
ColonyLevel$Genus=factor(ColonyLevel$Genus,levels=c("Porites sp.","Montipora sp.","Pocillopora sp." ))
ColonyLevel$TransitionTypeSimple=factor(ColonyLevel$TransitionTypeSimple,levels=c("GROWTH","SHRINK","RECR","MORT"))

zeq=0.01
TaxaTrans=ggplot(ColonyLevel,aes(x=StartingSize,y=AnnualEndingSize_E,
                       color=TransitionTypeSimple,
                       fill=TransitionTypeSimple,shape=TransitionTypeSimple))+
  geom_abline(intercept=0,slope=1)+
  geom_vline(xintercept = 1,lty=3,color="gray50")+
  geom_hline(yintercept = 1,lty=3,color="gray50")+
  geom_vline(xintercept = zeq,lty=1,color="black")+
  geom_hline(yintercept = zeq,lty=1,color="black")+
  geom_jitter(aes(x=StartingSize+zeq/2,y=EndingSize+zeq/2),height=0,width=10*zeq,
              data=subset(ColonyLevel,TransitionTypeSimple%in%c("RECR")),size=.5)+
  geom_point(size=1,data=subset(ColonyLevel,TransitionTypeSimple%in%c("GROWTH","SHRINK")))+
  geom_jitter(aes(x=StartingSize+zeq/2,y=EndingSize+zeq/2),height=10*zeq,width=0,
              data=subset(ColonyLevel,TransitionTypeSimple%in%c("MORT")),size=.5)+
  #geom_point(y=.001,aes(x=StartingSize),data=subset(ColonyLevel,TransitionTypeSimple=="MORT"),size=.5)+
  facet_grid(.~Genus)+
  scale_x_log10(name="Colony Area\nat T (cm^2)",labels=label_comma(drop0trailing =T),
                breaks=c(.1,1,10,100,1000,10000),limits=c(zeq/3,50000))+
  scale_y_log10(name="Colony Area\n at T + 1 Year (cm^2)",labels=label_comma(drop0trailing =T),
                breaks=c(.1,1,10,100,1000,10000),limits=c(zeq/3,50000))+
  scale_shape_manual(name="Transition Type",values=c(24,25,3,4),
                     breaks=c("GROWTH","SHRINK","RECR","MORT"),
                     labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  scale_color_manual(name="Transition Type",values=c("darkblue","darkcyan","darkgreen","darkred"),
                     breaks=c("GROWTH","SHRINK","RECR","MORT"),
                     labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  scale_fill_manual(name="Transition Type",values=c("darkblue","darkcyan","darkgreen","darkred"),
                    breaks=c("GROWTH","SHRINK","RECR","MORT"),
                    labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  coord_equal()+theme_bw()+ggtitle("Colony-Level Transitions: All Sites, Intervals")+
  theme(legend.position = "bottom",axis.text=element_text(size=8))
TaxaTrans
sc=.75
ggsave(filename = "./figs/TaxaTransitions_All.png",TaxaTrans,width=sc*16,height=sc*9)


CL_POSP=subset(ColonyLevel,Genus_Code=="POSP")
SiteTrans=ggplot(CL_POSP,aes(x=StartingSize,y=AnnualEndingSize_E,
                                 color=TransitionTypeSimple,
                                 fill=TransitionTypeSimple,shape=TransitionTypeSimple))+
  geom_abline(intercept=0,slope=1)+
  geom_vline(xintercept = 1,lty=3,color="gray50")+
  geom_hline(yintercept = 1,lty=3,color="gray50")+
  geom_vline(xintercept = zeq,lty=1,color="black")+
  geom_hline(yintercept = zeq,lty=1,color="black")+
  geom_point(size=1,data=subset(CL_POSP,TransitionTypeSimple%in%c("GROWTH","SHRINK")))+
  geom_jitter(aes(x=StartingSize+zeq/2,y=EndingSize+zeq/2),height=10*zeq,width=0,
              data=subset(CL_POSP,TransitionTypeSimple%in%c("MORT")),size=.5)+
  geom_jitter(aes(x=StartingSize+zeq/2,y=EndingSize+zeq/2),height=0,width=10*zeq,
              data=subset(CL_POSP,TransitionTypeSimple%in%c("RECR")),size=.5)+
  facet_wrap(facets = c("Site"),nrow = 3,ncol = 5)+
  scale_x_log10(name="Colony Area\nat T (cm^2)",labels=label_comma(drop0trailing =T),
                breaks=c(.1,1,10,100,1000,10000),limits=c(zeq/3,50000))+
  scale_y_log10(name="Colony Area\n at T + 1 Year (cm^2)",labels=label_comma(drop0trailing =T),
                breaks=c(.1,1,10,100,1000,10000),limits=c(zeq/3,50000))+
  scale_shape_manual(name="Transition Type",values=c(24,25,3,4),
                     breaks=c("GROWTH","SHRINK","RECR","MORT"),
                     labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  scale_color_manual(name="Transition Type",values=c("darkblue","darkcyan","darkgreen","darkred"),
                     breaks=c("GROWTH","SHRINK","RECR","MORT"),
                     labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  scale_fill_manual(name="Transition Type",values=c("darkblue","darkcyan","darkgreen","darkred"),
                    breaks=c("GROWTH","SHRINK","RECR","MORT"),
                    labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  coord_equal()+theme_bw()+ggtitle("Colony-Level Transitions: Porites, Each Site, All Intervals")+
  theme(legend.position = "bottom",axis.text=element_text(size=8))
sc=.75
ggsave(filename = "./figs/Porites_SiteTransitions_All.png",SiteTrans,width=sc*16,height=sc*9)


CL_POSP_SITE=subset(ColonyLevel,Genus_Code=="POSP"&Site=="MAI_SIO_OL3")
SITrans=ggplot(CL_POSP_SITE,aes(x=StartingSize,y=AnnualEndingSize_E,
                             color=TransitionTypeSimple,
                             fill=TransitionTypeSimple,shape=TransitionTypeSimple))+
  geom_abline(intercept=0,slope=1)+
  geom_vline(xintercept = 1,lty=3,color="gray50")+
  geom_hline(yintercept = 1,lty=3,color="gray50")+
  geom_vline(xintercept = zeq,lty=1,color="black")+
  geom_hline(yintercept = zeq,lty=1,color="black")+
  geom_point(size=1,data=subset(CL_POSP_SITE,TransitionTypeSimple%in%c("GROWTH","SHRINK")))+
  geom_jitter(aes(x=StartingSize+zeq/2,y=EndingSize+zeq/2),height=10*zeq,width=0,
              data=subset(CL_POSP_SITE,TransitionTypeSimple%in%c("MORT")),size=.5)+
  geom_jitter(aes(x=StartingSize+zeq/2,y=EndingSize+zeq/2),height=0,width=10*zeq,
              data=subset(CL_POSP_SITE,TransitionTypeSimple%in%c("RECR")),size=.5)+
  facet_wrap(facets = c("Interval"))+
  scale_x_log10(name="Colony Area\nat T (cm^2)",labels=label_comma(drop0trailing =T),
                breaks=c(.1,1,10,100,1000,10000),limits=c(zeq/3,50000))+
  scale_y_log10(name="Colony Area\n at T + 1 Year (cm^2)",labels=label_comma(drop0trailing =T),
                breaks=c(.1,1,10,100,1000,10000),limits=c(zeq/3,50000))+
  scale_shape_manual(name="Transition Type",values=c(24,25,3,4),
                     breaks=c("GROWTH","SHRINK","RECR","MORT"),
                     labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  scale_color_manual(name="Transition Type",values=c("darkblue","darkcyan","darkgreen","darkred"),
                     breaks=c("GROWTH","SHRINK","RECR","MORT"),
                     labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  scale_fill_manual(name="Transition Type",values=c("darkblue","darkcyan","darkgreen","darkred"),
                    breaks=c("GROWTH","SHRINK","RECR","MORT"),
                    labels=c("Growth","Partial Mortality","Recruitment","Mortality"))+
  coord_equal()+theme_bw()+ggtitle("Colony-Level Transitions: Porites, Maui, Olowalu #3, All Intervals")+
  theme(legend.position = "bottom",axis.text=element_text(size=8))
sc=.7
ggsave(filename = "./figs/Porites_OL3.png",SITrans,width=sc*16,height=sc*9)

#RecVal All Taxa
recvalpc <- 0.32599#35.53
recvalpp <- 2.229822#30.61
recvalmp <- 0.1669464#31.74 

recdf=data.frame(Taxon=c("Porites sp.","Montipora sp.","Pocillopora sp." ),rec=c(recvalpp,recvalmp,recvalpc))
recdf$Taxon=factor(recdf$Taxon,levels=c("Porites sp.","Montipora sp.","Pocillopora sp." ))
recplot=ggplot(recdf,aes(x=Taxon,y=rec))+
  geom_col(width=.5,fill="skyblue")+
  ylab("N. Recruits\n (relative to adult area)")+
  ggtitle("Recruitment Tuned to Population Replacement (Lambda = 1)")+theme_classic()
sc=.7
ggsave(filename = "./figs/RecruitComparison_tunedLam1.png",recplot,width=sc*16,height=sc*9)



