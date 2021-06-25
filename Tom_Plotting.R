#Tom_Plotting
library(ggplot2)
library(plyr)
head(PatchLevel)

#Get to clean Area Surveyed 
juv <- read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_SITE.csv")
cov=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier3_SECTOR_forTimeseries.csv")
jmhi=subset(juv,REGION=="MHI"&GENUS_CODE%in%c("POCS","MOSP","POSP"))
cmhi=subset(cov,REGION=="MHI")[,c("REGION", "ISLAND", "ANALYSIS_SEC", "ANALYSIS_YEAR",  "N",
                                  "Mean.POCS","PooledSE.POCS",
                                  "Mean.POMA","PooledSE.POCS"
                                  "Mean.POEN","PooledSE.POCS")]#&GENUS_CODE%in%c("POCS","MOSP","POSP"))
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


