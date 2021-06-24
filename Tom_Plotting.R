#Tom_Plotting
library(ggplot2)
library(plyr)
head(PatchLevel)

#Get to clean Area Surveyed 
setwd("T:/Benthic/Projects/Juvenile Project") # set working directory 
juv <- read.csv("JuvProject_temporal_SITE.csv")

jmhi=subset(juv,REGION=="MHI"&GENUS_CODE%in%c("POCS","MOSP","POSP"))

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
  geom_vline(xintercept = 35.75/113,color="red")

