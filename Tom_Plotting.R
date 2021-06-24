#Tom_Plotting
head(PatchLevel)

#Get to clean Area Surveyed 
setwd("T:/Benthic/Projects/Juvenile Project") # set working directory 
juv <- read.csv("JuvProject_temporal_SITE.csv")

jmhi=subset(juv,REGION=="MHI"&GENUS_CODE%in%c("POCS","MOSP","POSP"))

library(ggplot2)
ggplot(jmhi,aes(x=JuvColDen))+
  geom_histogram()+
  scale_x_log10()+
  facet_grid(GENUS_CODE~OBS_YEAR)+
  geom_vline(xintercept = 35.75/113,color="red")

