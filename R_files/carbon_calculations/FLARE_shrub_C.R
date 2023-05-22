########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED MAY 2023
### FOR CALCULATING ABOVEROUND SHRUB CARBON FROM THE FLARE PLOTS
########

## load in libraries
library(tidyr)
library(dplyr)
library(plotrix)

### load in data

shrubs<-read.csv("shrubs_2018_2019.csv")

##############
########## prepare data
##############

# don't include duck fire
  shrubs<-subset(shrubs,site != 'Duck')

#clean up data
  shrubs<-subset(shrubs,is.na(plot)==FALSE)
  shrubs$site<-as.factor(shrubs$site)
  
  
##############
########## biomass/carbon calculations
##############  
# use equations from Berner et al. 2015 (boreal yakutia)
shrubs$biomass<-ifelse(
                      # BETULA
                      shrubs$species=="Betula middendorffii" |
                      shrubs$species=="Betula nana" |
                      shrubs$species=="Betula nana ssp. exilis" |
                      shrubs$species=="Betula divaricata" |
                      shrubs$species=="Betula platyphylla" |
                      shrubs$species=="Betula spp."|
                      is.na(shrubs$species),
                      28.10*shrubs$basal_diameter_cm^2.97,
                ifelse(
                      #SALIX  
                      shrubs$species=="Salix spp."|
                      shrubs$species=="Unknown spp."  ,
                      23.53*shrubs$basal_diameter_cm^2.83,
                ifelse(
                      # ALNUS
                      shrubs$species=="Alnus spp.",
                      23.70*shrubs$basal_diameter_cm^2.68,
                NA)))

#convert biomass to carbon; use 48% C for shrubs, per Alexander et al. 2012
  shrubs$gC<-shrubs$biomass*0.48
  shrubs$area<-as.numeric(shrubs$width)*5
  shrubs$gC_m2<-shrubs$gC/shrubs$area

##############
########## aggregate by plot and clean data
##############  

# aggregate by p;pt
  shrubs_by_plot<-aggregate(gC_m2~site+transect+plot,data=shrubs, FUN=sum)
  names(shrubs_by_plot)<-c("site", "transect", "plot", "shrub_gCm2")

# replace NA with 0, since no data actually means 0 C was measured
  shrubs_by_plot<-shrubs_by_plot %>% replace(is.na(.),0)

### create new row for FRK T1, plot -50 and MAYA T1, plot -50, since shrub biomass is 0 here
### but that is not recorded in the dataset (per H.Alexander and A.Paulson)
  newrow1<-c("FRK", 1, "-50", 0)
  newrow2<-c("MAYA", 1, "-50", 0)
  
  names(newrow1)<-c("site", "transect", "plot", "shrub_gCm2")
  names(newrow2)<-c("site", "transect", "plot", "shrub_gCm2")
  
  newrows<-bind_rows(newrow1,newrow2)
  
  newrows$transect<-as.integer(newrows$transect)
  newrows$shrub_gCm2<-as.double(newrows$shrub_gCm2)

shrubs_by_plot<-bind_rows(shrubs_by_plot,newrows)

##############
########## export data
##############  

write.csv(shrubs_by_plot, row.names=FALSE,
          file = "FLARE_shrubs_C.csv")
