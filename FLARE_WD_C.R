########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED MAY 2023
### FOR CALCULATING WOODY DEBRIS FROM THE FLARE PLOTS
########

## load in libraries
library(tidyr)
library(dplyr)
library(plotrix)


### load in data

wd<-read.csv("woody_debris_2018_2019.csv")
cwd<-read.csv("cwd_2018_2019.csv")


##############
########## FINE WOODY DEBRIS
##############

# don't include duck fire
  wd<-subset(wd,site != 'Duck')
# clean up data
  wd$plot<-as.factor(as.character(wd$plot))
  wd<-subset(wd,is.na(plot)==FALSE)


## Mg/hectare = tally * multiplier/transect length
## per metadata, transect length differed between classes.
## class 1 and 2: 5 m; class 3: 10 m; class 4 and 5: 20 m
## For class I, multipliers are based on the average of multipliers for  P. mariana, 
## P. glauca, and P. banksiana in the Northwest Territories of Canada (Nalder et al. 1997)
## For class II through V, multipliers are from Delcourt and Veraverbeke 2022


wd$biomass_ha<- ifelse (
                    # CLASS 1
                    wd$class == 1,
                    0.0482*wd$tally/5,
             ifelse (
                    # CLASS 2
                    wd$class == 2,
                    0.628*wd$tally/5,
             ifelse (
                    # CLASS 3
                    wd$class == 3,
                    2.89*wd$tally/10,
             ifelse (
                    # CLASS 4
                    wd$class == 4,
                    12.1*wd$tally/20,
             ifelse (
                    #CLASS 5
                    wd$class == 5,
                    32.3*wd$tally/20,
             NA)))))

# use 47% C for WD per Alexander et al. 2012
# the 100 multiplier converts Mg/ha to g/m2
  wd$gC_m2<-wd$biomass_ha*100*0.47

### aggregate by plot
  fwd_by_plot<-aggregate(gC_m2~site+transect+plot,data=wd, FUN=sum)
  names(fwd_by_plot)<-c("site", "transect", "plot", "FWD_gCm2")


##############
########## COARSE WOODY DEBRIS
##############
# Don't include Duck fire
  cwd<-subset(cwd,site != 'Duck')

# clean up data
  cwd<-subset(cwd,decay_class!="NA")

# this is the wood density from table 3 of Ter-Mikaelian et al. 2008
# use the average of the boreal softwoods per Heather Alexander
  decay_class<-c(1,2,3,4,5)
  decay_density<-c(0.393, 0.324, 0.255, 0.186, 0.117)
  decay_table<-data.frame(decay_class,decay_density)

  cwd<-merge(decay_table,cwd,by="decay_class", all.y=TRUE)

## volume (of the cwd) is in m^3/ha
## volume = diameter (cm)^2*pi^2/8*transect (m) length
  cwd$vol<-cwd$diameter_cm^2*pi^2/(8*20)
# biomass (g dry weight/m2) - note the 100 converts to g/m2
  cwd$biomass_m2<-cwd$vol*cwd$decay_density*100
# use 47% C per Alexander et al. 2012
  cwd$gC_m2<-cwd$biomass_m2*0.47

### aggregate by plot
  cwd_by_plot<-aggregate(gC_m2~site+transect+plot,data=cwd, FUN=sum)
  names(cwd_by_plot)<-c("site", "transect", "plot", "CWD_gCm2")

##############
########## MERGE AND EXPORT
##############

Flare_wd<-merge(fwd_by_plot,cwd_by_plot,By=c("site", "transect", "plot"),all=TRUE)

# replace NA with 0, since no data actually means 0 C was measured
  Flare_wd<-Flare_wd %>% replace(is.na(.),0)

  
### export data
write.csv(Flare_wd, row.names=FALSE,
          file = "FLARE_WD_C.csv")
