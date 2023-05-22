########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED APRIL 2023
### FOR CALCULATING BASAL AREA FROM THE FLARE PLOTS 
########

## load in libraries
library(tidyr)
library(dplyr)


###############################
### load in data
##############################
trees<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)

trees_orig<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)
###############################
### prepare data
##############################

trees$basal_breast<-as.factor(trees$basal_breast)
trees$live<-as.factor(trees$live)

trees$standing<-ifelse(is.na(trees$standing),
                       ifelse(trees$live == 'live', 'y', 'NA'),
                       trees$standing)

trees<-subset(trees,standing =="y" | standing == 'stump')

trees<-trees[trees$live=="live",]

# Don't include Duck fire
trees<-subset(trees,site != 'Duck')
trees_orig<-subset(trees_orig,site != 'Duck')

###############################
###  basal area calculations
##############################

### convert bd to dbh
trees$diam<-ifelse(trees$basal_breast=="dbh",trees$diameter_cm,
                   trees$diameter_cm*0.7179798)

trees$ba<-(trees$diam/200)^2*pi ## dividing by 2 converts diameter to radius; 100 converts cm to m
      
trees$basal_area_ha<-(trees$ba/trees$area_sampled_m2)*10000 ## converts sq m to hectare

###############################
###  aggregate by plot and save
##############################

# aggregate by plot
perplot_ba<-aggregate(basal_area_ha~site+transect+plot,data=trees, FUN=sum)

## add to 0 to plots where there is no data
data<-merge(perplot_ba,select(trees_orig, site, transect, plot),
            by=c('site', 'transect', 'plot'), all.y=TRUE) %>% distinct()

data<-data %>% replace(is.na(.),0)

write.csv(data, row.names=FALSE,
          file = "basal_area.csv")
