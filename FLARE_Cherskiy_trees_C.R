########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED MARCH 2023
### FOR CALCULATING ABOVEGROUND LARCH CARBON FROM THE FLARE PLOTS IN CHERSKIY
########

## load in libraries
library(tidyr)
library(dplyr)
library(plotrix)


###############################
### load in data
##############################

trees<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)

###############################
### prepare data
##############################
# proportion of the crown that is fine branches; from Boby et al. 2012
    prop<-0.15

#subset only Cherskiy region
    trees<-subset(trees,region=="Cherskiy")
    
## save original trees for later
    trees_orig<-trees

# update variable type
    trees$crown_severity<-as.factor(trees$crown_severity)
    trees$site<-as.factor(trees$site)
    trees$basal_breast<-as.factor(trees$basal_breast)
    trees$live<-as.factor(trees$live)

# disregard downed trees; this C pool will be captured by the CWD transects
## if it is not noted if it is standing, we can assume it is standing if it is live
    trees$standing<-ifelse(is.na(trees$standing),
                           ifelse(trees$live == 'live', 'y', 'NA'),
                           trees$standing)
## only analyze standing trees
    trees<-subset(trees,standing =="y" | standing == 'stump')

## if crown severity is stump, change crown severity to 1, because we assume that heavily burned trees
## would not have been harvested
    levels(trees$crown_severity)[levels(trees$crown_severity)=="stump"] <- "1"

###############################
###  biomass/carbon calculations
##############################

# note that C concentration is different between stems, branches, and foliage per Alexander et al. 2012,
# so convert to C FIRST
#46 % for foliage
#47% for stemwood bark and snag
#48% for branches

# this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
trees$C_raw<-ifelse(trees$basal_breast=="bd",
                            # bd
                           .47*     8.00*trees$diameter_cm^2.56 +  # stems
                           .48*     22.91*trees$diameter_cm^2.13 + # branches
                           .46*     22.55*trees$diameter_cm^1.45 , # foliage
                           #dbh
                           .47*    81.42*trees$diameter_cm^2.10 +  # stems
                           .48*    69.66*trees$diameter_cm^1.99 + # branches
                           .46*    40.50*trees$diameter_cm^1.41) # foliage


# # C missing
# # for unburned plots, subtract the % tree crown missing; 
# # for burned plots, subtract the missing components according to the scorch index
trees$C_subtract<- ifelse(
                    #### 
                    #unburned
                    ####
                            is.na(trees$percent_canopy_remaining)!=TRUE,
                            ### if bd measurement
                        ifelse(trees$basal_breast=="bd",
                               (.48*     22.91*trees$diameter_cm^2.13 + # branches
                                .46*     22.55*trees$diameter_cm^1.45)*# foliage
                              (100-trees$percent_canopy_remaining)/100,
                            #### if dbh measurement  
                             (.48*    69.66*trees$diameter_cm^1.99 + # branches
                              .46*    40.50*trees$diameter_cm^1.41)*# foliage
                          (100-trees$percent_canopy_remaining)/100),
                    #### 
                    #burned
                    ####
                                ifelse(
                        # for burn severity class 1 (only needles consumed)
                        # use Alexander et al. 2012 needle equation
                                    trees$crown_severity==1,
                                            #if measurement is dbh
                                            ifelse(trees$basal_breast=="dbh",
                                                   .46*    (40.50)*(trees$diameter_cm)^1.41, # foliage
                                                   # if measurement is bd
                                                   .46*    (22.55)*(trees$diameter_cm)^1.45), # foliage
                        # for burn severity class 2 (foliage and fine branches)
                        # use Alexander et al. 2012 needle equation +
                        # Alexander et al. 2012 branch equation * proportion (fine branches/branches)
                        # from Boby et al., 2010
                                    ifelse(
                                      trees$crown_severity==2,
                                              #if measurement is dbh
                                              ifelse(trees$basal_breast=="dbh",
                                                     .46*    40.50*trees$diameter_cm^1.41 + #needles
                                                     .48*    69.66*trees$diameter_cm^1.99 *prop, # branches*0.15
                                                     #if measurement is bd
                                                     .46*   22.55*trees$diameter_cm^1.45 + #needles
                                                     .48*   22.91*trees$diameter_cm^2.13*prop), # branches*0.15
                       #for burn severity class 3 (most of the canopy combusted, including foliage and branches)
                       # use Alexander et al. 2012 needle equation + Alexander et al. 2012 branch equation 
                                      ifelse(
                                        trees$crown_severity==3,
                                        #if measurement is dbh
                                                ifelse(trees$basal_breast=="dbh",
                                                       .46*    40.50*trees$diameter_cm^1.41 + #needles (from Alexander 2012)
                                                       .48*    69.66*trees$diameter_cm^1.99, # branches

                                                #if measurement is bd
                                                       .46*    22.55*trees$diameter_cm^1.45 + #needles
                                                       .48*    22.91*trees$diameter_cm^2.13) # branches
                                                ,0))))
# subtract biomass missing from raw biomass
    trees$C_subtract<-ifelse(is.na(trees$C_subtract)==TRUE,0,trees$C_subtract)
    trees$gC<-trees$C_raw-trees$C_subtract
    trees$gC_m2<-trees$gC/trees$area_sampled_m2

#differentiate between snags and live trees
  deadtrees<-trees[trees$live=="dead",]
  livetrees<-trees[trees$live=="live",]

  
###############################
###  clean up data
##############################  
# aggregate by plot
  perplot_tree<-aggregate(gC_m2~site+transect+plot,data=livetrees, FUN=sum)
  perplot_snags<-aggregate(gC_m2~site+transect+plot,data=deadtrees, FUN=sum)

  names(perplot_tree)<-c("site", "transect", "plot", "trees_gCm2")
  names(perplot_snags)<-c("site", "transect", "plot", "snags_gCm2")

# merge together
  Cherskiy_tree<-merge(perplot_tree,perplot_snags,by=c("site", "transect", "plot"), all=TRUE)

# replace NA with 0, since no data actually means 0 C was measured
  Cherskiy_tree<-Cherskiy_tree %>% replace(is.na(.),0)

# go back to original data to make sure we got all plots
  finaldata<-merge(Cherskiy_tree,select(trees_orig, site, transect, plot),
                 by=c("site", "transect", "plot"), all=TRUE) %>% distinct()
  finaldata<-finaldata %>% replace(is.na(.),0)

# but we actually need NA for CN, transect 1, plots -25, -50, and 25
# for standing dead, since whether or not dead trees were standing
# was not recorded
  finaldata$snags_gCm2<- ifelse(finaldata$site=='CN' &
                                  finaldata$transect=='1',
                                ifelse (finaldata$plot =='-25' |
                                          finaldata$plot =='-50' |
                                          finaldata$plot =='25', 
                                    'NA', finaldata$snags_gCm2), 
                                finaldata$snags_gCm2)

###############################
###  export data
##############################  

write.csv(finaldata, row.names=FALSE,
          file = "Cherskiy_tree_C.csv")
