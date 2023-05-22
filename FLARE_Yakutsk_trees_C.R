########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED MAY 2023
### FOR CALCULATING ABOVEGROUND TREE CARBON FROM THE FLARE PLOTS IN YAKUTSK
########

## load in librarires
library(tidyr)
library(dplyr)
library(plotrix)
library(tidyverse)


###############################
### load in data
##############################
trees<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)


###############################
### prepare data
##############################

# proportion of the crown that is fine branches
  prop<-0.15

#subset only Yakutsk region
  trees<-subset(trees,region=="Yakutsk")
# Don't include Duck fire
  trees<-subset(trees,site != 'Duck')

## save for later
  trees_orig<-trees

# update variable type
  trees$crown_severity<-as.factor(trees$crown_severity)
  trees$site<-as.factor(trees$site)
  trees$basal_breast<-as.factor(trees$basal_breast)
  trees$live<-as.factor(trees$live)

# disregard downed trees; this C pool will be captured by the CWD transects
  trees<-subset(trees,standing !="n")

## if crown severity is stump, change crown severity to 1, because we assume that heavily burned trees
## would not have been harvested
  levels(trees$crown_severity)[levels(trees$crown_severity)=="stump"] <- "1"


##############
########## LARCH TREES
##############

larch<-subset(trees,species == 'Larix cajanderi')

#### biomass/carbon calculations
# note that C concentration is different between stems, branches, and foliage per Alexander et al. 2012,
# so convert to C FIRST
#46 % for foliage
#47% for stemwood bark and snag
#48% for branches

# this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
  larch$C_raw<-(.47*    0.079*larch$diameter_cm^2.435 +  # stems
                .48*    0.095*larch$diameter_cm^1.749 + # branches
                .46*    0.036 *larch$diameter_cm^1.586) # foliage

# # C missing
# # for unburned plots, subtract the % crown missing; 
# # for burned plots, subtract the missing components according to the scorch index
larch$C_subtract<- ifelse(
                    #### 
                    #unburned
                    ####
                            is.na(larch$percent_canopy_remaining)!=TRUE,
                            (.48*    0.095*larch$diameter_cm^1.749 + # branches
                            .46*    0.036 *larch$diameter_cm^1.586)* # foliage
                            (100-larch$percent_canopy_remaining)/100,
                    #### 
                    #burned
                    ####
                                ifelse(
                        # for burn severity class 1 (only needles consumed)
                        # use Delcourt and Veraverbeke 2022  needle equation for dbh
                                    larch$crown_severity==1,
                                                   .46*    (0.036)*(larch$diameter_cm)^1.586, # foliage
                                                   
                        # for burn severity class 2 (foliage and fine branches)
                        # use Delcourt and Veraverbeke 2022  needle equation +
                        # Delcourt and Veraverbeke 2022  branch equation  * proportion (fine branches/branches)
                        # from Boby et al., 2010
                                    ifelse(
                                      larch$crown_severity==2,
                                                     .46*    0.036*larch$diameter_cm^1.586  + #needles
                                                     .48*    0.095*larch$diameter_cm^1.749 *prop, # branches*0.15
        
                         #for burn severity class 3 (most of the canopy combusted, including foliage and branches)
                        # use Delcourt and Veraverbeke 2022 needle equation + Delcourt and Veraverbeke 2022 branch equation 
                                      ifelse(
                                        larch$crown_severity==3,
                                                     .46*    0.036*larch$diameter_cm^1.586 + #needles 
                                                     .48*    0.095*larch$diameter_cm^1.749, # branches
                                                0))))

## make NAs 0
  larch$C_subtract<-ifelse(is.na(larch$C_subtract)==TRUE,0,larch$C_subtract)

# subtract biomass missing from raw biomass
  larch$gC<-(larch$C_raw-larch$C_subtract)*1000 ## *1000 converts kg to g
  larch$gC_m2<-larch$gC/larch$area_sampled_m2

# aggregate by plot
  deadlarch<-larch[larch$live=="dead",]
  livelarch<-larch[larch$live=="live",]

  larch_trees<-aggregate(gC_m2~site+transect+plot,data=livelarch, FUN=sum)
  larch_snags<-aggregate(gC_m2~site+transect+plot,data=deadlarch, FUN=sum)

  names(larch_trees)<-c("site", "transect", "plot", "larch_trees_gCm2")
  names(larch_snags)<-c("site", "transect", "plot", "larch_snags_gCm2")


##############
########## PINE TREES
##############

## Use Makela and  Vanninen 1998, table 4 dataset II
  pine<-subset(trees,species == 'Pinus sylvestris')

# this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
  pine$C_raw<-exp(log(pine$diameter_cm)*2.2608 - 2.3042)*0.47 ## assume 47% carbon

# # C missing
# # for unburned plots, subtract the % crown missing; 
# # for burned plots, subtract the missing components according to the scorch index
    pine$C_subtract<- ifelse(
          #### 
          #unburned
          ####
                  is.na(pine$percent_canopy_remaining)!=TRUE,
                  # (%gone = 100-%remaining)
                    (.46*    exp(log(pine$diameter_cm)*2.5804 -5.6130)+ # foliage
                    .48*    exp(log(pine$diameter_cm)* 3.0119 -6.3284))* # branches
                    (100-pine$percent_canopy_remaining)/100,
          #### 
          #burned
          ####
                  ifelse(
                    # for burn severity class 1 (only needles consumed)
                    # use Makela and  Vanninen  1998  needle equation for dbh
                    pine$crown_severity==1,
                            .46*    exp(log(pine$diameter_cm)*2.5804 -5.6130), # foliage
                    # for burn severity class 2 (foliage and fine branches)
                    # use Makela and  Vanninen 1998 needle equation +
                    # Makela and  Vanninen 1998  branch equation * 15% (fine branches/branches)
                  ifelse(
                      pine$crown_severity==2,
                            .46*    exp(log(pine$diameter_cm)*2.5804 -5.6130)+ # foliage
                            .48*    exp(log(pine$diameter_cm)* 3.0119 -6.3284)*prop, # branches*0.15
                      
                      #for burn severity class 3 (most of the canopy combusted, including foliage and branches)
                      # use Makela and  Vanninen 1998  needle equation + Makela and  Vanninen 1998 branch equation 
                      ifelse(
                        pine$crown_severity==3,
                        .46*    exp(log(pine$diameter_cm)*2.5804 -5.6130)+ # foliage
                        .48*    exp(log(pine$diameter_cm)* 3.0119 -6.3284), # branches
                    0))))

## make NAs 0
  pine$C_subtract<-ifelse(is.na(pine$C_subtract)==TRUE,0,pine$C_subtract)

# subtract biomass missing from raw biomass
  pine$gC<-(pine$C_raw-pine$C_subtract)*1000 ## *1000 converts kg to g
  pine$gC_m2<-pine$gC/pine$area_sampled_m2

# aggregate by plot
  deadpine<-pine[pine$live=="dead",]
  livepine<-pine[pine$live=="live",]
  
  pine_trees<-aggregate(gC_m2~site+transect+plot,data=livepine, FUN=sum)
  pine_snags<-aggregate(gC_m2~site+transect+plot,data=deadpine, FUN=sum)
  
  names(pine_trees)<-c("site", "transect", "plot", "pine_trees_gCm2")
  names(pine_snags)<-c("site", "transect", "plot", "pine_snags_gCm2")


##############
########## BIRCH TREES
##############

    birch<-subset(trees,species == 'Betula platyphylla')
  
# this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
    birch$C_raw<-(.47*    147.96*birch$diameter_cm^2.25 +  # stems
                .48*    15.15*birch$diameter_cm^2.49 ) # crowns


# # C missing
# # for unburned plots, subtract the % tree missing; 
# # for burned plots, subtract the missing components according to the scorch index
      birch$C_subtract<- ifelse(
        #### 
        #unburned
        ####
                is.na(birch$percent_canopy_remaining)!=TRUE,
                # (%gone = 100-%remaining)
                (.48*    15.15*birch$diameter_cm^2.49)* # crown
                (100-birch$percent_canopy_remaining)/100,
        #### 
        #burned
        ####
                ifelse(
                  # for burn severity class 1 (only needles consumed)
                      birch$crown_severity==1,
                      .46*    (6.39)*(birch$diameter_cm)^2.10, # foliage
                      
                  # for burn severity class 2 (foliage and fine branches)
                  # estimate 10% of crown
                      ifelse(
                        birch$crown_severity==2,
                        .46*    (6.39)*(birch$diameter_cm)^2.10+
                        ((.48*    15.15*birch$diameter_cm^2.49)- (.46*6.39*(birch$diameter_cm)^2.10))*prop, # 15% of branches (crown-foliage)
                        
                 #for burn severity class 3 (most of the canopy combusted, including foliage and branches)
                        ifelse(
                          birch$crown_severity==3,
                          .48*    15.15*birch$diameter_cm^2.49,
                          0))))

## make NAs 0
  birch$C_subtract<-ifelse(is.na(birch$C_subtract)==TRUE,0,birch$C_subtract)

# subtract biomass missing from raw biomass
  birch$gC<-(birch$C_raw-birch$C_subtract)
  birch$gC_m2<-birch$gC/birch$area_sampled_m2

# aggregate by plot
  deadbirch<-birch[birch$live=="dead",]
  livebirch<-birch[birch$live=="live",]

  birch_trees<-aggregate(gC_m2~site+transect+plot,data=livebirch, FUN=sum)
  birch_snags<-aggregate(gC_m2~site+transect+plot,data=deadbirch, FUN=sum)
  
  names(birch_trees)<-c("site", "transect", "plot", "birch_trees_gCm2")
  names(birch_snags)<-c("site", "transect", "plot", "birch_snags_gCm2")


##############
########## ASPEN TREES
##############

  aspen<-subset(trees,species == 'Populus tremula')
    
# this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
  aspen$C_raw<-(.47*    64.01*aspen$diameter_cm^2.51 +  # stems
                    .48*    41.74*aspen$diameter_cm^1.83) # crowns
    
# # C missing
# # for unburned plots, subtract the % tree missing; 
# # for burned plots, subtract the missing components according to the scorch index
  aspen$C_subtract<- ifelse(
          #### 
          #unburned
          ####
                  is.na(aspen$percent_canopy_remaining)!=TRUE,
                  # (%gone = 100-%remaining)
                  ( .48*    41.74*aspen$diameter_cm^1.83)* # crown
                   (100-aspen$percent_canopy_remaining)/100,
          #### 
          #burned
          ####
                  ifelse(
                    # for burn severity class 1 (only needles consumed)
                          aspen$crown_severity==1,
                          .46*    (18.98)*(aspen$diameter_cm)^1.53, # foliage
                    # for burn severity class 2 (foliage and fine branches)
                    # estimate 10% of crown
                          ifelse(
                            aspen$crown_severity==2,
                            .46*    (18.98)*(aspen$diameter_cm)^1.53 + # foliage
                            ((.48*    41.74*aspen$diameter_cm^1.83)-( .46*    (18.98)*(aspen$diameter_cm)^1.53))*prop, # 15% of branches (crown-foliage)
                    #for burn severity class 3 (most of the canopy combusted, including foliage and branches)                      
                            ifelse(
                              aspen$crown_severity==3,
                              .48*    41.74*aspen$diameter_cm^1.83,
                              0))))
        
## make NAs 0
  aspen$C_subtract<-ifelse(is.na(aspen$C_subtract)==TRUE,0,aspen$C_subtract)
 
# subtract biomass missing from raw biomass
  aspen$gC<-(aspen$C_raw-aspen$C_subtract)
  aspen$gC_m2<-aspen$gC/aspen$area_sampled_m2

# aggregate by plot
  deadaspen<-aspen[aspen$live=="dead",]
  liveaspen<-aspen[aspen$live=="live",]
    
  aspen_trees<-aggregate(gC_m2~site+transect+plot,data=liveaspen, FUN=sum)
  aspen_snags<-aggregate(gC_m2~site+transect+plot,data=deadaspen, FUN=sum)
    
  names(aspen_trees)<-c("site", "transect", "plot", "aspen_trees_gCm2")
  names(aspen_snags)<-c("site", "transect", "plot", "aspen_snags_gCm2")
    

##############
########## MERGE TREE DATA TOGETHER
##############

list_of_dfs<-list(aspen_trees,aspen_snags,
                  birch_trees, birch_snags,
                  pine_trees, pine_snags,
                  larch_trees, larch_snags)

merged_trees<-Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,by=c("site", "transect", "plot")), list_of_dfs)

# replace NA with 0, since no data actually means 0 C was measured
  merged_trees<-merged_trees %>% replace(is.na(.),0)

# create new column for total live trees (all species)
  merged_trees <- merged_trees  %>% rowwise()  %>% dplyr::mutate(
                      trees_gCm2 = sum(
                        birch_trees_gCm2,
                        aspen_trees_gCm2,
                        pine_trees_gCm2,
                        larch_trees_gCm2, na.rm=TRUE))

# create new column for total dead trees (all species)
  merged_trees <- merged_trees  %>% rowwise()  %>% dplyr::mutate(
                 snags_gCm2 = sum(
                  birch_snags_gCm2,
                  aspen_snags_gCm2,
                  pine_snags_gCm2,
                  larch_snags_gCm2, na.rm=TRUE))

# go back to original data to make sure we got all plots
  finaldata<-merge(merged_trees,select(trees_orig, site, transect, plot),
                   by=c("site", "transect", "plot"), all=TRUE) %>% distinct()
  
  
  finaldata<-finaldata %>% replace(is.na(.),0)

##############
########## EXPORT DATA
############## 
write.csv(finaldata, row.names=FALSE,
          file = "Yakutsk_tree_C.csv")
