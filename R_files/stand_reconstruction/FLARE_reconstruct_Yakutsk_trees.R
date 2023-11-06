########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED OCTOBER 2023
### FOR CALCULATING ABOVEGROUND TREE CARBON FROM THE FLARE PLOTS IN YAKUTSK
########

## load in libraries
library(tidyr)
library(dplyr)
library(plotrix)


#### #### #### 
#### Load in data
#### #### ####

trees_orig<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)

prop_carbon<-read.csv("proportionCarbon.csv")

convertfactor<-10000 # to convert m2 to hectares
#### #### #### 
#### data prep
#### #### #### 


#subset only Yakutsk region
trees<-subset(trees_orig,region=="Yakutsk")

# Don't include Duck fire
trees<-subset(trees,site != 'Duck')

# subset only burned
trees<-subset(trees,plot>0)


trees$site<-as.factor(trees$site)
trees$basal_breast<-as.factor(trees$basal_breast)

###############################
###  biomass/carbon calculations
##############################

#### biomass/carbon calculations
# note that C concentration is different between stems, branches, and foliage per Alexander et al. 2012,
# so convert to C FIRST
#46 % for foliage
#47% for stemwood bark and snag
#48% for branches

########## LARCH TREES

      larch<-subset(trees,species == 'Larix cajanderi')
      
      larch$kgC<-(.47*    0.079*larch$diameter_cm^2.435 +  # stems
                      .48*    0.095*larch$diameter_cm^1.749 + # branches
                      .46*    0.036 *larch$diameter_cm^1.586) # foliage
      
      larch$gC_m2<-(larch$kgC*1000)/larch$area_sampled_m2## *1000 converts kg to g
      
      
      # aggregate by plot
      larch_trees<-aggregate(gC_m2~site+transect+plot,data=larch, FUN=sum)
      
      names(larch_trees)<-c("site", "transect", "plot", "larch_trees_gCm2")
      
      
      # replace NA with 0, since no data actually means 0 C was measured
      larch_trees<-larch_trees %>% replace(is.na(.),0)

      
########## PINE TREES
      ## USE M?kel? and  Vanninen 1998, table 4 dataset II
      
      pine<-subset(trees,species == 'Pinus sylvestris')
      
      pine$kgC<-exp(log(pine$diameter_cm)*2.2608 - 2.3042)*0.47 ## assume 47% carbon
      
      pine$gC_m2<-(pine$kgC*1000)/pine$area_sampled_m2## *1000 converts kg to g
      
            # aggregate by plot
      pine_trees<-aggregate(gC_m2~site+transect+plot,data=pine, FUN=sum)
      
      names(pine_trees)<-c("site", "transect", "plot", "pine_trees_gCm2")
      
      
      # replace NA with 0, since no data actually means 0 C was measured
      pine_trees<-pine_trees %>% replace(is.na(.),0)

########## BIRCH TREES      

      birch<-subset(trees,species == 'Betula platyphylla')
      
      # this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
      
      birch$gC<-(.47*    147.96*birch$diameter_cm^2.25 +  # stems
                      .48*    15.15*birch$diameter_cm^2.49 ) # crowns
      
      birch$gC_m2<-(birch$gC)/birch$area_sampled_m2
      
      # aggregate by plot
      birch_trees<-aggregate(gC_m2~site+transect+plot,data=birch, FUN=sum)
      
      names(birch_trees)<-c("site", "transect", "plot", "birch_trees_gCm2")
      
      # replace NA with 0, since no data actually means 0 C was measured
      birch_trees<-birch_trees %>% replace(is.na(.),0)
      
########## ASPEN TREES        
      aspen<-subset(trees,species == 'Populus tremula')
      
      # this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
      
      aspen$gc<-(.47*    64.01*aspen$diameter_cm^2.51 +  # stems
                      .48*    41.74*aspen$diameter_cm^1.83) # crowns
      
      aspen$gC_m2<-(aspen$gc)/aspen$area_sampled_m2
      
      # aggregate by plot
      aspen_trees<-aggregate(gC_m2~site+transect+plot,data=aspen, FUN=sum)
      
      names(aspen_trees)<-c("site", "transect", "plot", "aspen_trees_gCm2")
      
      # replace NA with 0, since no data actually means 0 C was measured
      aspen_trees<-aspen_trees %>% replace(is.na(.),0)
      
##########  combine raw C calculations with percent alive/snag from the unburned
      ### merge different species
      list_dfs<-list(aspen_trees, birch_trees, pine_trees, larch_trees)
      
      perplot_tree<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                                                 by=c("site", "transect", "plot")),
                                                list_dfs) %>% drop_na(site)
      
      
   
      
      treecarbon <- perplot_tree  %>% rowwise()  %>% dplyr::mutate(
        trees_raw = sum(
          birch_trees_gCm2,
          aspen_trees_gCm2,
          pine_trees_gCm2,
          larch_trees_gCm2, na.rm=TRUE))

      treecarbon<-left_join(treecarbon, prop_carbon %>% filter(region=='Yakutsk'), 
                            by=c('site'))
      
      treecarbon$trees_gCm2<-treecarbon$trees_raw*treecarbon$mean_trees
      treecarbon$snags_gCm2<-treecarbon$trees_raw*treecarbon$mean_snags
      
      
      
###############################
###  Tree density
##############################

### individuals per hectare
treecount<- trees %>%
  group_by(site, transect, plot) %>% dplyr::count(site) %>%
  left_join(select(trees, site, transect, plot, area_sampled_m2),
            by=c('site', 'transect', 'plot'))%>% distinct() 

## assume that 90% of the trees are live (i.e., count*0.9)
treecount$live_tree_density<-round(0.91*treecount$n/treecount$area_sampled_m2*convertfactor,0)
treecount$standing_dead_density<-round(0.09*treecount$n/treecount$area_sampled_m2*convertfactor,0)
treecount<-treecount %>% select(site, transect, plot, live_tree_density, standing_dead_density)



### individuals per hectare
larchcount<- trees %>% filter(species=='Larix cajanderi') %>% 
  group_by(site, transect, plot) %>% dplyr::count(site) %>%
  left_join(select(trees, site, transect, plot, area_sampled_m2),
            by=c('site', 'transect', 'plot'))%>% distinct() 

## assume that 90% of the trees are live (i.e., count*0.9)
larchcount$live_larch_density<-round(0.91*larchcount$n/larchcount$area_sampled_m2*convertfactor,0)
larchcount$standing_larch_density<-round(0.09*larchcount$n/larchcount$area_sampled_m2*convertfactor,0)
larchcount<-larchcount %>% select(site, transect, plot, live_larch_density)



###############################
### Merge and write.csv
##############################

list_of_dfs<-list(treecount,treecarbon, larchcount)

merged_trees<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                                           by=c("site", "transect", "plot")), list_of_dfs) %>% drop_na(site)

merged_trees$treatment<-"pre-burn"
merged_trees$region<-'Yakutsk'

write.csv(merged_trees, row.names=FALSE,
          file = "Yakutsk_reconstructed_trees.csv")
