########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED OCTOBER 2023
### FOR RECONSTRUCTING TREE CARBON AND TREE DENSITIES AT THE FLARE PLOTS IN CHERSKIY
########

library(tidyr)
library(dplyr)
library(plotrix)


### load in data

trees<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)
prop_carbon<-read.csv("proportionCarbon.csv")

convertfactor<-10000 # to convert m2 to hectares

#### #### #### 
#### data prep
#### #### #### 

#subset only cherskiy region
trees<-subset(trees,region=="Cherskiy")
# subset only burned
trees<-subset(trees,plot>0)


trees$site<-as.factor(trees$site)
trees$basal_breast<-as.factor(trees$basal_breast)

###############################
###  biomass/carbon calculations
##############################

####
# note that C concentration is different between stems, branches, and foliage per Alexander et al. 2012,
# so convert to C FIRST
#46 % for foliage
#47% for stemwood bark and snag
#48% for branches

trees$gC<-ifelse(trees$basal_breast=="bd",
                 .47*     8.00*trees$diameter_cm^2.56 +  # stems
                   .48*     22.91*trees$diameter_cm^2.13 + # branches
                   .46*     22.55*trees$diameter_cm^1.45 , # foliage
                 #dbh
                 .47*    81.42*trees$diameter_cm^2.10 +  # stems
                   .48*    69.66*trees$diameter_cm^1.99 + # branches
                   .46*    40.50*trees$diameter_cm^1.41) # foliage



trees$gC_m2<-trees$gC/trees$area_sampled_m2


# aggregate by plot
perplot_tree<-aggregate(gC_m2~site+transect+plot,data=trees, FUN=sum)

names(perplot_tree)<-c("site", "transect", "plot", "trees_raw")


# replace NA with 0, since no data actually means 0 C was measured
perplot_tree<-perplot_tree %>% replace(is.na(.),0)

## combine raw C calculations with percent alive/snag from the unburned
# 
treecarbon<-left_join(perplot_tree, prop_carbon %>% filter(region=='Cherskiy'), 
                       by=c('site'))

treecarbon$trees_gCm2<-treecarbon$trees_raw*treecarbon$mean_trees
treecarbon$snags_gCm2<-treecarbon$trees_raw*treecarbon$mean_snags


###############################
###  Tree density
##############################

### live individuals per hectare
treecount<- trees %>%
  group_by(site, transect, plot) %>% dplyr::count(site) %>%
  left_join(select(trees, site, transect, plot, area_sampled_m2),
            by=c('site', 'transect', 'plot'), multiple='all')%>% distinct() 

## assume that 78% of the trees are live (i.e., count*0.78)
treecount$live_tree_density<-round(0.78*treecount$n/treecount$area_sampled_m2*convertfactor,0)
treecount$standing_dead_density<-round(0.22*treecount$n/treecount$area_sampled_m2*convertfactor,0)
treecount<-treecount %>% select(site, transect, plot, live_tree_density, standing_dead_density)
### all trees are larch
treecount$live_larch_density<-treecount$live_tree_density


###############################
### Merge and write.csv
##############################

list_of_dfs<-list(treecount,treecarbon)

merged_trees<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                                           by=c("site", "transect", "plot")), list_of_dfs) %>% drop_na(site)

merged_trees$treatment<-"pre-burn"
merged_trees$region<-'Cherskiy'

write.csv(merged_trees, row.names=FALSE,
          file = "Cherskiy_reconstructed_trees.csv")
