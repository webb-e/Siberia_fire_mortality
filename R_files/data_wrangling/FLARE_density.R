########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED APRIL 2023
### FOR CALCULATING TREE DENSITIES AT THE FLARE PLOTS 
########


## load in librarires
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)


###############################
### load in data
##############################

    tree_original<-read.csv("tree_survey_2018_2019.csv")
    recruits_original<-read.csv("recruit_count_2018_2019.csv")
    flare_info<-read.csv("site_info_2018_2019.csv")
    
    convertfactor<-10000 # to convert m2 to hectares
    
    # Don't include Duck fire
    tree_original<-subset(tree_original,site != 'Duck')
    recruits_original<-subset(recruits_original,site != 'Duck')

###############################
### live trees
##############################
    livetrees<-tree_original[tree_original$live=="live",]
    
    ### individuals per hectare
    live_trees<- livetrees %>%
                  group_by(site, transect, plot) %>% dplyr::count(live) %>%
                  left_join(select(livetrees, site, transect, plot, area_sampled_m2),
                            by=c('site', 'transect', 'plot'))%>% distinct() 
    
    live_trees$live_tree_density<-round(live_trees$n/live_trees$area_sampled_m2*convertfactor,0)
    live_trees<-live_trees %>% select(site, transect, plot, live_tree_density)
    
    ### larch individuals per hectare
    live_larch<- livetrees %>% subset(species == "Larix cajanderi") %>%
      group_by(site, transect, plot) %>% dplyr::count(live) %>%
      left_join(select(livetrees, site, transect, plot, area_sampled_m2),
                by=c('site', 'transect', 'plot'))%>% distinct() 
    
    live_larch$live_larch_density<-round(live_larch$n/live_larch$area_sampled_m2*convertfactor,0)
    live_larch<-live_larch %>% select(site, transect, plot, live_larch_density)


###############################
### dead trees
##############################
    
    deadtrees<-tree_original[tree_original$live=="dead",]
    
    ### individuals per hectare
    dead_trees<- deadtrees %>%
      group_by(site, transect, plot) %>% dplyr::count(live) %>%
      left_join(select(deadtrees, site, transect, plot, area_sampled_m2),
                by=c('site', 'transect', 'plot'))%>% distinct() 
    
    dead_trees$dead_tree_density<-round(dead_trees$n/dead_trees$area_sampled_m2*convertfactor,0)
    dead_trees<-dead_trees %>% select(site, transect, plot, dead_tree_density)
    
    
    ### larch individuals per hectare
    dead_larch<- deadtrees %>% subset(species == "Larix cajanderi") %>%
      group_by(site, transect, plot) %>% dplyr::count(live) %>%
      left_join(select(deadtrees, site, transect, plot, area_sampled_m2),
                by=c('site', 'transect', 'plot'))%>% distinct() 
    
    dead_larch$dead_larch_density<-round(dead_larch$n/dead_larch$area_sampled_m2*convertfactor,0)
    dead_larch<-dead_larch %>% select(site, transect, plot, dead_larch_density)
    
    
    ### standing dead per hectare
    standing_dead<- deadtrees %>% subset(standing == "y") %>%
      group_by(site, transect, plot) %>% dplyr::count(live) %>%
      left_join(select(deadtrees, site, transect, plot, area_sampled_m2),
                by=c('site', 'transect', 'plot'))%>% distinct() 
    
    standing_dead$standing_dead_density<-round(standing_dead$n/standing_dead$area_sampled_m2*convertfactor,0)
    standing_dead<-standing_dead %>% select(site, transect, plot, standing_dead_density)
    

###############################
### recruits
##############################

    ### only larix
    recruits_larch<-recruits_original[recruits_original$species=="Larix cajanderi",]
    ### scale recruit count to same area - per km2
    recruits_larch$recruit_larch_density<-round(recruits_larch$total_count/recruits_larch$area_sampled_m2*convertfactor,0)
    recruits_larch<-recruits_larch %>% select(site, transect, plot, recruit_larch_density)
    
    ### only pinus
    recruits_pine<-recruits_original[recruits_original$species=="Pinus sylvestris",]
    ### scale recruit count to same area - per km2
    recruits_pine$recruit_pine_density<-round(recruits_pine$total_count/recruits_pine$area_sampled_m2*convertfactor,0)
    recruits_pine<-recruits_pine %>% select(site, transect, plot, recruit_pine_density)
    
    ### only pinus
    recruits_birch<-recruits_original[recruits_original$species=="Betula platyphylla",]
    ### scale recruit count to same area - per km2
    recruits_birch$recruit_birch_density<-round(recruits_birch$total_count/recruits_birch$area_sampled_m2*convertfactor,0)
    recruits_birch<-recruits_birch %>% select(site, transect, plot, recruit_birch_density)
    
    ### only aspen
    recruits_aspen<-recruits_original[recruits_original$species=="Betula platyphylla",]
    ### scale recruit count to same area - per km2
    recruits_aspen$recruit_aspen_density<-round(recruits_aspen$total_count/recruits_aspen$area_sampled_m2*convertfactor,0)
    recruits_aspen<-recruits_aspen %>% select(site, transect, plot, recruit_aspen_density)
    

###############################
### Merge everything together
##############################
    
    list_of_dfs<-list(recruits_aspen,recruits_birch,
                      recruits_pine, recruits_larch,
                      standing_dead, dead_larch,
                      dead_trees, live_larch, live_trees)
    
    merged_trees<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                      by=c("site", "transect", "plot")), list_of_dfs) %>% drop_na(site)
    
    merged_trees <- merged_trees  %>% rowwise()  %>% dplyr::mutate(
                                      recruit_density = sum(
                                      recruit_aspen_density,
                                      recruit_larch_density,
                                      recruit_birch_density,
                                     recruit_pine_density, na.rm=TRUE))

    
###############################
### Export
##############################
    
    merged_trees<-merged_trees %>% replace(is.na(.),0)
    
    write.csv(merged_trees, row.names=FALSE,
              file = "tree_recruit_density.csv")
    
    