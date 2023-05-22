########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED APRIL 2023
### FOR CALCULATING THE PERCENTAGE OF LIVE/DEAD TREE CARBON AT THE FLARE PLOTS
########

## load in libraries
library(car)
library(dplyr)
library(tidyr)
library(plyr)
library(gap)


###############################
###  Load in data
##############################

trees_orig<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)

yakutsk_processed<-read.csv("Yakutsk_tree_C.csv")
cherskiy_processed<-read.csv("Cherskiy_tree_C.csv")


##############################
###  First, calculate carbon in unburned plots as if all trees were standing
###  and alive
##############################

##############################
###############################
###  YAKUTSK 
##############################
###############################
        
        
        #### #### ################################## 
        #### data prep 
        #### #### ################################## 
        

        #subset only Yakutsk region
        treesy<-subset(trees_orig,region=="Yakutsk")
        
        # Don't include Duck fire
        treesy<-subset(treesy,site != 'Duck')
        
        # subset only unburned
        treesy<-subset(treesy,plot<0)
        
        yakutsk_processed<-subset(yakutsk_processed,plot<0)
        
        
        treesy$site<-as.factor(treesy$site)
        treesy$basal_breast<-as.factor(treesy$basal_breast)
        
        #############################################################
        ###  biomass/carbon calculations 
        ############################################################
        
        #### biomass/carbon calculations
        # note that C concentration is different between stems, branches, and foliage per Alexander et al. 2012,
        # so convert to C FIRST
        #46 % for foliage
        #47% for stemwood bark and snag
        #48% for branches
        
        ########## LARCH trees
        
            larch<-subset(treesy,species == 'Larix cajanderi')
        
            # use equations from  Delcourt and Veraverbeke 2022
            
            larch$kgC<-(.47*    0.079*larch$diameter_cm^2.435 +  # stems
                          .48*    0.095*larch$diameter_cm^1.749 + # branches
                          .46*    0.036 *larch$diameter_cm^1.586) # foliage
            
            larch$gC_m2<-(larch$kgC*1000)/larch$area_sampled_m2## *1000 converts kg to g
            
            
            # aggregate by plot
            larch_trees<-aggregate(gC_m2~site+transect+plot,data=larch, FUN=sum)
            
            names(larch_trees)<-c("site", "transect", "plot", "larch_all_gCm2")
            
            
            # replace NA with 0, since no data actually means 0 C was measured
            larch_trees<-larch_trees %>% replace(is.na(.),0)
        
        
        ########## PINE trees
        ## USE Use Makela and  Vanninen 1998, table 4 dataset II
        
            pine<-subset(treesy,species == 'Pinus sylvestris')
            
            pine$kgC<-exp(log(pine$diameter_cm)*2.2608 - 2.3042)*0.47 ## assume 47% carbon
            
            pine$gC_m2<-(pine$kgC*1000)/pine$area_sampled_m2## *1000 converts kg to g
            
            # aggregate by plot
            pine_trees<-aggregate(gC_m2~site+transect+plot,data=pine, FUN=sum)
            
            names(pine_trees)<-c("site", "transect", "plot", "pine_all_gCm2")
        
        
        # replace NA with 0, since no data actually means 0 C was measured
            pine_trees<-pine_trees %>% replace(is.na(.),0)
        
        ########## BIRCH trees      
        
            birch<-subset(treesy,species == 'Betula platyphylla')
            
            # this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
            
            birch$gC<-(.47*    147.96*birch$diameter_cm^2.25 +  # stems
                          .48*    15.15*birch$diameter_cm^2.49 ) # crowns
            
            birch$gC_m2<-(birch$gC)/birch$area_sampled_m2
            
            # aggregate by plot
            birch_trees<-aggregate(gC_m2~site+transect+plot,data=birch, FUN=sum)
            
            names(birch_trees)<-c("site", "transect", "plot", "birch_all_gCm2")
        
        # replace NA with 0, since no data actually means 0 C was measured
            birch_trees<-birch_trees %>% replace(is.na(.),0)
        
        ########## ASPEN trees       
            aspen<-subset(treesy,species == 'Populus tremula')
        
        # this is the C of each tree before subtracting for % tree gone, how much was burned, etc.
        
            aspen$gc<-(.47*    64.01*aspen$diameter_cm^2.51 +  # stems
                          .48*    41.74*aspen$diameter_cm^1.83) # crowns
            
            aspen$gC_m2<-(aspen$gc)/aspen$area_sampled_m2
            
        # aggregate by plot
            aspen_trees<-aggregate(gC_m2~site+transect+plot,data=aspen, FUN=sum)
        
            names(aspen_trees)<-c("site", "transect", "plot", "aspen_all_gCm2")
            
            # replace NA with 0, since no data actually means 0 C was measured
            aspen_trees<-aspen_trees %>% replace(is.na(.),0)
            
        ###############################
        ### Merge 
        ##############################
            list_of_dfs<-list(aspen_trees, birch_trees, pine_trees, larch_trees)
            
            merged_trees<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                                                       by=c("site", "transect", "plot")), list_of_dfs) %>% drop_na(site)
            
            merged_trees<-merged_trees %>% replace(is.na(.),0)
            
            
            raw_yakutsk_trees <- merged_trees  %>% rowwise()  %>% dplyr::mutate(
              trees_all_gCm2 = sum(
                birch_all_gCm2,
                aspen_all_gCm2,
                pine_all_gCm2,
                larch_all_gCm2, na.rm=TRUE))
            
            
            ## merge with 'raw' with processed' data
            yakutsk<-merge(yakutsk_processed, raw_yakutsk_trees, by=c('site', 'transect', 'plot'))  
      ###############################
      ### Percent live/dead
      ##############################
            
           ## percent live trees by species
            yakutsk$larch_per_live<-yakutsk$larch_trees_gCm2/yakutsk$larch_all_gCm2
            yakutsk$larch_per_snag<-yakutsk$larch_snags_gCm2/yakutsk$larch_all_gCm2
            yakutsk$pine_per_live<-yakutsk$pine_trees_gCm2/yakutsk$pine_all_gCm2
            yakutsk$pine_per_snag<-yakutsk$pine_snags_gCm2/yakutsk$pine_all_gCm2
            yakutsk$birch_per_live<-yakutsk$birch_trees_gCm2/yakutsk$birch_all_gCm2
            yakutsk$birch_per_snag<-yakutsk$birch_snags_gCm2/yakutsk$birch_all_gCm2
            yakutsk$aspen_per_live<-yakutsk$aspen_trees_gCm2/yakutsk$aspen_all_gCm2
            yakutsk$aspen_per_snag<-yakutsk$aspen_snags_gCm2/yakutsk$aspen_all_gCm2
           ## percent live all species 
            yakutsk$per_trees<-yakutsk$trees_gCm2/yakutsk$trees_all_gCm2
            yakutsk$per_snag<-yakutsk$snags_gCm2/yakutsk$trees_all_gCm2

##############################
###############################
###  CHERSKIY 
##############################
###############################
            
            #### #### #### 
            #### data prep
            #### #### #### 
            
            #subset only cherskiy region
            treesC<-subset(trees_orig,region=="Cherskiy")
            # subset only unburned
            treesC<-subset(treesC,plot<0)
            cherskiy_processed<-subset(cherskiy_processed,plot<0)
            
            
            treesC$site<-as.factor(treesC$site)
            treesC$basal_breast<-as.factor(treesC$basal_breast)
            
            ###############################
            ###  biomass/carbon calculations
            ##############################
            
            ####
            # note that C concentration is different between stems, branches, and foliage per Alexander et al. 2012,
            # so convert to C FIRST
            #46 % for foliage
            #47% for stemwood bark and snag
            #48% for branches
            
            treesC$gC<-ifelse(treesC$basal_breast=="bd",
                             .47*     8.00*treesC$diameter_cm^2.56 +  # stems
                               .48*     22.91*treesC$diameter_cm^2.13 + # branches
                               .46*     22.55*treesC$diameter_cm^1.45 , # foliage
                             #dbh
                             .47*    81.42*treesC$diameter_cm^2.10 +  # stems
                               .48*    69.66*treesC$diameter_cm^1.99 + # branches
                               .46*    40.50*treesC$diameter_cm^1.41) # foliage
            
            
            
            treesC$gC_m2<-treesC$gC/treesC$area_sampled_m2
            
            
            # aggregate by plot
            perplot_tree<-aggregate(gC_m2~site+transect+plot,data=treesC, FUN=sum)
            
            names(perplot_tree)<-c("site", "transect", "plot", "trees_all_gCm2")
            
            
            # replace NA with 0, since no data actually means 0 C was measured
            perplot_tree<-perplot_tree %>% replace(is.na(.),0)
            
            ###############################
            ### Merge and aggregate
            ##############################
            
            
            ## merge with 'raw' with processed' data
            cherskiy<-merge(cherskiy_processed, perplot_tree, by=c('site', 'transect', 'plot'))  
            
            ###############################
            ### Percent live/dead
            ##############################
            
            ## percent live trees  
            cherskiy$per_trees<-cherskiy$trees_gCm2/cherskiy$trees_all_gCm2
            cherskiy$per_snag<-cherskiy$snags_gCm2/cherskiy$trees_all_gCm2
            

##############################
###  Combine Cherskiy and Yakutsk percentages and test for differences 
###  between regions and sites
##############################  
      cherskiy$region<-'Cherskiy'
      yakutsk$region<-'Yakutsk'
      
      data <- cherskiy %>% select(per_trees, per_snag, site, region)  %>%
            bind_rows(yakutsk %>% select(per_trees, per_snag, site, region)) 
      
      ## sanity check
      data$total_per<-data$per_snag+ data$per_trees
      
      ###############################
      ###  does the proportion of C in standing live vs. dead vary region or site?
      ##############################
      

      ## does the proportion vary by region or site?
      
      treeaov<-aov(per_trees~region + site ,data=data)
      snagaov<-aov(per_snag~region + site ,data=data)
      
      ### check residuals for normality
      hist(treeaov$residuals) ## looks good
      qqPlot(treeaov$residuals) ## ok
      hist(snagaov$residuals) ## looks good
      qqPlot(snagaov$residuals) ## ok
      
      
      ### is site or region statistically significant?
      summary(treeaov)  ## significant differences region but not sites
      summary(snagaov)  ## significant differences region but not sites
      ###############################
      ###  mean proportion by region and site
      ##############################

      
      finalprod<-data %>% group_by(region) %>%
                       dplyr::summarize(mean_trees = mean(per_trees),
                         mean_snags = mean(per_snag, na.rm=TRUE))
      
      ###############################
      ###  Save as csv
      ##############################
      write.csv(finalprod,row.names=FALSE, file="proportionCarbon.csv")
      
      ### help from: https://statsandr.com/blog/anova-in-r/          
            