########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED MAY 2023
### FOR CALCULATING THE PERCENTAGE OF LIVE TREES AT THE FLARE PLOTS
########

## load in libraries
library(car)
library(dplyr)
library(gap)


###############################
###  Load in data
##############################

trees_orig<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)
##############################
###  Live:Dead ratio in unburned
##############################
unburned<-subset(trees_orig,plot<0)

unburnedcount<- unburned %>%
  group_by(site, transect, plot) %>% dplyr::count(live) %>%
  left_join(select(trees_orig, site, transect, plot, area_sampled_m2, standing),
            by=c('site', 'transect', 'plot'))%>% distinct() 

unburnedcount$density<-unburnedcount$n/unburnedcount$area_sampled_m2
  
unburnedratios<-unburnedcount %>%
  group_by(site, transect, plot) %>% 
  dplyr::summarise(live_per= density[live=='live']/(density[live=='dead']+ density[live=='live']),
                  snag_per = density[live=='dead' & standing=='y']/(density[live=='dead']+ density[live=='live'])) %>%
  left_join(select(trees_orig, site, region),
            by=c('site'))%>% distinct() 


###############################
###  does the proportion of live vs. dead vary region or site?
##############################

## does the proportion vary by region or site?
      treeaov2<-aov(live_per~region + site ,data=unburnedratios)
        
### check residuals for normality
        hist(treeaov2$residuals) ## looks good
        qqPlot(treeaov2$residuals) ## looks good
        
        ### check data for homogeneity of variance
        leveneTest(live_per ~ region,
                   data = unburnedratios) # cannot reject null; assume variances to be equal
        
        ### is site or region statistically significant?
        summary(treeaov2)  ## significant differences between regions but  
                          ## not between sites
        
        
        snagaov<-aov(snag_per~region + site ,data=unburnedratios)
        
        ### check residuals for normality
        hist(snagaov$residuals) ## looks good
        qqPlot(snagaov$residuals) ## looks good
        
        ### check data for homogeneity of variance
        leveneTest(snag_per ~ region,
                   data = unburnedratios) # cannot reject null; assume variances to be equal
        
        ### is site or region statistically significant?
        summary(snagaov)  ## significant differences between regions but  
                          ## not between sites 
        
###############################
###  mean proportion by region
##############################
        unburnedratios %>% group_by(region) %>% 
                dplyr::summarize(mean = mean(live_per))
        
        ##  region    mean
        ##  Cherskiy 0.776
        ##  Yakutsk  0.906
        
        unburnedratios %>% group_by(region) %>% 
          dplyr::summarize(mean = mean(snag_per, na.rm=TRUE))
        
        ## Cherskiy 0.224 
        ## Yakutsk  0.0945

        ##
### help from: https://statsandr.com/blog/anova-in-r/          