########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED NOVEMBER 2023
### FOR PERFORMING STATISTICAL TESTS ON THE FLARE DATA
########

library(MASS)
library(lmtest)
library(tidyverse)
setwd("G:/My Drive/PostDoc/mortality/")

#######
### READ IN DATA
#######

dat_orig<-read.csv("data/FLAREdata.csv")
info<-read.csv("data/FLARE_site_info.csv")

#######
### PREPARE DATA
#######

## add column for time since fire

flare<-left_join(dat_orig,info %>% select(site, year), by='site') %>% 
      distinct() %>%
      mutate(tsf = year - burn_year)  

####  CALCULATE PERCENT CHANGES IN CARBON 
##### total aboveground biomass (burn/unburned only)
      AGBdf<- flare %>%
                group_by(site, treatment, region) %>%
                dplyr::summarize(AGB = mean(AGB_gCm2, na.rm=TRUE),
                                 se = sd(AGB_gCm2)/sqrt(n())) %>% drop_na(site)%>% 
                left_join(select(flare, site, treatment, up_flood, tsf ))%>%  distinct()%>%  
                drop_na(up_flood)
      
      AGBdf2<- AGBdf%>% select(-se) %>% 
                pivot_wider(names_from = treatment, values_from = AGB )%>% 
                left_join(select(flare, site, up_flood, tsf, region ))%>% distinct()
              
      AGBdf2$differenceAG<-AGBdf2$unburned-AGBdf2$burned
      AGBdf2$percentAG<-AGBdf2$differenceAG/AGBdf2$unburned *100
      
      AG_c_df<- select(AGBdf2, site, percentAG)

##### soil organic layer (burn/unburned only)
      BGdf<- flare %>%
              group_by(site, treatment, region) %>%
              dplyr::summarize(BG = mean(soil_gCm2, na.rm=TRUE)) %>% drop_na(site)%>% 
              left_join(select(flare, site, up_flood, tsf ))%>%  distinct()
      
      BGdf2<- BGdf%>% 
              pivot_wider(names_from = treatment, values_from = BG )%>% 
              left_join(select(flare, site, up_flood, tsf, region ))%>% distinct()
            
      BGdf2$difference_BG<-BGdf2$unburned-BGdf2$burned
      BGdf2$percentBG<-BGdf2$difference_BG/BGdf2$unburned *100   
      
      BG_c_df<- select(BGdf2, site, percentBG)

####  SITE-WISE MEAN DNBR/CROWNSEVERITY

dnbrmean<-flare %>% group_by(site) %>% summarize(meandnbr = mean(dnbr, na.rm=T))

crownmean<-flare %>% group_by(site) %>% summarize(crownmean = mean(crown_severity, na.rm=T))


#### NOW MERGE
    df<-left_join(BG_c_df, AG_c_df, 
                  by=c('site'))%>% 
              left_join(dnbrmean, by='site') %>% 
              left_join(crownmean, by='site') %>% distinct()
    
######### ######### 
######### DOES DNBR RELATE TO PERCENT CHANGES IN  CARBON??
######### #########        

            ## linear model
    dnbrmodel<-lm(meandnbr~percentAG,data=df)
    
    ### check normality assumption
        shapiro.test(residuals(dnbrmodel))  #p-value = 0.8239
        
    ### check heteroscedasticity  assumption
        bptest(dnbrmodel) # p-value = 0.2857
        
    ### p-value of linear model 
        summary(dnbrmodel)$coefficients[,4] #0.171019611
        
######### ######### 
######### DOES CROWN SEVERITY RELATE TO PERCENT CHANGES IN  CARBON??
######### #########   
        
        ## linear model
        crownmodel<-lm(crownmean~meandnbr,data=df)

        ### check normality assumption
        shapiro.test(studres(crownmodel))  #p-value = 0.3918

        ### check heteroscedasticity  assumption
        bptest(crownmodel) # p-value =  0.5643
        
        ### p-value of linear model 
        summary(crownmodel)$coefficients[,4] #p-value =0.1015

######### ######### 
######### DOES TIME-SINCE-FIRE AFFECT C LOSS ESTIMATES??
######### #########  
  
    ### linear models
        AGmodel<-lm(differenceAG~tsf, data=AGBdf2) 
        BGmodel<-lm(difference_BG~tsf, data=BGdf2) 
        
    ### check normality assumptions
        shapiro.test(residuals(AGmodel))  #p-value =  0.7415
        shapiro.test(residuals(BGmodel))  #p-value = 0.5064
    
    ### check heteroscedasticity  assumptions
        bptest(AGmodel) #p-value = 0.8013
        bptest(BGmodel) #p-value = 0.109
        
        ### p-value of linear model
        summary(AGmodel)$coefficients[,4] #0.16216125 
        summary(BGmodel)$coefficients[,4] #.9206319 

        
        