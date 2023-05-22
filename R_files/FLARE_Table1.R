########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED APRIL 2023
### FOR  CALCULATING CREATING TABLE 1 IN THE PUBLICATION
########

library(dplyr)
library(tidyverse)
library(janitor)

#######
### READ IN DATA
#######

flare<-read.csv("FLAREdata.csv")
flare_info<-read.csv("FLARE_site_info.csv")

#######
### CREATE TABLE
#######

######################################
##########################
#### SITE CHARACTERISTICS
##########################
######################################

### lat lon - plot 75 into burned area

latlon <- flare %>% filter(treatment == "burned")  %>% group_by(site)  %>% 
                    arrange (desc(plot)) %>%  filter(row_number()==1) %>% 
                    select(site, latitude, longitude, region)

### number of burned and unburned plots

num_sites<- flare %>% filter(treatment == "burned" | treatment == 'unburned')  %>% 
                    count(site, treatment, region) %>%   group_by(site, region)%>%
                    spread(treatment, n) %>%  rename('number of burned plots' = burned,
                                                     'number of unburned plots' = unburned)
######################################
###################
#### UNBURNED FOREST STRUCTURE
###################
######################################

### unburned  tree density (10000 converts stems/ha to stems/m2)
tree_density <- flare %>% filter(treatment == "unburned")  %>% group_by(region, site)   %>% 
                      dplyr::summarise(density_mean = as.numeric(round(mean(live_tree_density/10000),2)),
                                       density_SE =  as.numeric(round(sd(live_tree_density/10000,na.rm=TRUE)/
                                                                            sqrt(sum(!is.na(live_tree_density))),2)))  %>% 
                      group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                            density_mean = as.numeric(round(mean(.x$density_mean, na.rm = TRUE),2)),
                                            density_SE = as.numeric(round(sd(.x$density_mean)/sqrt(sum(!is.na(.x$density_mean))),2))))%>% 
                      ungroup() %>%
                      mutate (unburn_density = ifelse(is.na(density_SE) ,
                                                      sprintf("%.2f",density_mean), 
                                                      paste(sprintf("%.2f",density_mean),sprintf("%.2f",density_SE), sep=" ± " )))%>% 
                      select(site, region, unburn_density)  %>% rename("tree density" =  unburn_density)

### basal area
unburn_BA <- flare %>% filter(treatment == "unburned")  %>% group_by(site, region)   %>% 
                      dplyr::summarise(BA_mean = as.numeric(round(mean(basal_area_ha),1)),
                                       BA_SE =  as.numeric(round(sd(basal_area_ha,na.rm=TRUE)/sqrt(sum(!is.na(live_tree_density))),1)))  %>% 
                      group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                    BA_mean = as.numeric(round(mean(.x$BA_mean, na.rm = TRUE),1)),
                                                    BA_SE = as.numeric(round(sd(.x$BA_mean)/sqrt(sum(!is.na(.x$BA_mean))),1))))%>% 
                      ungroup() %>%
                      mutate (unburn_BA = ifelse(is.na(BA_SE), 
                                                 sprintf("%.1f",BA_mean),
                                                 paste(sprintf("%.1f",BA_mean),sprintf("%.1f",BA_SE),
                                                       sep=" ± " )))%>% 
                      select(site, region, unburn_BA)  %>% rename("basal area" =  unburn_BA)


### unburned AGC
pre_AGC <- flare %>% filter(treatment == "unburned")  %>% group_by(site, region)   %>% 
                    dplyr::summarise(AGC_mean = round(mean(AGB_gCm2, na.rm=TRUE),0),
                                     AGC_SE = round(sd(AGB_gCm2,na.rm=TRUE)/sqrt(sum(!is.na(AGB_gCm2))),0)) 

unburned_AGC <- pre_AGC %>% group_by(region) %>% 
                  group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                AGC_mean = as.numeric(round(mean(.x$AGC_mean, na.rm = TRUE),0)),
                                                AGC_SE = as.numeric(round(sd(.x$AGC_mean)/sqrt(sum(!is.na(.x$AGC_mean))),0))))%>% 
                  ungroup() %>%
                            mutate(pre_AGC = ifelse(is.na(AGC_SE), 
                                          AGC_mean, paste(AGC_mean,AGC_SE, sep=" ± " )))%>% 
                            select(site,region, pre_AGC) %>% rename("aboveground C pool" =  pre_AGC)



### unburned BGC
pre_BGC <- flare %>% filter(treatment == "unburned")  %>% group_by(site, region)   %>% 
                      dplyr::summarise(BGC_mean = round(mean(soil_gCm2, na.rm=TRUE),0),
                                       BGC_SE = round(sd(soil_gCm2,na.rm=TRUE)/sqrt(sum(!is.na(soil_gCm2))),0))  

unburned_BGC <- pre_BGC %>% group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                    BGC_mean = as.numeric(round(mean(.x$BGC_mean, na.rm = TRUE),0)),
                                                    BGC_SE = as.numeric(round(sd(.x$BGC_mean)/sqrt(sum(!is.na(.x$BGC_mean))),0))))%>% 
                      ungroup() %>%
                              mutate(pre_BGC = ifelse(is.na(BGC_SE), 
                                                BGC_mean, paste(BGC_mean,BGC_SE, sep=" ± " )))%>% 
                              select(site,region, pre_BGC) %>% rename("belowground C pool" =  pre_BGC)


### unburned SOL
pre_SOL <- flare %>% filter(treatment == "unburned")  %>% group_by(site, region)   %>% 
                    dplyr::summarise(SOL_mean = as.numeric(round(mean(SOL_depth, na.rm=TRUE),1)),
                                     SOL_SE = as.numeric(round(sd(SOL_depth,na.rm=TRUE)/sqrt(sum(!is.na(SOL_depth))),1))) %>% 
                              group_by(region) %>% 
                                group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                              SOL_mean = as.numeric(round(mean(.x$SOL_mean, na.rm = TRUE),0)),
                                                              SOL_SE = as.numeric(round(sd(.x$SOL_mean)/sqrt(sum(!is.na(.x$SOL_mean))),0))))%>% 
                                ungroup() %>%
                               mutate(pre_SOL = ifelse(is.na(SOL_SE),
                                                       sprintf("%.1f",SOL_mean),
                                                       paste(sprintf("%.1f",SOL_mean),sprintf("%.1f",SOL_SE),
                                                             sep=" ± " )))%>%
                              select(site,region, pre_SOL) %>% rename("SOL depth" =  pre_SOL)

## total unburned C pool
pre_totalC <-flare %>% filter(treatment == "unburned")   %>% rowwise()  %>%  
                        mutate(total_C = sum(soil_gCm2, AGB_gCm2, na.rm=TRUE)) %>% 
                        group_by(site, region)   %>%
                        dplyr::summarise(totalC_mean = round(mean(total_C, na.rm=TRUE),0),
                               totalC_SE = round(sd(total_C,na.rm=TRUE)/sqrt(sum(!is.na(total_C)))),0)
                        
unburned_totalC <- pre_totalC %>%   
                        group_by(region) %>% 
                        group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                      totalC_mean  = as.numeric(round(mean(.x$totalC_mean , na.rm = TRUE),0)),
                                                      totalC_SE  = as.numeric(round(sd(.x$totalC_mean )/sqrt(sum(!is.na(.x$totalC_mean  ))),0))))%>% 
                        ungroup() %>%
                      mutate(pre_totalC = ifelse(is.na(totalC_SE), 
                                                   totalC_mean, paste(totalC_mean,totalC_SE, sep=" ± " )))%>% 
                        select(site, pre_totalC, region) %>% rename("total C pool" =  pre_totalC)


## Proportion of unburned C stored AG
pre_above_p<-flare %>% filter(treatment == "unburned")   %>% rowwise()  %>%  
                    mutate(prop_AG = AGB_gCm2/(sum(soil_gCm2, AGB_gCm2, na.rm=TRUE))) %>% 
                    group_by(site, region)   %>%
                    dplyr::summarise(prop_AG_mean = round(mean(prop_AG, na.rm=TRUE)*100,1),
                                     prop_AG_SE = round(100*sd(prop_AG,na.rm=TRUE)/sqrt(sum(!is.na(prop_AG))),1)) %>% 
                            
                      group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                    prop_AG_mean  = as.numeric(round(mean(.x$prop_AG_mean , na.rm = TRUE),0)),
                                                    prop_AG_SE  = as.numeric(round(sd(.x$prop_AG_mean )/sqrt(sum(!is.na(.x$prop_AG_mean  ))),0))))%>% 
                      ungroup() %>%
                      
                          mutate(pre_above_percent = ifelse(is.na(prop_AG_SE),
                                                            sprintf("%.1f",prop_AG_mean),
                                                            paste(sprintf("%.1f",prop_AG_mean),sprintf("%.1f",prop_AG_SE),
                                                                  sep=" ± " )))%>%
                            select(site, region, pre_above_percent) %>% rename("percentage of C pool from aboveground" =  pre_above_percent)




######################################
##########################
#### BURNED FOREST STRUCTURE
##########################
######################################
burn_tree_density <- flare %>% filter(treatment == "burned")  %>% group_by(region, site)   %>% 
                    dplyr::summarise(density_mean = as.numeric(round(mean(live_tree_density/10000),2)),
                                     density_SE =  as.numeric(round(sd(live_tree_density/10000,na.rm=TRUE)/
                                                                      sqrt(sum(!is.na(live_tree_density))),2)))  %>% 
                    group_by(region) %>% 
                    group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                  density_mean = as.numeric(round(mean(.x$density_mean, na.rm = TRUE),2)),
                                                  density_SE = as.numeric(round(sd(.x$density_mean)/sqrt(sum(!is.na(.x$density_mean))),2))))%>% 
                    ungroup() %>%
                    mutate (burn_density = ifelse(is.na(density_SE) ,
                                                    sprintf("%.2f",density_mean), 
                                                    paste(sprintf("%.2f",density_mean),sprintf("%.2f",density_SE), sep=" ± " )))%>% 
                    select(site, region, burn_density)  %>% rename("tree density" =  burn_density)

### basal area
burn_BA <- flare %>% filter(treatment == "burned")  %>% group_by(site, region)   %>% 
                    dplyr::summarise(BA_mean = as.numeric(round(mean(basal_area_ha),1)),
                                     BA_SE =  as.numeric(round(sd(basal_area_ha,na.rm=TRUE)/sqrt(sum(!is.na(live_tree_density))),1)))  %>% 
                    group_by(region) %>% 
                    group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                  BA_mean = as.numeric(round(mean(.x$BA_mean, na.rm = TRUE),1)),
                                                  BA_SE = as.numeric(round(sd(.x$BA_mean)/sqrt(sum(!is.na(.x$BA_mean))),1))))%>% 
                    ungroup() %>%
                    mutate (burn_BA = ifelse(is.na(BA_SE), 
                                               sprintf("%.1f",BA_mean),
                                               paste(sprintf("%.1f",BA_mean),sprintf("%.1f",BA_SE),
                                                     sep=" ± " )))%>% 
                    select(site, region, burn_BA)  %>% rename("basal area" =  burn_BA)


### unburned AGC
burn_AGC <- flare %>% filter(treatment == "burned")  %>% group_by(site, region)   %>% 
                    dplyr::summarise(AGC_mean = round(mean(AGB_gCm2, na.rm=TRUE),0),
                                     AGC_SE = round(sd(AGB_gCm2,na.rm=TRUE)/sqrt(sum(!is.na(AGB_gCm2))),0)) 
                  
burned_AGC <- burn_AGC %>% group_by(region) %>% 
                    group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                  AGC_mean = as.numeric(round(mean(.x$AGC_mean, na.rm = TRUE),0)),
                                                  AGC_SE = as.numeric(round(sd(.x$AGC_mean)/sqrt(sum(!is.na(.x$AGC_mean))),0))))%>% 
                    ungroup() %>%
                    mutate(burn_AGC = ifelse(is.na(AGC_SE), 
                                            AGC_mean, paste(AGC_mean,AGC_SE, sep=" ± " )))%>% 
                    select(site,region, burn_AGC) %>% rename("aboveground C pool" =  burn_AGC)
                  


### unburned BGC
burn_BGC <- flare %>% filter(treatment == "burned")  %>% group_by(site, region)   %>% 
                dplyr::summarise(BGC_mean = round(mean(soil_gCm2, na.rm=TRUE),0),
                                 BGC_SE = round(sd(soil_gCm2,na.rm=TRUE)/sqrt(sum(!is.na(soil_gCm2))),0))  
              
burned_BGC <- burn_BGC %>% group_by(region) %>% 
                group_modify(~ .x %>% add_row(site = 'regional mean', 
                                              BGC_mean = as.numeric(round(mean(.x$BGC_mean, na.rm = TRUE),0)),
                                              BGC_SE = as.numeric(round(sd(.x$BGC_mean)/sqrt(sum(!is.na(.x$BGC_mean))),0))))%>% 
                ungroup() %>%
                mutate(burn_BGC = ifelse(is.na(BGC_SE), 
                                        BGC_mean, paste(BGC_mean,BGC_SE, sep=" ± " )))%>% 
                select(site,region, burn_BGC) %>% rename("belowground C pool" =  burn_BGC)


### unburned SOL
burn_SOL <- flare %>% filter(treatment == "burned")  %>% group_by(site, region)   %>% 
                  dplyr::summarise(SOL_mean = as.numeric(round(mean(SOL_depth, na.rm=TRUE),1)),
                                   SOL_SE = as.numeric(round(sd(SOL_depth,na.rm=TRUE)/sqrt(sum(!is.na(SOL_depth))),1))) %>% 
                  group_by(region) %>% 
                  group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                SOL_mean = as.numeric(round(mean(.x$SOL_mean, na.rm = TRUE),0)),
                                                SOL_SE = as.numeric(round(sd(.x$SOL_mean)/sqrt(sum(!is.na(.x$SOL_mean))),0))))%>% 
                  ungroup() %>%
                  mutate(burn_SOL = ifelse(is.na(SOL_SE),
                                          sprintf("%.1f",SOL_mean),
                                          paste(sprintf("%.1f",SOL_mean),sprintf("%.1f",SOL_SE),
                                                sep=" ± " )))%>%
                  select(site,region, burn_SOL) %>% rename("SOL depth" =  burn_SOL)

## total unburned C pool
burn_totalC <-flare %>% filter(treatment == "burned")   %>% rowwise()  %>%  
                    mutate(total_C = sum(soil_gCm2, AGB_gCm2, na.rm=TRUE)) %>% 
                    group_by(site, region)   %>%
                    dplyr::summarise(totalC_mean = round(mean(total_C, na.rm=TRUE),0),
                                     totalC_SE = round(sd(total_C,na.rm=TRUE)/sqrt(sum(!is.na(total_C)))),0)
                  
burn_totalC <- burn_totalC %>%   
                    group_by(region) %>% 
                    group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                  totalC_mean  = as.numeric(round(mean(.x$totalC_mean , na.rm = TRUE),0)),
                                                  totalC_SE  = as.numeric(round(sd(.x$totalC_mean )/sqrt(sum(!is.na(.x$totalC_mean))),0))))%>% 
                    ungroup() %>%
                    mutate(burn_totalC = ifelse(is.na(totalC_SE), 
                                               totalC_mean, paste(totalC_mean,totalC_SE, sep=" ± " )))%>% 
                    select(site, burn_totalC, region) %>% rename("total C pool" =  burn_totalC)


## Proportion of unburned C stored AG
burn_above_p<-flare %>% filter(treatment == "burned")   %>% rowwise()  %>%  
                      mutate(prop_AG = AGB_gCm2/(sum(soil_gCm2, AGB_gCm2, na.rm=TRUE))) %>% 
                      group_by(site, region)   %>%
                      dplyr::summarise(prop_AG_mean = round(mean(prop_AG, na.rm=TRUE)*100,1),
                                       prop_AG_SE = round(100*sd(prop_AG,na.rm=TRUE)/sqrt(sum(!is.na(prop_AG))),1)) %>% 
                      
                      group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                    prop_AG_mean  = as.numeric(round(mean(.x$prop_AG_mean , na.rm = TRUE),0)),
                                                    prop_AG_SE  = as.numeric(round(sd(.x$prop_AG_mean )/sqrt(sum(!is.na(.x$prop_AG_mean ))),0))))%>% 
                      ungroup() %>%
                      
                      mutate(burn_above_percent = ifelse(is.na(prop_AG_SE),
                                                        sprintf("%.1f",prop_AG_mean),
                                                        paste(sprintf("%.1f",prop_AG_mean),sprintf("%.1f",prop_AG_SE),
                                                              sep=" ± " )))%>%
                      select(site, region, burn_above_percent) %>% rename("percentage of C pool from aboveground" =  burn_above_percent)
                    


#############################################
##########################
#### FIRE SEVERITY AND COMBUSTION
##########################
#############################################

dnbr <- flare %>% filter(treatment == "burned")  %>% group_by(site, region)   %>% 
                dplyr::summarise(dnbr_mean = round(mean(dnbr,na.rm=TRUE),0),
                                 dnbr_SE = round(sd(dnbr,na.rm=TRUE)/sqrt(sum(!is.na(dnbr))),0)) %>%
                group_by(region) %>% 
                group_modify(~ .x %>% add_row(site = 'regional mean', 
                                              dnbr_mean  = as.numeric(round(mean(.x$dnbr_mean , na.rm = TRUE),0)),
                                              dnbr_SE  = as.numeric(round(sd(.x$dnbr_mean, na.rm=TRUE )/sqrt(sum(!is.na(.x$dnbr_mean))),0))))%>% 
                ungroup() %>%
                mutate(dnbr = ifelse(dnbr_mean == 'NaN', "-",
                                     ifelse(is.na(dnbr_SE), dnbr_mean, paste(dnbr_mean,dnbr_SE, sep=" ± " )))) %>% 
                  select(site, region, dnbr)  %>% rename("dNBR" =  dnbr)


crown_severity <- flare %>% filter(treatment == "burned")  %>% group_by(site, region)   %>% 
                dplyr::summarise(crown_severity_mean = round(mean(crown_severity,na.rm=TRUE),1),
                                 crown_severity_SE = round(sd(crown_severity,na.rm=TRUE)/sqrt(sum(!is.na(crown_severity))),1)) %>%
                      group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                    crown_severity_mean  = as.numeric(round(mean(.x$crown_severity_mean , na.rm = TRUE),0)),
                                                    crown_severity_SE  = as.numeric(round(sd(.x$crown_severity_mean, na.rm=TRUE )/
                                                                                            sqrt(sum(!is.na(.x$crown_severity_mean))),0))))%>% 
                      ungroup() %>%
                      mutate(crown_severity = ifelse(is.na(crown_severity_SE),
                                                     sprintf("%.1f",crown_severity_mean),
                                                     paste(sprintf("%.1f",crown_severity_mean),
                                                           sprintf("%.1f",crown_severity_SE), sep=" ± " ))) %>% 
                      select(site, region, crown_severity) %>% rename("crown severity index" =  crown_severity)
##
### changes in carbon
##

#####  aboveground
        AGBdf<- flare %>%
                  group_by(site, treatment, region) %>%
                  dplyr::summarize(AGB = mean(AGB_gCm2, na.rm=TRUE),
                                   se = sd(AGB_gCm2)/sqrt(n())) %>% drop_na(site)%>% 
                  left_join(select(flare, site, up_flood, burn_year ))%>%  distinct()%>%  
                  drop_na(up_flood)%>% select(-se) %>% 
                  pivot_wider(names_from = treatment, values_from = AGB ) %>% 
                  left_join(select(flare, site, up_flood, burn_year, region ))%>% distinct() %>%
                  mutate(differenceAG = round(unburned-burned,0), differenceAG_SE = NA)  
        
        AboveChanges <- AGBdf %>%
                      group_by(region) %>% 
                      group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                    differenceAG  = as.numeric(round(mean(.x$differenceAG , na.rm = TRUE),0)),
                                                    differenceAG_SE  = as.numeric(round(sd(.x$differenceAG, na.rm=TRUE )/sqrt(sum(!is.na(.x$differenceAG))),0))))%>% 
                      ungroup() %>%
                      mutate(differenceAG = ifelse(is.na(differenceAG_SE),
                                                     differenceAG,paste(differenceAG,differenceAG_SE, sep=" ± " ))) %>% 
                      select(site, region, differenceAG) %>% rename("aboveground C combusted" =  differenceAG)
            

##### below ground
  BGdf<- flare %>%
        group_by(site, treatment, region) %>%
        dplyr::summarize(BG = mean(soil_gCm2, na.rm=TRUE)) %>% drop_na(region)%>% 
        left_join(select(flare, site, up_flood, burn_year ))%>%  drop_na(up_flood) %>% distinct()%>% 
        pivot_wider(names_from = treatment, values_from = BG )%>% 
        left_join(select(flare, site, up_flood, burn_year, region ))%>% distinct()%>%
        mutate(differenceBG = round(unburned-burned,0), differenceBG_SE = NA) 
        
    
    BelowChanges <- BGdf %>% group_by(region) %>% 
                    group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                  differenceBG  = as.numeric(round(mean(.x$differenceBG , na.rm = TRUE),0)),
                                                  differenceBG_SE  = as.numeric(round(sd(.x$differenceBG, na.rm=TRUE )/sqrt(sum(!is.na(.x$differenceBG))),0))))%>% 
                    ungroup() %>%
                    mutate(differenceBG = ifelse(is.na(differenceBG_SE),
                                                 differenceBG,paste(differenceBG,differenceBG_SE, sep=" ± " ))) %>% 
                    select(site, region, differenceBG) %>% rename("belowground C combusted" =  differenceBG)
                  

##### above + below 
        both_C<- plyr::join_all(list(select(BGdf, site, region, differenceBG),
                                     select(AGBdf, site, region, differenceAG)), by=c("site", "region"), type="left")
      
      
        totalmean_df<-both_C %>% filter(site != 'regional mean') %>% 
                            mutate(total = as.numeric(differenceAG)  + as.numeric(differenceBG ))  %>% 
                            group_by(site, region) %>% 
                            dplyr::summarize(mean_combusted = mean(total , na.rm=TRUE), combustion_SE = NA) %>% 
                            group_by(region) %>% 
                            group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                          mean_combusted  =
                                                            round(mean(.x$mean_combusted , na.rm = TRUE),0),
                                                            combustion_SE = round(sd(.x$mean_combusted, na.rm=TRUE )/
                                                                               sqrt(sum(!is.na(.x$mean_combusted))),0)))%>% 
                            ungroup() %>%
                            mutate(total_combusted = ifelse(is.na(combustion_SE),
                                                            mean_combusted,paste(mean_combusted,combustion_SE, sep=" ± " ))) %>% 
                            select(site, region, total_combusted) %>% rename("total C combusted" =  total_combusted)
##
### proportion of total pre-fire C gone
##
    prop_combusted<-merge(pre_totalC, filter (both_C, site != "site mean"), by=c('site', 'region')) %>% 
                    mutate(total_combusted = as.numeric(differenceBG)  +  as.numeric(differenceAG), 
                                 combustion_SE= NA )%>% 
                    mutate(prop_prefire_combusted = round(total_combusted/totalC_mean,3)*100) %>% 
                    group_by(region) %>% 
                    group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                  prop_prefire_combusted  =
                                                    round(mean(.x$prop_prefire_combusted , na.rm = TRUE),3),
                                                  combustion_SE = round(sd(.x$prop_prefire_combusted, na.rm=TRUE )/
                                                                          sqrt(sum(!is.na(.x$prop_prefire_combusted))),3)))%>% 
                    ungroup() %>%
                    mutate(prop_prefire_combusted = ifelse(is.na(combustion_SE),
                                                           sprintf("%.1f",prop_prefire_combusted),
                                                           paste(sprintf("%.1f",prop_prefire_combusted),
                                                                 sprintf("%.1f",combustion_SE), sep=" ± " ))) %>% 
                    select(site, region, prop_prefire_combusted) %>% rename("percentage of total C pool combusted" =  prop_prefire_combusted)

##
### proportion of AG pre-fire C gone
    prop_AG_combusted<-merge(pre_AGC, AGBdf, by=c('site', 'region')) %>% 
                      mutate(prop_AG_combusted = round(differenceAG/AGC_mean,3)*100,
                             prop_SE = NA) %>% 
                        group_by(region) %>% 
                        group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                      prop_AG_combusted  =
                                                        round(mean(.x$prop_AG_combusted , na.rm = TRUE),3),
                                                      prop_SE = round(sd(.x$prop_AG_combusted, na.rm=TRUE )/
                                                                              sqrt(sum(!is.na(.x$prop_AG_combusted))),3)))%>% 
                        ungroup() %>%
                        mutate(prop_AG_combusted = ifelse(is.na(prop_SE),
                                                               sprintf("%.1f",prop_AG_combusted),
                                                               paste(sprintf("%.1f",prop_AG_combusted),
                                                                     sprintf("%.1f",prop_SE), sep=" ± " ))) %>% 
                        select(site, region, prop_AG_combusted)  %>% rename("percentage of aboveground C pool combusted" =  prop_AG_combusted)
 

    
    ### proportion of BG pre-fire C gone
    prop_BG_combusted<-merge(pre_BGC, BGdf, by=c('site', 'region')) %>% 
                          mutate(prop_BG_combusted = round(differenceBG/BGC_mean,3)*100,
                                 prop_SE = NA) %>% 
                          group_by(region) %>% 
                          group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                        prop_BG_combusted  =
                                                          round(mean(.x$prop_BG_combusted , na.rm = TRUE),3),
                                                        prop_SE = round(sd(.x$prop_BG_combusted, na.rm=TRUE )/
                                                                          sqrt(sum(!is.na(.x$prop_BG_combusted))),3)))%>% 
                          ungroup() %>%
                          mutate(prop_BG_combusted = ifelse(is.na(prop_SE),
                                                            sprintf("%.1f",prop_BG_combusted),
                                                            paste(sprintf("%.1f",prop_BG_combusted),
                                                                  sprintf("%.1f",prop_SE), sep=" ± " ))) %>% 
                          select(site, region, prop_BG_combusted) %>% rename("percentage of belowground C pool combusted" =  prop_BG_combusted)
       
## proportion of C combustion attributed to BG
    BG_combust_fract<- filter (both_C, site != "site mean") %>% 
                         mutate(BG_combust_fract = round(differenceBG/sum(differenceAG, differenceBG),3)*100,
                                prop_SE = NA) %>%
                          group_by(region) %>% 
                          group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                        BG_combust_fract  =
                                                          round(mean(.x$BG_combust_fract , na.rm = TRUE),3),
                                                        prop_SE = round(sd(.x$BG_combust_fract, na.rm=TRUE )/
                                                                          sqrt(sum(!is.na(.x$BG_combust_fract))),3)))%>% 
                          ungroup() %>%
                          mutate(BG_combust_fract = ifelse(is.na(prop_SE),
                                                            sprintf("%.1f",BG_combust_fract),
                                                            paste(sprintf("%.1f",BG_combust_fract),
                                                                  sprintf("%.1f",prop_SE), sep=" ± " ))) %>% 
                          select(site, region, BG_combust_fract)  %>% rename("percentage of C combusted from belowground" =  BG_combust_fract)
                     

#############################################
##########################
### PUT IT ALL TOGETHER
##########################
#############################################
        
## site characteristics
    flare_sites<- select(flare, site, region, burn_year, up_flood) %>% distinct () %>% group_by(region) %>% drop_na(up_flood) %>%
                arrange(site)%>% mutate(burn_year = as.character(burn_year))%>%
                group_modify(~ .x %>% add_row(site = 'regional mean', burn_year = "", up_flood = ""))   %>%
                rename('year of fire' = burn_year,'landscape position' = "up_flood")
    
    
    flare_info2<- select(flare_info, site, year, region) %>% distinct ()  %>% mutate(year = as.character(year))%>% group_by(region) %>% 
                              arrange(site)%>% 
                              group_modify(~ .x %>% add_row(site = 'regional mean', 
                                                            year = ""))   %>% rename('year sampled' = year)
  
    
    site_dfs<-list(flare_info2, num_sites, latlon, flare_sites)
    
    site_row_order <- c("site", "year of fire", "year sampled", "landscape position", "latitude", "longitude", 
                        "number of burned plots", "number of unburned plots")
    
    site_characteristics<- plyr::join_all(site_dfs, by=c("site", "region"), type="full")  %>%
                                  drop_na(region, `landscape position`) %>% arrange(region) %>% select(-region) %>%
                                  mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) %>% t()%>%
                                  as.data.frame() %>%  slice(match(site_row_order, rownames(.))) %>%
                                  row_to_names(row_number = 1) 
                                    
                                
                                  
    
## unburned forest structure    
    unburn_dfs<- list(tree_density,unburn_BA,unburned_AGC, unburned_BGC, pre_SOL, unburned_totalC, 
                   pre_above_p)
    
    unburn_row_order<-c("site", "tree density", "basal area", "SOL depth", "belowground C pool", "aboveground C pool",
                        "total C pool", "percentage of C pool from aboveground")
    
    site_order<-c("Alnus", "ANS", "BP", "CN", "FOC", "FU", "Gonzo", "HR", "Shark",  "regional mean",
                  "FRK", "Korova", "Maya", "ROB")
    
    unburn_fire_structure<- plyr::join_all(unburn_dfs, by=c("site", "region"), type="left")  %>%   
                          select(-region) %>% t() %>% 
                          as.data.frame() %>%  
                          slice(match(unburn_row_order, rownames(.))) %>% 
                          row_to_names(row_number = 1) 
                        
## burned forest structure 
    burned_dfs<- list(burn_tree_density, burn_BA, burned_AGC,  burned_BGC, burn_SOL, burn_totalC,
                      burn_above_p)
    
    burned_fire_structure<-  plyr::join_all(burned_dfs, by=c("site", "region"), type="left")  %>%   
                                select(-region) %>% t() %>% 
                                as.data.frame() %>%  
                                slice(match(unburn_row_order, rownames(.))) %>% 
                                row_to_names(row_number = 1) 
                              
## fire severity and combustion
    severity_dfs<- list(dnbr, crown_severity, AboveChanges, prop_AG_combusted, BelowChanges, prop_BG_combusted, totalmean_df, prop_combusted,
                        BG_combust_fract)
    
    
    severity_and_combustion <-  plyr::join_all(severity_dfs, by=c("site", "region"), type="left")  %>%   
                                  select(-region) %>% t() %>% 
                                  as.data.frame() %>%  
                                #  slice(match(unburn_row_order, rownames(.))) %>% 
                                  row_to_names(row_number = 1) 

    ## all together

    final <-rbind(site_characteristics,unburn_fire_structure, burned_fire_structure,severity_and_combustion )
    
    
    
    write.csv(final,  file = "figures/Table1.csv")
    
