########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED APRIL 2023
### FOR CALCULATING CHANGES IN CARBON DUE TO FIRE AT THE FLARE PLOTS
########

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggnewscale)

#######
### READ IN DATA
#######

flare<-read.csv("FLAREdata.csv")


######### ######### 
######### prepare data
######### ######### 

      ## aboveground
      ag_means_1<- flare %>%
                      group_by(treatment, site, region) %>%
                      dplyr::summarize(mean_CWD = mean(CWD_gCm2, na.rm=TRUE),
                                       mean_shrub = mean(shrub_gCm2, na.rm=TRUE),
                                       mean_FWD = mean(FWD_gCm2, na.rm=TRUE),
                                       mean_tree = mean(trees_gCm2, na.rm=TRUE),
                                       mean_snags =  mean(snags_gCm2, na.rm=TRUE))

      ag_means_2 <- ag_means_1 %>% group_by(treatment, region) %>% 
                        dplyr::summarize(CWD = mean(mean_CWD, na.rm=TRUE),
                                         shrub = mean(mean_shrub, na.rm=TRUE),
                                         FWD = mean(mean_FWD, na.rm=TRUE),
                                         tree = mean(mean_tree, na.rm=TRUE),
                                         snags =  mean(mean_snags, na.rm=TRUE)) %>%
                        gather(CWD:snags, key ='type', value ='mean')
      
      ag_se_2 <- ag_means_1 %>% group_by(treatment, region) %>% 
                        dplyr::summarize( CWD = sd(mean_CWD)/sqrt(n()),
                                          shrub = sd(mean_shrub)/sqrt(n()),
                                          FWD = sd(mean_FWD)/sqrt(n()),
                                          tree = sd(mean_tree)/sqrt(n()),
                                          snags = sd(mean_snags)/sqrt(n())) %>%
                        gather(CWD:snags, key ='type', value ='se')
      
      ag_data<- left_join(ag_means_2, ag_se_2, by=c('treatment', 'type', 'region')) %>%
                          mutate(treatment = fct_relevel(treatment, "pre-burn", "unburned", "burned" ))
                        
      ## belowground
      
      bg_means1<- flare %>%
                      group_by(treatment, site, region) %>%
                      dplyr::summarize(organic = mean(soil_gCm2, na.rm=TRUE),
                                       SOL= mean(SOL_depth, na.rm=TRUE) )
      
      bg_means2 <- bg_means1 %>% group_by(treatment, region) %>% 
                            dplyr::summarize(organic = mean(organic, na.rm=TRUE),
                                             SOL = mean(SOL, na.rm=TRUE))%>%
                            gather(organic:SOL, key ='type', value ='mean')
                
      bg_SE2 <- bg_means1 %>% group_by(treatment, region) %>% 
                        dplyr::summarize(organic = sd(organic)/sqrt(n()),
                                          SOL = sd(SOL)/sqrt(n())) %>%
                        gather(organic: SOL, key ='type', value ='se')
      
      bg_data<- left_join(bg_means2, bg_SE2, by=c('treatment', 'type', 'region')) %>%
                          mutate(treatment = fct_relevel(treatment, "pre-burn", "unburned", "burned" ))
      
######### ######### 
######### STATISTICAL TESTS
######### ######### 
      
      ####################################        
      ###### SOIL CARBON -  CHERSKIY
      #################################### 
      soil_carbon_CH <- bg_means1 %>% filter(treatment != 'pre-burn' && region=='Cherskiy')
      soil_carbon_CH$treatment<-as.factor(soil_carbon_CH$treatment)
      
      ## test for normality
      hist(log10(soil_carbon_CH$organic)) 
      
      soil_carbon_CH$transformedorganic<-log10(soil_carbon_CH$organic)
      ## test for homogeneity of variance
      bartlett.test(transformedorganic~treatment, data =soil_carbon_CH) #p-value = 0.6786
      
      ## t-test
      soilCH_ttest<-pairwise.t.test(soil_carbon_CH$transformedorganic, soil_carbon_CH$treatment, paired=TRUE,
                                  pool.sd=FALSE,alternative = 'greater')  
        

      ####################################        
      ###### SOIL CARBON -  YAKUTSK
      #################################### 
      soil_carbon_YK <- bg_means1 %>% filter(treatment != 'pre-burn' && region=='Yakutsk')
      soil_carbon_YK$treatment<-as.factor(soil_carbon_YK$treatment)
      
      ## test for normality
      hist(log10(soil_carbon_YK$organic)) 
      
      soil_carbon_YK$transformedorganic<-log10(soil_carbon_YK$organic)
      ## test for homogeneity of variance
      bartlett.test(transformedorganic~treatment, data =soil_carbon_YK) #p-value = 0.4881
      
      ## t-test
      soilYK_ttest<-pairwise.t.test(soil_carbon_YK$transformedorganic, soil_carbon_YK$treatment, paired=TRUE,
                                    pool.sd=FALSE,alternative = 'greater')  
      
      
      
      ##################        
      ###### FWD -CHERSKIY
      ################## 
      above_CH <- ag_means_1 %>% filter(treatment != 'pre-burn' && region=='Cherskiy')
      above_CH$treatment<-as.factor(above_CH$treatment)
      
      ## test for normality
      hist(log(above_CH$mean_FWD)) 
      above_CH$transformedFWD<-log(above_CH$mean_FWD)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedFWD~treatment, data =above_CH) #p-value = 0.9127
      
      ## t-test
      FWD_CH_ttest<-pairwise.t.test(above_CH$transformedFWD, above_CH$treatment, paired=TRUE,
                                    pool.sd=FALSE,alternative = 'less')  
      

      
      ##################        
      ###### FWD -YAKUTSK
      ################## 
      above_YK <- ag_means_1 %>% filter(treatment != 'pre-burn' && region=='Yakutsk')
      above_YK$treatment<-as.factor(above_YK$treatment)
      
      ## test for normality
      hist(log10(above_YK$mean_FWD))
      above_YK$transformedFWD<-log10(above_YK$mean_FWD)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedFWD~treatment, data =above_YK) #p-value = 0.8312
      
      ## t-test
      FWD_YK_ttest<-pairwise.t.test(above_YK$transformedFWD, above_YK$treatment, paired=TRUE,
                                    pool.sd=FALSE,alternative = 'less')  
      
      
      ##################        
      ###### CWD -CHERSKIY
      ################## 

      ## test for normality
      hist(log(above_CH$mean_CWD)) 
      above_CH$transformedCWD<-log(1+above_CH$mean_CWD)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedCWD~treatment, data =above_CH) #p-value = 0.9144
      
      ## t-test
      CWD_CH_ttest<-pairwise.t.test(above_CH$transformedCWD, above_CH$treatment, paired=TRUE,
                                    pool.sd=FALSE,alternative = 'less')  
      
      
      
      ##################        
      ###### CWD -YAKUTSK
      ################## 
      ## test for normality
      hist(log(1+above_YK$mean_CWD))
      above_YK$transformedCWD<-log(1+above_YK$mean_CWD)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedCWD~treatment, data =above_YK) #p-value = 0.0783
      
      ## t-test
      CWD_YK_ttest<-pairwise.t.test(above_YK$transformedCWD, above_YK$treatment, paired=TRUE,
                                    pool.sd=FALSE,alternative = 'less')     
      
      ##################        
      ###### shrubs -CHERSKIY
      ################## 
      
      ## test for normality
      hist(log(above_CH$mean_shrub)) 
      above_CH$transformedshrub<-log(above_CH$mean_shrub)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedshrub~treatment, data =above_CH) #p-value =  0.8723
      
      ## t-test
      shrub_CH_ttest<-pairwise.t.test(above_CH$transformedshrub, above_CH$treatment, paired=TRUE,
                                                          pool.sd=FALSE,alternative = 'two.sided')  
      
      
      ##################        
      ###### shrub -YAKUTSK
      ################## 
      
      ## test for normality
      hist(log10(1+above_YK$mean_shrub))
      above_YK$transformedshrub<-log10(1+above_YK$mean_shrub)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedshrub~treatment, data =above_YK) #p-value = 0.8825
      
      ## t-test
      shrub_YK_ttest<-pairwise.t.test(above_YK$transformedshrub, above_YK$treatment, paired=TRUE,
                                                        pool.sd=FALSE,alternative = 'two.sided')  
      
      
      ##################        
      ###### snags -CHERSKIY
      ################## 
      
      all_above_CH <- ag_means_1 %>% filter(region=='Cherskiy')
      all_above_CH$treatment<-as.factor(all_above_CH$treatment)
      
      
      ## test for normality
      hist(log(all_above_CH$mean_snags)) 
      all_above_CH$transformedsnag<-log(all_above_CH$mean_snags)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedsnag~treatment, data =all_above_CH) #p-value =  0.6865
      
      ## t-test
      snagCH_ttest<-pairwise.t.test(all_above_CH$transformedsnag, all_above_CH$treatment, paired=TRUE,
                      p.adjust.method="bonferroni", pool.sd=FALSE,alternative = 'less')  
      ##################        
      ###### snag -YAKUTSK
      ################## 
      
      all_above_YK <- ag_means_1 %>% filter(region=='Yakutsk')
      all_above_YK$treatment<-as.factor(all_above_YK$treatment)
      
      
      ## test for normality
      hist(log(1+all_above_YK$mean_snags)) 
      all_above_YK$transformedsnag<-log(1+all_above_YK$mean_snags)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedsnag~treatment, data =all_above_YK) #p-value = 0.1025
      
      ## t-test
      snag_YK_ttest<-pairwise.t.test(all_above_YK$transformedsnag, all_above_YK$treatment, paired=TRUE,
                                      p.adjust.method="bonferroni", pool.sd=FALSE,alternative = 'less')  
      
      ##################        
      ###### trees -CHERSKIY
      ################## 
    
      
      ## test for normality
      hist(log10(1+all_above_CH$mean_tree)) 
      all_above_CH$transformedtree<-log10(1+all_above_CH$mean_tree)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedtree~treatment, data =all_above_CH) #p-value = 0.002261
      
      ## t-test
      tree_CH_ttest<-pairwise.t.test(all_above_CH$transformedtree, all_above_CH$treatment, paired=TRUE,
                                     pool.sd=FALSE,p.adjust.method="bonferroni", alternative = 'greater')  
      ##################        
      ###### trees -YAKUTSK
      ################## 
      
      ## test for normality
      hist(log10(1+all_above_YK$mean_tree)) 
      all_above_YK$transformedtree<-log10(1+all_above_YK$mean_tree)
      
      
      ## test for homogeneity of variance
      bartlett.test(transformedtree~treatment, data =all_above_YK) #p-value =  0.07827
      
      ## t-test
      tree_YK_ttest<-pairwise.t.test(all_above_YK$transformedtree, all_above_YK$treatment, paired=TRUE,
                                     p.adjust.method="bonferroni", alternative = 'greater')  
      
      
######### ######### 
######### PREPARE FOR PLOTTING
######### ######### 
      ### labels
          labelssite<-list("Cherskiy" = "Arctic", "Yakutsk" = "Subarctic")
          fun_labeller <- function(variable,value){ return(labelssite[value])}
          
      ### avg and sd by group
          AboveBelow<-bind_rows(ag_data,bg_data ) %>% filter(type != "SOL") %>%
                             mutate(type = fct_relevel(type, "FWD", "CWD", "shrub", "snags", "tree", "organic"))
          
          AboveBelow$type<-recode_factor(AboveBelow$type, shrub = "live shrubs", 
                                         tree= "live trees", 
                                         organic = "soil organic layer",
                                         FWD = 'fine woody debris',
                                         CWD = 'coarse woody debris',
                                         snags = 'standing dead trees')
          
          AboveBelow<- AboveBelow %>%
                              mutate(type = fct_relevel(type, "fine woody debris",
                                                        "coarse woody debris",
                                                        "live shrubs", 
                                                        "standing dead trees", 
                                                        "live trees", 
                                                        "soil organic layer"))%>%
            mutate(treatment = fct_relevel(treatment, "pre-burn", "unburned", "burned" ))
    
      
      ### what is significant?
          ### cherskiy
          soilCH_ttest$p.value
          FWD_CH_ttest$p.value
          CWD_CH_ttest$p.value
          shrub_CH_ttest$p.value
          snagCH_ttest$p.value[,1] ## SNAGS: pre-burn is significant
          tree_CH_ttest$p.value[,1] ## TREES: pre-burn and unburned is significant
          
          
          ### Yakutsk
          soilYK_ttest$p.value
          FWD_YK_ttest$p.value ## FWD: significant
          CWD_YK_ttest$p.value
          shrub_YK_ttest$p.value  ### shrub: significant
          snag_YK_ttest$p.value[,1] 
          tree_YK_ttest$p.value[,1] ## TREES: pre-burn and unburned is significant
          
      ### add significance column to data   
          AboveBelow<-AboveBelow %>% 
            mutate(sig = case_when(region == 'Cherskiy' & type =='standing dead trees' & treatment =='pre-burn' ~ 1,
                                   region == 'Cherskiy' & type =='live trees' & treatment =='pre-burn' ~ 1,
                                   region == 'Cherskiy' & type =='live trees' & treatment =='unburned' ~ 1,
                                   region == 'Yakutsk' & type =='fine woody debris' & treatment =='unburned' ~ 1,
                                   region == 'Yakutsk' & type =='live shrubs' & treatment =='unburned' ~ 1,
                                   region == 'Yakutsk' & type =='live trees' & treatment =='pre-burn' ~ 1,
                                   region == 'Yakutsk' & type =='live trees' & treatment =='unburned' ~ 1))
       AboveBelow$sig<-ifelse(is.na(AboveBelow$sig), "2",1)
       AboveBelow$sig<-as.factor(AboveBelow$sig)
       

######### ######### 
######### PLOT
######### #########         
      
      Above_Below_plot<-ggplot(AboveBelow %>% filter(is.na(region)==FALSE), aes(y=mean , x=type, fill=treatment)) +  theme_light() + 
                                    geom_bar(stat = "identity", position=position_dodge()) +
                                    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, linewidth=0.2,
                                                  position=position_dodge(0.9))   +
                                    scale_fill_manual(values=c("#606c38","#283618", "#bc6c25"),
                                                      guide = guide_legend(override.aes = list(size = 0.8)))  +
                                    scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
                                    facet_grid(~region, labeller =fun_labeller)+
                                    labs(y=expression(paste("Carbon pool  (g C ", m^{-2}, ")")),
                                         x= " ", fill=" ")+ 
                            
                                   geom_point(position=position_dodge(0.9), na.rm=TRUE,shape = "*", size=5, 
                                               aes(y = mean+se+300, color = sig, group=treatment), show.legend = FALSE)+
                                    scale_color_manual(values=c("black","transparent"))+
                                    
                                    theme(strip.text = element_text(color="black", size = rel(1)),
                                          strip.background = element_rect( size = rel(5), color = 'grey', fill="grey"),
                                          axis.line = element_line(color='grey'),
                                          plot.background = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          panel.grid.major = element_blank(),
                                          legend.title = element_blank(),
                                          legend.position=c(0.1, 0.85),
                                          legend.margin=margin(t=-0.25,l=0.05,b=0.0,r=0.05, unit='cm'))
                                     
      
      ggsave("Carbon_pools.jpg", Above_Below_plot, path="figures/",
             width=160, height=100,units="mm", dpi=500)                                 
                                                                
