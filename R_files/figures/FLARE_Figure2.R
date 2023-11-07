########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED OCTOBER 2023
### FOR  CALCULATING PERCENT CHANGES IN TREE DENSITY AND CARBON POOLS DUE TO FIRE AT THE FLARE PLOTS
########

library(dplyr)
library(tidyverse)
library(ggnewscale)

#######
### READ IN DATA
#######

flare<-read.csv("FLAREdata.csv")


######### ######### 
######### PERCENT CHANGES IN TREE AND SNAG DENSITY
######### ######### 

##### live tree density
  densitydf1 <- flare %>%
              group_by(treatment, site) %>%
              dplyr::summarize(mean_livetree = mean(live_tree_density, na.rm=TRUE))


   densitydf<- densitydf1%>% 
      pivot_wider(names_from = treatment, values_from = mean_livetree)  %>% drop_na(site) %>% 
      left_join(select(flare, site, up_flood, burn_year, region )) %>%  distinct() %>%  
      drop_na(up_flood)
   
   densitydf$difference_preburnd_density<-densitydf$`pre-burn`-densitydf$burned
   densitydf$percent_preburn_density<-densitydf$difference_preburnd_density/densitydf$`pre-burn` *100   
   
   densitydf$difference_unburned_density<-densitydf$unburned-densitydf$burned
   densitydf$percent_unburned_density<-densitydf$difference_unburned_density/densitydf$unburned *100   

   tree_density_df<- select(densitydf, site, region, up_flood, 
                      burn_year, percent_unburned_density,percent_preburn_density)
   

######### ######### 
######### PERCENT CHANGES IN  CARBON
######### #########    
   
##### total aboveground biomass (burn/unburned only)
      AGBdf<- flare %>%
        group_by(site, treatment, region) %>%
        dplyr::summarize(AGB = mean(AGB_gCm2, na.rm=TRUE),
                         se = sd(AGB_gCm2)/sqrt(n())) %>% drop_na(site)%>% 
        left_join(select(flare, site, treatment, up_flood, burn_year ))%>%  distinct()%>%  
           drop_na(up_flood)
      
      AGBdf2<- AGBdf%>% select(-se) %>% 
        pivot_wider(names_from = treatment, values_from = AGB )%>% 
        left_join(select(flare, site, up_flood, burn_year, region ))%>% distinct()
      
      AGBdf2$differenceAG<-AGBdf2$unburned-AGBdf2$burned
      AGBdf2$percentAG<-AGBdf2$differenceAG/AGBdf2$unburned *100
      
      AG_c_df<- select(AGBdf2, site, region, up_flood, 
                       burn_year, percentAG)

##### soil organic layer (burn/unburned only)
      BGdf<- flare %>%
        group_by(site, treatment, region) %>%
        dplyr::summarize(BG = mean(soil_gCm2, na.rm=TRUE)) %>% drop_na(site)%>% 
        left_join(select(flare, site, up_flood, burn_year ))%>%  distinct()
      
      BGdf2<- BGdf%>% 
        pivot_wider(names_from = treatment, values_from = BG )%>% 
        left_join(select(flare, site, up_flood, burn_year, region ))%>% distinct()
      
      BGdf2$difference_BG<-BGdf2$unburned-BGdf2$burned
      BGdf2$percentBG<-BGdf2$difference_BG/BGdf2$unburned *100   
      
      BG_c_df<- select(BGdf2, site, region, up_flood, 
                         burn_year, percentBG)

##### live trees (preburn, unburned / burned)
    treedf<- flare %>%
      group_by(site, treatment, region) %>%
      dplyr::summarize(tree = mean(trees_gCm2, na.rm=TRUE)) %>% drop_na(site)%>% 
      left_join(select(flare, site, burn_year ))%>%  distinct() %>% drop_na(burn_year)
    
    treedf2<- treedf%>% 
      pivot_wider(names_from = treatment, values_from = tree )%>% 
      left_join(select(flare, site, up_flood, burn_year, region ))%>% distinct()
    
    treedf2$difference_unburned<-treedf2$unburned-treedf2$burned
    treedf2$percent_unburned<-treedf2$difference_unburned/treedf2$unburned *100        
    
    treedf2$difference_preburnd<-treedf2$`pre-burn`-treedf2$burned
    treedf2$percent_preburn<-treedf2$difference_preburnd/treedf2$`pre-burn` *100  
    
    tree_c_df<- select(treedf2, site, region, up_flood, 
                       burn_year, percent_preburn,percent_unburned)


######### ######### 
######### MERGE TOGETHER
######### #########   

    list_of_dfs<-list(AG_c_df, BG_c_df,tree_c_df, tree_density_df)
        
    merged_df<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                                            by=c("site", "region", "up_flood", "burn_year")), list_of_dfs) %>% 
                            drop_na(site, region, up_flood) 
    
    
    percentdifdf1<- merged_df %>%
                group_by(region)%>%  dplyr::summarize(BG = -mean(percentBG , na.rm=TRUE),
                                            AG = -mean(percentAG , na.rm=TRUE),
                                            TC_unburn = -mean(percent_unburned, na.rm=TRUE),
                                            TC_preburn = -mean(percent_preburn, na.rm=TRUE),
                                            TD_unburn = -mean(percent_unburned_density, na.rm=TRUE),
                                            TD_preburn = -mean(percent_preburn_density, na.rm=TRUE))%>%
                gather(BG:TD_preburn, key ='type', value ='mean')
    
    percentdifdf2<- merged_df %>%
                group_by(region)%>%  dplyr::summarize(BG = sd(percentBG,na.rm=TRUE)/sqrt(sum(!is.na(percentBG))),
                                                      AG = sd(percentAG,na.rm=TRUE)/sqrt(sum(!is.na(percentAG))),
                                                      TC_unburn = sd(percent_unburned,na.rm=TRUE)/sqrt(sum(!is.na(percent_unburned))),
                                                      TC_preburn =sd(percent_preburn,na.rm=TRUE)/sqrt(sum(!is.na(percent_preburn))),
                                                      TD_unburn =sd(percent_unburned_density,na.rm=TRUE)/sqrt(sum(!is.na(percent_unburned_density))),
                                                      TD_preburn =sd(percent_preburn_density,na.rm=TRUE)/sqrt(sum(!is.na(percent_preburn_density))))%>%
                gather(BG:TD_preburn, key ='type', value ='se')
    
    diffdf<-merge(percentdifdf1, percentdifdf2, by=c("region", "type"))  

######### ######### 
######### PREPARE DATA FOR PLOTTING
######### ######### 
    diffdf$group<-ifelse(diffdf$type == 'TC_preburn' | diffdf$type== 'TC_unburn','C in live trees', 
                        ifelse(diffdf$type== 'AG', 'Aboveground C', 
                         ifelse(diffdf$type=='BG','Belowground C', 'Live tree density')))
    
    diffdf$treatment<-recode_factor(diffdf$type, AG = "unburned",
                                    TC_unburn = "unburned",
                                    BG= "unburned", 
                                    TD_unburn = 'unburned',
                                    TC_preburn = "pre-burn", 
                                     TD_preburn = "pre-burn") 
    
    diffdf$region<-recode_factor(diffdf$region, Cherskiy = "Arctic",
                                                Yakutsk = "Subarctic") 
    ### reorder factors
    diffdf<- diffdf %>%
              mutate(group = fct_relevel(group, 
                                         "Belowground C", 
                                         "Aboveground C",
                                         "C in live trees",
                                         "Live tree density"))%>%
              mutate(treatment = fct_relevel(treatment, "pre-burn", "unburned" ))
    
    myjit <- ggproto("fixJitter", PositionDodge,
                     width = 0.8,   dodge.width = 0.8, jit = NULL,
                     compute_panel =  function (self, data, params, scales) 
                     {if(is.null(self$jit) ) {
                         self$jit <-jitter(rep(0, nrow(data)), amount=self$dodge.width)}
                      data <- ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
                       data$x <- data$x + self$jit
                       if("xmin" %in% colnames(data)) data$xmin <- data$xmin + self$jit
                       if("xmax" %in% colnames(data)) data$xmax <- data$xmax + self$jit
                       data   } )
    
diffdf$region<-as.factor(diffdf$region)
######### ######### 
######### PLOT
######### ######### 

diff_plot<-ggplot(diffdf) + 
          theme_bw() +
    
          ### error bars
          geom_errorbar(aes(xmin=mean-se, xmax=mean+se, y=group, x=mean, fill=region,
                            color=treatment),width=0.2,  
                        position= myjit,
                       # position=position_jitterdodge()
                        linewidth=0.3, show.legend = FALSE) +
          scale_color_manual(values=c("black", 'black', 'black','black','black','black')) + 

          ### points
          new_scale_color() +
          geom_point(aes(y=group, x=mean, fill=region,color=treatment),       
                     position= myjit, 
                     #position=position_dodge(),
                     size = 4, pch=21, stroke =1.8) +
          scale_fill_manual(values=c("#FEC287FF", "#8b7bc5"))  +
          scale_color_manual(values = c("black", "grey79")) +
          
          ### labels
          labs(y=" ",
               x= "Percent change", color="Comparison \nmethod", fill = "Region")+
          scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
          theme(strip.text = element_text(color="black", size = rel(2)),
                strip.background = element_rect( size = rel(5), color = 'grey', fill="grey"),
                axis.line = element_line(color='grey'),
                plot.background = element_blank(),
                panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                axis.text=element_text(size=12)) +
          guides(fill=guide_legend(override.aes=list(color=NA)))

ggsave("Percent_change.jpg", diff_plot, path="figures/",
       width=180, height=100,units="mm", dpi=500)              







## starting point: https://www.rebeccabarter.com/blog/2018-05-29_getting_fancy_ggplot2/
