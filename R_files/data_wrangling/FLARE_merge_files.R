########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED APRIL 2023
### FOR MERGING ALL FILES AT THE FLARE PLOTS
########



###############################
### load in data
##############################

density<-read.csv('tree_recruit_density.csv')
soil<-read.csv('Flare_soil_C.csv')
tree1<-read.csv('Cherskiy_tree_C.csv')
tree2<-read.csv('Yakutsk_tree_C.csv')
crownsev<-read.csv('CrownSeverity.csv')
shrub<-read.csv('FLARE_shrubs_C.csv')
dnbr<-read.csv('FLARE_site_dnbr.csv')
wd<-read.csv('FLARE_WD_C.csv')
info<-read.csv('FLARE_site_info.csv')
Y_reconst<-read.csv('Yakutsk_reconstructed_trees.csv')
C_reconst<-read.csv('Cherskiy_reconstructed_trees.csv')
basal_area<-read.csv('basal_area.csv')

###############################
### prepare data
##############################

dnbr<- dnbr %>% select (dnbr, transect, plot, site)
info<- info %>% select (region, transect, plot, site, burn_year, up_flood, slope, aspect, char_percent, elevation)

tree2$plot<-as.character(tree2$plot)
trees<-bind_rows(tree1,tree2)
###############################
### merge data
##############################

list_of_dfs<-list(density, soil, trees, crownsev, shrub,dnbr,wd, info, basal_area)

merged_df<- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE,
                                           by=c("site", "transect", "plot")), list_of_dfs) %>% 
                                          drop_na(site, region) %>% filter (site != 'Duck')

###############################
### calculate AGB 
##############################
merged_df <- merged_df  %>% rowwise()  %>% dplyr::mutate(
  AGB_gCm2 = sum(
    trees_gCm2,
    snags_gCm2,
    shrub_gCm2,
    FWD_gCm2,
    CWD_gCm2, na.rm=TRUE))

###############################
### Add in burned/unburned column
##############################

merged_df$treatment<-as.factor(ifelse(merged_df$plot>0, 'burned', 'unburned'))



###############################
### Add in reconstructed data
##############################
C_reconst$plot<-as.character(C_reconst$plot)
Y_reconst$plot<-as.character(Y_reconst$plot)
finaldf<-bind_rows(merged_df, Y_reconst, C_reconst)

###############################
### Write.csv
##############################


write.csv(finaldf, row.names=FALSE,
          file = "FLAREdata.csv")
        