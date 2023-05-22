########
### THIS CODE WRITTEN BY ELIZABETH WEBB, LAST UPDATED MARCH 2023
### FOR CALCULATING SCORCH INDEX
########

## load in libraries
library(tidyr)
library(dplyr)
library(plotrix)


### load in data
# crown severity data
trees<-read.csv("tree_survey_2018_2019.csv", stringsAsFactors = FALSE)

#### data prep

# Don't include Duck fire
trees<-subset(trees,site != 'Duck')
trees$crown_severity<-as.numeric(trees$crown_severity)

# aggregate by plot
perplot<- trees %>% group_by(site, transect, plot) %>% 
               summarise(crown_severity=mean(crown_severity))
  
  
write.csv(perplot_info, row.names=FALSE,
          file = "PostDoc/mortality/data/CrownSeverity.csv")

