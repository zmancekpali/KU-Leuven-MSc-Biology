
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Thesis")
getwd()

plots <- read.csv("veldwerkWP1WP2_219plots.csv")

library(dplyr)


(plots.subset <- plots %>%  
    filter(Management %in% c('schapen', 'klepelen', 'maaien')) %>% #filter by tree species 
    filter(Suitability %in% c('very suitable')) %>%
    group_by(Vegtype, Management) %>% 
    count()) #count how many samples are in each group
length(plots.subset$Management)

(plots.subset1 <- plots %>%  
    filter(Management %in% c('schapen', 'klepelen', 'maaien')) %>% #filter by tree species 
    filter(Suitability %in% c('very suitable')) %>%
    group_by(Vegtype, Management) %>% 
    count()) #count how many samples are in each group
