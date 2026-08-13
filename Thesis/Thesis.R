#WD
setwd("~/")
setwd("Desktop/KU Leuven/Thesis")
getwd()

#Libraries
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(ggspatial)
library(geosphere)
library(patchwork)

#Data
plots <- read.csv("Data/veldwerkWP1WP2_219plots.csv")
head(plots)
str(plots)

#Map of 58 selected sites
plots_sf <- st_read("Data/WP1WP2_SampledLocations/WP1WP2_SampledLocations.shp")
plots_sf <- plots_sf %>%
  mutate(Management_en = recode(Management,
                                "schapen"  = "Sheep grazing",
                                "klepelen" = "Flail mowing",
                                "maaien"   = "Mowing"))
st_crs(plots_sf)
#Map of all 219 INBO sites
plots_219 <- read.csv("Data/veldwerkWP1WP2_219plots.csv") %>% distinct(plotID, .keep_all = TRUE)

plots_219 <- plots_219 %>%
  mutate(Eligibility = case_when(
    Suitability == "very suitable" ~ "Eligible",
    Suitability == "relatively suitable" ~ "Eligible (reduced\nsample size)",
    Suitability == "not really suitable" ~ "Excluded (ambiguous\nmanagement type)",
    Suitability == "definitely not suitable" ~ "Excluded",
    TRUE ~ NA_character_))
plots_219_sf <- st_as_sf(plots_219,
                         coords = c("Lambert72X_middentussenhoekpunten", "Lambert72Y_middentussenhoekpunten"),
                         crs = 31370) %>% st_transform(3857)

#Flow diagram for plot selection
plots_unique <- plots %>% distinct(plotID, .keep_all = TRUE)
nrow(plots_unique) #n = 219 for 2009/2010 samples by INBO
  plots_unique %>% count(SuitableLocation) #14 removed; 205 in suitable cluster
  plots_unique %>% count(SuitableDimensions) #5 not, 214 yes
  plots_unique %>% count(SuitableDikeSide) #210 yes, 9 no
  plots_unique %>% count(limiteddataORextensivedata) #119 limited, 100 extensive
  plots_unique %>% count(Management, sort = TRUE) #97 sheep, 45 flail mowing (klepelen), 
  #29 mowing (maaien), 14 grazed and mown (begraasd en gemaaid), 11 none (niets), 
  #9 mowed?, 6 flail mowing?, 4 flail mowing/mowing, 3 flail mowing/none, 1 sheep? (so 97 + 45 + 29 are ok)
  plots_unique %>% count(Vegtype) #56 = 1, 35 = 2, 41 = 3, 47 = 4, 40 = 50
  plots_unique %>% count(Suitability) #22 definitely now, 39 not really, 88 relatively, 70 very suitable
#to get flow diagram, these need to be ordered according to Merel's ppt: 
  #extensive data -> location -> dimensions -> dike side -> management
  step1 <- plots_unique %>% filter(limiteddataORextensivedata == "extensive data")
  n_removed_data <- nrow(plots_unique) - nrow(step1) #119 removed
  
  step2 <- step1 %>% filter(SuitableLocation == "yes")
  n_removed_location <- nrow(plots_unique) - nrow(step2) #127 removed
  
  step3 <- step2 %>% filter(SuitableDimensions == "yes")
  n_removed_dimensions <- nrow(step2) - nrow(step3) #1 removed
  
  step4 <- step3 %>% filter(SuitableDikeSide == "yes")
  n_removed_dikeside <- nrow(step3) - nrow(step4) #2 removed
  
  step5 <- step4 %>% filter(Management %in% c("schapen", "klepelen", "maaien"))
  n_removed_management <- nrow(step4) - nrow(step5) #19 removed
  
  tibble::tibble(
    stage = c("Start", "After data", "After location", "After dimensions", "After dike side", "After management"),
    n = c(nrow(plots_unique), nrow(step1), nrow(step2), nrow(step3), nrow(step4), nrow(step5)),
    removed = c(NA, n_removed_data, n_removed_location, n_removed_dimensions, n_removed_dikeside, n_removed_management))
  
  
#Maps
(map_of_58 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = st_transform(plots_sf, 3857), aes(color = Management_en), size = 2) +
  coord_sf() + xlab("Longitude") + ylab("Latitude") + labs(color = "Management") +
  scale_color_manual(values = c("dodgerblue2", "orange", "orangered3")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                          style = north_arrow_fancy_orienteering (text_col = 'black',
                                                                  line_col = 'black',
                                                                  fill = 'black')) +
  annotation_scale(location = "br", width_hint = 0.1, unit_category = "metric", style = "bar") +
  guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
ggsave("Plots/map1b.jpg", map_of_58, width = 10, height = 6)

(map_of_58_suitability <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 12) +
    geom_sf(data = st_transform(plots_sf, 3857), aes(color = Suitabilit), size = 2) +
    coord_sf() + xlab("Longitude") + ylab("Latitude") + labs(color = "Suitabilit") +
    scale_color_manual(breaks = c("very suitable", "relatively suitable"), 
                       values = c("darkgreen", "yellow2")) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering (text_col = 'black',
                                                                   line_col = 'black',
                                                                   fill = 'black')) +
    annotation_scale(location = "br", width_hint = 0.1, unit_category = "metric", style = "bar") +
    guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
ggsave("Plots/map1b.jpg", map_of_58, width = 10, height = 6)


(map_of_219 <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11) +
    geom_sf(data = st_transform(plots_219_sf, 31370), aes(color = Eligibility), size = 1.5) +
    coord_sf() + xlab("Longitude") + ylab("Latitude") + labs(color = "Eligibility") +
    scale_color_manual(breaks = c("Eligible", "Eligible (reduced\nsample size)", 
                                  "Excluded (ambiguous\nmanagement type)", "Excluded"), 
                       values = c("green2", "yellow2","orange2", "red2")) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering (text_col = 'black',
                                                                   line_col = 'black',
                                                                   fill = 'black')) +
    annotation_scale(location = "br", width_hint = 0.1, unit_category = "metric", style = "bar") +
    guides(color = guide_legend(ncol = 1)) + theme(legend.position = "right"))
ggsave("Plots/map1a.jpg", map_of_219, width = 8, height = 6)

(map_of_219_vegtype <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 11) +
    geom_sf(data = st_transform(plots_219_sf, 31370), aes(color = Vegtype), size = 1.5) +
    coord_sf() + xlab("Longitude") + ylab("Latitude") + labs(color = "Vegtype") +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering (text_col = 'black',
                                                                   line_col = 'black',
                                                                   fill = 'black')) +
    annotation_scale(location = "br", width_hint = 0.1, unit_category = "metric", style = "bar") +
    guides(color = guide_legend(ncol = 1)) + theme(legend.position = "right") +
    labs(color = "Vegetation type after\nVandevoorde et al. (2019)"))


(figure1ab <- map_of_219 + map_of_219_vegtype + plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold")))
ggsave("Plots/figure1ab.jpg", figure1ab, width = 12, height = 8)
