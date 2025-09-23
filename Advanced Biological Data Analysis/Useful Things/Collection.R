##%#########################################################################%##
#                                                                             #
#          A collection of useful functions that I've encountered             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd(setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Useful Things"))
getwd() #check that it's worked

#Libraries
library(ggmap)
library(ggpubr)
library(ggspatial)
library(readxl)
library(Rmisc)
library(tidyverse)
require(mgcv)

#Data
data("ToothGrowth")
df <- ToothGrowth
leaf <- read.csv("leaf.csv")
LMA <- read_excel("Data/LMA.xlsx")
mesocosm_data <- read.csv("Data/mesocosm_data.csv")
data("mtcars")
dfm <- mtcars
nobel <- read.csv("Data/nobel_prize_data.csv")
leaves <- read.csv("Data/traits_analysis.csv")


#Tidyverse basics ----
dim(nobel) #dimensions of the dataset
str(nobel) #class for the whole dataset
head(nobel) #fist few lines of the dataset
tail(nobel) #last few lines of the dataset
summary(nobel) #summary of the dataset

nobel <- nobel %>%
  mutate(pop_clean = gsub(",", "", population_2018)) %>% #replace the commas in the numbers
  mutate(pop_clean = gsub("[6]","", pop_clean, fixed=T)) %>% #'fixed = T' gives an exact match; delete the "[6]'
  mutate(pop_clean = as.numeric(pop_clean)) %>% #changed from character to numeric
  mutate(per_capita = nobel_prizes/pop_clean * 1000000) %>%
  mutate(continent = case_when(
    continent == "Central America" ~ "North America",
    continent == "Middle East" ~ "Asia",
    continent == "Australasia" ~ "Oceania",
    TRUE ~ continent
  )) #this moves some of the countries into another continent (i.e. bc we had some grouped into Australasia, Middle East, ... that are not really continents)

nobel %>% filter(continent == "Africa") #filter only data for Africa from the dataset

(trees.subset <- LMA %>%  
    filter(TID %in% c('Alder', 'Rowan', 'Birch', "Oak")) %>% #filter by tree species 
    group_by(TID, age, lma_final) %>% 
    tally() %>% mutate(TID = recode(TID, "Alder" = "A. glutinosa",
                                    "Birch" = "B. pendula",
                                    "Oak" = "Q. robur",
                                    "Rowan" = "S. aucuparia"))) #count how many samples are in each group


#Summarise (dplyr and summarySE) ----
(summary <- summarise(LMA, total.lma = sum(lma_final), #total LMA
                     mean.lma = mean(lma_final), #mean LMA
                     sd.lma = sd(lma_final))) #sd of LMA values

(summ.all <- summarise_all(LMA, mean)) #gives mean of all numerical categories (some columns are non-numerical so it gives an error)

(leaf_fummary <- summarySE(data = leaf, measurevar = "stomata_final", 
                           groupvars = c("type"), na.rm = TRUE)) #gives the mean, sample size, sd, se, and ci

#Count (dplyr) ----
(trees.grouped <- group_by(LMA, TID)) #groups by tree species
(trees.summary <- tally(trees.grouped)) #counts how many samples in each group

#Aggregate() ----
aggregate(LMA$lma_final, list(LMA$TID), FUN = mean) #FUN= can be mean, sd, ...



#Expressions ----
expression(The ~ "~" ~ character ~ forms ~ spaces)
expression(Super ^ script~text) #script will be in superscript
expression(Super ^ "script text") #script text will be in superscript
expression(Speed ~ bold(ms ^ -1)) #precede each fontface by a ~ or a * for it to work

opt <- par(cex = 1.5)
plot(1:10, 1:10, type = "n", 
     xlab = "X-vals", ylab = "Y-vals")

text(1, 1, expression(hat(x)))
text(2, 1, expression(bar(x))) #for the mean sign 
text(2, 2, expression(alpha==x))
text(3, 3, expression(beta==y))
text(4, 4, expression(frac(x, y)))
text(5, 5, expression(sum(x)))
text(6, 6, expression(sum(x^2)))
text(7, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(8, 8, expression(sqrt(x)))
text(9, 9, expression(sqrt(x, 3)))
par(opt)




#ggplot2 ----
Water <- subset(mesocosm_data, area == "water")
Foliage <- subset(mesocosm_data, area == "foliage")
Sediment <- subset(mesocosm_data, area == "sediment")
Global <- subset(mesocosm_data, area == "Total")
Area <- as.factor(mesocosm_data$area)

meso <- mesocosm_data %>% 
  mutate(area = recode(area, "water" = "Water",
                       "sediment" = "Sediment", 
                       "foliage" = "Foliage", 
                       "Total" = "Global community"))

ggplot(meso, aes(x=rank, y=pi, group=area, color=area)) + 
  geom_line() +
  geom_point(size=0.9) +
  scale_color_manual(values = c("springgreen4", "red", "orange", "royalblue")) + 
  theme_classic() + labs(x = "Rank") + labs(y = "Relative abundance") +
  theme(legend.position = c(0.90, 0.87)) + 
  theme(legend.title=element_blank())

#glm
ggplot(data = LMA, aes(x = age, y = lma_final)) +     
  geom_point(color='blue') +    
  geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +  
  theme_classic()

#ggpubr ----
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58))) #some imaginary data -> shows 200 of each male and female weight datapoints 

#Histogram
gghistogram(wdata, x = "weight",
            add = "mean", rug = TRUE,
            color = "sex", fill = "sex",
            palette = c("#00AFBB", "#E7B800"))

#Boxplot (with p-values)
(p <- ggboxplot(df, x = "dose", y = "len",
               color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter", shape = "dose"))

my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
(p + stat_compare_means(comparisons = my_comparisons)+ #Add pairwise comparisons p-value
  stat_compare_means(label.y = 50))                   #Add global p-value

#Boxplot pt.2
alder <- subset(LMA, TID == "Alder")
birch <- subset(LMA, TID == "Birch")
oak <- subset(LMA, TID == "Oak")
rowan <- subset(LMA, TID == "Rowan")
tid <- as.factor(LMA$TID)

my_data <- data.frame (  
  group = tid,  
  LMA = c(alder$lma_final, birch$lma_final, oak$lma_final, rowan$lma_final),  
  age = c(alder$age, birch$age, oak$age, rowan$age)
) %>% mutate(group = recode(group, "Alder" = "A. glutinosa",
                            "Birch" = "B. pendula",
                            "Oak" = "Q. robur",
                            "Rowan" = "S. aucuparia"))

ggboxplot(my_data, x = "group", y = "LMA",            
          fill = "group",
          ylab = "Mean LMA (g/m^2)",
          xlab = "Tree species",
          legend = "none") +
  font("x.text", face = "italic") +
  scale_fill_manual(values = c("darkorange", "violetred", "skyblue1", "aquamarine4")) +
  theme(text = element_text(size = 9)) +
  annotate("text", x = 1, y = 112, label = "53.59 ± 24.50") +
  annotate("text", x = 2, y = 112, label = "68.20 ± 24.80") + 
  annotate("text", x = 3, y = 112, label = "57.15 ± 22.30") +
  annotate("text", x = 4, y = 112, label = "66.01 ± 13.8")

#Barplot
dfm$cyl <- as.factor(dfm$cyl)
dfm$name <- rownames(dfm)
head(dfm[, c("name", "wt", "mpg", "cyl")])

ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",  #change fill color by cyl
          color = "white", #set bar border colors to white
          palette = "jco",           
          sort.val = "desc", #don't the value in descending order
          sort.by.groups = FALSE, #don't sort inside each group
          x.text.angle = 90) #rotate the x-axis text vertically


ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl", #change fill color by cyl
          color = "white", #set bar border colors to white
          palette = "jco",           
          sort.val = "asc", #sort the value in descending order
          sort.by.groups = TRUE, #sort inside each group
          x.text.angle = 90) #rotate the x-axis text vertically

#Scatterplot (with regression lines)
lma_total_mod <- lm(lma_final ~ age, data = LMA)
anova(lma_total_mod) #p = 0.7734, df = 38
(coef <- coefficients(lma_total_mod))
(intercept <- coef[1])
(slope <- coef[2])

alder_mod <- lm(lma_final ~ age, data = alder)
anova(alder_mod) 
(coef_alder <- coefficients(alder_mod))
(intercept_alder <- coef_alder[1])
(slope_alder <- coef_alder[2])

birch_mod <- lm(lma_final ~ age, data = birch)
anova(birch_mod)
(coef_birch <- coefficients(birch_mod))
(intercept_birch <- coef_birch[1])
(slope_birch <- coef_birch[2])

oak_mod <- lm(lma_final ~ age, data = oak)
anova(oak_mod)
(coef_oak <- coefficients(oak_mod))
(intercept_oak <- coef[1])
(slope_oak <- coef[2])

rowan_mod <- lm(lma_final ~ age, data = rowan)
anova(rowan_mod)
(coef_rowan <- coefficients(rowan_mod))
(intercept_rowan <- coef_rowan[1])
(slope_rowan <- coef_rowan[2])

(plot1 <- ggplot(my_data, aes(x=age, y = LMA, group = group)) +  
  geom_point(aes(color=group), size = 1.5)+ 
  theme_classic() +  
  scale_color_manual(values=c("darkorange", "violetred","skyblue1", "aquamarine4")) +  
  geom_abline(intercept = intercept, slope = slope, color = "black", linetype = "dashed", size = 0.5) +  
  annotate("text", x = 100, y = 68, size = 3,  label=expression("R"^2* " = 0.002"), color = "black") +  
  theme(legend.position = c(0.9, 0.9)) +  
  theme(legend.title = text("Species"))+  
  theme(legend.title=element_blank()) +  
  labs(x = "Tree age (years)", y = expression("Mean LMA (g/m"^2*")")))

(plot2 <- ggplot(my_data, aes(x=age, y = LMA, group = group)) +  
  geom_point(aes(color=group), size = 1.5)+ 
  theme_classic() +  
  scale_color_manual(values=c("darkorange", "violetred","skyblue1", "aquamarine4")) +  
  geom_abline(intercept = intercept_oak, slope = slope_oak, color = "skyblue1", linetype = "dashed", size = 0.5) +  
  annotate("text", x = 100, y = 68, size = 3,  label=expression("R"^2* " = 0.014"), color = "skyblue1") +  
  geom_abline(intercept = intercept_rowan, slope = slope_rowan, color = "aquamarine4", linetype = "dashed", size = 0.5) +  
  annotate("text", x = 105, y = 81, size = 3, label=expression("R"^2* " = 0.008"), color = "aquamarine4") +  
  geom_abline(intercept = intercept_birch, slope = slope_birch, color = "violetred", linetype = "dashed", size = 0.5) +  
  annotate("text", x = 109, y = 73, size = 3,  label=expression("R"^2* " = 0.003"), color = "violetred") +  
  geom_abline(intercept = intercept_alder, slope = slope_alder, color = "darkorange", linetype = "dashed", size = 0.5) +  
  annotate("text", x = 100, y = 57, size = 3,  label=expression("R"^2* " = 0.003"), color = "darkorange") +  
  theme(legend.position = "none") +  theme(legend.title = text("Species"))+  
  theme(legend.title=element_blank()) +  
  labs(x = "Tree age (years)", y = expression("Mean LMA (g/m"^2*")")))

grid.arrange(plot1, plot2, ncol = 2)

#or an even quicker way
group <- as.factor(my_data$group)

ggplotRegression <- function (fit) {    
  require(ggplot2)    
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(aes(color=group), size = 1.5) +
    scale_color_manual(values=c("darkorange", "violetred","skyblue1", "aquamarine4")) +
    theme_classic() +
    labs(x = "Tree age (years)", y = expression("Mean LMA (g/m"^2*")")) +
    theme(legend.title=element_blank()) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       "Slope =",signif(fit$coef[[2]], 5),
                       "P =",signif(summary(fit)$coef[2,4], 5)))}

ggplotRegression(lm(lma_final ~ age, data = LMA)) #The shaded band behind the reg line 
#indiciates the 95% confidence interval of the line (the shaded band contains the 
#true (population) regression line with 95% probability). 

#facet_wrap()
ggscatter(my_data, x = "age", y = "LMA",  
          color = "group", palette = "jco",  add = "reg.line") + 
  labs(x = "Tree age (years)", y = expression("Mean LMA (g/m"^2*")")) +
  facet_wrap(~group) +  
  stat_cor(aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 20) +  
  theme_classic() +
  theme(legend.position="none")

#quadratic and polynomial regression lines
#simple linear model
(plot_1 <- ggplot(data = LMA, aes(x = age, y = lma_final)) +   
  geom_point(color='blue') +  
  stat_smooth(method = "lm", formula = y ~ x, size = 1) +  
  theme_classic())

#loess
(plot_2 <- ggplot(data = LMA, aes(x = age, y = lma_final)) +   
  geom_point(color='blue') +  
  stat_smooth(method = "loess", formula = y ~ x, size = 1) +  
  theme_classic())

#quadratic
(plot_3 <- ggplot(data = LMA, aes(x = age, y = lma_final)) +   
  geom_point(color='blue') +  
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +  
  theme_classic())

#second order polynomial
(plot_4 <- ggplot(data = LMA, aes(x = age, y = lma_final)) +   
  geom_point(color='blue') +  
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
  theme_classic())

#generalised additive models (mgcv package)
(plot_5 <- ggplot(data = LMA, aes(x = age, y = lma_final)) +   
  geom_point(color='blue') +  
  stat_smooth(method = "gam", formula = y ~ s(x), size = 1) +
  theme_classic())

#compare all the lines
(plot_6 <- ggplot(data = LMA, aes(x = age, y = lma_final)) +   
  geom_point(color='blue') +  
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE, colour = "black") + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, colour = "blue") + 
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red") + 
  stat_smooth(method = "gam", formula = y ~  s(x), size = 1, se = FALSE, colour = "green") + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = FALSE, colour = "violet") +  
  theme_classic())

grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, ncol = 3)

#ggmap ----
leaves <- leaves %>% 
  select("type", "code", "latin_name", "long", "lat") %>%  #select the relevant columns
  mutate(type = recode(type, "Alien" = "Alien species",
                       "Invasive" = "Invasive species", 
                       "Naturalised" = "Naturalised species", 
                       "Native" = "Native species")) %>%  #recode the invasion type names
  distinct(long, lat, .keep_all = TRUE) #remove multiple rows (avoids overplotting)

ggmap::register_google(key = "AIzaSyDnersipSvcXuK4tCDbr8NOpa-qsrYf9pc", 
                       write = TRUE) #register your own Google API Key here

(edinburgh <- get_googlemap("edinburgh", zoom = 16))
rbge <- c(left = -3.2140, bottom = 55.9627, right = -3.2025, top = 55.9682) #set the map view window accordingly; I want to view the Botanics
edi_map_satellite <- get_map(rbge, maptype ='satellite', source = "google", zoom = 16) #specify what kind of map you want


(rbge_map_with_names <- ggmap(edi_map_satellite) +
    geom_point(data = leaves, aes(x = long, y = lat, color = type, shape = type), 
               size = 3) +
    scale_color_manual(values = c("#5EA8D9", "#CD6090", "#2CB82E", "#EEC900"),
                       name = "Invasion type") +
    scale_shape_manual(values = c(16, 17, 18, 15), name = "Invasion type") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = c(0.85, 0.87),
          legend.key = element_rect(fill = "floralwhite"),
          legend.background = element_rect(fill = "floralwhite")) +
    ggrepel::geom_label_repel(data = leaves, aes(x = long, y = lat, label = latin_name),
                              max.overlaps = 200, box.padding = 0.5, point.padding = 0.1, 
                              segment.color = "floralwhite", size = 3, fontface = "italic") +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering (text_col = 'floralwhite',
                                                                   line_col = 'floralwhite',
                                                                   fill = 'floralwhite')))

(map_with_codes <- ggmap(edi_map_satellite) +
    geom_point(data = leaves, aes(x = long, y = lat, color = type, shape = type), 
               size = 3) +
    scale_color_manual(values = c("#5EA8D9", "#CD6090", "#698B69", "#EEC900"),
                       name = "Invasion type") +
    scale_shape_manual(values = c(16, 17, 18, 15), name = "Invasion type") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = c(0.85, 0.87),
          legend.key = element_rect(fill = "floralwhite"),
          legend.background = element_rect(fill = "floralwhite")) +
    ggrepel::geom_label_repel(data = leaves, aes(x = long, y = lat, label = code),
                              max.overlaps = 200, box.padding = 0.5, 
                              point.padding = 0.1, segment.color = "floralwhite", 
                              size = 3, fontface = "italic") +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering (text_col = 'floralwhite',
                                                                   line_col = 'floralwhite',
                                                                   fill = 'floralwhite')))

(rbge_grid <- grid.arrange(rbge_map_with_names, map_with_codes, ncol = 2))


