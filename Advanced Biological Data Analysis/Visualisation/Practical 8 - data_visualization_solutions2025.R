# DATA VISUALISATION : SOLUTIONS #### 

# set working directory, adjust path as appropriate (Session...Set Working Directory...To Source File Location)
setwd("")

library(ggplot2)
library(openxlsx)
library(dplyr)
library(lattice) 
library(ggsignif)
library(gridExtra)
library(effects)
library(emmeans)
library(afex)
library(grid)
library(ggpubr)
library(ggthemes)
library(ggprism)
library(scales)
library(svglite)
library(gtools) # for the stars.pval function
library(export)
library(car)

set_sum_contrasts() # use effect coding for all fitted models


# 1. EXERCISE 1 ####

# We have data ("isolation.xlsx") on the presence or absence of a species on
# several islands in an archipelago. For each island, we also have its size
# (km2) and the distance to the mainland (in km). We are interested in whether
# the species" presence on an island can be predicted by how big it is and how
# far it is from the mainland. To investigate this, we fit a binomial GLM and
# make effect plots to show the effect of both area and distance :

d1 = read.xlsx("isolation.xlsx")
fit_ex1 = glm(presence ~ area + distance, family=binomial(link=logit), data=d1)

plot(allEffects(fit_ex1, xlevels=100), type="response") # NOTE: xlevels=100 added here to show effect plot for 100 X values; 
#For a continuous predictor, xlevels contains a sequence of values at which you want to evaluate the effects.


## AIM ####

# Make more beautiful effect plots illustrating the effect of area and distance
# using ggplot2, using marginal means calculated with the emmeans package 

# a) coerce their output to a dataframe before plotting using data.frame.

# b) use a geom_ribbon ggplot2 layer to illustrate the 95% confidence intervals and a
# geom_line layer to illustrate the marginal means. 
# c) Also superimpose the actual datapoints from the dataset (in dataframe d1), using a semi-transparent colour
# and solid disks (shape=16). 
# d) Also provide appropriate axis titles. 
# e) Use theme_few with a base_size of 16. 

# f) Show your 2 graphs below each other in a single column using grid.arrange and specify a common spanning Y axis title
# using argument left being provided with a correctly specified textGrob (with
# rot=90 and gp = gpar(fontsize = 17)), and removing the previously specified Y
# axis titles using + rremove("ylab"), see ?grid.arrange and ?textGrob.
# g) Export the figure to Powerpoint .pptx and .svg formats at a size of 4 x 7 inches.


#step a: making a dataframe of the effect of area and distance on the probability of presence

# First we need to extract the marginal means for both area and distance over 
# approximately the range that they vary in the dataset (here slightly extended).

# To do this we will need the "at=" argument in which we specify a list with a
# sequence of the desired values of each variable (using seq with length.out=100
# to get 100 values between those extremes) :

# for the effect of area:
fit1_ex1_area_df = data.frame(emmeans(fit_ex1, ~ area, at = list(area = seq(0, max(d1$area)+1, length.out=100)), 
                        type="response"))
head(fit1_ex1_area_df) # check column names to use in ggplot
colnames(fit1_ex1_area_df)

# for the effect of distance:
fit1_ex1_distance_df = data.frame(emmeans(fit_ex1, ~ distance, at = list(distance = seq(0, max(d1$distance)+2, length.out=100)), 
                        type="response"))
head(fit1_ex1_distance_df) # check column names to use in ggplot
colnames(fit1_ex1_distance_df)

# ggplot2 plots:
plot1 = ggplot(data=fit1_ex1_area_df, aes( x=area , y=prob ))+                           # specify dataframe & x & y variables
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha = 0.1)+                         # show 95% confidence intervals using a semitransparent ribbon
  geom_line(linewidth=1)+                                                                # show marginal mean with linewidth of 1
  # Important for plotting from the original dataset is to include it in the front and then use the mapping so the function knows where to get the data
  geom_point(data=d1, mapping=aes(x=area, y=presence), shape=16, color="red2", size=3, alpha=0.3)+ # show original datapoints as semitransparent red disks
  xlab(expression("Island area (km"^2*")"))+                                             # X axis title, using expression to show ^2 as superscript
  ylab("Probability of species being present")+                                          # Y axis title
  theme_few(base_size=16)                                                                # theme_few with base text size of 16
plot1

plot2 = ggplot(data=fit1_ex1_distance_df, aes( x=distance , y=prob ))+                   # specify dataframe & x & y variables
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha = 0.1)+                         # show 95% confidence intervals using a semitransparent ribbon
  geom_line(linewidth=1)+                                                                # show marginal mean with linewidth of 1
  # Important for plotting from the original dataset is to include it in the front and then use the mapping so the function knows where to get the data
  geom_point(data=d1, mapping=aes(x=distance, y=presence), shape=16, color="red2", size=3, alpha=0.3)+ # show original datapoints as semitransparent red disks
  xlab("Distance from mainland (km)")+                                                   # X axis title, .
  ylab("Probability of species being present")+                                          # Y axis title
  theme_few(base_size=16)                                                                # theme_few with base text size of 16
plot2

# grid.arrange allows you to put multiple plots in a specific arrangement of your choice
# (here in a single column) & allows you to specify a common spanning Y axis title using
# argument left with a correctly specified textGrob (with rot = 90, gp = gpar(fontsize = 17)).
# Before adding the common Y axis title you can remove the previously provided separate Y axis titles
# by passing plot1 + rremove("ylab") & plot2 + rremove("ylab") as your ggplot objects  :
plot3 = grid.arrange(plot1 + rremove("ylab"), 
                     plot2 + rremove("ylab"), 
                     left = textGrob("Probability of species being present", # common spanning Y axis title
                                     rot = 90, gp = gpar(fontsize = 17)), 
                     ncol=1) # single column (one figure below the other)
plot3
?textGrob
#The textGrob function in R is part of the grid package, which is used for creating graphical objects in a grid layout. The textGrob function specifically is 
#used to create a text graphical object (a "grob") that can be placed on a plot. It allows you to specify various attributes of the text, 
#such as its content, position, font, color, and alignment.

# export to .svg (editable with Inkscape) at size of 4 x 7 inches :
ggsave(plot3, file="1_effect_of_area_and_distance_on_speciespresent.svg", width=4, height=7)


# 2. EXERCISE 2 ####

# In a study by Maes et al. (2005), the authors investigated influence of
# genetic variation (measured as the "multilocus allozyme heterozygosity") on
# bioaccumulation of heavy metals in three different river basins (Maas, IJzer,
# Schelde) in the Atlantic eel (Anguilla anguilla). The hypothesis was that
# individuals with a greater multilocus heterozygosity score
# (MULTILOCUS_HETEROZYGOSITY_ALLOZYME) can better "detoxify" themselves compared
# to homozygous individuals, and we are also interested in how this relationship
# might be different between the different river systems. To this end, we fitted
# a homogeneity-of-slopes linear model & made an effect plot showing the
# relationship between heavy metal accumulation & multilocus heterozygosity in
# the different river basins. The first aim is to remake this effect plot in a more
# beautiful way using emmeans & ggplot2. A second aim is to plot the estimated
# slopes of the heavy metal accumulation in function of multilocus heterozygosity
# across the different rivers plus overall (calculated using emtrends) & annotate
# the significance of the difference from zero.


# load data
eel = read.xlsx("eel2.xlsx")
eel = mutate_if(eel, is.character, as.factor)
# specify desired order & custom labels (this will be order in later plots,
# here ordered from highest avg heavy metal accumulation to lowest) :
eel$RIVER = factor(eel$RIVER, 
                   levels=c("MAAS", "SCHELDE", "IJZER"),
                   labels=c("Maas", "Schelde", "Ijzer"))
eel$RIVER # all caps looked a bit ugly in later plot, so changed labels

# this was our homogeneity of slopes model
fit_ex2 = lm(HEAVY_METAL_ACCUM ~ RIVER * MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=eel) 
Anova(fit_ex2, type="III")

# effect plot of combined effects of river & multilocus heterozygosity 
plot(allEffects(fit_ex2))

#here we already made the effect plot a bit nicer:
plot(Effect(focal.predictors=c("MULTILOCUS_HETEROZYGOSITY_ALLOZYME","RIVER"),
            mod=fit_ex2, residuals=T), 
     smooth.residuals=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2), 
     residuals.pch=16,
     band.colors="grey2")


## AIM 1 ####

# Make more beautiful effect plot illustrating the effect of river and
# multilocus heterozygosity using ggplot2, using marginal means calculated with
# the emmeans package, and coerce output to a dataframe before plotting
# using as.data.frame. 

# Use a geom_ribbon ggplot2 layer to illustrate the 95%
# confidence intervals and a geom_line layer to illustrate the marginal means.
# Facet by river & show the different river systems in a single row. 
# Colour points & lines by RIVER & give them the colours red2, purple & blue.
# Use a semi-transparent fill in the same colours for the confidence interval
# ribbons & omit the outlines by specifying color=NA in the geom_ribbon layer.
# Also superimpose the actual datapoints from the dataset (in dataframe eel),
# using a semi-transparent colour and solid disks (shape=16). 
# Also provide appropriate axis titles. 
# Use theme_par with a base_size of 8. 
# Also provide appropriate options via theme to show the X axis labels
# rotated 45 degrees and adjust your axis title size to use a font size of 12.
# Suppress the legend, which is here superfluous.
# Export the figure in Powerpoint .pptx and .svg formats at a size of 6.5 x 4.5 inches.


# First: calculate the marginal means with emmeans (try it yourself this time):

# Calculate marginal means by river & multilocus heterozygosity (RIVER * MULTILOCUS_HETEROZYGOSITY_ALLOZYME) for 100 values  of 
# multilocus heterozygosity, ranging between the extremes (min and max) observed in the dataset
# using emmeans (and again the "at" argument) & convert output to a dataframe using as.data.frame


fit_ex2_emmeans_df = data.frame( emmeans(fit_ex2, ~ RIVER * MULTILOCUS_HETEROZYGOSITY_ALLOZYME, 
                                      at = list(MULTILOCUS_HETEROZYGOSITY_ALLOZYME = seq(min(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME),
                                                                                         max(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME),
                                                                                         length.out=100))) )
head(fit_ex2_emmeans_df) # check column names to use in ggplot
colnames(fit_ex2_emmeans_df)

# Make ggplot:
plot4 = ggplot(data = fit_ex2_emmeans_df,                                                # dataframe with emmeans
             aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, y = emmean, color = RIVER))+    # specify x & y & color points and lines by RIVER
  facet_wrap(~RIVER, nrow=1)+                                                            # facet by RIVER
  geom_ribbon(aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, ymin = lower.CL, ymax = upper.CL, fill = RIVER), 
              alpha = 0.1, color = NA)+                                                  # add 95% confidence intervals ribbon, using ribbon transparancy of 0.1 and color=NA (or linetype=0) to remove outline
  geom_line(linewidth = 1)+                                                              # marginal means shown as lines with line thicknes of 1
  geom_point(eel,mapping=aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME,
                             y = HEAVY_METAL_ACCUM, color = RIVER), 
             shape = 16, alpha = 0.3)+                                                   # original data points shown as semi-transparent solid disks
  scale_color_manual(values=c("red2", "purple", "blue"))+                                # manual colours for lines & points
  scale_fill_manual(values=c("red2", "purple", "blue"))+                                 # manual colours for ribbon fill
  ylab("Heavy metal accumulation")+                                                      # Y axis title
  xlab("Allozyme multilocus heterozygosity")+                                            # X axis title
  theme_par(base_size=8) +                                                               # use theme theme_par with base_size for text of 8
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),                            # rotate X axis labels 45 degrees & adjust placement with hjust & vjust
        axis.title=element_text(size=12)) +                                              # adjust axis titles to font size of 12
  theme(legend.position="none")                                                          # leave out legend (redundant)

plot4

# export to .svg (editable with Inkscape) at size of 6.5 x 4.5 inches:
ggsave(plot4, file="2A_heavy_metal_accumulation_vs_heterozygosity_per_river.svg", width=6.5, height=4.5)


# #EXTRA: PREDICTION INTERVALS
#
# # NOTE: In the same way we can also plot total model predictions. In that case, we can
# # include both confidence & prediction intervals and superimpose them as two
# # semi-transparent geom_ribbon layers. 
# 
# # We then first have to calculate predictions & CIs & PIs over a grid of specific covariate values : 
# 
# # Dataframe with values of independent variables for which you would like to 
# # get predicted values for :
newdata = expand.grid(MULTILOCUS_HETEROZYGOSITY_ALLOZYME = seq(min(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME), 
                                                               max(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME)+0.05, 
                                                           length.out=100),
                     RIVER = levels(eel$RIVER))

# # Dataframe with model predictions & 95% confidence & prediction intervals
# # for covariate values in newdata :
preds = newdata %>% # we are using dplyr here; we are adding predictions & CIs & PIs to newdata
mutate(data.frame(predict(fit_ex2, # dataframe with confidence intervals
                            newdata = newdata, 
                             interval = "confidence", 
                            level = 0.95))) %>% # mutate = add columns
  rename(lower.CI = lwr, upper.CI = upr) %>% # rename columns: NEW_NAME = OLD_NAME
  mutate(data.frame(predict(fit_ex2, # dataframe with prediction intervals
                            newdata = newdata, 
                            interval = "prediction", 
                            level = 0.95))) %>% # mutate = add columns
  rename(lower.PI = lwr, upper.PI = upr) # rename columns: NEW_NAME = OLD_NAME
head(preds)
# 
# # Make ggplot:
 plot4B = ggplot(data = preds,                                                # dataframe with model predictions & CIs & PIs
                aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, y = fit, color = RIVER))+     # specify x & y & color points and lines by RIVER
  facet_wrap(~RIVER, nrow=1)+                                                            # facet by RIVER
   geom_ribbon(aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, ymin = lower.CI, ymax = upper.CI, fill = RIVER), 
               alpha = 0.4, color = NA)+                                                  # add 95% confidence intervals ribbon, using ribbon transparancy of 0.4 and color=NA (or linetype=0) to remove outline
   geom_ribbon(aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, ymin = lower.PI, ymax = upper.PI, fill = RIVER), 
               alpha = 0.2, color = NA)+                                                  # add 95% prediction intervals ribbon, using ribbon transparancy of 0.2 and color=NA (or linetype=0) to remove outline
  geom_line(linewidth = 1)+                                                              # model predictions shown as lines with line thicknes of 1
 geom_point(eel,mapping=aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME,
                              y = HEAVY_METAL_ACCUM, color = RIVER), 
              shape = 16)+                                                                # original data points shown as non-transparent solid disks
  scale_color_manual(values=c("red2", "purple", "blue"))+                                # manual colours for lines & points
   scale_fill_manual(values=c("red2", "purple", "blue"))+                                 # manual colours for ribbon fill
   ylab("Heavy metal accumulation")+                                                      # Y axis title
   xlab("Allozyme multilocus heterozygosity")+                                            # X axis title
   theme_grey(base_size=10) +                                                             # here using default grey theme with base_size for text of 10
   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),                            # rotate X axis labels 45 degrees & adjust placement with hjust & vjust
         axis.title=element_text(size=12)) +                                              # adjust axis titles to font size of 12
   coord_cartesian(expand=FALSE) +                                                        # make ribbon stretch to side of plot & don"t extend axis range
   theme(legend.position="none")                                                          # leave out legend (redundant)
 plot4B
# 
# # export to Powerpoint at size of 6.5 x 4.5 inches:
# graph2ppt(file="2A_EXTRA_heavy_metal_accumulation_vs_heterozygosity_per_river_model_predictions_with_CIs_and_PIs.pptx", width=5.5, height=3)
# 
# # export to .svg (editable with Inkscape) at size of 6.5 x 4.5 inches:
# ggsave(plot4B, file="2A_EXTRA_heavy_metal_accumulation_vs_heterozygosity_per_river_model_predictions_with_CIs_and_PIs.svg", width=5.5, height=3)



## AIM 2 ####

# Plot the marginal mean trends of the heavy metal accumulation in function of
# heterozygosity. 


# for the plot:
# Use red2, purple, blue and black for the estimates for the 3 rivers & the overall mean slope. 
# Use a geom_errorbar layer for the confidence intervals (using a width of 0.2 and a
# linewidth of 0.5) and a geom_point layer for the actual estimated slopes (use
# a point size of 2). 

# Use theme_par with a base_size of 10. 
# Add an appropriate Y axis label & use a blank X axis
# title (not needed here). Rotate the X axis labels 45 degree & play with hjust
# & vjust to have them correctly positioned. Remove the legend as that one is
# superfluous. Export figure at 4 x 4.5 inches in .pptx Powerpoint and .svg
# formats.



# first, we need to calculate the marginal trends for the different rivers as well as overall,
# i.e. the slopes of heavy metal accumulation in function of heterozygosity plus 95% confidence intervals.
# we use emtrends (from the emmeans package) to estimate trends (= slopes) of the fit
# and store them in a dataframe fit_ex2_emtrends_df 

fit_ex2_emtrends_df = rbind(data.frame(confint(emtrends(fit_ex2, ~ RIVER, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"))),
                            data.frame(confint(emtrends(fit_ex2, ~ 1, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"))) %>% rename(RIVER=X1) )
fit_ex2_emtrends_df # this gives the slope per river and overall in a dataframe


#####EXTRA/OPTIONAL: we can also plot in ggplot asterisks of significances. In this case you can try it for slopes significantly different from 0, 
# to test if every slope is significant different from 0 we use 'test' (instead of 'contrast') and null=0 (instead of e.g. method="pairwise") also adjust
# for multiple testing with adjust="sidak"
####################################################################################################################################################

# the significances of the difference of these slopes from zero based on Sidak posthoc tests (using 'test' and adjust="sidak"):
fit_ex2_emtrends_signif_df = rbind(data.frame(test(emtrends(fit_ex2, ~ RIVER, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"), null=0, adjust="sidak")), 
                                   data.frame(test(emtrends(fit_ex2, ~ 1, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"), null=0) %>% rename(RIVER=1) )) %>%
  mutate(stars.pval = stars.pval(p.value)) # convert p value to sign asterisks & add it to dataframe, see ?gtools::stars.pval
fit_ex2_emtrends_signif_df

# we added the significance asterisk for the overall difference in slope across all rivers to our earlier dataframe
fit_ex2_emtrends_df$stars.pval = fit_ex2_emtrends_signif_df$stars.pval
fit_ex2_emtrends_df
#     RIVER MULTILOCUS_HETEROZYGOSITY_ALLOZYME.trend        SE df   lower.CL    upper.CL stars.pval
# 1    Maas                               -0.1269687 0.1774889 66 -0.4813366  0.22739932           
# 2 Schelde                               -0.3317813 0.1794692 66 -0.6901032  0.02654061           
# 3   Ijzer                               -0.3108771 0.2147662 66 -0.7396717  0.11791738           
# 4 overall                               -0.2565424 0.1104717 66 -0.4771063 -0.03597845          *
################################################################################################################################


# ggplot code:
plot5 = ggplot(fit_ex2_emtrends_df, aes(x=RIVER, y=MULTILOCUS_HETEROZYGOSITY_ALLOZYME.trend, color=RIVER))+ # specify input dataframe & x and y variables and colour by RIVER
  scale_color_manual(values=c("red2","purple","blue","black"))+                                             # specify custom colours
  geom_errorbar(aes(x=RIVER, ymin=lower.CL, ymax=upper.CL), width=0.2, linewidth=0.5)+   # add errorbars , change width and linewidth of the errorbars to 0.2 and 0.5
  geom_point(size=2)+                                                                    # plot estimated marginal slopes and change point size to 2
  geom_text(aes(label=stars.pval, y=upper.CL), vjust=-0.5, size=4) +                     # add significance labels for difference from zero using font size of 4 and place it above upper.CL   
  theme_par(base_size=10) +                                                              # use theme_par with base size of 10   
  ylab("Slope of Heavy Metal Accumulation\nvs. Multilocus Heterozygosity")+              # add Y axis title (use \n for break)
  xlab("")+                                                                              # use blank X axis title
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),                            # rotate X axis labels 45 degrees & adjust placement with hjust & vjust
        axis.title=element_text(size=10)) +                                              # adjust axis titles to font size of 12
  theme(legend.position="none")                                                          # legend not required here

plot5

# export to .svg (editable with Inkscape) at size of 4 x 4.5 inches:
ggsave(plot5, file="2B_slope_heavy_metal_accumulation_vs_heterozygosity_per_river.svg", width=4, height=4.5)




# 3. EXERCISE 3 ####

# We have a dataset ("bloodcells") with information on the number of damaged
# red blood cells per mm^2 in a microscope image counting grid/chamber for a number of
# individuals in function of their weight (in kg), their sex and
# whether or not they are a smoker. The determine whether those variables were 
# predictive of the number of damaged blood cells observed we fitted a Poisson glm.
# The aim is to recreate the standard effect plots for the sex:smoker and smoker:weight
# interaction effects in a prettier way using emmeans and ggplot2.

d3 = read.xlsx("bloodcells.xlsx")
d3 = mutate_if(d3, is.character, as.factor)
d3$smoker = factor(d3$smoker, levels=c("no","yes"), labels=c("non-smoker","smoker")) # recode factor levels to later allow us to omit legend title

# log link Poisson glm fit (best fitting model) :
fit_ex3 = glm(cells ~ smoker + weight + sex + sex:smoker + smoker:weight, family=poisson, data=d3)

# standard effect plot
plot(allEffects(fit_ex3), type="response", rug=F)

    
# effect plot also showing partial residuals
plot(allEffects( fit_ex3, residuals=T, xlevels=100) , type="response" , multiline=F,
                 smooth.residuals=F,
                 residuals.color=adjustcolor("red2",alpha.f=0.02), 
                 residuals.pch=16,
                 band.colors="blue",
                 ylim=c(0, 15) )   


# instead of using these rather ugly effect plots, we would like to plot the
# following marginal means calculated using emmeans to make a more beautiful
# multipanel effect plot using ggploT




## AIM ####

# Recreate the plot(allEffects(fit_ex3)) effect plots for the effects of
# the sex:smoker and smoker:weight interaction effects based on the calulated emmeans


# emmeans for effect of smoker & sex :
em_smoker_sex_df = data.frame(emmeans(fit_ex3, ~ smoker*sex, type="response"))
head(em_smoker_sex_df)
colnames(em_smoker_sex_df) # use these column names in your ggplot2 code

# emmeans for effect of smoker & weight :
em_smoker_weight_df = data.frame(emmeans(fit_ex3, ~ smoker*weight, type="response", 
                                         at = list(weight = seq(round(min(d3$weight)),
                                                                round(max(d3$weight)),
                                                                by=1))))
head(em_smoker_weight_df)
colnames(em_smoker_weight_df) # use these column names in your ggplot2 code



### EXTRA
# in the smoker x sex interaction effect effect plot we would also like to show
# significance asterisks for the contrasts in the red blood cells damage
# between smoking & nonsmoking men & woman based on the following
# posthoc tests
contrasts_df = data.frame(contrast(emmeans(fit_ex3, ~ smoker|sex, type="response"), method="revpairwise")) %>%
  mutate(stars.pval = stars.pval(p.value), # convert p value to sign asterisks & add it to dataframe, see ?gtools::stars.pval
         rate = c(2.75, 1.5),              # Y value where you would like significance asterisks to appear
         smoker = c(NA, NA))               # NA column for smoker (required when this dataframe is plotted in a geom_text layer alongside the other layers)    
contrasts_df  # use these column names in your ggplot2 code

#####################################################################################



# For the sex:smoker effect plot use a grouped column plot with error bars
# (geom_linerange layer) and place sex on the X axis. 
# Fill & group by smoker. 
# In the geom_col layer use columns with a width of 0.6 and use
# position=position_dodge(0.75) to place the columns in a staggered way to make
# a grouped column plot. 
# Use geom_linerange to display the 95% confidence intervals and use a linewidth of 0.5 and the same position specification as
# for the columns. 
# 
# Use theme_few with a base_size font size of 16.
# Use scale_fill_manual to specify the fill colour of the columns as grey40 and red3 and use name="" to omit legend title. 
# Add custom X and Y axis titles and use expression to show the 2 in mm^2 in superscript. 
# Use a floating legend at position c(0.75, 0.9) for smoker (fill aesthetic) 
# and use a blank transparent legend background by specifying the right options using theme.


# For the smoker:weight effect plot use a geom_ribbon layer for the 95%
# confidence intervals (disable the outline by using color=NA) 
# and a geom_line layer for the marginal means. 
# Use the same manual fill & colour colors using scale_fill_manual and scale_colour_manual as in the first plot. 
# Specify Y axis breaks at values 0 to 11 using scale_y_continuous. 
# Specify custom X and Y axis titles.
# Disable all legends in this plot, as the legend is the same as in the first
# plot & we will show both below each other in a multipanel plot, so we just need
# 1 legend. 
# Use coord_cartesian to make the Y axis go between 0 and 11.

# Finally, use grid.arrange with ncol=1 and argument left to place both plots below
# each other in a multipanel plot (leave out the Y axes specified in the individual
# plots by passing plot6 + rremove("ylab") & plot7 + rremove("ylab")).
# (cf EXERCISE 1)

# Make ggplot2 effect plot for effect of smoker & sex :
plot6 = ggplot(data=em_smoker_sex_df, aes(x = sex, y=rate, fill=smoker, group=smoker ))+            # specify dataframe, show sex on X axis and rate on Y axis, fill by smoker & group by sex to make position dodging below (we use a grouped column plot)
  geom_col(width=0.6, position=position_dodge(0.75))+                                               # in the geom_col column layer we use different alpha values for the 2 sexes, using a width of 0.6 for the columns & use position_dodge to get a staggered grouped column plot
  geom_linerange(aes(x=sex,ymin=asymp.LCL,ymax=asymp.UCL), linewidth=0.5, position=position_dodge(0.75))+  # add 95% confidence intervals, here using geom_linerange & using a linewidth of 0.3 & also using positidion_dodge for position to get error bars in correct place
  geom_text(data=contrasts_df, aes(label=stars.pval, y=rate, fill=NULL),  vjust=-0.5, size=4) +   # Adding the asterisks of significances
  theme_few(base_size=16)+                                                                          # use theme_few theme                                                                                                                                 
  scale_fill_manual(name="", values=c("grey40","red3"))+                                            # specify custom fill colors & suppress legend title using name=""
  ylab(expression(paste("Number of damaged red blood cells per  ",  mm^2)))+                        # specify Y axis title with mm^2 shown in superscript using expression
  xlab("Sex")+                                                                                      # specify X axis title
  theme(legend.position = "inside", legend.position.inside = c(0.75, 0.9),                                                             # use a floating legend for sex & put it at position (0.75, 0.9) using a transparent background
        legend.background = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))                                                       # show smoker first in legend (to match visual order in plot below)
plot6

?theme


# Make ggplot2 effect plot for effect of smoker * weight :


plot7 = ggplot(data=em_smoker_weight_df, aes(x=weight, y=rate, color=smoker))+                      # specify dataframe, show weight on X axis and rate on Y axis, color by smoker
  geom_ribbon(aes(x=weight, ymin=asymp.LCL, ymax=asymp.UCL, fill=smoker), alpha=0.2, color=NA)+     # add 95% confidence intervals using geom_ribbon layer using semi-transparency of 0.2 and color=NA to remove outline
  geom_line(linewidth=1)+                                                                           # show marginal means using line with linewidth of 1
  theme_few(base_size=16)+                                                                          # use theme_few with a base_size of 16
  scale_y_continuous(breaks=c(0:11))+                                                               # put labels on the y-axis for specific positions
  scale_fill_manual(values=c("grey40","red3"))+                                                     # specify custom fill colours 
  scale_colour_manual(values=c("grey40","red3"))+                                                   # specify custom line colours 
  ylab(expression(paste("Number of damaged red blood cells per  ",  mm^2)))+                        # custom Y axis label with ^2 shown in superscript
  xlab("Weight (kg)") +                                                                             # custom X axis label
  coord_cartesian(ylim=c(0,11), expand=T) + 
  theme(legend.position="none")                                                          # leave out legend (redundant)

plot7

# grid.arrange allows you to put multiple plots in a specific arrangement of your choice
# (here in a single column) & allows you to specify a common spanning Y axis title using
# argument left with a correctly specified textGrob (with rot = 90, gp = gpar(fontsize = 17)).
# Before adding the common Y axis title you can remove the previously provided separate Y axis titles
# by passing plot6 + rremove("ylab") & plot7 + rremove("ylab") as your ggplot objects  :
plot8 = grid.arrange(plot6 + rremove("ylab"), 
                     plot7 + rremove("ylab"), 
                     left = textGrob(expression(paste("Number of damaged red blood cells per  ",  mm^2)), # common spanning Y axis title
                                     rot = 90, gp = gpar(fontsize = 17)), 
                     ncol=1) # single column (one figure below the other)
plot8

# export to Powerpoint at size of 4 x 7 inches :
graph2ppt(file="3_effect_plots_effect_smoking_damaged_blood_cells.pptx", width=4, height=7)

# export to .svg (editable with Inkscape) at size of 4 x 7 inches :
ggsave(plot8, file="3_effect_plots_effect_smoking_damaged_blood_cells.svg", width=4, height=7)

