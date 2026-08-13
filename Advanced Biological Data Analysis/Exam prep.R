################################################################################
#               Advanced Biological Data Analysis - all work for exam          #
################################################################################

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis")
getwd()

#Libraries
library(readxl)
remotes::install_version("car", version = "3.1-2", repos = "https://cloud.r-project.org")
library(car)
library(multcomp)
library(effects) 
library(emmeans)
library(tidyverse)
library(rockchalk)
library(lmtest)
library(afex)
library(MuMIn)
library(nnet)
library(reshape2)
library(ggplot2)
library(lme4)
library(nlme)
library(robustbase)
library(MultiKink)
library(splines)
library(lattice)
library(Matrix)
library(afex) 
library(nlme)
library(nlstools)
library(digest)
library(devtools)
library(ggbiplot)
library(nlstools)
library(nlsMicrobio) # contains some bacterial growth data and fit functions
library(investr) # for plotFit function
library(openxlsx)
library(factoextra)
library(vegan)
library(ade4)
library(gclus)
library(ggeffects)
library(ggdendro)

#Data
metabolism <- read_excel("Linear Models/metabolism.xlsx")
yields <- read_excel("Linear Models/yields.xlsx")
eel <- read_excel("Multivariable linear models/eel2.xlsx")
seeds <- read_excel("Multivariable linear models/seedset.xlsx")
minnow <- read_excel("Multivariable linear models/minnow.xlsx")
blood <- read_excel("Generalised Linear Models/bloodcells.xlsx")
isolation <- read_excel("Generalised Linear Models/isolation.xlsx")
tortoises <- read_excel("Generalised Linear Models/tortoises.xlsx")
babbler <- read_excel("Advanced GLMs and Mixed Models/Babbler_Dataset.xlsx")
owls <- read_xlsx("Advanced GLMs and Mixed Models/owls.xlsx")
squirrels <- read_excel("Advanced GLMs and Mixed Models/squirrels.xlsx")
glucose <- read_excel("Remedying assumptions/blood_glucose.xlsx")
pressure <- read_excel("Remedying assumptions/bloodpressure.xlsx")
soil_management <- read_excel("Remedying assumptions/soilmanagement.xlsx")
fish <- read_xlsx("PCA/Data_Ponds.xlsx")
aster <- read_xlsx("PCA/Aster.xlsx")
dolicho <- read.xlsx("PCA/Dolichovespula saxonica log-ratio.xlsx", rowNames = T)
vespula <- read.xlsx("PCA/Vespula vulgaris log-ratio.xlsx", rowNames = T)
mycor.all<-read.xlsx("NMDS/Epipactis_fungi.xlsx", rowNames = T) #rowNames=True allows for the labels of the specimens to be used
plants<-read.xlsx("NMDS/meadow_plant_species.xlsx")
bryo<-read.xlsx("RDA:PERMANOVA/Bryophyte_community.xlsx",rowNames = T)    # Bryophyte communities
env<-read.xlsx("RDA:PERMANOVA/Environment_Vegetation.xlsx",rowNames = T)  # Environmental variables (including: chemical compounds, slope and two NMDS axes that represent the surrounding vegetation)
manova_data <- read.xlsx("RDA:PERMANOVA/manova_data.xlsx")
OTU = read.xlsx("RDA:PERMANOVA/OTU.xlsx")
id = read.xlsx("RDA:PERMANOVA/id_variables.xlsx")

#Linear models ----
  #Metabolsm (LM) ----
head(metabolism) #compares bodyweight with resting metabolic rate
str(metabolism) #both numeric

metabolism <- metabolism %>% rename(weight = bodyweight,
                                    mr = resting_mr) #renaming

plot(mr ~ weight, data = metabolism) #looks like mr increases with weight linearly
model1 <- lm(mr ~ weight, data = metabolism)
summary(model1) #significant effect of weight on resting metabolic rate
#with every one unit weight increase, we get a 7.437 unit increase in resting mr
#53.34% of the variance in metabolic rate explained by body weight
plot(allEffects(model1))

#d.Based on our model, what do we expect the resting metabolic rate to be for 
#a woman of 60 kilos? What about a woman of 80 kg?
predict(model1, list("weight" = 60)) #1232.453 
predict(model1, list("weight" = 80)) #1381.194

#e.Check if the residuals of your model are normally distributed, both visually 
#and formally (with a test).
hist(model1$residuals)
hist(rstudent(model1), probability=T, ylim=c(0,0.4), 
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-3,3,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

shapiro.test(model1$residuals) #p > 0.05, W > 0.9; assumption not violated

#f. Check for homogeneity of variances, both visually and formally.
spreadLevelPlot(model1)
ncvTest(model1) #p > 0.05, assumption not violated

#g. Test visually for linearity
residualPlots(model1) #lines are quite straight, not deviation from linearity

#h. Check if there are any outliars, again both visually and formally.
outlierTest(model1) #no significant outliers, although observation 40 is very close
influenceIndexPlot(model1, vars = c("Studentized", "Bonf")) 


#i. Finally, check for influential observations, both visually and formally.
cd <- cooks.distance(model1)
inflobs=which(cd>1)
inflobs #no influential observations

influenceIndexPlot(model1, vars = c("Cook")) #point n40 is most influential but not significant

  #Yields (LM) ----
head(yield)
str(yield)

yields.stacked <- stack(yield)
names(yields.stacked) <- c("yield", "soil")
head(yields.stacked)
str(yields.stacked)
boxplot(yield ~ soil, data = yields.stacked)

yield_long <- yield %>% pivot_longer(cols = c(clay, loam, sand), 
                                     names_to = "soil_type",
                                     values_to = "yield_value")  %>% 
  mutate(soil_type = factor(soil_type))
plot(yield_value ~ soil_type, data = yield_long)



model2 <- lm(yield_value ~ soil_type, data = yield_long)
summary(model2) #overall p < 0.05, significant effect of soil on yield
#Adjusted r2 only 00.1829; model still significant (soil type does have an effect)

Anova(model2, type="III") #we again see the same P-value for the OVERALL effect of soil on yield

plot(allEffects(model2))
plot(allEffects(model2), lty = 0) #lty = 0 removes the line b/w the points

  #Assumptions
  #Normality
  hist(model2$residuals)
  hist(rstudent(model2), probability=T, ylim=c(0,0.4), 
       main="Distribution of Studentized Residuals",
       xlab="Studentized residuals")
  xfit=seq(-3,3,length=100)
  yfit=dnorm(xfit)
  lines(xfit, yfit, col="red",lwd=2)
  
  shapiro.test(model2$residuals) #p > 0.05, W = 0.9, assumption not violated
  
  #Homogeneity of variances
  spreadLevelPlot(model2)
  ncvTest(model2) #all good
  
  #Outliers
  outlierTest(model2) #there are no significant outliers, although observation 8 is somewhat close
  influenceIndexPlot(model2, vars = c("Studentized", "Bonf")) #only observation 8 is close to being an outlier
  
  #Influential observations
  cd <- cooks.distance(model2)
  inflobs=which(cd>1)
  inflobs #the vector is empty, so no influential observations
  influenceIndexPlot(model2, vars = c("Cook")) 
  #even observation 8 has a Cook's distance of only ~0.25, nothing to worry about

#Posthoc test
contrast(emmeans(model2, ~soil_type), method = 'pairwise', adjust = 'Tukey')
  #clay - loam     -2.8 1.53 27 -1.832  0.1785 
  #clay - sand      1.6 1.53 27  1.047  0.5546 
  #loam - sand      4.4 1.53 27  2.878  0.0204 *
  
#only the difference between loam and sand comes out as significant. 
#conclusion: plants growing on loam and sand lead to different yields, 
#whereas there is no detectable difference between clay and loam or clay and sand.
  

#Multivariable models ----
#we use sum coding (=effects coding) by default from now on - you can just always put this on the top of the code
set_sum_contrasts()

  #Seeds (MLMs) ----
head(seeds)
str(seeds)
seeds$population<-as.factor(seeds$population)
seeds$plant<-as.factor(seeds$plant)
str(seeds)

plot(seeds[,6:8]) #columns 6, 7, and 8

flower_model <- lm(seed.number ~ flowers, data = seeds)
summary(flower_model) #significant positive effect, 48.7 % of variance explained
#number of seeds increases with increasing number of flowers (each 1 unit increase in flowers -> 164.42 more seeds)

weight_model <- lm(seed.number ~ seed.weight, data = seeds)
summary(weight_model) #significant negative effect, 30.6 % of variance explained
#number of seeds decreases with increasing seed weight (each 1 unit increase in seed weight -> 84.42 less seeds)

additive_model <- lm(seed.number ~ flowers + seed.weight, data=seeds)
summary(additive_model) #significant model (both terms significant); 71.4 % of variance explained
Anova(additive_model, type="III") #Anova here not really necessary as 2 continuous predictors

plot(allEffects(additive_model)) #visualise the effects of all predictors in the model
plot(effect("flowers",additive_model)) #you can also visualise only one predictor of the model
plot(effect("flowers",additive_model, residuals=TRUE), smooth.residuals=FALSE) # you can also plot the raw datapoints on it

interaction_model <- lm(seed.number ~ flowers * seed.weight, data=seeds)
summary(interaction_model) #significant model (flowers and interaction significant); 75.8 % of variance explained
Anova(interaction_model, type="III")  #Anova here not really necessary as 2 continuous predictors
plot(allEffects(interaction_model), multiline=T, confint=list(style="auto")) 
plot(allEffects(interaction_model))

AICc(flower_model, weight_model, additive_model, interaction_model)  #the interaction model has the lowest AICc.


#Assumptions
residualPlots(interaction_model) #linearity assumption is ok

spreadLevelPlot(interaction_model)
ncvTest(interaction_model) #homogeneity of variance assumption is ok

hist(rstudent(interaction_model), probability=T, col="steelblue")
xfit = seq(-6,6,length=100)
yfit = dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)
shapiro.test(residuals(interaction_model)) # normality of residuals assumption is ok

vif(interaction_model)  #collinearity assumption is strongly violated because of the interaction!!
#if VIF > 5; remove variables and fit again or do residual centering as below
fit.residcent <- residualCenter(interaction_model)

vif(fit.residcent)     #can be avoided by using residual centering.
summary(fit.residcent)
summary(interaction_model) #compared to the intitial interaction model, the main effect of seed.weight now has a significant effect
#in case of collineatirity, we should interpret the p-values of the model with residual centering
Anova(fit.residcent, type="III") #same as summary 

test=outlierTest(fit.residcent)
outl=as.numeric(names(which(test$bonf.p<0.05)));outl #no outliers


cd=cooks.distance(fit.residcent)
inflobs=which(cd>1);inflobs #no influential observations

# The interpretation goes something like this: The number of flowers produced over the season is a strong
# determinant of how many seeds will be produced, and there is also a significant main effect of the seed weight on the number of seeds. However, there is a trade-off between the number 
# and weight of the seeds. Hence, you can see that the relationship between flower number and seed
# number is only apparent for light seeds and not for heavy ones.

  #Eels (MLMs) ----
head(eel)
str(eel) 
eel<-mutate_if(eel, is.character, as.factor)#the categorical variables have now been coded as factors!

plot(HEAVY_METAL_ACCUM~MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=eel, col=RIVER, cex=2, pch=16)
#this line of code makes sure the next three graphs will be plotted next to each other:
par(mfrow=c(1,3))
#make three separate scatterplots, one for each river system.
#this is just a different way of visualizing.
plot(HEAVY_METAL_ACCUM~MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=subset(eel, eel$RIVER == "IJZER"), cex=2, pch=16)
plot(HEAVY_METAL_ACCUM~MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=subset(eel, eel$RIVER == "MAAS"), cex=2, pch=16)
plot(HEAVY_METAL_ACCUM~MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=subset(eel, eel$RIVER == "SCHELDE"), cex=2, pch=16)

#switch back to one graph at a time
par(mfrow=c(1,1))

#alternative with 'xyplot' from the package 'lattice'
library(lattice) 
xyplot(HEAVY_METAL_ACCUM~MULTILOCUS_HETEROZYGOSITY_ALLOZYME,data=eel,group=eel$RIVER,type=c("p","r"), col = c(2,3,4),cex=1.8,pch=16,key = list(text = list(levels(eel$RIVER)), space="right", points = list(pch = 16, cex=1.8, col = c(2,3,4))))

fit1=lm(HEAVY_METAL_ACCUM~RIVER+MULTILOCUS_HETEROZYGOSITY_ALLOZYME,data=eel) 
summary(fit1) 
Anova(fit1, type="III") #both river and allozyme are significant
plot(allEffects(mod=fit1)) #clear differences between the rivers; and hm accum decreases with increasing multilocus allozyme heterozygosity
levels(eel$RIVER) #you can check the levels of the categorical variable
  #IJZER = river0; MAAS = river1; Schelde = River3

contrast(emmeans(fit1, ~RIVER), method="pairwise",adjust="tukey") #Ijzer - Maas = significant difference (Ijzer accumulation < Maas accummulation) and Maas - Schelde = sig. diff (Maas > Schelde)

fit2=lm(HEAVY_METAL_ACCUM~RIVER*MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=eel)
summary(fit2) #now only the allozyme is significant
Anova(fit2, type="III") 
plot(allEffects(mod=fit2),multiline=T, ci.style="band") 

# check for differences in slopes:
contrast(emtrends(fit2, "RIVER", var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"), method="pairwise", adjust="tukey")
# there no pairwise differences in the effect of heterozygosity on heavy metal accumulation between any of the rivers

AICc(fit1, fit2) #fit 1, WITHOUT the interaction term has lower AIC, so is the better model!

#Assumptions:
test=outlierTest(fit1);test
outl=as.numeric(names(which(test$bonf.p<0.05)));outl # no outliers
influenceIndexPlot(fit1,vars=c("Studentized","Bonf"))

cd=cooks.distance(fit1)
inflobs=which(cd>1);inflobs 
# no cases with cook's distance > 1
influenceIndexPlot(fit1,vars="Cook") 

# homogeneity of variance
spreadLevelPlot(fit1,xlab="log(fitted values)",ylab="log(absolute studentized residuals)") # graphical test, there should be no strong correlation
ncvTest(lm(fit1,data=eel)) # variances don't deviate from homogeneity, but it's close

# normality
hist(rstudent(fit1), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2) 
shapiro.test(residuals(fit1)) # Shapiro Wilk's W>0.9, so normality OK

# residual plots to check for systematic trends in residuals vs covariate or residuals vs fitted values
residualPlots(fit1)

# We conclude that heterozygosity negatively affects the heavy metal accumulation in minnows.
# We also detected overall differences in heavy metal concentration between the different 
# river systems, but there is no significant interaction. In other words, the effect of 
# heterozygosity on heavy metal accumulation is the same for all river systems.

  #Minnows (MLMs) ----
head(minnow)
str(minnow)
minnow$STRESS <- as.factor(minnow$STRESS)
minnow$CONTAMINATION <- as.factor(minnow$CONTAMINATION)

par(mfrow=c(1,2))
plot(BODY.LENGTH ~ STRESS, data=minnow)
plot(BODY.LENGTH ~ CONTAMINATION, data=minnow)
par(mfrow=c(1,1))

fit3 <- lm(BODY.LENGTH ~ STRESS+CONTAMINATION, data=minnow)
summary(fit3) #lead and sound are significant influences on body length
Anova(fit3, type="III") #both stress and contamination have an effect on minnow body length
plot(allEffects(fit3))
levels(minnow$CONTAMINATION) #chrome = 0; lead = 1; manganese = 2
levels(minnow$STRESS) #control = 0; predation = 1; sound = 2

fit4 <- lm(BODY.LENGTH ~STRESS*CONTAMINATION, data=minnow) #same model coded in a shorter way

Anova(fit4, type="III") #interaction also significant
plot(allEffects(fit4))
#you can make the visualisation different if you switch position of the predictors (is still the same fit)
fit4b <- lm(BODY.LENGTH ~CONTAMINATION*STRESS, data=minnow)
plot(allEffects(fit4b))

AICc(fit3, fit4, fit4b) #interaction models = best

contrast(emmeans(fit4, ~CONTAMINATION|STRESS), method="pairwise",adjust="tukey")
#in the control and predation treatments, we see a difference in body length between
#all pairwise combinations of the contamination treatments. However, in the sound
#stress treatment, we do not see a difference between the chrome and lead treatments
#(but we do see differences between the other pairs of contamination treatments).

contrast(emmeans(fit4, ~STRESS|CONTAMINATION), method="pairwise",adjust="tukey")
#we can also test pairwise the other way around: is there difference between stress treatment within each contamination

#Assumptions:
ncvTest(fit4) #ok

#Normality: 
hist(rstudent(fit4), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2) 
#Shapiro-Wilk test:
shapiro.test(residuals(fit4)) #Ok

# Outliers & influential observations
outlierTest(fit4)
outl=as.numeric(names(which(outlierTest(fit4)$bonf.p<0.05)))
outl # empty, so no outliers here
influenceIndexPlot(fit4,vars=c("Studentized","Bonf"))
cd=cooks.distance(fit4)
inflobs=which(cd>1) 
inflobs # empty, so no outliers here
influenceIndexPlot(fit4,vars="Cook")


# We conclude that contamination type and type of stress have significant effects 
# on the adult body length in minnows. On top of that, there is a significant interaction: the effects
# of contamination treatment is different between the stress treatments (or the other
# way around). Visually, it seems like there is a negative synergy: if there is already
# a strong negative effect of one of the treatments (especially STRESS), then the negative effect of the
# other treatment (contamination) is smaller.

#Remedying assumptions ----
  #Blood glucose ----
  #    Glycemic control is a crucial aspect of managing diabetes and prediabetes, as 
  #    it reflects the average blood glucose levels over time and helps in preventing 
  #    long-term complications. This study investigates the effects of physical activity 
  #    and dietary carbohydrate intake on glycemic control, as measured by blood glucose
  #    levels, in individuals with varying levels of exercise and carbohydrate consumption. 
  #    The researchers were specifically interested in how carbohydrate consumption
  #    affects the benefits of exercise for lowering blood glucose.
  
  #   a.  Read the data (blood_glucose), and start with the usual checks and visual 
  #       explorations. 
  head(glucose)
  str(glucose)
  
  par(mfrow = c(1,2))
  plot(blood_glucose ~ carb_consumption, data=glucose) #+
  plot(blood_glucose ~ weekly_exercise, data=glucose) #-
  par(mfrow = c(1,1))
  
  #   b.  Since the researchers were specifically interested in the interaction effect
  #       between weekly exercise and carbohydrate consumption, run a full factorial
  #       model to investigate this. Also provide a visualisation of the model.
  #       Based on this model, does the effect of weekly exercise on blood glucose 
  #       depend on the level of carbohydrate consumption?
  
  fit1 <- lm(blood_glucose ~ weekly_exercise * carb_consumption, data = glucose)
  
  summary(fit1) #carb consumption significant; exercise borderline
  Anova(fit1, type="III")
  plot(allEffects(fit1), multiline=T, confint=list(style="auto"))
  # the interaction effect is not significant - no matter what the carb consumption
  # of the person is, the effect of weekly exercise on blood glucose is about the same 
  # (the lines in our effect plot are parrallel)
  
  #   c.   Check for heterogeneity of variances, normality of residuals, linearity, collinearity, 
  #       outliers and influential observations. Can we trust our conclusions?
  
  shapiro.test(residuals(fit1)) #no deviation from normality
  ncvTest(fit1) #no deviation from homogeneous variances
  residualPlots(fit1)  #no strong deviation from linearity
  vif(fit1) #we have clear collinearity! Let's residual center to address this:
  
  fit1b <- residualCenter(fit1)
  vif(fit1b) # variance inflation factors are OK now
  summary(fit1b) #still no significant interaction; but weekly exercise now significant
  Anova(fit1b, type="III")
  
  outlierTest(fit1b) #there is an outlier: point 23
  outlierTest(fit1)
  
  which(cooks.distance(fit1b)>1) #this point is also an influential observation
  cooks.distance(fit1b)[which(cooks.distance(fit1b)>1)] #it has a cook's distance of 1.6
  
  #   d.  Attempt to remedy any assumption violations. Again provide a model 
  #       visualization. Do your conclusions change? Carefully formulate your conclusions.
  
  fit2 <- lmrob(blood_glucose ~ weekly_exercise * carb_consumption, data = glucose)
  vif(fit2) #since we had high variance inflation factors before, we check it, and they are high
  fit2b <- residualCenter(fit2) #remedy this by residual centering
  vif(fit2b) #fixed
  
  summary(fit2b) #all significant now
  Anova(fit2b, type="III")
  
  # R has trouble plotting a residual-centered robust model. You can do it with ggplot,
  # but for now we can just check the non-centered model, as the interaction coefficient
  # is the same so we can accurately visualize that effect by plotting that model
  plot(allEffects(fit2), multiline=T, confint=list(style="auto"))
  
  # we now conclude that there is a strongly significant interaction. For people with
  # a diet low in carbohydrates, weekly exercise has essentially no effect on their
  # blood glucose. However, the higher the consumption of carbohydrates, the more 
  # exercise helps to reduce blood glucose.
  #Blood pressure ----
  #     We have a dataset about blood pressure of 16 hypertension patients who have either been
  #     treated with a medicine or a placebo (control). Their blood pressure was measured
  #     every week for a total of 10 weeks.
  #
  #     We would like to know if the medicine leads to a stronger decrease over time
  #     in blood pressure than the placebo.
  
  #   a.  Start with reading the data, checking everything and visually exploring.
  head(pressure)
  str(pressure)
  pressure$subject <- as.factor(pressure$subject)
  pressure$treatment <- as.factor(pressure$treatment)
  
  plot(bloodpressure~treatment, data=pressure)
  plot(bloodpressure~week, data=pressure, col=pressure$subject)
  
  #   b.  Construct a linear mixed model to predict the effects of treatment and 
  #       time on blood pressure. Also include their interaction.
  #       Include the appropriate random effects. Visualize the model.
  
  fit8 <- lmer(bloodpressure~treatment*week + (1|subject), data=pressure)
  summary(fit8)
  Anova(fit8, type="III") #significant interaction only
  
  #looks like there is no difference in treatment alone, but the change over time
  #is different between the medicine and the placebo
  plot(allEffects(fit8), multiline=T, confint=list(style="auto"))
  #the effects plot shows that the slope for the medicine is more downward than
  #for the placebo - based on this, we would conclude that the medicine is effective.
  
  #   c.  Because our measurements have been done over time, we might have 
  #       temporal autocorrelations in the data. 
  #       First check if there indeed is temporal autocorrelation in the previous model.
  #       Then fit a new model where you account for these temporal 
  #       autocorrelations and check if it is better.
  #       If so, visualize it. Do your conclusions change?
  
  #autocorrelation plot
  acf(residuals(fit8)) #we can see that for short lags there is a positive correlation
  
  #first run lme WITHOUT the autocorrelation structure (should give the same as above)
  fit9 <-lme(bloodpressure~treatment*week, random=~1|subject, data=pressure)
  summary(fit9)
  Anova(fit9, type="III")
  #and here we add temporal autocorrelation structure
  fit10 <-lme(bloodpressure~treatment*week, random=~1|subject, correlation=corAR1(form=~week), data=pressure)
  summary(fit10)
  Anova (fit10, type="III")
  AICc(fit8, fit9, fit10) #the model that accounts for autocorrelation is much better!
  
  plot(allEffects(fit10), multiline=T, confint=list(style="auto"))
  #after accounting for temporal autocorrelation, we no longer have support that 
  #bloodpressure declines faster in the patients who got the medicine than in 
  #those who got the placebo...
  
  #       d. we cannot test for homogeneity of variances with mixed models, but we could run a mixed model that allows
  #       the variances between the treatments to be different, 
  #       and then compare these models with our first model based on AICc.
  
  fit11 <-lme(bloodpressure~treatment*week, random=~1|subject, weight=varIdent(form=~1|treatment), correlation=corAR1(form=~week), data=pressure)
  summary(fit11)
  Anova (fit11, type="III")
  AICc(fit8, fit9, fit10, fit11) #
  # this is slightly worse than the previous one, se we keep the model without custom variances. The difference in AICc is small (<2) so we can report this model, 
  # thought our conclusions do not change
  
  fit12 <-lme(bloodpressure~treatment*week, random=~1|subject, weight=varIdent(form=~1|treatment), data=pressure)
  AICc(fit8, fit9, fit10, fit11, fit12)
  # the model with custom variances but not accounting for temporal autocorrelation,is clearly worse
  
  
  #   e.  Check normality of residuals and collinearity on the best fit.
  #       Note: checking outliers, influential observations and linearity
  #       requires some different techniques than for lm/glm.
  #       We don't go into it in this course.
  
  shapiro.test(residuals(fit10)) #no deviation (W>0.9, don't worry about the p-value)
  vif(fit10) #no collinearity
  
  #   f.  What are your conclusions?
  
  #we have no reliable support that the medicine is effective - bloodpressure does not
  #change significantly differently over time in the patients that got the medicine vs
  #the patients that got the placebo
  
  #Soil management ----
  #    We have a dataset on the use of different types of soil management techniques 
  #    and their effect on the concentration of phenolics (a plant defense compound)
  #    in yarrow (Achillea millefolium), a common herb. 
  #    
  #    The study was carried out in 16 plots of 5x5 meters which were randomly assigned 
  #    to one of four treatments: 1) surrounded by a fence exclosure to exclude herbivores; 
  #    2) fertilized with N-P-K fertilizer; 3) fenced AND fertilized; and 4) untreated 
  #    control. Then each plot was divided in two: one half was treated for the first
  #    ten years and then left to revert to the untreated state, while the other half
  #    was treated for the full 20 years of the experiment.
  # 
  #   a.  Read the data (soilmanagement), and start with the usual checks and visual 
  #       explorations. Think carefully which variables should be coded as factors.
  head(soil_management)
  str(soil_management)
  
  #we need to code plot, treatment and duration as factors! 
  soil_management$plot <- factor(soil_management$plot)
  soil_management$treatment <- factor(soil_management$treatment)
  soil_management$duration <- factor(soil_management$duration)
  
  par(mfrow=c(1,3))
  plot(phenolics_conc ~ plot, data=soil_management)
  plot(phenolics_conc ~ treatment, data=soil_management)
  plot(phenolics_conc ~ duration, data=soil_management)
  par(mfrow=c(1,1))
  
  #   b.  Run a linear  mixed-effects model to predict the effect of treatment, duration 
  #       and their interaction on phenolics concentration. What should be the random
  #       factor in this model? Visualize the model. 
  
  #the random factor should be 'plot'. This is because we are not specifically interested
  #in which plots lead to more phenolics - it is just a factor in the data that groups
  #some data points together, and the levels of this factor (the plots) have been randomly drawn
  #from a large number of possible plots. 
  fit <- lmer(phenolics_conc ~ treatment * duration + (1|plot), data = soil_management)
  summary(fit)
  Anova(fit, type="III")
  plot(allEffects(fit))
  plot(allEffects(fit), multiline=TRUE)
  #to be completely correct, we should test an additive model and one with interaction and check which is best with AICc
  
  #   c.  Do some posthoc comparisons based on your model. Specifically, compare all
  #       treatments against the control for the same duration. To do this, first relevel 
  #       the treatment variable so that the reference level is the control treatment.
  #       Then run the model again (otherwise you will get nonsensical results).       
  #       Then do posthoc comparisons between the treatments using method 'trt.vs.ctrl'
  #       (see lecture 4) within each duration (see lecture 2). What do you conclude?
  
  levels(soil_management$treatment)
  soil_management$treatment = relevel(soil_management$treatment, ref="control")
  fit <- lmer(phenolics_conc ~ treatment * duration + (1|plot), data = soil_management)
  Anova(fit, type="III") ##stays the same, as it is the same fit
  contrast(emmeans(fit, ~ treatment|duration), method='trt.vs.ctrl', adjust='Tukey')
  plot(allEffects(fit), multiline=TRUE)
  #We can see that the fertilizer treatment leads to lower phenolic concentrations for
  #both durations, while we have no evidence that this is the case for the exclosure treatment
  #(also for both durations). For the 'both' treatment, we find that the phenolics contentration
  #is significantly lower than the control for the permanent duration, but have no evidence
  #that this is also the case for the reverse duration.
  
  #   d.  You cannot test for homogeneity of variances using our standard methods, 
  #       because we ran a mixed model. But we could run a mixed model that allows
  #       the variances between the treatments and durations (and both) to be different, 
  #       and then compare these models with our first model based on AICc. We have to do 
  #       this with the function lme. First reconstruct the model you made in (b) with 
  #       lme and assure yourself that it produces the same outcome as the model you 
  #       made under b. Then run three extra models: one that allows for different variances
  #       between treatments, one that allows for different variances between durations,
  #       and one that allows for different variances for each combination of treatment
  #       and duration (use 'form=~1|treatment*duration'). 
  
  #       Which model is the best? Do your conclusions change?
  #       Visualize the best model if you have not yet done so.
  
  fit2 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, data = soil_management)
  ?lme
  summary(fit2)
  Anova(fit2, type="III") 
  
  AICc(fit, fit2) #it's indeed the same model as the original
  
  fit3 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|treatment), data = soil_management)
  fit4 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|duration), data = soil_management)
  fit5 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|treatment*duration), data = soil_management)
  
  AICc(fit2, fit3, fit4, fit5) #our original fit has the best AICc, so we keep that one!
  
  #   e.  Check assumptions: normality of residuals, outliers/influential observations.
  
  shapiro.test(residuals(fit)) # no deviation from normality
  outlierTest(fit) #no outliers (note that this test works for an lmer but not for a lme model)
  max(cooks.distance(fit)) #no points with Cook's distance over 1
  #NOTE: if a model with custom variances would be best we should have used lme to make the fit. testing outliers/influential observations
  # with lme is not straightforward and not part of this course; So now we are just lucky we can use the original model
  # which we can fit with lmer
  
  
  #   f.  What are your conclusions based on this model?
  
  #We conclude that the effect of soil management treatment on phenolics in some cases depends on 
  #the duration these treatments are applied for. The application of fertilizer (but no fencing)
  #leads to lower phenolics compared to the control treatment, irrespective of the duration treatment.
  #In contrast, we have no evidence that the application of exclosure (but no fertilizer) leads to 
  #differences, again irrespective of the duration treatment. However, if BOTH are applied,
  #then the duration matters: we see an effect if they are applied permanently, but not
  #if they are only applied for 10 years and then let to reverse to their untreated state.
  
#Generalised linear models ----
set_sum_contrasts()
  #Tortoises (GLMs) ----
  str(tortoises)
  head(tortoises) # check whether the data look OK
  plot(Clutch_weight ~ Length, data = tortoises) # get a first impression of the relationship between carapace length and clutch size

  fit1=lm(Clutch_weight~Length,data=tortoises) # First produce a simple linear model
  summary(fit1) # there is no significant effect of clutch size
  Anova(fit1, type="III") #same results for single continuous variable, but the summary gives you the estimates
  plot(allEffects(fit1))
  
  residualPlots(fit1) #it looks like there is a strong curvilinear effect
  
  fit2=lm(Clutch_weight~poly(Length,2),data=tortoises) # include a second order polynomial term
  summary(fit2)
  AICc(fit1, fit2) # the model with the polynomial term is better!
  
  fit3=lm(Clutch_weight~poly(Length,3),data=tortoises) # Third order polynomial model
  summary(fit3)
  AICc(fit1, fit2, fit3) #this model is worse - keep the model with only the second order polynomial term
  
  plot(allEffects(fit2, residuals=TRUE), smooth.residuals = FALSE) #effect plot that also include the data points
  #Note that the 'poly' function that we use to include polynomal terms automatically performs residual centering.
  #This is necessary because the linear term (carapace length) and the polynomial term (carapace length squared)
  #will usually be correlated, leading to collinearity. 
  
  #Assumptions:
  residualPlots(fit2) #residualplots - there is no prediction bias
  
  #normality of residuals:
  hist(rstudent(fit2), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5), breaks=6,
       main="Distribution of Studentized Residuals",
       xlab="Studentized residuals")
  xfit=seq(-6,6,length=100)
  yfit=dnorm(xfit)
  lines(xfit, yfit, col="red",lwd=2) 
  shapiro.test(residuals(fit2)) # W > 0.9, so no deviation from normality
  
  #homogeneity of variances
  spreadLevelPlot(fit2,xlab="log(fitted values)",ylab="log(absolute studentized residuals)") # graphical test, there should be no strong correlation
  ncvTest(fit2) # no divergence from homogeneous variances
  
  #outliers/influential observations - we do this a bit quicker now then before, without visualizations.
  #of course you can still do those visualizations if you prefer. This is just to check if there are any outliers or
  #influential observations. If there are, you'll have to investigate more which data points they are.
  outlierTest(fit2) #no significant residuals
  max(cooks.distance(fit2)) #maximum cook's distance in our model is lower than 1, so no influential observations
  
  #   f.  What are your conclusions based on this analysis?
  
  # We conclude there is a quadratic effect of carapace length on clutch weight: tortoises with intermediate carapace lengths
  # lay the heaviest clutches.
  summary(fit2)
  plot(allEffects(fit2, residuals=TRUE), smooth.residuals = FALSE) #

  
  #Isolation (GLMs) ----  
  head(isolation)
  str(isolation)
  
  plot(presence ~ area, data=isolation)
  plot(presence ~ distance, data=isolation)
  
  #another way is with 'xyplot' from the library 'lattice'. This allows us to also draw a quick-and-dirty linear
  #fit in the plot to get an idea (by including type=c("p","r") we include both points ("p") and a linear regression line
  #("r"). Of course that linear fit is just for illustration - we cannot based any conclusions on this!
  library(lattice)
  xyplot(presence ~ area, data=isolation ,type=c("p","r"))
  xyplot(presence ~ distance, data=isolation, type=c("p","r"))
  
  #We need to run a binomial glm because our response variable is binomial (has 2 levels). The appropriate link function
  #is the logit link, although we actually don't have to specify this - it's the default for a binomial model.
  fit <- glm(presence ~ area + distance, family=binomial(link=logit), data=isolation)
  summary(fit) #both predictors have a significant effect!
  plot(allEffects(fit))
  #to get a graph on the linear scale, we need to include type="response":
  plot(allEffects(fit), type="response")
  #note that there are some rendering issues in the drawing of the graph - the lines go beyond 0 and 1. 
  #The model doesn't actually predict this - it is just the graph. Later we will learn to make graphs with different
  #functions that are not as vulnerable to these problems.
  
  fit2 <- glm(presence ~ area * distance, family=binomial(link=logit), data=isolation)
  summary(fit2) #the interaction is not significant!
  AICc(fit, fit2) #the fit without the interaction is better - keep the original model.
  #We've already visualized the best model in the previous step.
  
  #What is the probability that the species occurs on an island with a surface are of
  #3 km^2 and that is 8 km from the mainland? How about an island of 6 km^2 that is 4 km from the mainland?
  #Use the 'predict' function for this. 
  
  preds <- predict(fit, list(area=c(3, 6), distance=c(8, 4)), type="response")
  preds
  #for the first island, we predict the probability for the species to be there at 7%.
  #for the second island, we predict a 99% probability that the species occurs there.
  
  #Assumptions:
  #linearity: check residual plots
  residualPlots(fit) #no serious problems here
  
  #collinearity: check if our variances are inflated:
  vif(fit) #vifs are <5, so no collinearity
  
  #overdispersion: make a quasibinomial fit and check the dispersion parameter
  fitb <- glm(presence ~ area + distance, family=quasibinomial(link=logit), data=isolation)
  summary(fitb) #dispersion parameter <1, so no overdispersion. We can interpret the original model (not the quasibinomial)
  
  outlierTest(fit) #no significant outliers
  max(cooks.distance(fit)) #largest cook's distance is 0.2 (<1, so no influential observations)
  
  #   f.  What are your conclusions based on this analysis?
  
  # We conclude that probability that the species occurs on the island increases with its area and decreases with its
  # distance from the mainland. There is no significant interaction effect, so no matter how far the island is from
  # the mainland, the effect of area on the presence of the species is the same (and vice versa).
  
  summary(fit) #both predictors have a significant effect!
  
  #to get a graph on the linear scale, we need to include type="response":
  plot(allEffects(fit), type="response")
  
  #Bloodcells (GLMs) ----
  head(blood)
  str(blood) # Check the structure of your data (convert characters to factors if needed)
  #conversion of all the 'str' variables in our dataset to factors can conveniently be done this way:
  blood <- mutate_if(blood, is.character, as.factor)
  #but we can of course also just code each of them as factor separately using as.factor
  
  plot(cells~smoker, data=blood)
  plot(cells~sex, data=blood)
  plot(cells~weight, data=blood)

  # The dependent variable (cells) contains count data, so we need to construct a generalized linear model with 
  # a poisson error distribution and a log link function. 
  
  # Make the model
  fit=glm(cells~smoker+sex+weight,family=poisson(link=log),data=blood)
  # We actually don't have to specify the log link, as it is the default for a poisson error structure.
  # So this would produce the exact same model:
  fit=glm(cells~smoker+sex+weight,family=poisson,data=blood)
  
  summary(fit)   
  Anova(fit, type="III")  #note that the p-values from the Anova table and the summary table are very similar, but not
  #exactly the same. That is because they use slightly different tests - the summary table
  #uses a Wald test, whereas the Anova uses a Likelihood Ratio test. The latter is more 
  #exact but the two will yield very similar results. It is possible to use the Wald test in
  #the Anova too, using the argument test.statistic="Wald" - if you do that, you'll see that
  #the p-values are now exactly the same as in the summary table.
  
  plot(allEffects(fit),ylab="Number of damaged cells")
  #use type="response" to plot on the linear scale:
  plot(allEffects(fit),ylab="Number of damaged cells",type="response")
  
  fit2=glm(cells~smoker*weight+sex,family=poisson,data=blood)
  summary(fit2) # interaction is significant, so the effect of weight is different between smokers and non-smokers
  Anova(fit2, type="III")
  
  plot(allEffects(fit2), multiline=T, confint=list(style="auto"))
  plot(allEffects(fit2), multiline=T, confint=list(style="auto"), type="response")
  #we can see that there is a stronger effect of weight on damaged blood cells for smokers than for non-smokers
  
  AICc(fit, fit2) #the model WITH the interaction is better!

  #  d.  Now check all models with all possible combinations of one-way interactions. Which has the best AICc?
  #      Present this model visually and perform model diagnosis: linearity on the transformed scale, collinearity
  #      overdispersion, outliers/influential observations.
  #
  fit3=glm(cells~smoker+weight+sex + sex:weight, family=poisson,data=blood)
  fit4=glm(cells~smoker+weight+sex + sex:smoker, family=poisson,data=blood)
  fit5=glm(cells~smoker+weight+sex + sex:weight + sex:smoker, family=poisson,data=blood)
  fit6=glm(cells~smoker+weight+sex + sex:weight + smoker:weight, family=poisson,data=blood)
  fit7=glm(cells~smoker+weight+sex + sex:smoker + smoker:weight, family=poisson,data=blood)
  fit8=glm(cells~smoker+weight+sex + sex:smoker + smoker:weight + sex:weight, family=poisson,data=blood)
  
  AICc(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8) #fit 7 is the best!
  
  summary(fit7)
  Anova(fit7, type="III") 
  #visual representation:
  plot(allEffects(fit7), multiline=T, confint=list(style="auto"), type="response")
  
  
  #linearity: check residualplots
  residualPlots(fit7) #looks OK!
  
  #collinearity: check variance inflation factors
  vif(fit7) #because of the interactions, we have strong collinearity
  #this is not a problem for our predictions, but it renders the p-values of our main effects uninterpretable.
  #solve it with residual centering:
  fit7b <- residualCenter(fit7)
  vif(fit7b)
  summary(fit7b) #the p-values of the main effects have now changed, but the model predictions are the same
  summary(fit7) 
  Anova(fit7, type="III")
  Anova(fit7b, type="III")
  # this shows checking for overdispersion and correcting with residualcentering is important, as it (in this case at least) leads to different conclusions regarding
  # the significance(s) of the main effects
  
  vif(fit7b) #variance inflation factors are ok now (<5)
  
  #overdisperion: construct a quasipoisson model and check if the dispersion parameter is not much larger than 1:
  fit7c <- glm(cells~smoker+weight+sex + sex:smoker + smoker:weight, family=quasipoisson,data=d3)
  summary(fit7c) #dispersion parameter is fine, keep original model (though WITH residualCentering)
  
  outlierTest(fit7b) # no outliers
  max(cooks.distance(fit7b)) #maximum cook's distance is 0.11 - no influential observations
  
  #  e.  What are your conclusions based on this analysis?
  summary(fit7b) #
  Anova(fit7b, type="III")
  #visual representation:
  # a residual centered model cannot be nicely visualised with effect-plots, so you can visualize the non-residual centered model 
  # for interpretation of the trends/directions, as predictions stay the same
  plot(allEffects(fit7), multiline=T, confint=list(style="auto"), type="response")
  
  #  We conclude that there are overall effects of smoking and weight on the damaged bloodcell count.
  #  Also,  being a smoker impacts this count more strongly in females than in males.
  #  Finally, the effect of weight is much stronger in smokers than in non-smokers.
  
  #  f.  OPTIONAL: You just ran all possible models with all possible combinations of one-way interactions manually.
  #      There are also ways to do this automatically - much more convenient. You can do this with the package
  #      'glmulti' - install it with install.packages("glmulti") and then load it with library(glmulti).
  
  #      NOTE: you need "RJava" for this package. Sometimes you already have it, sometimes it still needs to be installed.
  #      On windows, you can just install it with "install.packages", but it is more difficult on mac. If you have a 
  #      mac, you can try yourself to install it, but if it is too difficult, we suggest to skip this part of the 
  #      exercise for now. glmulti will not be part of the exam.
  
  #      Once you installed glmulti, you need to use the function that is also called glmulti. 
  #      You will have to give a few input arguments. First, you need to specify a model (just like you would do in glm)
  #      in the form "RESPONSE VARIABLE ~ PREDICTOR1 + PREDICTOR2 (etc), family = RESIDUAL_DISTRIBUTION". 
  #      Don't include interactions - glmulti will automatically consider all one-way interactions between the predictors 
  #      that you gave. Also specify your data like in glm (data = MYDATA). Then you should specify how many best models 
  #      you want to save. In our case saving the 5 best models is enough. Do this with the argument confsetsize = NUMBER.
  #      Finally, you should give the information criterion you want to use, in our case AICc. You can do this with 
  #      crit= "aicc". Save the output of glmulti in a variable, something like BEST_MODEL_OUTPUT <- glmulti(ARGUMENTS).
  
  #      After you run that, you can check the top 5 models with BEST_MODEL_OUTPUT@formulas
  #      You can also save your best model like this: BESTFIT <- BEST_MODEL_OUTPUT@objects[[1]]
  #      Then you can just check the summary and Anova and do effect plots of that model like normal.
  
  #      Based on this algorithm, what is the best model? Is it the same model as you got before (under e)?
  
  #if you still need to install glmulti (sometimes this requires still installing rJava):
  install.packages("rJava")
  install.packages("glmulti")
  library(rJava)
  
  #open glmulti:
  library(glmulti)
  
  
  best = glmulti(cells ~ smoker + sex + weight, family="poisson", data = blood, confsetsize = 5, crit = "aicc") 
  best@formulas
  bestfit=best@objects[[1]]
  
  #glmulti came to the same conclusion as we did in c - the algorithm works! (though also here we still need to correct for overdispersion)
  
#Mixed models ---
  #

#Advanced GLMs and Mixed models ----
  #Squirrels ----
  # Red squirrels (Sciurus vulgaris) are known to host a species of parasitic nematode (Strongyloides robustus), 
  # which can influence their health outcomes. Researchers have observed that the severity of infection—measured 
  # by parasite load—can lead to different health trajectories: recovery, chronic illness, or mortality. 
  # Additionally, the impact of parasite load may vary depending on the age of the squirrels. In this study, 
  # data has been collected on the age (in months), parasite load, and health outcomes of a population of red 
  # squirrels. 
  head(squirrels)
  str(squirrels)
  squirrels$Outcome <- as.factor(squirrels$Outcome)
  
  #    a.  Start with some visual inspection of the data. 
  plot(squirrels)
  plot(squirrels$Outcome ~ squirrels$Age)
  plot(squirrels$Outcome ~ squirrels$Parasite_load)
  
  #    b.  Fit multinomial models to investigate the influence of age and parasite load on disease outcomes.
  #        Fit a model with and without an interaction and pick the best one based on AICc.
  #        NOTE: in including your predictor variables, list 'Age' first. Otherwise the effect plot is hard to interpret.
  #        Which variables have a significant effect?
  
  fit1 <- multinom(Outcome ~ Age + Parasite_load, data = squirrels)
  fit2 <- multinom(Outcome ~ Age * Parasite_load, data = squirrels)
  AICc(fit1, fit2) # fit with interaction is much better
  summary(fit2)
  Anova(fit2, type="III")
  
  #    c.  Visualize the model with a regular effects plot. 
  #        Interpret the resulting graph carefully. What do you conclude?
  
  plot(allEffects(fit2), multiline=TRUE, confint=list(style="auto"))
  
  # Age on its own has a limited effect on outcomes. Parasite load has a clear effect: a low load 
  # leads to recovery, an intermediate load often leads to a chronic condition, while a high load 
  # almost inevitably leads to mortality. There is an interaction between age and parasite load: 
  # for a low parasite load, age has little effect, but for higher parasite load, age makes mortality 
  # more likely as an outcome.
  
  #    d.  OPTIONAL: The effect plot is not an ideal visualization.
  #        As an alternative, try to produce 'stacked' plots with ggplot.
  #        For inspiration, you can check the R code from the lecture (but don't copy this one on one, it won't work!)
  #        To get you started, the following code creates a dataframe with values of age and parasite load. You will
  #        still have to generate model predictions for these predictor values (using the 'predict' function) and then
  #        make a dataframe that includes both these predictions and the corresponding predictor values. Then, you have to
  #        use the 'melt' function to transform these predictions into 'long format' (see lecture code for an example).
  #        From that dataframe, you can then make the plots with ggplot. If you get stuck, google it or ask ChatGPT!
  
  # create a sequence of parasite loads from 10 to 150 in 100 steps
  parasite_loads <- seq(10, 150, length.out = 100)
  # create a sequence of 3 ages: 10, 30 and 50
  ages <- c(10, 30, 50)
  # create a dataframe that has all combinations of ages and parasite loads
  new_data <- expand.grid(Parasite_load = parasite_loads, Age = ages)
  
  #generate model predictions for all these combinations
  predicted_probs <- predict(fit2, newdata = new_data, type = "probs")
  #append the new_data dataframe so that the predictions and their associated predictor values are all in one dataframe
  predicted_df <- cbind(new_data, predicted_probs)
  #transform the data to long format
  predicted_long <- melt(predicted_df, id.vars = c("Parasite_load", "Age"), variable.name = "Outcome", value.name = "Probability")
  
  #create a plot
  ggplot(predicted_long, aes(x = Parasite_load, y = Probability, fill = Outcome)) +
    geom_area() +
    facet_wrap(~ Age, ncol = 1) +
    labs(x = "Parasite Load",
         y = "Probability") + 
    theme_minimal()
  
  
  #Testing for assumptions for multinomial models is not straightforward
  #so we will not do this for multinomial/ordinal models in this course
  
  #Babblers ----
  head(babbler)
  str(babbler)
  babbler$ID <- as.factor(babbler$ID)
  babbler$SEX <- as.factor(babbler$SEX)
  babbler$GROUP_ID <- as.factor(babbler$GROUP_ID)
  
  babbler <- subset(babbler, babbler$SEX != "U")
  plot(GCP~SEX, data=babbler)
  plot(GCP~AGE, data=babbler)
  
  #   b.  First construct models to predict cognitive performance of the birds based on age and sex that WITHOUT
  #       including any random effects. Run models for each possible combination of predictors and choose the best.
  #       Since the research question is exploratory, there is no need to keep any of the predictor variable in the
  #       model at all costs - just decide based on AICc. 
  #       What would you conclude based on the best model?
  
  fit1 <- lm(GCP ~ SEX * AGE, data=babbler)
  fit2 <- lm(GCP ~ SEX + AGE, data=babbler)
  fit3 <- lm(GCP ~ SEX, data=babbler)
  fit4 <- lm(GCP ~ AGE, data=babbler)
  
  AICc(fit1, fit2, fit3, fit4)
  
  summary(fit4)
  Anova(fit4, type="III")
  #we would conclude that none of the predictors have a significant effect on cognitive performance
  
  #   c.  Run your models again, but now include the appropriate random effects structure. Again choose
  #       the best model. Do you reach a different conclusion?
  
  fit1b <- lmer(GCP ~ SEX * AGE + (1|GROUP_ID), data=babbler)
  fit2b <- lmer(GCP ~ SEX + AGE + (1|GROUP_ID), data=babbler)
  fit3b <- lmer(GCP ~ SEX + (1|GROUP_ID), data=babbler)
  fit4b <- lmer(GCP ~ AGE + (1|GROUP_ID), data=babbler)
  
  
  AICc(fit1b, fit2b, fit3b, fit4b)
  
  summary(fit4b)
  Anova(fit4b, type="III")
  # now we conclude that age has a significant negative effect on cognitive performance
  
  AICc(fit4, fit4b) #the model with random effect is also better based on AICc
  
  
  summary(fit3b)
  Anova(fit3b, type="III")
  # since fit3b had an AICc that was very close to the one of fit4b, we also consider the conclusions
  # of this model. Here, the outcome is that sex does not have a significant effect on cognitive performance.
  # Taken together, we can conclude that age has a significant negative effect and we have not found any
  # evidence for a positive effect of sex on cognitive performance.
  
  #   d.  Visualize the best model
  plot(allEffects(fit4b))
  
  #   e.  Check for normality of residuals, outliers, and influential observations. You can skip
  #       checking for homogeneity of variances (we will see how to deal with is in a next class).
  #       What final conclusion do you reach?
  
  shapiro.test(resid(fit4b))
  # W > 0.9 so no deviation from normality
  outlierTest(fit4b)
  cooks.distance(fit4b) > 1
  # No problem
  
  #checking for linearity for mixed models is not seen in this course
  
  # We can conclude that age has a significant negative effect on cognitive performance,
  # but we have no evidence for any effect of sex.
  
  #Owls ----
  head(owls)
  str(owls)
  #we here use 'mutate_if' from the package 'dplyr' to change all character variables
  #to factors at the same time, but you can of course also change them one by one.
  owls <- mutate_if(owls,is.character, as.factor)
  str(owls)
  
  par(mfrow=c(1,2))
  plot(Vocalizations~SexParent, data = owls)
  plot(Vocalizations~FoodTreatment, data = owls)
  par(mfrow=c(1,1))
  
  #   b.  Construct a generalized linear mixed model to predict the number of vocalizations
  #       depending on the food treatment and the sex of the feeding parent. Include the appropriate
  #       random effects and specify the right error structure (and link function).
  #       Do not include the interaction between food treatment and feeding parent.
  #       Visualize the model with effect plots.
  
  fit5=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=owls)
  summary(fit5)    #sig effect of treatment and sex on owl vocalisations
  Anova(fit5, type="III") #we detect a significant effect of food treatment and of the sex of the parent
  plot(allEffects(fit5), type="response") #satiated = less vocalisations; male parent = less vocalisations
  levels(owls$FoodTreatment) #deprived = 0; satiated = 1
  levels(owls$SexParent) #female = 0; male = 1
  #   c.  Now include the interaction between food treatment and feeding parent in the model.
  #       Is this model better? Visualize the best model with effect plots if you have not yet done so.
  
  fit6=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest),family=poisson(link=log),data=owls)
  summary(fit6) #the interaction is significant + both individually
  Anova(fit6, type="III") #all three significant
  AICc(fit5, fit6) #keep the model with the interaction
  plot(allEffects(fit6), type="response", multiline=T, confint=list(style="auto"))
  
  #   d.  Check if there is any overdispersion in the model.
  #       Recall that has to be done in a different way than if you have a glm (without random factors).
  #       If the overdispersed model is better, check the summary table.
  #       Do any of your conclusions change?
  #       Does it make sense to try to fit any other models based on your conclusions?
  #       Visualize the final version of your model.
  
  #we must first include an observation-level variable
  owls$obs <- factor(1:nrow(owls))
  head(owls)
  
  #then we run the same model, now including that new variable as a random factor:
  fit6b=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest)+(1|obs),family=poisson(link=log),data=owls)
  
  AICc(fit6, fit6b) #the model with overdispersion has much better AICc, keep it!
  #make an effect plot
  summary(fit6b) #only food treatment now significant
  Anova(fit6b, type="III") #interaction and effect of sex no longer significant!
  plot(allEffects(fit6b), type="response", multiline=T, confint=list(style="auto"))
  
  #since the interaction is now no longer significant, we may try to fit the
  #model without interaction, but with accounting for overdispersion. 
  fit7=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=owls)
  fit7b=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest)+(1|obs),family=poisson(link=log),data=owls)
  
  AICc(fit6, fit6b, fit7, fit7b) 
  #it is close, but we drop the interaction from the model and keep the observation level random effect
  summary(fit7b)
  Anova(fit7b, type="III") #food treatment significant; parent of sex borderline
  plot(allEffects(fit7b), type="response")
  
  #since the AICc models 6b and 7b are very close in AICc, we may report both. Our conclusions will remain largely
  #the same between both models: the coefficients for the main effects are similar and the interaction
  #is not significant in the model with the interaction.
  
  #testing for outliers and influential observations for a glmer is not straigthforward and we do not do this in this course
  
  #   e.  What are your conclusions based on this model?
  
  #we conclude that the owl chicks do more 'sibling negotiation vocalizations' if they
  #are food deprived than if they are satiated. Based on our data, we can not conclude
  #that the sex of the parent matters for the number of vocalizations and we have no support
  #for a significant interaction between the two.
  
  
  
#Non linear relationships ----
  #DNAse ----
  #     We have a dataset DNase (it is a dataset already available in R) that contains
  #     the optical density that was measured for a number of different concentrations
  #     of the enzyme DNase. In this exercise, we consider only the "Run = 1" assay.
  #     We have all measurements in duplo.
  d1 <- subset(DNase, Run == 1)
  
  #   a.   Start with some explorations, including a plot of our raw data.
  
  head(d1)
  str(d1)
  plot(density~conc, data=d1)
  
  #   b.   We want to predict the optical density as a function of concentration.
  #        We know that the relationship between the optical density and the concentration
  #        has the following form:
  
  #        optical density ~ max_density/(1+exp((xmid-log(concentration))/scale))
  
  #        In that equation, max_density, xmid and scale are parameters that 
  #        need to be estimated by our nonlinear model, while 'concentration' is 
  #        our predictor variable (named 'conc' in our dataset).
  
  #        Construct a nonlinear model to predict optical density from concentration
  #        according to this relationship. You can just take 1 for all your starting 
  #        values.
  
  #        Which values do you find for the model parameters? 
  
  fit <- nls(density ~ max_density/(1+exp((xmid-log(conc))/scale)), 
             start = list(max_density=1, xmid=1, scale=1), data=d1)
  
  fitx <- lm(density ~ conc, data=d1) #if you would fit a simple lm, you would see 
  # a strong deviation from the linearity assumption
  residualPlots(fitx)
  
  
  summary(fit)#the parameters estimates are given under 'Estimate' in the summary table
  
  #   c.  Make two plots: one of the fit with 95% confidence intervals and
  #       one of the fit with 95% prediction intervals
  
  par(mfrow = c(1,2)) #this allows us to make two plots next to each other 
  plotFit(fit, interval="confidence") # interpretation: if you ran the experiment again, you have 95%
  # confidence that your fit will fall within the interval
  plotFit(fit, interval="prediction") # interpretation: if you have a new observation, you have 95% 
  # confidence that it will fall within the interval
  par(mfrow = c(1,1)) # go back to just one graph in the graphics device
  
  preds <- predict(fit, list(conc=4)) #we can make predicions now based on our fit
  preds
  
  #    d.  Test the assumption that the residuals are normally distributed
  shapiro.test(residuals(fit)) # residuals do not diverge from normal distribution
  
  #    e.  Check if there are systematic trends in your residuals.
  plot(fit) #no clear systematic deviations - the non-linear function that we used
  #seems to have been appropriate for fitting the data
  
  ##testing homogeneity of variances and outliers is more difficult for non-linear models, and we will not do this in this course
  
  #    f.  What are your conclusions from this model?
  #        Try to frame them in terms of your estimated parameter values max_density
  #        and xmid.
  
  #Our model estimated that the maximum optical density is 2.34. In other words, our
  #model estimates based on this nonlinear function that no matter how high we raise
  #the DNAse concentration, the optical density will approach but not exceed 2.34.
  #Also, our model estimates xmid to be 1.48, meaning that half of the maximum optical
  #density is reached at a log(concentration) of 1.48.
  
  
  #Medicinal treatment ----
  #      In the dataset 'Theoph' we have the serum concentration of drug Theophylline 
  #      for different individuals at various times after the drug has been administered.
  #      The individuals all got somewhat different doses of the drug.
  
  #      We are interested in predicting the concentration "conc" as a function of 
  #      dose and time, while also accounting for random individual variation between 
  #      the subjects.
  
  #      For 'nlme', we must structure the data with "groupedData", but our data set
  #      is already structured this way so we do not need to worry about it.
  d2 <- Theoph
  
  #   a.  Start with explorations: check the data and make some graphs.
  #       Hint: because we used 'groupedData', you can just use the plot command
  #       on the dataset to get separate graphs for each individual.
  
  head(d2)
  str(d2)
  plot(d2) #because we used groupedData, this plots separate graphs for all subjects.
  
  #   b.  We want to predict the concentration as a function of time and dose, while
  #       also accounting for random variation between subjects. Assume that you knew
  #       beforehand that the self-starting function 'SSfol' is the appropriate
  #       function to model this relationship. Check the helpfile of SSfol to see
  #       what arguments this function needs. Which of the arguments are predictor 
  #       variables from your data, and which are parameters that the model will have
  #       to estimate?
  #       
  #       If you are curious, the actual function is also given under 'Value' in the
  #       help file.
  ?SSfol
  
  
  #there are five arguments: Dose, input, lKe, lKa and lCl. The first two are predictor
  #variables from our data, while the other three are parameters that are to be estimated
  #by our model.
  
  #   c.  Construct a nonlinear mixed model to fit the parameters. Allow all parameters
  #       to vary between individual subjects.
  #       As you can see in the help file, the parameters of this function are natural
  #       logarithms of rates, so they will be somewhat difficult to interpret. 
  #       Nevertheless, check that you can find the estimated parameters and the random
  #       effects for each parameter in the summary table of the model.
  
  fit2 = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKa + lCl + lKe ~ 1|Subject)
  
  summary(fit2)
  
  #   d.  Plot the model predictions for each Subject. Plot predictions based on only
  #       the fixed factors as well as based on both fixed AND random factors.
  #       Are the fixed parts of the predictions the same for each individual? Why (not)?
  
  plot(augPred(fit2, level=c(0,1)))
  #the fixed parts are different between the individuals, because the individuals
  #all got a different dose. So even without the random individual variation between
  #Subjects, our model arrives at different predictions for them.
  
  #   e.  Check visually if there seems to be any temporal autocorrelation in the model.
  #       Based on this, do you think a model that takes autocorrelation into account
  #       will be better? Construct this model and compare it with the original model
  #       based on AICc. Because Time is a continuous variable in our data, use 
  #       'correlation = corCAR1(form = ~Time)' for your autocorrelation term. 
  #
  #       Which model is better? Was your intuition correct?
  
  plot(ACF(fit2)) #there does not seem to be any temporal autocorrelation (there is
  #only a serious correlation at lag 0, which is 1 by definition, 
  #because points correlate perfectly with themselves)
  
  fit2b = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKa + lCl + lKe ~ 1|Subject,
    correlation = corCAR1(form = ~Time))
  
  AICc(fit2, fit2b) #the first model is better, so we keep it!
  
  #   f.  For the best model, check normality of residuals and if there are any systematic 
  #       deviations from your predictions.
  
  shapiro.test(residuals(fit2)) #it's close, but W is just over 0.9 so we are OK
  plot(fit2) #there are no very obvious systematic deviations from our predictions here.
  
  #   g.  OPTIONAL: The model we built allowed all three parameters (lKe, lKa, and lCl) 
  #       to vary randomly between Subjects. It is possible that this is not the best 
  #       possible model - perhaps including allowing only some of these parameters to 
  #       vary between Subjects would give a better AICc. 
  #       Build all (six) possible models that allow only one or two of the parameters 
  #       to randomly vary between individuals.
  #       Do any of these models have a better AICc than the original model? 
  #       Which model is the best?
  
  #these are the six possible models:
  fit2c = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKe + lCl ~ 1|Subject)
  
  fit2d = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random =  lKa + lCl ~ 1|Subject)
  
  fit2e = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKe + lKa ~ 1|Subject)
  
  fit2f = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKe  ~ 1|Subject)
  
  fit2g = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKa ~ 1|Subject)
  
  #the model above does not converge... Let's just not take it into account
  #for now.
  
  fit2h = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lCl ~ 1|Subject)
  
  #let's check all their AICs, including the AIC of our original model (nlme1):
  AICc(fit2,fit2c,fit2d,fit2e,fit2f,fit2h) 
  #In fact, our original model turns out to be the best model
  
  
  # TO ILLUSTRATE: We can now make predictions based on this model, 
  #Say you want to predict for a Dose = 300 mg and Time = 5 hours.
  #we make a new dataframe of one value of dose and time:
  new <- data.frame(Dose = 300, Time = 5)
  #and make predictions for it based on out fit:
  predict(fit2, newdata = new, level = 0)
  
  #Triceps skinfold ----
  #     We have data of the 'triceps skin fold' of 892 women in West-Africa of different 
  #     ages (this measurement gives an indication of their fat reserves). The data is 
  #     available in the package 'MultiKink' which you'll have to (install and) load.
  d3 <- triceps
  
  
  #   a.  Start with exploring that data, including a raw data plot of 'triceps' (these 
  #       are the skin fold measurements in cm) as a function of age. The dataset also
  #       contains a column containing log-transformed data of the measurements, but
  #       we will not use these in this exercise.
  
  head(d3)
  str(d3)
  plot(triceps~age, d3)
  
  #   b.  Construct a linear model that contains a spline term for age.
  #       Start with 10 knots. Visualize the model predictions.
  
  fit3 <- lm(triceps ~ ns(age, df=10), data=d3)
  summary(fit3)
  
  #I load library scales to be able to make our points partly see-through with 'alpha'
  library(scales)
  plot(triceps~age, d3, col=alpha("black", 0.2))
  xvals=0:550/10
  preds=predict(fit3,data.frame(age=xvals),se=T)
  lines(xvals, preds$fit, col="red", lwd=2)
  lines(xvals, preds$fit + preds$se.fit * 1.96, col="red", lty=2)
  lines(xvals, preds$fit - preds$se.fit * 1.96, col="red", lty=2)
  
  
  #   c.  Find the best number of knots for the spline based on AICc and visualize
  #       the best model.
  
  fit3df <- lm(triceps ~ ns(age, df=3), data=d3)
  fit4df <- lm(triceps ~ ns(age, df=4), data=d3)
  fit5df <- lm(triceps ~ ns(age, df=5), data=d3)
  fit6df <- lm(triceps ~ ns(age, df=6), data=d3)
  fit7df <- lm(triceps ~ ns(age, df=7), data=d3)
  fit8df <- lm(triceps ~ ns(age, df=8), data=d3)
  fit9df <- lm(triceps ~ ns(age, df=9), data=d3)
  fit10df <- lm(triceps ~ ns(age, df=10), data=d3)
  
  AICc(fit3df,fit4df,fit5df,fit6df,fit7df,fit8df,fit9df,fit10df)
  #the model with 5 knots is the best!
  fit4 <- fit5df
  
  plot(triceps~age, d3, col=alpha("black", 0.2))
  xvals=0:550/10
  preds=predict(fit4,data.frame(age=xvals),se=T)
  lines(xvals, preds$fit, col="red", lwd=2)
  lines(xvals, preds$fit + preds$se.fit * 1.96, col="red", lty=2)
  lines(xvals, preds$fit - preds$se.fit * 1.96, col="red", lty=2)
  
  #   d.  Check normality of residuals, homogeneous variances, outliers and 
  #       influential observations.
  
  hist(residuals(fit4), breaks=-15:25)
  shapiro.test(residuals(fit4)) #deviation from normality...
  ncvTest(fit4) #clear deviation from homogeneous variances
  spreadLevelPlot(fit4) 
  outlierTest(fit4)#there are a number of significant outliers
  max(cooks.distance(fit4))#but no influential observations
  
  library(robustbase)
  fit_rob<- lmrob(triceps ~ ns(age, df=5), data=d3) #lmrob does not converge to a better, more robust model
  
  #  specifying custom variance along a continuous predictor requires specification of how you want to let your residuals vary
  # along this continuous predictor. This is not required for this course
  
  
  #   e.  ILLUSTRATIVELY: (you don't need to be able to do this for the exam!!!): We have some problems with normality of residuals and homogeneous 
  #       variances. In this case we will try to fix this by transforming our response 
  #       variable. You can do this by creating a new variable in your dataset which 
  #       takes the triceps data to the power -1.2 (d3$triceps.trans <- d3$triceps^-1.2).
  
  #       For now, use the same number of knots as above. Plot your model predictions.
  #       (try to plot them on the original scale - hint: to reverse the power
  #       transformation, take your predictions to the power 1/-1.2.)
  #       Does this model lead to very different predictions?
  #       Does it resolve the violations of our assumptions we had? 
  
  d3$triceps.trans <- d3$triceps^-1.2
  
  fit5 <- lm(triceps.trans ~ ns(age, df=5), data=d3)
  
  par(mfrow=c(1,2)) #this is to plot the original model and the new model predictions
  #next to each other
  
  #original model:
  plot(triceps~age, d3, col=alpha("black", 0.2))
  xvals=0:550/10
  preds=predict(fit4,data.frame(age=xvals),se=T)
  lines(xvals, preds$fit, col="red", lwd=2)
  lines(xvals, preds$fit + preds$se.fit * 1.96, col="red", lty=2)
  lines(xvals, preds$fit - preds$se.fit * 1.96, col="red", lty=2)
  
  #new model. I take the predictions to the power (1/-1.2) to get back to the original
  #scale so we can compare the predictions of both models side by side easily:
  plot(triceps~age, d3, col=alpha("black", 0.2))
  xvals=0:550/10
  preds=predict(fit5,data.frame(age = xvals),se=T)
  lines(xvals, preds$fit^(1/-1.2), col="red", lwd=2)
  lines(xvals, (preds$fit + preds$se.fit * 1.96)^(1/-1.2), col="red", lty=2)
  lines(xvals, (preds$fit - preds$se.fit * 1.96)^(1/-1.2), col="red", lty=2)
  
  #the differences in both model predictions are not enormous, but certainly noticeable.
  #We now have more confidence in the areas where the data have less variance, as it
  #should be.
  
  par(mfrow=c(1,1))
  
  shapiro.test(residuals(fit5)) #no more deviation from normality
  ncvTest(fit5) #no more deviation from homogeneous variances
  spreadLevelPlot(fit5)  
  outlierTest(fit5)#there are a number of significant outliers
  max(cooks.distance(fit5))#but no influential observations
  
  
#PCA ----
  #Diploid-hexaploid contact zone of Aster amellus ----
  View(aster)
  str(aster)
  aster$Ploidy=as.factor(aster$Ploidy)
  which(is.na(aster))
  asternew <- na.omit(aster) #a PCA on a dataset with missing data gives errors
  which(is.na(asternew))
  aster.data = asternew[,2:ncol(asternew)]            # Selection of the columns that contain data, leaving out the "Ploidy" level of the individuals
  str(aster.data)
  View(aster.data)
  #
  aster.data$Nr.branches <- as.numeric(aster.data$Nr.branches)
  pairs(aster.data, cex=0.8, pch=21, col=c("blue","green","red")[asternew$Ploidy])
  cor(aster.data, use="complete.obs", method="pearson") #complete.obs is to include only the individuals where all data is present (omitting those with missing values)
  #cor function gives correlation coefficients
  # Variables that are correlated are: 
  #ligule.length and Nr.stem.leaves
  #ligule.length and Stem.length
  #Nr.stem.leaves and Stem.length
  #Nr.stem.leaves and Ligule.width
  
  #yes, the PCA will be usefull to reduce the dimensions as quite some variables are correlated (however rather low correlation coefficients)
  
  # You should use a correlation matrix, since not all data are in the same unit (mm, cm, count data) -> scale.=TRUE; center always has to be TRUE
  aster.data <- na.omit(aster.data)
  pca.aster <- prcomp(aster.data, center = TRUE, scale. = TRUE)
  
  #
  pca.aster #s.d and eigenvectors --> Show which variables contribute most to each component (eigenvectors of which elements are loadings)
  #eigenvectors needed for interpretation of the data (see later)
  
  eigenvalues = pca.aster$sdev^2 #amount of variance explained per PC
  eigenvalues #amount of variance of the data along the principal axes; one eigenvalue per PC
  sum(eigenvalues)  #total amount of variation of the variables; sum eigenvectors = total variability
  summary(pca.aster)  # Shows how much variance is explained by the different principal components (in proportions)
  screeplot(pca.aster, main = "Scree plot", type="lines") ##PC1 and PC2 important, rest not that important
  
  #
  plot(pca.aster$x[,c(1,2)],col ="black", cex=1) #pca.aster$x contains the scores --> position on graph per object (F=U*Yc)
  plot(pca.aster$x[,c(1,3)],col ="black", cex=1)
  plot(pca.aster$x[,c(2,3)],col ="black", cex=1)
  
  # Different color for each group: blue=diploid, green=hexaploid, red=tetraploid
  # use the Ploidy column of the asternew dataset, because pca.aster uses the dataset without the NA point 
  levels(asternew$Ploidy) #check the order of the factor to interpret colours
  plot(pca.aster$x[,c(1,2)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  legend("topleft",c("diploid", "hexaploid", "tetraploid"), fill=c("blue","green","red"))
  text(pca.aster$x[,c(1,2)], labels=row.names(asternew), pos=3) #add ID's of objects; pos=3 meansadding the text above the coordinates
  ?text
  
  plot(pca.aster$x[,c(1,3)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(2,3)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(1,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(2,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(3,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  
  # Diploids and hexaploids are clearly separated on the first PC axis, tetraploids (the current hybrid of the two cytotypes)
  # are not morphologically very distinct from the other two groups.
  # In the literature the diploids and hexaploids are considered to be two different subspecies.
  # Since intermediate hybrids (tetraploids) are easily formed, all three groups still belong to the same species.
  
  ####compare graph with the loadings (eigenvectors) to check which original variables contribute most to the variability on the axes:
  pca.aster
  
  #these variables can also visualised on biplots:
  #
  par(mfrow = c(1, 2))
  biplot(pca.aster, scale=0)           # Distance biplot - distances among objects are ~ Euclidian (cfr PCAplot above)
  biplot(pca.aster, pc.biplot=TRUE)    # Correlation biplot - angles between descriptors reflect their correlation
  
  par(mfrow = c(1, 1))
  
  # a nicer way to make correlation biplots with using ggbiplot
  library(ggbiplot)
  asternew$Nr.branches <- as.numeric(asternew$Nr.branches)  # if not already done
  asternew_clean <- na.omit(asternew)
  
  aster.data <- asternew_clean[, c("Ligule.length", "Ligule.width", "Bract.length",
                                   "Bract.width", "Nr.stem.leaves", "Nr.branches",
                                   "Nr.flowerheads", "Stem.length")]
  
  # Rerun PCA
  pca.aster <- prcomp(aster.data, center = TRUE, scale. = TRUE)
  
  # use the Ploidy column of the asternew dataset, because pca.aster uses the dataset without the NA point 
  g <- ggbiplot(pca.aster, obs.scale = 1, var.scale = 1, groups = asternew$Ploidy, ellipse = TRUE, circle = FALSE); 
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g;
  ?ggbiplot
  
  
  # Hexaploids are generally bigger; higher stem length, larger ligules and a higher number of stem leaves
  
  
  #### FYI: nicer biplot (see code in theory slides)
  # ggfortify::autoplot() is a wrapper that produces ggplot2 graphics; autoplot returns ggplot objects so you can add ggplot layers
  library(ggfortify);
  
  biplot2 <- autoplot(pca.aster, data=asternew, colour='Ploidy', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot2;
  
  
  ##plotting other PC's can be done by specifying the axes with x= and y= (default are PC1 and PC2):
  biplot2b <- autoplot(pca.aster, x=3, y=4, data=asternew, colour='Ploidy', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot2b;
  
  
  ##once you loaded ggfortify, making an 'ordinary ggbiplot will give errors.
  #unload ggfortify in order to ggbiplot to work again
  
  ##for your interest: visualisation with fviz_pca from package factoextra 
  fviz_pca(pca.aster)#with biplot
  fviz_pca_ind(pca.aster)#without biplot
  fviz_pca_ind(pca.aster, habillage=asternew$Ploidy, invisible="quali", geom= c ("point", "text"), 
               pointsize=2, labelsize=4)#different colours for different groups
  #the argument invisible=quali is to remove the mean point for each habillage that is generated
  fviz_pca_ind(pca.aster, axes = c(1,2), habillage=asternew$Ploidy, invisible="quali", label="var",
               addEllipses=T, ellipse.type="convex",title="PCA aster", 
               palette=c("blue","green","red"))
  fviz_pca(pca.aster, axes = c(1,2), habillage=asternew$Ploidy, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA aster", 
           palette=c("blue","green","red"))
  # habillage: convex hulls around groups
  #axes: define which PC axes to visualise
  fviz_pca_ind(pca.aster, axes = c(1,3), habillage=asternew$Ploidy, invisible="quali", label="var",
               addEllipses=T, ellipse.type="convex",title="PCA aster", 
               palette=c("blue","green","red"))

  #Queen pheromones in social wasps ----
  
  # Social wasp datasets - cuticular hydrocarbon data was log-ratio transformed to account for differences in concentrations among samples
  View(dolicho)
  str(dolicho)
  dolicho<-mutate_if(dolicho, is.character, as.factor)
  dolicho.data = dolicho[,6:ncol(dolicho)]
  which(is.na(dolicho.data)) #no missing data so ok
  
  # PCA Dolichovespula saxonica
  # Use a covariance matrix because all data is in the same unit (log ratio transformed cuticular hydrocarbon data) -> scale.=FALSE
  pca.dolicho = prcomp(dolicho.data, center=T, scale.=F)  
  
  pca.dolicho #it is not the case that some components stand out in contributing to the variance explained; it is more the total combination of components 
  # that explain variability (and explain differences between groups)
  
  eigenvalues=pca.dolicho$sdev^2 #amount of variance of the data along the principal axes; one eigenvalue per PC
  sum(eigenvalues)#total amount of variation of the variables; sum eigenvectors = total variability
  eigenvalues
  summary(pca.dolicho) #proportion variance explained per PC
  screeplot(pca.dolicho, main= "Scree plot",type="lines") #first 4 PC axes are meaningfull
  
  levels(dolicho$Caste)
  
  # PCA plots of Dolichovespula saxonica - queen=blue, reproductive worker=green, sterile worker=red
  plot(pca.dolicho$x[,c(1,2)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
  legend("bottomleft", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  text(pca.dolicho$x, labels=row.names(dolicho), pos=3) 
  plot(pca.dolicho$x[,c(1,3)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
  legend("topright", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  plot(pca.dolicho$x[,c(2,3)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
  legend("topright", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  # PC1 shows that queens smell different than sterile workers
  # when workers become reproductive they start smelling like the queen
  # The Queen Pheromone is a fertility signal in D. saxonica
  
  par(mfrow = c(1, 2))
  # biplot
  biplot(pca.dolicho,cex=0.9, scale=0) # Distance biplot - distances among objects are ~ Euclidian (cfr PCAplot above)
  biplot(pca.dolicho, pc.biplot=TRUE) # Correlation biplot - angles between descriptors reflect their correlation
  par(mfrow = c(1, 1))
  
  #  biplot using ggbiplot
  #  if the package ggfortify is loaded, you might have to unload it first for ggbiplot to work
  g <- ggbiplot(pca.dolicho, obs.scale = 1, var.scale = 1, groups = dolicho$Caste, ellipse = TRUE, circle = FALSE);
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g

  
  #### FYI: nicer biplots (see code in theory slides)
  library(ggfortify);
  
  biplot2 <- autoplot(pca.dolicho, data=dolicho, colour='Caste', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot2;
  
  
  #biplot using factoextra
  fviz_pca(pca.dolicho, axes = c(1,2), habillage=dolicho$Caste, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA dolicho", 
           palette=c("blue","green","red"))
  
  
  
  #Vespula vulgaris##############################################################################################
  vespula<-mutate_if(vespula, is.character, as.factor)
  vespula.data = vespula[,3:ncol(vespula)]
  which(is.na(vespula.data)) 
  View(vespula)
  # PCA Vespula vulgaris
  # Use a covariance matrix because all data is in the same unit (log ratio transformed cuticular hydrocarbon data) -> scale.=FALSE
  pca.vespula = prcomp(vespula.data,center=T,scale.=F)  
  
  eigenvalues=pca.vespula$sdev^2
  sum(eigenvalues)
  eigenvalues
  summary(pca.vespula)
  screeplot(pca.vespula, main= "Scree plot",type="lines")
  
  pca.vespula
  
  
  
  # PCA plots of Vespula vulgaris - queen=blue, reproductive worker=green, sterile worker=red
  levels(vespula$Caste) #check the order of the different groups to link the colours
  plot(pca.vespula$x[,c(1,2)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
  legend("topleft", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  text(pca.vespula$x[,c(1,2)], labels= row.names(vespula), pos=2) 
  plot(pca.vespula$x[,c(1,3)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
  text(pca.vespula$x[,c(1,3)], labels=row.names(vespula), pos=2) 
  plot(pca.vespula$x[,c(2,3)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
  text(pca.vespula$x[,c(2,3)], labels=row.names(vespula), pos=2) 
  # PC1 shows that queens smell different than all workers
  #SW4 is probably an outlier that needs to be checked
  # when workers become reproductive they do not smell like the queen.
  # The Queen Pheromone is not a fertility signal in V. vulgaris
  
  #  biplot using ggbiplot
  g <- ggbiplot(pca.vespula, obs.scale = 1, var.scale = 1, groups = vespula$Caste, ellipse = TRUE, circle = FALSE);
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g
  
  
  #  biplot using autoplot
  biplot3 <- autoplot(pca.vespula, data=vespula, colour='Caste', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot3;
  
  
  #Fish in ponds ----
  str(fish)
  fish$VIS=as.factor(fish$VIS)
  fish$"Poel Id."=as.factor(fish$"Poel Id.")
  View(fish)
  
  #
  fish.data=fish[,c(2:17,19:ncol(fish))] # Remove the variables fish and Poel(=pond) from the dataset 
  #(poel = ID and is not data to enter in the PCA)
  #vis (fish) is our categorizing variable to examine differences for (~ predictor variable)
  
  # Use a correlation matrix because not all variables have the same unit
  pca.fish=prcomp(fish.data,center=T,scale.=T)
  eigenvalues=pca.fish$sdev^2
  sum(eigenvalues)
  summary(pca.fish)
  
  pca.fish
  
  #
  screeplot(pca.fish, main= "Scree plot",type="lines")
  #the first  three PC's seem informative, containing most of the variability
  
  
  #yellow = no fish
  levels(fish$VIS)
  plot(pca.fish$x[,c(1,2)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
  plot(pca.fish$x[,c(1,3)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
  plot(pca.fish$x[,c(2,3)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
  
  #
  biplot(pca.fish,scale=0)
  biplot(pca.fish,pc.biplot=TRUE)
  
  #
  g <- ggbiplot(pca.fish, obs.scale = 1, var.scale = 1, groups = fish$VIS, ellipse = TRUE, circle = FALSE);
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g;
  
  #
  fviz_pca(pca.fish, axes = c(1,2), habillage=fish$VIS, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA Fish", 
           palette=c("yellow", "blue"))
  fviz_pca(pca.fish, axes = c(3,4), habillage=fish$VIS, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA Fish", 
           palette=c("yellow", "blue"))
  
  #EXTRA: Test whether the principal components differ significantly between the two groups.
  
  hist(pca.fish$x[,1])        # Check whether your variables (the positions of the points on the PC's: columns of matrix F) are normally distributed (to perform following analyses)
  shapiro.test(pca.fish$x[,1])
  hist(pca.fish$x[,2])
  shapiro.test(pca.fish$x[,2])
  
  library(afex)
  library(car)
  set_sum_contrasts()
  fish.data.pc=cbind(fish,pca.fish$x[,1:4]) # Adding the first four PCs to the original dataset
  mod1=lm(PC1~VIS,data=fish.data.pc)
  summary(mod1)#The  presence/absence of fish has a significant effect on the position on PC1
  Anova(mod1, type="III")
  plot(fish.data.pc$VIS,fish.data.pc$PC1, xlab="VIS", ylab="PC1")
  
  #The relationship between pc1 and the presence/absence of fish is significant, however, because we really don't see clear 
  # structuring on the PCA, we doubt the biological relevance of this 'statistical difference'
  
  mod2=lm(PC2~VIS,data=fish.data.pc)
  summary(mod2)  
  Anova(mod2, type="III") # Not significant
  plot(fish.data.pc$VIS,fish.data.pc$PC2,  xlab="VIS", ylab="PC2")

#NMDS ----
  #Fungi ----
  str(mycor.all)
  mycor.all<-mutate_if(mycor.all, is.character, as.factor)
  mycor = mycor.all[,c(2:84)] #only select the numerical data to work with

  #creating the Dissimilarity matrix
  #for non-binary data, you could use the following methods
  mycor.euc <- vegdist(mycor, method = "euclidean")
  mycor.man <- vegdist(mycor, method = "manhattan")
  mycor.bc <- vegdist(mycor, method = "bray")
  mycor.chord <- vegdist(decostand(mycor, "norm"), method="euclidean")
  #for binary data (which we have)
  mycor.jac<-dist.binary(mycor, method=1)
  # Dissimilarity matrix visualization, pink=similar, blue is dissimilar
  ##build coldiss function: see code on Toledo to create the function
  "coldiss" <- function(D, nc = 4, byrank = TRUE, diag = FALSE)
  {
    require(gclus)
    
    if (max(D)>1) D <- D/max(D)
    
    if (byrank) {
      spe.color <- dmat.color(1-D, cm.colors(nc))
    }
    else {
      spe.color <- dmat.color(1-D, byrank=FALSE, cm.colors(nc))
    }
    
    spe.o <- order.single(1-D)
    speo.color <- spe.color[spe.o, spe.o]
    
    op <- par(mfrow=c(1,2), pty="s")
    
    if (diag) {
      plotcolors(spe.color, rlabels=attributes(D)$Labels, 
                 main="Dissimilarity Matrix", 
                 dlabels=attributes(D)$Labels)
      plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
                 main="Ordered Dissimilarity Matrix", 
                 dlabels=attributes(D)$Labels[spe.o])
    }
    else {
      plotcolors(spe.color, rlabels=attributes(D)$Labels, 
                 main="Dissimilarity Matrix")
      plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
                 main="Ordered Dissimilarity Matrix")
    }
    
    par(op)
  }
  coldiss(mycor.bc, byrank=FALSE, diag=TRUE) 
  coldiss(mycor.jac, byrank=FALSE, diag=TRUE) 
  
  par(mfrow=c(1,1))
  ### NMDS - Euclidean (Don't use the Euclidean distance matrix because it cannot handle zeros very well)
  mycor.nmds.euc <- metaMDS(mycor, distance = "euclidean") #use original dataset as input, metaMDS calculates distance matrix
  mycor.nmds.euc #stress = 0.12
  plot(mycor.nmds.euc, type="text", main=paste("NMDS/Euclidean -Stress =", round(mycor.nmds.euc$stress,3)))
  
  # Stress: 0.05 = excellent; 0.1 = great; 0.2 = ok; 0.3 = poor representation
  
  # Orchid species are colored
  levels(mycor.all$Species)
  plot(mycor.nmds.euc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Euclidean - Stress =", round(mycor.nmds.euc$stress,3  )))
  legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"), text.font=8, text.width =0.5, bty="n" )
  
  ## $points: plotting only the plant objects; if you want to plot the columns (OTU's in this case) you can put $species
  text(mycor.nmds.euc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
  ## display the mycor. OTU's as text
  ##if you want to plot the text of the plant objects (the rows) you put display="sites"
  
  #shepards plot compares the distances in the ordination plot with the original distances
  stressplot(mycor.nmds.euc, main=paste("Shepard plot - Euclidean", round(mycor.nmds.euc$stress,3)))
  gof=goodness(mycor.nmds.euc)
  plot(mycor.nmds.euc, type="t", main="goodness-of-fit")
  points(mycor.nmds.euc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  ### NMDS - Bray-Curtis (You could use the Bray-Curtis distance matrix since it can also handle binary data very well)
  mycor.nmds.bc <- metaMDS(mycor, distance = "bray")
  mycor.nmds.bc #stress=0.1264
  plot(mycor.nmds.bc, type="t", main=paste("NMDS/Bray-Curtis - Stress =", round(mycor.nmds.bc$stress,3)))
  
  
  # Stress: 0.05 = excellent; 0.1 = great; 0.2 = ok; 0.3 = poor representation
  
  # Orchid species are colored
  plot(mycor.nmds.bc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Bray-Curtis - Stress =", round(mycor.nmds.bc$stress,3  )))
  legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"),text.font=8, text.width =0.5, bty="n")
  ##$points: plotting only the plant objects
  
  text(mycor.nmds.bc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
  ## display the mycor. OTU's as text
  
  # Stress and goodness of fit
  stressplot(mycor.nmds.bc, main=paste("Shepard plot - Bray-Curtis", round(mycor.nmds.bc$stress,3)))
  gof=goodness(mycor.nmds.bc)
  plot(mycor.nmds.bc, type="t", main="goodness-of-fit")
  points(mycor.nmds.bc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  ### NMDS - Jaccard (The Jaccard distance matrix is also very suitable for binary data)
  mycor.nmds.jacc <- metaMDS(mycor, distance = "jaccard")
  plot(mycor.nmds.jacc, type="t", main=paste("NMDS/Jaccard - Stress =", round(mycor.nmds.jacc$stress,3)))
  
  # Orchid species are colored
  
  plot(mycor.nmds.jacc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Jaccard - Stress =", round(mycor.nmds.jacc$stress,3  )))
  text(mycor.nmds.jacc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
  legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"),text.font=8, text.width =0.2, bty="n")
  
  
  # Stress and goodness of fit
  stressplot(mycor.nmds.jacc, main=paste("Shepard plot - Jaccard", round(mycor.nmds.jacc$stress,3)))
  gof=goodness(mycor.nmds.jacc) 
  plot(mycor.nmds.jacc, type="t", main="goodness-of-fit")
  points(mycor.nmds.jacc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  ####in this case, with this dataset, completely the same results as with the BRAY_CURTIS distance matrix
  ################## Other distance matrices are also well suited for binary data. Not only the ones mentioned above.e.g. Sorenssen
  
  #Meadow ----
  str(plants)
  
  ### Distance matrix 
  #p.dist = vegdist(plants,"euclidean") # Euclidean - Not good for this dataset due to the zero's
  #p.dist = vegdist(plants,"bray") # Bray-Curtis distance 
  p.dist = vegdist(decostand(plants,"norm"),method="euclidean") # Chord distance - Good option 
  
  # Chord distance is especially useful for community data
  
  
  ### Different clustering methods
  par(mfrow=c(2,2))
  # Single linkage agglomerative clustering
  p.dist.single <- hclust(p.dist, method="single")
  plot(p.dist.single, main="Single linkage")
  
  # Complete linkage agglomerative clustering
  p.dist.complete <- hclust(p.dist, method="complete");
  plot(p.dist.complete, main="Complete linkage")
  
  # Unweighted average linkage agglomerative clustering (UPGMA)
  p.dist.UPGMA <- hclust(p.dist, method="average");
  plot(p.dist.UPGMA, main="UPGMA")
  
  
  # Ward's minimum variance method
  p.dist.ward <- hclust(p.dist, method="ward.D");
  plot(p.dist.ward,main="Ward")
  #p.dist.ward$height <-sqrt(p.dist.ward$height);
  #plot(p.dist.ward)
  
  
  ### Assessing which clustering method works best
  # Single linkage agglomerative clustering
  p.dist.single.coph <- cophenetic(p.dist.single)
  plot(p.dist, p.dist.single.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("Single linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.single.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.single.coph), col="red")
  gow.dist.single <- sum((p.dist-p.dist.single.coph)^2); 
  gow.dist.single # Small values indicate a high fit
  
  
  # Complete linkage agglomerative clustering
  p.dist.complete.coph <- cophenetic(p.dist.complete);
  plot(p.dist, p.dist.complete.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("complete linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.complete.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.complete.coph), col="red")
  gow.dist.complete <- sum((p.dist-p.dist.complete.coph)^2); 
  gow.dist.complete # Small values indicate a high fit
  
  # Unweighted average linkage agglomerative clustering (UPGMA)
  p.dist.UPGMA.coph <- cophenetic(p.dist.UPGMA);
  plot(p.dist, p.dist.UPGMA.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("UPGMA linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.UPGMA.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.UPGMA.coph), col="red")
  gow.dist.UPGMA <- sum((p.dist-p.dist.UPGMA.coph)^2); 
  gow.dist.UPGMA # Small values indicate a high fit
  
  # Ward's minimum variance method
  p.dist.ward.coph <- cophenetic(p.dist.ward);
  plot(p.dist, p.dist.ward.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("ward linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.ward.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.ward.coph), col="red")
  gow.dist.ward <- sum((p.dist-p.dist.ward.coph)^2); 
  gow.dist.ward # Small values indicate a high fit
  
  ##UPGMA is best
  ##for illustrative purposes, for the subsequent analyses we give the codes for all different clustering methods. But actually the next steps need
  # to be only done for the best clustering method (UPGMA in this case)
  par(mfrow=c(1,1))
  
  ### The optimal amount of clusters - fusion plots
  # Single linkage
  plot(p.dist.single$height, nrow(plants):2, type="S", main="Fusion levels - Single",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.single$height, nrow(plants):2,nrow(plants):2, col="red", cex = 0.8)  
  
  # Complete linkage
  plot(p.dist.complete$height, nrow(plants):2, type="S", main="Fusion levels - Complete",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.complete$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  
  
  # UPGMA
  plot(p.dist.UPGMA$height, nrow(plants):2, type="S", main="Fusion levels - UPGMA",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.UPGMA$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  
  
  # Ward's D
  plot(p.dist.ward$height, nrow(plants):2, type="S", main="Fusion levels - ward",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.ward$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  
  
  # It is quite difficult and arbitrary to choose the optimal number of clusters, but based on 
  # the fusion plots (and the information given with the exercise) I'd choose 5 clusters
  par(mfrow=c(1,2))
  par(mfrow=c(1,1))
  ### The optimal amount of clusters - silhouette width approach
  library(cluster)
  
  # Single linkage
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.single, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.single <- which.max(asw);
  k.best.single
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, single",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.single, paste("optimum",k.best.single,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.single, max(asw), pch=16, col="red", cex=1.5)  
  
  # Complete linkage
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.complete, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.complete <- which.max(asw);
  k.best.complete
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, complete",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.complete, paste("optimum",k.best.complete,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.complete, max(asw), pch=16, col="red", cex=1.5)  
  
  # UPGMA
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.UPGMA, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.upgma <- which.max(asw);
  k.best.upgma
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, UPGMA",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.upgma, paste("optimum",k.best.upgma,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.upgma, max(asw), pch=16, col="red", cex=1.5)  
  
  
  # Ward
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.ward, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }  
  k.best.ward <- which.max(asw);
  k.best.ward
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, Ward",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.ward, paste("optimum",k.best.ward,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.ward, max(asw), pch=16, col="red", cex=1.5)  
  
  # The optimal amount of clusters - Mantel approach
  
  ##first define the function to make the binary matrix:
  grpdist <- function(X)
  {require(cluster)
    gr <- as.data.frame(as.factor(X))
    distgr <- daisy(gr, "gower")
    distgr}
  kt <- data.frame(k=1:nrow(plants), r=0)
  
  
  # Single linkage
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.single, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, single",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  # Complete linkage
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.complete, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, complete",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  # UPGMA
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.UPGMA, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, UPGMA",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  # Ward
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.ward, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, Ward",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  par(mfrow=c(1,1))
  
  
  ### Silhouette plots # Use the highest number of optimal clusters found in the last exercise
  cutg1 <- cutree(p.dist.single, k=7);
  sil1 <- silhouette(cutg1, p.dist);
  silo1 <- sortSilhouette(sil1);
  rownames(silo1) <- row.names(plants)[attr(silo1,"iOrd")];
  plot(silo1, main="Silhouette plot - Single", cex.names=0.8,col=silo1+1, nmax.lab=100)
  
  cutg2 <- cutree(p.dist.complete, k=7);
  sil2 <- silhouette(cutg2, p.dist);
  silo2 <- sortSilhouette(sil2);
  rownames(silo2) <- row.names(plants)[attr(silo2,"iOrd")];
  plot(silo2, main="Silhouette plot - Complete", cex.names=0.8,col=silo2+1, nmax.lab=100)
  
  cutg3 <- cutree(p.dist.UPGMA, k=7); #you can try different numbers of clusters (e.g the max k=7, or the intermediate k=5)
  sil3 <- silhouette(cutg3, p.dist);
  silo3 <- sortSilhouette(sil3); ##orders the rows of sil by clusters and decreasing silhouette width
  rownames(silo3) <- row.names(plants)[attr(silo3,"iOrd")];#also order the plant numbers accordingly
  plot(silo3, main="Silhouette plot - UPGMA", cex.names=0.8,col=silo3+1,  nmax.lab=100)
  
  
  cutg4 <- cutree(p.dist.ward, k=7);
  sil4 <- silhouette(cutg4, p.dist);
  silo4 <- sortSilhouette(sil4);
  rownames(silo4) <- row.names(plants)[attr(silo4,"iOrd")];
  plot(silo4, main="Silhouette plot - Ward", cex.names=0.8,col=silo4+1, nmax.lab=100)
  
  
  ### Plotting the different clusters on the dendrogram
  # Single
  plants.final.single <- reorder(p.dist.single, p.dist);
  plot(plants.final.single, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - Single (reordered)", labels=cutree(plants.final.single,k=25));
  rect.hclust(plants.final.single,k=5)  
  
  # Complete
  plants.final.complete <- reorder(p.dist.complete, p.dist);
  plot(plants.final.complete, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - Complete (reordered)", labels=cutree(plants.final.complete,k=25));
  rect.hclust(plants.final.complete,k=5)  
  
  # UPGMA
  plants.final.upgma <- reorder(p.dist.UPGMA, p.dist);#reorder: making sure nearby object pairs are adjacent
  plot(plants.final.upgma, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - UPGMA (reordered)", labels=cutree(plants.final.upgma,k=25));
  ##you could also use labels = row.names(plants) insead of labels=cutree(plants.final.upgma,k=25)
  rect.hclust(plants.final.upgma,k=5) #you can test here the difference between 5 and 7 clusters 
  #or with hcoplot
  source("NMDS/hcoplot.R")
  hcoplot(p.dist.UPGMA, p.dist, k=5)
  
  
  # Ward
  plants.final.ward <- reorder(p.dist.ward, p.dist);
  plot(plants.final.ward, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - Ward (reordered)", labels=cutree(plants.final.ward,k=25));
  rect.hclust(plants.final.ward,k=5)  
  
  
  ### Combine the NMDS with the cluster results
  plants.nmds <- metaMDS(decostand(plants,"norm"),distance="euclidean") # Compare to the same distance matrix - currently Chord
  #metaMDS does not have an in-built"Chord" distance matrix to use. You need to use distance=euclidean on a standardized (decostand) dataset), 
  #similar as for constructing the distance matrix itself (see Ex1)
  plants.nmds 
  plot(plants.nmds, type="n", main=paste("NMDS/Chord - Stress =", round(plants.nmds$stress,3)))
  text(plants.nmds,display=c("sites")) #(fyi: display=c("species") plots the columns)
  #or
  plot(plants.nmds, type="text", main=paste("NMDS/Chord - Stress =", round(plants.nmds$stress,3)))
  
  # give colours based on the the clusters of the cluster analysis
  plants.groups <- cutree(p.dist.UPGMA, k=5)
  str(plants.groups)
  plants.groups<-as.factor(plants.groups)
  grp.lev <- levels((plants.groups)) #for the legend
  p <- ordiplot(plants.nmds, type="n", main="NMDS/Chord - clusters UPGMA")
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  points(plants.nmds$points, pch=21, cex=1.5, bg=plants.groups)
  legend("bottomleft",legend=grp.lev, pch=16, col=grp.lev)
  
  
  text(plants.nmds$points, row.names(plants), pos=4, cex =0.7)
  text(plants.nmds,display=c("species")) #not very aesthetically
  
  
  # Add the dendrogram 
  ordicluster(p, p.dist.UPGMA, col="dark grey")
  
  
#RDA/PERMANOVA ----
  #Bryophytes ----
  # NMDS scores here are a proxy of the communities of higher plants in the environment
  # At the bottom of this script the  NMDS analysis of the vascular plant data to obtain this data is shown
  
  ##### Exercise 1.1
  # RDA on untransformed bryophyte data
  bryo.rda <- rda(bryo~.,env) # the dot refers to including all variables of the dataset env; 
  #bryo = response matrix; env = explanatory matrix
  summary(bryo.rda)
  RsquareAdj(bryo.rda)$r.squared                # R^2 = proportion of constrained variance
  RsquareAdj(bryo.rda)$adj.r.squared            # adjusted R^2
  
  # RDA on Hellinger transformed bryophyte data
  bryo.hel <- decostand(bryo,"hellinger")
  bryo.hel.rda <- rda(bryo.hel~.,env) 
  summary(bryo.hel.rda)
  RsquareAdj(bryo.hel.rda)$r.squared                # R^2
  RsquareAdj(bryo.hel.rda)$adj.r.squared            # adjusted R^2
  # adjusted R²is % of variance explained, adjusted is to compensate for the fact that more variables lead to higher R²
  
  # Use the hellinger transformed dataset for further analyses (it has the highest Adj. R^2)
  
  ##### Exercise 1.2
  anova.cca(bryo.hel.rda, step=1000)                
  anova.cca(bryo.hel.rda, by="axis", step=1000)  
  #model significant --> bryophyte explained by env
  
  
  ##### Exercise 1.3
  # Distance triplot: distances between sites, between species or between sites and species approximate their euclidean distances.
  par(mar=c(5,5,5,5)) #set plot margins
  plot(bryo.hel.rda, scaling=1,  main="Triplot RDA bryophytes scaling = 1",type="none") # empty plot  
  bryo.sc1 <- scores(bryo.hel.rda, scaling=1) #retrieve the scores for sites and species of bryophytes
  points(bryo.sc1$sites,col="black",pch=21,cex=0.7) #plot the site scores in points
  text(bryo.sc1$species, row.names(bryo.sc1$species), col="red",cex=0.8)#plot the bryo species in text (response variables as labels)
  ##we could also add lines (or arrows) to these labels:
  arrows(0,0,bryo.sc1$species[,1], bryo.sc1$species[,2], length =0.05, lty=1, col="red")
  #length=0.05 is length of the arrow triangle (tips)
  text(bryo.hel.rda, display="bp",col="blue", scaling=1)#display biplot --> gives the environmental characteristics in arrows (arrows for continuous explanatiory variables))
  
  
  #display: bp= biplot, cn= centroids, wa or sites= sites, sp= species
  
  #####R code from theory
  plot(bryo.hel.rda, scaling=1,  main="Triplot RDA bryophytes scaling = 1") #everything at once
  ##we can also add lines for the response variables
  bryo.sc1 <- scores(bryo.hel.rda, choices= 1:2, display="sp", scaling=1)
  arrows(0,0,bryo.sc1[,1], bryo.sc1[,2], length =0, lty=1, col="red")
  
  
  # Correlation triplot: the angles in the biplot between response (abundance data) and explanatory (environment) variables, and 
  # between response variables themselves or explanatory variables themselves, reflect their correlations.
  par(mar=c(5,5,5,5))
  plot(bryo.hel.rda, scaling=2,  main="Triplot RDA bryophytes scaling = 2",type="none")   
  bryo.sc1 <- scores(bryo.hel.rda, scaling=2)
  points(bryo.sc1$sites,col="black",pch=21,cex=0.7)
  text(bryo.sc1$species, row.names(bryo.sc1$species),col="red",cex=0.8)
  arrows(0,0,bryo.sc1$species[,1], bryo.sc1$species[,2], length =0.05, lty=1, col="red")
  text(bryo.hel.rda,display="bp",col="blue")
  
  ##### Exercise 1.4
  vif.cca(bryo.hel.rda)
  # Correlations among the environmental variables are <10
  
  ##### Exercise 1.5
  # Criterion is P < 0.05
  step.forward <- ordistep(rda(bryo.hel ~ 1, data = env), scope=formula(bryo.hel.rda), direction="forward", pstep=1000)
  # Final model:  rda(bryo.hel ~ NMDS1 + NMDS2 + NH3 + Si + Corg + Ca + pH ,data=env)
  # The final model can differ (!!)
  ?ordistep
  step.forward$call
  
  # Criterion is to stop when adj.R2 of full model is exceeded
  step.forward.R2 <- ordiR2step(rda(bryo.hel ~ 1, data = env), scope=formula(bryo.hel.rda), direction="forward", pstep=1000)
  step.forward.R2$call
  # Final model:  rda(bryo ~ NMDS1 + NMDS2 + Si + NH3 + Corg + Ca ,data=env)
  # In this case the final model without ph is better (the R² of the reduced model cannot exceed the R² of the full model)
  
  # Let's continue with the model with ph in ex 1.6: NMDS1 + NMDS2 + Si + NH3 + Corg + Ca + pH
  
  ##### Exercise 1.6
  bryo.hel.rda2 = rda(bryo.hel ~ NMDS1 + NMDS2 + Si + NH3 + Corg + Ca + pH ,data=env)
  RsquareAdj(bryo.hel.rda2)$adj.r.squared 
  
  
  ##### Exercise 1.7
  #distance triplot
  par(mar=c(5,5,5,5))
  plot(bryo.hel.rda2, scaling=1,  main="Triplot RDA bryophytes (forward selection) - scaling = 1",type="none")
  bryo.sc.new <- scores(bryo.hel.rda2, scaling = 1)
  points(bryo.sc.new$sites,col="black",pch=21,cex=0.7)
  text(bryo.sc.new$species, row.names(bryo.sc.new$species),col="red",cex=0.8)
  arrows(0,0,bryo.sc.new$species[,1], bryo.sc.new$species[,2], length =0.05, lty=1, col="red")
  text(bryo.hel.rda2, display="bp",col="blue")
  
  #label sites: use text instead of points: 
  text(bryo.sc.new$sites,col="black")
  #or 
  text(bryo.hel.rda2,scaling=1, display="wa",col="black", cex=0.8)
  
  #correlation triplot
  par(mar=c(5,5,5,5))
  plot(bryo.hel.rda2, scaling=2,  main="Triplot RDA bryophytes (forward selection) - scaling = 2",type="none")
  bryo.sc.new <- scores(bryo.hel.rda2, scaling = 2)
  points(bryo.sc.new$sites,col="black",pch=21,cex=0.7)
  text(bryo.sc.new$species, row.names(bryo.sc.new$species),col="red",cex=0.8)
  arrows(0,0,bryo.sc.new$species[,1], bryo.sc.new$species[,2], length =0.05, lty=1, col="red")
  text(bryo.hel.rda2, display="bp",col="blue")
  
  ##### Exercise 1.8 - partial RDA
  env2 <- env[,c(1:15)] # Environmental variables
  veg <- env[,c(16:17)] # NMDS axes representing the vegetation data
  
  bryo.pRDA <- rda(bryo.hel, veg, env2)
  bryo.pRDA
  # Conditioned: Variance explained by the covariable 'env2' and removed
  # Constrained: Variance explained purely by 'veg'
  # Unconstrained: Residual variance
  RsquareAdj(bryo.pRDA)$adj.r.squared 
  
  anova.cca(bryo.pRDA, step = 1000) # Vascular plant communities (i.e. NMDS axes) significantly affect the bryophyte community even though the constrained variance is low
  
  bryo.pRDA2 <- rda(bryo.hel, env2, veg)
  bryo.pRDA2
  RsquareAdj(bryo.pRDA2)$adj.r.squared 
  # Conditioned: Variance explained by 'veg' and removed
  # Constrained: Variance explained purely by 'env2'
  # Unconstrained: Residual variance
  
  anova.cca(bryo.pRDA2, step = 1000) # The environmental variables also significantly affect the bryophyte community
  
  #### Exercise 1.9
  bryo.varpart <- varpart(bryo.hel, veg, env2)
  par(mar=c(1,1,1,1))
  plot(bryo.varpart, bg=c("blue","green"),Xnames=c("Veg","Env2"),cex=1.5)
  
  bryo.varpart
  
  #visualise outcomes-->use these values in function below (draw.pairwise.venn)
  #the values in the venndiagram obtained from 'varpart' are the R²adjusted values
  #the outcomes (conditioned, constrained and unconstrained) obtained from the partial RDA are the
  #unadjusted R² values
  
  # Or you could use:
  library(VennDiagram)
  par(mar=c(1,1,1,1))
  plot.new()
  draw.pairwise.venn(0.26,0.28, 0.21, c("Vegetation","Environment"), col=c("blue","green"), 
                     fill=c("light blue","lightgreen"),cex=1.3, cat.cex = 1.5)
  
  ## the numbers you need to fill in are the [a+b], [b+c] and [b] results respectively from the varpart
  
  # FYI:  NMDS on the vascular plant community: an illustration of how the columns of the NMDS axes have been obtained
  head(plant)
  
  nmds = metaMDS(decostand(plant,"nor"),distance="euclidean") # Chord distance
  nmds
  scores(nmds) ##these are the values in the dataset 'Environment_vegetation'
  plot(nmds$points,pch=21,col="black",cex=1,main=paste("NMDS Stress =", round(nmds$stress,3  )))
  text(nmds, display = "species",cex=0.7) # Adding plant species

  #MANOVA ----
  #Exercise 2.1
  str(manova_data)
  manova_data$IC<-as.factor(manova_data$IC)
  manova_data$year<-as.factor(manova_data$year)
  levels(manova_data$IC)
  
  
  # Exercise 2.2
  MSP.manova <- manova(cbind(MSP1, MSP2, MSP3)~IC, data=manova_data) 
  summary(MSP.manova, test="Wilks")
  
  
  # Exercixe 2.3
  summary(MSP.manova, test="Wilks")
  summary(MSP.manova, test="Pillai")
  summary(MSP.manova, test="Hotelling-Lawley")
  summary(MSP.manova, test="Roy")
  
  # Exercise 2.4
  summary(aov(cbind(MSP1, MSP2, MSP3)~IC, data=manova_data))
  # MSP-2 is most affected by inbreeding, but MSP-1 also affected
  
  
  #gives same results:
  library(afex)
  library(car)
  set_sum_contrasts()
  fit1<-lm(MSP1~IC, data=manova_data)
  fit2<-lm(MSP2~IC, data=manova_data)
  fit3<-lm(MSP3~IC, data=manova_data)
  
  Anova(fit1, type="III")
  Anova(fit2, type="III")
  Anova(fit3, type="III")
  
  # Exercise 2.5 --> two-way MANOVA
  IC.two.manova <- manova(cbind(MSP1,MSP2,MSP3)~IC*year, data=manova_data)
  summary(IC.two.manova,test="Wilks")
  summary(IC.two.manova, test="Pillai")
  
  # MSP production did not differ significantly between years. However, we found a significant interaction of 'level of inbreeding' and 'year' on MSP production. 
  # This indicates that MSP production varied slightly between years (but not significantly) and this variation was stronger in some of the IC groups. 
  
  
  # Exercise 2.6
  flight.manova <- manova(cbind(drythor,dryabdo,dryhead)~IC*year, data=manova_data)
  summary(flight.manova,test="Wilks")
  summary(flight.manova,test="Pillai")
  # Inbreeding significantly affects flight performance and flight performance also differs between years, and the effect of inbreeding is different over the different years
  summary(aov(cbind(drythor, dryabdo, dryhead)~IC*year, data=manova_data))
  # Inbreeding mainly affects size of the thorax and the size of the head
  # Furthermore, the size of the head differed between years and we found a significant interaction between inbreeding and year on the size of the thorax.
  
  #in a model containing many response variables, the model could be simplified by removing the response variables that are not affected by inbreeding, year or an interaction between inbreeding and year
  
  
  #PERMANOVA ----
  # Exercise 3.1
  # Exercise 3.2
  m1 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="bray")
  m1 #only gives an oveall p-value for the model
  m1 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="bray", by="terms")
  m1
  # There is a significant effect of location and a marginally significant (0.05<P<0.1) interaction effect 
  ?adonis2
  # Exercise 3.3
  m2 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="euclidean", by="terms")
  m2
  m3 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="jaccard", by="terms")
  m3 
  m4 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="manhattan", by="terms")
  m4 
  m5 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="canberra", by="terms")
  m5
  
  # Exercise 3.4 try additive models as interactions term is not or only marginally significant
  m6 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="bray", by="terms")
  m6
  m7 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="euclidean", by="terms")
  m7
  m8 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="jaccard", by="terms")
  m8
  m9 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="manhattan", by="terms")
  m9
  m10 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="canberra", by="terms")
  m10
  
  # Exercise 3.5
  m11 <- adonis2(OTU ~ location, data=id, permutations=1000, method="bray" , by="terms")
  m11
  
  # Exercise 3.6
  # Bacteria are rather generalists. They are not really associated with specific bug species, but they are location specific.

#January Exam ----
  #Question 1:
  penguins <- read.csv("EXAM/Exam data/penguins.csv")

  #context
  #penguins cannot moult and mate at the same time - significant life history event
  #Age and time of breeding may influence moult timing - individuals who breed later
  #may delay moulting 
  #Repeated measurements of same individual over different years (data points not independent)
  #Recorded: age, breeding outcome, time of moult (0 = earliest moult, 10 = 10 days after earliest moult)

  set_sum_contrasts()
  head(penguins)
  str(penguins)
  unique(peng$breeding_status)
  
  peng <- penguins %>% mutate(breeding_status = as.factor(breeding_status),
                          bird_id = as.factor(bird_id)) #change character + integer values to factors
  peng <- peng %>% dplyr::rename(status = breeding_status,
                                 ID = bird_id,
                                 moult = moult_start_days)
  #renamed for clarity
  (summ_peng <- summarise_all(peng, mean)) #gives mean of all numerical categories (some columns are non-numerical so it gives an error)
  #mean age: 9.5806; mean start date: 17.96129
  
  plot(moult ~ age, data = peng) #seems to be a slight positive trend
  boxplot(moult ~ status, data = peng) #moult time longest in successful breeders - in line with hypothesis

  #y = moult time = from 0 - counts (poisson regression best)
  #x = age = counts data
  #x = status = categorical
  #random effect: bird ID (because data points not independent) -> mixed poisson GLM best
  
  peng$status <- relevel(peng$status, ref = "Non-breeder")
  
  #Poisson GLMM
  model_null <- glmer(moult ~ 1 + (1|ID), data = peng, family = poisson) #null model
  model_status <- glmer(moult ~ status + (1|ID), data = peng, family = poisson) #reproductive status only
  model_age <- glmer(moult ~ age + (1|ID), data = peng, family = poisson) #age only
  model_add <- glmer(moult ~ status + age + (1|ID), data = peng, family = poisson) #additive model
  model_int <- glmer(moult ~ status * age + (1|ID), data = peng, family = poisson) #interactive model
  
  #to determine best model:
  MuMIn::AICc(model_null, model_status, model_age, model_add, model_int) #interactive model = best
  
  models_list <- list(model_null, model_status, model_age, model_add, model_int)
  model.sel(models_list) #model_int has the lowest AICc and delta > 2
  
  plot(allEffects(model_int), multiline = T, confint = list(style = "auto"))
  #we can see that non-breeders have the most flat slope; successful breeders the steepest positive slope
  #(meaning that time to moulting event increases with age); and failed breeders show a negative relationship
  #between age and moult, suggesting they moult earlier as they age. 
  #This all being on the log scale
  plot(allEffects(model_int), multiline = T, confint = list(style = "auto"), type = 'response')
  #With the actual responses, we see that the successful breeder curve looks non-linear (convex) and with a big
  #confidence interval, while for non-breeders and failed breeders the relationships stay the same as above
  
  summary(model_int)
  Anova(model_int, type = 3)
  vif(model4) #GVIF < 5; no collinearity!
  
  par(mfrow = c(1, 2))
  plot(residuals(model_int) ~ fitted(model_int), main = "Residuals vs. Fitted")
  qqnorm(residuals(model_int)) #looks ok
  par(mfrow = c(1, 1))
  
  #overdispersion check
  peng$obs <- factor(1:nrow(peng))
  model_overdisp <- glmer(moult ~ status * age + (1|ID) + (1|obs), data = peng, family = poisson)
  
  simulationOutput <- DHARMa::simulateResiduals(fittedModel = model_int, plot = F)
  DHARMa::testDispersion(simulationOutput) #no sign of overdisperson, statistically
  
  #plots
  Status <- peng$status
  (peng_plot <- ggplot(peng, aes(x = age, y = moult, col = status)) +
      geom_point(size = 3, alpha = 0.8) +
      theme_classic() +
      labs(
        x = "Penguin age",
        y = "Time to moulting event (days)",
        caption = "Model: moult ~ status * age + (1|ID), data = peng, family = poisson") +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()))
  ggpredict(model_int, se = TRUE, type = "count", interactive = TRUE)

  #Vegetation ----
  env <- read_xlsx("EXAM/Exam data/Env_data.xlsx")
  comm <- read_xlsx("EXAM/Exam data/Comm_data.xlsx")

  head(comm) #species abundances
  head(env) #env variables
  
  env_m <- env[,(2:25)]
  #Abundances are not continuous variables, so I will use an NMDS
  str(comm)
  comm <- mutate_if(comm, is.character, as.factor)
  com_m = comm[,c(2:34)] #only selecting the numerical data to work with
  
  #NMDS
  nmds <- metaMDS(com_m, distance = "bray") #use original dataset as input, metaMDS calculates distance matrix
  nmds #stress = 0.19; not ideal
  plot(nmds, type="text", main=paste("NMDS/Bray -Stress =", round(nmds$stress,3)))
  
  stressplot(nmds, main=paste("Shepard plot - Euclidean", round(nmds$stress,3)))
  gof=goodness(nmds)
  plot(nmds, type="t", main="goodness-of-fit")
  points(nmds, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  bray_dist <- vegdist(com_m, method = "bray") #0 = completely similar, 1 = completely different communities
  hc <- hclust(bray_dist, method = "average") #UPGMA clustering, unless there is a reason not to use it, use this
  
  
  plot(hc,
       labels = FALSE,
       hang = -1,
       main = "Hierarchical clustering of ponds (Bray–Curtis)",
       xlab = "Ponds",
       ylab = "Bray–Curtis dissimilarity")
  
  p.dist.UPGMA <- hclust(p.dist, method="average");
  plot(p.dist.UPGMA, main="UPGMA")
  
  plot(p.dist.UPGMA$height, nrow(com_m):2, type="S", main="Fusion levels - UPGMA",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.UPGMA$height, nrow(com_m):2, nrow(com_m):2, col="red", cex = 0.8)  
  
  asw <- numeric(nrow(com_m));
  for (k in 2:(nrow(com_m)-1)) {
    sil <- silhouette(cutree(p.dist.UPGMA, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.single <- which.max(asw);
  k.best.single
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, single",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.single, paste("optimum",k.best.single,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.single, max(asw), pch=16, col="red", cex=1.5)  
  
  #two clusters!
  nmds 
  plot(nmds, type="n", main=paste("NMDS/Chord - Stress =", round(nmds$stress,3)))
  text(nmds,display=c("species")) 
  
  
  library(vegan)
  nmds <- metaMDS(comm = com_m, distance = "bray", trace = FALSE, autotransform = FALSE)
  plot(nmds)    
  nmds_xy <- data.frame(nmds$points)  
  nmds_xy$pond <- comm$Pond_ID
  ggplot(nmds_xy, aes(MDS1, MDS2)) +
    geom_point() +
    theme_classic()
  
  
  nmds$stress  #acceptable
  envfit_res <- envfit(nmds, env_m)
  plot(nmds)
  plot(envfit_res)

#Practice exam 1 ----
  # ── PACKAGES ──────────────────────────────────────────────────────────────────
  library(vegan)     # metaMDS, vegdist, envfit, adonis2, scores
  library(cluster)   # silhouette
  library(ggplot2)
  library(lme4)      # glmer
  library(MuMIn)     # AICc, model.sel
  library(DHARMa)    # simulateResiduals, testDispersion
  library(emmeans)   # emmeans, pairs
  library(car)       # Anova(type=3)
  library(ggeffects) # ggpredict
  
  #Community ecology ----
  #Oribatid mites (Acari: Oribatida) are minute soil arthropods that play an essential role in
  #decomposing organic matter and regulating microbial communities. Despite their ecological
  #importance, the environmental factors structuring oribatid mite communities remain incompletely
  #understood. In a study conducted in southern Quebec (Canada), researchers collected Sphagnum
  #moss substrate cores from 35 sampling sites and identified 70 oribatid mite species, recording
  #the abundance of each species at each site. Five environmental characteristics were measured at
  #each site to describe the local moss substrate conditions.
  library(vegan)
  data(mite) # species abundance matrix — 35 sites × 70 species
  data(mite.env) # environmental data — 35 sites × 5 variables
  head(mite) #abundances
  str(mite)
  head(mite.env)
  #Can you identify groups of moss sites that show similar oribatid mite
  #communities? How many distinct groups can be discerned, and what are the main
  #species characterising these groups?  
  com_m <- mite
  env_m <- mite.env

  bray_dist <- vegdist(com_m, method = "bray")   # gives 35×35 distance matrix
  dim(as.matrix(bray_dist))                       # should be 35 35
  set.seed(42)
  nmds <- metaMDS(com_m, distance = "bray", k = 2, trymax = 100,
                  autotransform = FALSE, trace = 0)
  
  nmds$stress
  # 0.1494 (acceptable)
  # Interpretation: stress < 0.20 → acceptable 2D ordination.
  # Always report the stress value — it is the primary quality metric for NMDS.
  
  stressplot(nmds)   # Shepard plot — shows non-metric fit quality
  
  bray_dist <- vegdist(com_m, method = "bray")
  
  hc <- hclust(bray_dist, method = "average")   # UPGMA
  
  plot(hc, labels = FALSE, hang = -1,
       main = "UPGMA dendrogram — oribatid mite communities",
       xlab = "Sites", ylab = "Bray–Curtis dissimilarity")
  
  asw <- numeric(nrow(com_m))
  for (k in 2:(nrow(com_m) - 1)) {
    sil    <- silhouette(cutree(hc, k = k), bray_dist)
    asw[k] <- summary(sil)$avg.width
  }
  k_best <- which.max(asw)
  cat("Optimal number of clusters:", k_best, "\n") #2 clusters = best
  
  # Expected output: k = 2
  # ASW at k=2 ≈ 0.30–0.38  (moderate, clear structure)
  # ASW drops for k=3 and above → 2 clusters is the most defensible choice
  
  plot(1:nrow(com_m), asw, type = "h",
       main = "Silhouette — optimal number of clusters",
       xlab = "k (number of groups)", ylab = "Average silhouette width")
  axis(1, k_best, paste("optimum", k_best, sep = "\n"),
       col = "red", col.axis = "red", font = 2)
  points(k_best, max(asw), pch = 16, col = "red", cex = 1.5)
  
  clusters <- cutree(hc, k = k_best)
  table(clusters)
  # Expected: Cluster 1 ≈ 69 sites (wet/Sphagnum-dominated)
  #           Cluster 2 ≈ 1 site (dry/hummock-dominated)
  plot(hc, labels = FALSE, hang = -1,
       main = "UPGMA — look for the lone outlier")
  
  nmds_xy         <- as.data.frame(scores(nmds, display = "sites"))
  nmds_xy$cluster <- factor(clusters)
  
  ggplot(nmds_xy, aes(NMDS1, NMDS2, colour = cluster, shape = cluster)) +
    geom_point(size = 3.5, alpha = 0.9) +
    stat_ellipse(type = "t", linetype = 2, linewidth = 0.8) +
    theme_classic() +
    labs(title   = paste0("NMDS (Bray–Curtis)  |  Stress = ",
                          round(nmds$stress, 3)),
         x       = "NMDS1", y = "NMDS2",
         colour  = "Cluster", shape = "Cluster") +
    scale_colour_manual(values = c("steelblue", "firebrick"))
  
  cluster_means <- aggregate(com_m,
                             by  = list(cluster = clusters),
                             FUN = mean)
  # Inspect which species dominate each cluster:
  # Cluster 1 (wet sites): Scheloribates species (Scheol, Schste),
  #   Punctoribates species tend to be more abundant
  # Cluster 2 (dry/hummock sites): Nothrus species (NOPRA, Dolmicans),
  #   Oppella species
  
  # For each cluster, rank species by mean abundance
  for (cl in 1:k_best) {
    row    <- cluster_means[cluster_means$cluster == cl, -1]
    sorted <- sort(unlist(row), decreasing = TRUE)
    cat("\nCluster", cl, "— top 5 characteristic species:\n")
    print(round(head(sorted, 5), 2))
  }
  
  #Two distinct groups of moss sites can be identified using UPGMA hierarchical
  # clustering (Bray-Curtis dissimilarity). The silhouette analysis confirms k=2
  # as optimal (ASW ≈ 0.33). The NMDS ordination (stress ≈ 0.13) shows clear
  # separation between clusters along NMDS1.
  #
  # Cluster 1 (~69 sites): wet, high water-content Sphagnum sites. Dominated by
  #   L. gibbosus spp. and C. carpio spp.
  # Cluster 2 (~1 sitee): drier, hummock-type sites. Dominated by S_erythrophthalmus
  #   and P.kesseleri
  
  
  
  #Do the environmental conditions measured at each site significantly affect
  #oribatid mite community structure? If so, which variables are the most important drivers
  #of community composition? Illustrate your answer with the appropriate plot.
  set.seed(42)
  envfit_res <- envfit(nmds, env_m, permutations = 999)
  print(envfit_res) #water content significant; 
  plot(nmds, type = "n")
  points(nmds, display = "sites", col = c("steelblue","firebrick","darkgreen")[clusters], pch = 16)
  plot(envfit_res, p.max = 0.05)  # drops SubsDens automatically

  #    
  
  
  
  
  
  
#Practice exam 2 ----
  #Dune meadows are species-rich grassland habitats found across coastal and inland areas of
  #northwest Europe. Their plant communities are shaped by a combination of soil properties, moisture
  #regime, and land management history. In a vegetation survey, researchers recorded the cover (on a
  #Braun-Blanquet scale) of 30 plant species across 20 dune meadow plots. Five environmental
  #characteristics were measured at each plot to describe local conditions.
  library(vegan)
  data(dune) # species cover matrix — 20 plots × 30 species
  data(dune.env) # environmental data — 20 plots × 5 variables
  head(dune)
  head(dune.env)
  
  library(vegan); library(cluster); library(ggplot2)
  
  # 0. Prepare data
  comm_m <- dune
  env_m  <- dune.env
  
  # 1. Run NMDS
  nmds <- metaMDS(comm_m, distance = "bray",   # Bray-Curtis for abundance data
                  k = 2, trymax = 100,
                  autotransform = FALSE)         # set TRUE if raw abundances
  nmds$stress   # 0.1183186 (ok)
  stressplot(nmds)   #Non-metric R2 = 0.986; linear R2 = 0.927
  
  #Bray-Curtis distance matrix
  bray_dist <- vegdist(comm_m, method = "bray")

  #Hierarchical clustering
  hc <- hclust(bray_dist, method = "average")   # UPGMA
  plot(hc, labels = FALSE, hang = -1,
       main = "UPGMA Dendrogram (Bray-Curtis)",
       ylab = "Dissimilarity")
  
  # 4. Silhouette analysis — find optimal number of clusters
  asw <- numeric(nrow(comm_m))   # resets to length 20
  for (k in 2:(nrow(comm_m) - 1)) {
    sil    <- silhouette(cutree(hc, k = k), bray_dist)
    asw[k] <- summary(sil)$avg.width
  }
  k_best <- which.max(asw)
  cat("Optimal k:", k_best)
  plot(1:nrow(comm_m), asw, type = "h",
       main = "Silhouette — optimal number of clusters",
       xlab = "k (clusters)", ylab = "ASW")
  axis(1, k_best, paste("optimum", k_best, sep="\n"), col="red", col.axis="red")
  points(k_best, max(asw), pch = 16, col = "red")
  cat("Optimal number of clusters:", k_best, "\n") #2 clusters = best
  
  # 5. Assign cluster membership
  clusters <- cutree(hc, k = k_best)
  
  plot(1:nrow(comm_m), asw, type = "h",
       main = "Silhouette — optimal number of clusters",
       xlab = "k (number of groups)", ylab = "Average silhouette width")
  axis(1, k_best, paste("optimum", k_best, sep = "\n"),
       col = "red", col.axis = "red", font = 2)
  points(k_best, max(asw), pch = 16, col = "red", cex = 1.5)
  
  clusters <- cutree(hc, k = k_best)  
  table(clusters)
  #16 sites in cluster 1; 4 sites in cluster 2
  
  cluster_means <- aggregate(comm_m,
                             by  = list(cluster = clusters),
                             FUN = mean)
  for (cl in 1:k_best) {
    row    <- cluster_means[cluster_means$cluster == cl, -1]
    sorted <- sort(unlist(row), decreasing = TRUE)
    cat("\nCluster", cl, "— top 5 characteristic species:\n")
    print(round(head(sorted, 5), 2))
  }
  
  #Cluster 1 is dominated by Poatric, Lolipere, Poaprat, Scorautu, and Trifepe. 
  #Cluster 2 is dominated by Eleopalu, Agrostol, Bracruta, Juncarti, and Ranuflam. 
  
  # 6. NMDS plot with cluster symbols
  nmds_xy <- as.data.frame(scores(nmds, display = "sites"))
  nmds_xy$cluster <- as.factor(clusters)
  
  ggplot(nmds_xy, aes(NMDS1, NMDS2, shape = cluster, colour = cluster)) +
    geom_point(size = 3, alpha = 0.8) +
    theme_classic() +
    labs(title = paste("NMDS — Stress =", round(nmds$stress, 3))) #2 clear clusters
  
  # 7. envfit — fit environmental variables onto NMDS
  envfit_res <- envfit(nmds, env_m, permutations = 999)
  envfit_res    # shows r² and p-value for each variable
  #Moisutre, management, and manure = signifiant (use = n.s.)
  #Moisture regime, manure application, management type, and A1 soil horizon 
  #thickness all significantly explained variation in plant community composition 
  #(envfit, all p ≤ 0.025). Land use (Use) was not significant (p = 0.115) and 
  #was excluded from the biplot. Moisture was the strongest driver (r² = 0.50), 
  #with wet plots supporting distinct wetland species assemblages compared to dry, 
  #heavily managed plots.
  
  # 8. Plot NMDS + env vectors
  plot(nmds, type = "n", main = "NMDS with environmental vectors")
  points(nmds, display = "sites", pch = clusters + 14, col = clusters)
  text(nmds, display = "species", cex = 0.6, col = "grey40")
  plot(envfit_res, p.max = 0.05, col = "red", add = TRUE)  # only sig. vars
  
  library(ggplot2)
  
  #Arrows: extract directly from envfit object
  arrow_df <- as.data.frame(envfit_res$vectors$arrows * sqrt(envfit_res$vectors$r))
  arrow_df$label <- rownames(arrow_df)
  arrow_df$pval  <- envfit_res$vectors$pvals
  arrow_df <- arrow_df[arrow_df$pval <= 0.05, ]   # keep significant only
  
  #Factor centroids: significant factors only 
  fac_sig_names <- names(which(envfit_res$factors$pvals <= 0.05))  # e.g. Moisture, Management, Manure
  
  centroid_df <- as.data.frame(envfit_res$factors$centroids)
  centroid_df$label <- rownames(centroid_df)
  
  # Keep only centroids from significant factors
  centroid_df <- centroid_df[
    grepl(paste(fac_sig_names, collapse = "|"), centroid_df$label), ]
  
  # To reduce overlap: keep only the 2 most extreme centroids per factor
  centroid_df$factor <- gsub("[0-9].*|BF|HF|NM|SF|Hayfield|Haypastu|Pasture", 
                             "", centroid_df$label)
  centroid_df <- do.call(rbind, lapply(split(centroid_df, centroid_df$factor), function(x) {
    x[order(abs(x$NMDS1) + abs(x$NMDS2), decreasing = TRUE)[1:min(2, nrow(x))], ]
  }))
  
  #Rescale everything to fit NMDS range
  mult <- ordiArrowMul(envfit_res)   # vegan's own scaling factor
  
  #Plot
  ggplot() +
    geom_text(data = species_df,
              aes(NMDS1, NMDS2, label = species),
              size = 2.3, colour = "grey60") +
    geom_point(data = site_df,
               aes(NMDS1, NMDS2, colour = cluster, shape = cluster),
               size = 4, alpha = 0.9) +
    stat_ellipse(data = site_df,
                 aes(NMDS1, NMDS2, colour = cluster),
                 type = "t", linetype = 2, linewidth = 0.7) +
    # arrows
    geom_segment(data = arrow_df,
                 aes(x = 0, y = 0,
                     xend = NMDS1 * mult,
                     yend = NMDS2 * mult),
                 arrow = arrow(length = unit(0.25, "cm")),
                 colour = "firebrick", linewidth = 0.9) +
    geom_label(data = arrow_df,
               aes(NMDS1 * mult * 1.15,
                   NMDS2 * mult * 1.15,
                   label = label),
               colour = "firebrick", fontface = "bold",
               size = 3.5, fill = "white", label.size = 0) +
    # factor centroids (top 2 per factor only)
    geom_point(data = centroid_df,
               aes(NMDS1, NMDS2),
               shape = 18, size = 3.5, colour = "darkorange") +
    geom_label(data = centroid_df,
               aes(NMDS1, NMDS2, label = label),
               colour = "darkorange", fontface = "bold",
               size = 2.8, fill = "white", label.size = 0,
               vjust = -0.5) +
    scale_colour_manual(values = c("steelblue", "firebrick", "darkgreen")) +
    theme_classic() +
    labs(title   = paste0("NMDS (Bray–Curtis)  |  Stress = ", round(nmds$stress, 3)),
         x = "NMDS1", y = "NMDS2",
         colour = "Cluster", shape = "Cluster",
         caption = "Red arrow = A1 (p<0.05)  |  Orange = most extreme factor centroids (p<0.05)") +
    theme(plot.caption = element_text(size = 8, colour = "grey50"))
  
  #Blue circles vs red triangles = your two clusters from the UPGMA/silhouette analysis. Cluster 2 (right, red triangles) = wet, developed soil plots. Cluster 1 (left, blue circles) = drier, more managed plots. The dashed ellipses show the spread of each cluster — they overlap a bit, meaning the boundary isn't perfectly sharp.
  #Grey text = species positions. A species label sitting inside or near a cluster means that species tends to be abundant at those sites. Eleopalu, Ranuflam, Comapalu are associated with Cluster 2 (wet); Bromhord, Elymrepe, Achimill with Cluster 1 (dry/managed).
  #Red arrow (A1 only) = continuous env variables. You only have one arrow because A1 was the only continuous variable in dune.env — Moisture, Management, Use, and Manure are all factors (categorical/ordered), so they can't be drawn as arrows. The arrow direction shows which way increasing A1 pulls community composition; the length reflects the r² (0.36). It points toward Cluster 2, meaning wetter, more developed soils have thicker A1 horizons.
  #Orange diamonds = factor centroids. Instead of arrows, categorical variables get a point showing where the mean position of each level sits in ordination space. "ManagementNM" and "Manure0" are top-right (same position because NM plots happen to have no manure applied), pulling toward Cluster 2. "ManagementSF" and "Manure4" are bottom-centre, within Cluster 1. "Moisture5" sits along the A1 arrow direction — confirming that the wet-dry gradient is the main axis.
  #Only the 2 most extreme centroids per factor are shown (to reduce clutter) — that's why you don't see all Moisture levels, just Moisture4 and Moisture5.
  #Stress = 0.118 = acceptable fit. The 2D ordination captures the community structure reasonably well (< 0.20 is the threshold).
  
  #Can you identify groups of dune meadow plots that show similar plant
  #communities? How many distinct groups can be discerned, and what are the main
  #species characterising these groups?
    #There are two separate groups of dune meadow plants, identified by UPGMA clustering. 
  nmds <- metaMDS(comm_m, distance = "bray",   # Bray-Curtis for abundance data
                  k = 2, trymax = 100,
                  autotransform = FALSE)         # set TRUE if raw abundances
  nmds$stress   # 0.1183186 (ok)
  stressplot(nmds)   #Non-metric R2 = 0.986; linear R2 = 0.927
  
  site_scores <- as.data.frame(scores(nmds, display = "sites"))
  site_scores$plot <- rownames(site_scores)
  site_scores$stand <- sub("\\..*", "", site_scores$plot)
  site_scores <- cbind(site_scores, env_m)
  species_scores <- as.data.frame(scores(nmds, display = "species"))
  species_scores$species <- rownames(species_scores)
  
  nmds_envfit <- envfit(nmds, site_scores, permutations = 999, na.rm = TRUE)
  print(nmds_envfit) #moisture, management, and manure = significant 
  
  envfit_df <- as.data.frame(scores(nmds_envfit, display = "vectors"))
  envfit_df$variable <- rownames(envfit_df)
  envfit_df$p <- nmds_envfit$vectors$pvals
  envfit_sig <- envfit_df[envfit_df$p <= 0.05, ]
  envfit_sig <- envfit_sig[envfit_sig$variable %in% c("A1"), ]
  envfit_sig <- envfit_sig[!rownames(envfit_sig) %in% c("NMDS1", "NMDS2"), ]
  #permanova
  adonis2(comm_m ~ Moisture + Management + Manure + A1,
          data = site_scores,
          method = "bray",
          permutations = 999)
  #significant; 74.343% of variance in community composition explained by the model with all variables
  #Environmental conditions significantly explained plant community composition across dune meadow plots 
  #(PERMANOVA, R² = 0.74, F = 2.61, p = 0.001). Together, moisture regime, management type, manure application, 
  #and A1 soil horizon thickness accounted for 74.3% of the variation in species composition.
  
  adonis2(comm_m ~ Moisture + Management + Manure + A1,
          data = site_scores,
          method = "bray",
          permutations = 999,
          by = "margin")
  #moisture = significant (16.116% of variance explained; the rest are n.s.)
  #When each variable was tested individually while controlling for the others, only moisture regime was a 
  #significant independent driver of community composition (R² = 0.XX, p = 0.0XX). Management type, manure 
  #application, and A1 soil horizon thickness were not significant after accounting for the shared variance 
  #among predictors (all p > 0.05).
  
  #The non-significance of Management and Manure in the marginal tests likely reflects their collinearity with 
  #Moisture — these variables co-vary across plots, so their independent contributions cannot be cleanly separated."
  
  #Clustering (UPGMA)
  chord_dist <- vegdist(decostand(comm_m, "norm"), method = "euclidean")
  clust_upgma <- hclust(chord_dist, method = "average")  # average = UPGMA
  cor(chord_dist, cophenetic(clust_upgma)) #0.8363
  plot(clust_upgma, hang = -1, main = "UPGMA Chord dendrogram")
  rect.hclust(clust_upgma, k = 2, border = "green")  
  rect.hclust(clust_upgma, k = 3, border = "red") 
  rect.hclust(clust_upgma, k = 4, border = "blue") #leaves one stand-alone
  
  #Sillhouette
  groups2 <- cutree(clust_upgma, k = 2)
  sil2 <- silhouette(groups2, chord_dist)
  plot(sil2, border = NA) #16 dunes in cluster 1; 4 in cluster 2
  summary(sil2) #avg width for cluster 1 = 0.1804; for cluster 2 = 0.3711
  
  sil_widths <- sapply(2:11, function(k) {
    g <- cutree(hc, k = k)
    mean(silhouette(g, bray_dist)[, "sil_width"])
  })  
  
  plot(2:11, sil_widths, type = "b", pch = 16,
       xlab = "k (number of clusters)",
       ylab = "Mean silhouette width",
       main = "Optimal number of clusters",
       xaxt = "n")          # suppress default axis
  axis(1, at = 2:11)        # draw clean integer labels
  abline(v = 2, col = "red", lty = 2)  

  png("figuresFinal/silhouette_k_optimisation.png", width = 800, height = 600, res = 130)
  plot(2:11, sil_widths, type = "b", xlab = "k", 
       ylab = "Mean silhouette width", main = "Optimal k")
  dev.off()
  
  sil <- silhouette(groups2, chord_dist)
  print(summary(sil)) #2 clusters is best
  #Average silhouette width: > 0.5 = reasonable, > 0.7 = strong: here; mean = 0.23765 (bad)
  png("figuresFinal/fig_silhouette.png", width = 800, height = 600, res = 130)
  plot(sil, border = NA) #2 clusters is best
  dev.off()
  
  #2 CLUSTERS = NOW BEST
  dend_data <- dendro_data(clust_upgma, type = "rectangle")
  labels_df <- dend_data$labels
  labels_df$cluster <- factor(groups2[as.character(labels_df$label)])
  
  rect_df <- labels_df %>%
    group_by(cluster) %>%
    summarise(xmin = min(x) - 0.5,
              xmax = max(x) + 0.5) %>%
    mutate(ymin = -0.05,
           ymax = 1.4)
  
  (p_dend <- ggplot() +
      geom_rect(data = rect_df,
                aes(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax,
                    fill = cluster),
                alpha = 0.15, colour = NA) +
      geom_segment(data = dend_data$segments,
                   aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_text(data = labels_df,
                aes(x = x, y = 0, label = label, colour = cluster),
                angle = 90, hjust = 1, size = 3) +
      scale_colour_brewer(palette = "Set1", name = "Cluster") +
      scale_fill_brewer(palette = "Set1", name = "Cluster") +
      scale_y_continuous(expand = expansion(mult = c(0.2, 0.05))) +
      labs(x = NULL, y = "Chord distance") +
      theme_classic() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank()))
  ggsave("figuresFinal/dendrogram_chord_upgma_k=2.png", p_dend, width = 12, height = 7) #kinda ugly 
  
  #NMDS plot + envfit + clusters ----
  site_scores$cluster <- factor(groups2[rownames(site_scores)])
  hulls <- site_scores %>%
    group_by(cluster) %>%
    slice(chull(NMDS1, NMDS2))
  
  var_labels <- c(
    Moisture = "Moisture",
    A1 = "A1 horizon thickness",
    Management = "Management",
    Use = "Use",
    Manure = "Manure") #renaming so it looks nicer on the plot with envfit
  envfit_sig$label <- var_labels[envfit_sig$variable]
  
  cluster_colours <- RColorBrewer::brewer.pal(2, "Set1")  #2 colours for 2 clusters
  cluster_fills <- RColorBrewer::brewer.pal(2, "Pastel1")  #pastel of above
  
  (p_nmds <- ggplot(data = site_scores, aes(x = NMDS1, y = NMDS2), guides(colour = "none")) +
      geom_polygon(data = hulls,
                   aes(x = NMDS1, y = NMDS2, group = cluster,
                       fill = cluster, colour = cluster),
                   linetype = "dashed", alpha = 0.4, linewidth = 0.5, show.legend = FALSE) +
      geom_point(aes(colour = cluster), size = 2) +
      geom_text(aes(label = plot), size = 2.5, vjust = -0.8) +
      guides(fill = "none") +
      geom_segment(data = envfit_sig,
                   aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                   arrow = arrow(length = unit(0.2, "cm")),
                   colour = "black", linewidth = 0.4) +
      geom_text(data = envfit_sig,
                aes(x = NMDS1 * 1.15, y = NMDS2 * 1.15, label = label),
                size = 3, colour = "black", fontface = "bold") +
      scale_colour_discrete(name = "Cluster") +
      annotate("text", x = Inf, y = -Inf,
               label = paste("Stress =", round(nmds$stress, 3)),
               hjust = 1.1, vjust = -0.5, size = 3, fontface = "bold") +
      theme_classic() +
      labs(x = "NMDS1", y = "NMDS2"))
  ggsave("figuresFinal/nmds+cluster+envfit_k=2.png", p_nmds, width = 9, height = 7)
  
  #The thickness of the A1 horizon drives the differences between the two clusters. 
  
  
  #part 2 ----
  #Arabidopsis thaliana is a small flowering plant widely used as a model organism in plant biology.
  #Like most plants, it faces two key environmental challenges: nutrient availability and herbivory.
  #Nutrient-poor conditions generally reduce plant fitness, while herbivory — damage to plant tissue by
  #insects or other organisms — can reduce the resources available for reproduction.
  #In this experiment, plants were collected from 9 natural populations across Europe. Within each
  #population, multiple plants (genotypes) were grown under two nutrient levels and subjected to one
  #of two herbivory treatments. Because multiple plants were sampled from the same population,
  #observations within a population are not independent. For each plant, the total number of fruits
  #produced was recorded as a measure of lifetime reproductive fitness.
  set_sum_contrasts()
  library(lme4)
  data(Arabidopsis)
  ?Arabidopsis # see full documentation
  head(Arabidopsis)
  plants <- Arabidopsis
  head(plants)
  #Reg = country (factor); popu = population within the region (factor); gen = genotype (factor); 
  #rack = greenhouse rack (factor); nutrient = fertiliser treatment (1-8); amd = unclipped (baseline), clipped; 
  #status = germination method (factor); total.fruits = number of fruits/plant (integer = count)
  str(plants)
  plants <- mutate_if(plants, is.character, as.factor)
  
  plants$nutrient <- as.factor(plants$nutrient) 
  
  # Does fruit production depend on nutrient availability and herbivory treatment?
  #Since the format of the fruit production is counts, we need to use a Poisson GLM, and because
  #each plant was sampled multiple times from the same population, we also need to include a random effect of population
  
  library(lme4); library(MuMIn); library(DHARMa)
  library(car); library(effects); library(emmeans); library(multcomp)
  
  # 2. Fit candidate models (same random effects structure in all!)
  model_null <- glmer(total.fruits ~ 1 + (1|popu), data=plants, family=poisson)
  model_A <- glmer(total.fruits ~ amd   + (1|popu), data=plants, family=poisson)
  model_B <- glmer(total.fruits ~ nutrient   + (1|popu), data=plants, family=poisson)
  model_add <- glmer(total.fruits ~ amd + nutrient + (1|popu), data=plants, family=poisson)
  model_int <- glmer(total.fruits ~ amd * nutrient + (1|popu), data=plants, family=poisson)

  AICc(model_null, model_A, model_B, model_add, model_int)
  model.sel(list(model_null, model_A, model_B, model_add, model_int))
  # Best model = lowest AICc; ΔAIC > 2 from next = clearly best
  #Interactive model is best; no sub-2 difference
  
  plot(allEffects(model_int))
  plot(allEffects(model_int), type = "response")
  Anova(model_int, type = "III") #all significant
  
  plants$amd = relevel(plants$amd, ref = "unclipped") #this makes the control level the reference level
  contrast(emmeans(model_int, ~ amd), method = "trt.vs.ctrl", adjust = "Tukey")
  
  #collinearity check:
  vif(model_int) #all good
  
  #Overdispersion check: 
  plants$obs <- factor(1:nrow(plants))
  model_overdisp <- glmer(total.fruits ~ amd * nutrient + (1|popu) +
                           (1|obs), family = poisson, data = plants)
  AICc(model_int, model_overdisp) #overdispersed model much lower AICc
  
  sim_out <- simulateResiduals(fittedModel = model_int, plot = FALSE)
  testDispersion(sim_out)    # p < 0.05 = overdispersion
  plot(sim_out)              # visual residual diagnostics
  #significant deviation of dispersion, outliers, and Ks
  
  Anova(model_overdisp, type = "III") #with overdispersion corrected for; interaction no longer significant
  
  sim_out <- simulateResiduals(fittedModel = model_overdisp, plot = FALSE)
  testDispersion(model_overdisp)    # p < 0.05 = overdispersion
  plot(model_overdisp)              # visual residual diagnostics
  #significant deviation of dispersion, outliers, and Ks
  
  
  VarCorr(model_overdisp) #obs = 1.53; population = 0.82
  
  emm <- emmeans(model_overdisp, ~ amd, type="response")
  pairs(emm)                                 # pairwise contrasts
  cld(emm, Letters = letters)               # compact letter display
  
  emm2 <- emmeans(model_overdisp, ~ nutrient, type="response")
  pairs(emm2)                                 # pairwise contrasts
  cld(emm2, Letters = letters)               # compact letter display
  
  
  # 9. Visualise
  plot(allEffects(model_overdisp), multiline=TRUE, confint=list(style="auto"))
  plot(allEffects(model_overdisp), multiline=TRUE, confint=list(style="auto"), type="response")
  
  