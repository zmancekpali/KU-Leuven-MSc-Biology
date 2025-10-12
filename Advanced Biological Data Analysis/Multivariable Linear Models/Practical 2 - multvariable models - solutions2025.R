###############################################
# EXERCISES PRACTICAL 2: MULTIVARIABLE MODELS #
###############################################

#set your own working directory!
dir <- "C:/Users/Piet/GDrive/Teaching/Advanced Biological Data Analysis/Aanpassingen 2024/Practical 2 - multivariable models/"
setwd(dir)

library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(afex)
library(MuMIn)
library(emmeans)
library(openxlsx)

#we use sum coding (=effects coding) by default from now on - you can just always put this on the top of the code
set_sum_contrasts()

#####################
# EXERCISE 1: SEEDS #
#####################

#     Use the dataset seedset.xls, which contains data for individual plants in the two 
#     populations of Den Bood (DB) and Zaterde Woede (ZW). The variables are their production 
#     of pollen (pollen), their height at flowering (height), the number of flowers 
#     they produced over the season (flowers), the average weight of the seeds
#     (seed.weight) and the number of seeds that were produced (seed.number).

#     Read the data and code the categorical predictors as factors if necessary.


seedset.data <- read.xlsx("seedset.xlsx", sheet=1)
head(seedset.data)
str(seedset.data)
seedset.data$population<-as.factor(seedset.data$population)
seedset.data$plant<-as.factor(seedset.data$plant)
str(seedset.data)

#    a.  Start with some visual inspection of the data. 
#        You can get a matrix of scatterplots by just using plot() and then providing the
#        columns of the dataset that you want to include (for example DATASET[,3:5] would include columns
#        3 to 5). Include variables flowers, seed.weight and seed.number
#        We are mostly interested in the relationships between seed number and the other variables.
#        Get a first impression of these relationships from the graphs.

plot(seedset.data[,6:8])

#    b.  Which predictor variable best explains the variance in the number of seeds? Construct separate
#        linear models to check the effects of flowers and seed weight on seed number. 
#        Are the slopes significantly different from zero? What are the adjusted R^2 values of each? 
#        What does this mean? Are the coefficients positive or negative? Can you explain why?

flower.lm <- lm(seed.number ~ flowers, data=seedset.data)
summary(flower.lm)              # significant positive effect, 48.7 % of variance explained

weight.lm <- lm(seed.number ~ seed.weight, data=seedset.data)
summary(weight.lm)              # significant negative effect, 30.6 % of variance explained

#    c.  Now construct a linear model to predict the number of seeds by the number of
#        flowers over the season (flowers) and seed weight (seed.weight). First fit an additive model
#        (no interactions). What proportion of the variance in seed number is explained by this model?
#        Also make effect plots of your model.

additive.lm <- lm(seed.number ~ flowers + seed.weight, data=seedset.data)
summary(additive.lm)                   # 71.4 % of variance explained
Anova(additive.lm, type="III") #Anova here not really necessary as 2 continuous predictors
plot(allEffects(additive.lm)) #visualise the effects of all predictors in the model
plot(effect("flowers",additive.lm)) #you can also visualise only one predictor of the model
plot(effect("flowers",additive.lm, residuals=TRUE), smooth.residuals=FALSE) # you can also plot the raw datapoints on it

#   d.  Now fit a model that also includes an interaction term between flowers and seed.weight. 
#       Also make an effect plot of the model. 

interaction.lm <- lm(seed.number ~ flowers * seed.weight, data=seedset.data)
summary(interaction.lm)                # 75.8 % of variance explained
Anova(interaction.lm, type="III")  #Anova here not really necessary as 2 continuous predictors
plot(allEffects(interaction.lm), multiline=T, confint=list(style="auto"))
plot(allEffects(interaction.lm))

#    e.  Which model is the best, based on their AICc scores? Is it one of the univariable models,
#        the additive model, or the interaction model? Based on the differences in AICc, are any
#        of the other models worth reporting?

AICc(flower.lm, weight.lm, additive.lm, interaction.lm)  # the interaction model has the lowest AICc.
#all other models have a delta AICc of at least 15 so not worth reporting

#   f.  Check the assumptions of your best model: linearity, homogeneity of variances,
#       normality of residuals, collinearity. Note the models with an interaction term often
#       suffer from collinearity because the main effects are correlated with the interaction
#       terms they appear in. You can avoid this by 'residual centering'. Instead of 
#       checking the colinearity of your fit, you can check the collinearity of 
#       residualCenter(FIT). This has the extra advantage that you can now also more clearly
#       interpret the main effects of your model as the parts of the main effects that are
#       not explained by your interaction. Also check for outliers

residualPlots(interaction.lm)           # linearity assumption is ok

spreadLevelPlot(interaction.lm)
ncvTest(interaction.lm)                 # homogeneity of variance assumption is ok

hist(rstudent(interaction.lm), probability=T, col="steelblue")
xfit = seq(-6,6,length=100)
yfit = dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)
shapiro.test(residuals(interaction.lm)) # normality of residuals assumption is ok

vif(interaction.lm)                     # collinearity assumption is strongly violated because of the interaction!!

fit.residcent <- residualCenter(interaction.lm)

vif(fit.residcent)     # can be avoided by using residual centering.
summary(fit.residcent)
summary(interaction.lm) #compared to the intitial interaction model, the main effect of seed.weight now has a significant effect
# in case of collineatirity, we should interpret the p-values of the model with residual centering
Anova(fit.residcent, type="III") #same as summary 

test=outlierTest(fit.residcent)
outl=as.numeric(names(which(test$bonf.p<0.05)));outl #no outliers


cd=cooks.distance(fit.residcent)
inflobs=which(cd>1);inflobs #no influential observations


#     g.  What are your general conclusions based on this analysis?

# The interpretation goes something like this: The number of flowers produced over the season is a strong
# determinant of how many seeds will be produced, and there is also a significant main effect of the seed weight on the number of seeds. However, there is a trade-off between the number 
# and weight of the seeds. Hence, you can see that the relationship between flower number and seed
# number is only apparent for light seeds and not for heavy ones.



####################
# EXERCISE 2: EELS #
####################

#    In a study by Maes et al. (2005), the authors investigated the relationship 
#    between bioaccumulation of heavy metals, genetic variation and fitness in the 
#    Atlantic eel (Anguilla anguilla). We want to study the influence of genetic variation 
#    (measured as the 'multilocus allozyme heterozygosity' on bioaccumulation of heavy metals
#    in three different river basins (Maas, IJzer, Schelde). 
#    We want to know whether heterozygous individuals can better "detoxify" themselves 
#    compared to homozygous individuals, and we are also interested in how this relationship 
#    might be different between the different river systems.

#    Use the variables HEAVY_METAL_ACCUM, MULTILOCUS_HETEROZYGOSITY_ALLOZYMES and
#    RIVER. Load the dataset (eel) and, check if everything looks OK and code
#    categorical variables as factors.


eel<-read.xlsx("eel2.xlsx")
head(eel)
str(eel) 
library(dplyr)
eel<-mutate_if(eel, is.character, as.factor)#the categorical variables have now been coded as factors!

#    a.  First and foremost, visually explore the data. Make some graphs that you think make sense 
#        to get a better idea of the relationships between the three variables that we're interested in.

#plot the data with different colors for each river
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

#    b.  Build a linear model only including the main effects of river and allozyme heterozygosity. Check
#        the Anova table to see if there are overall effects of your predictors and draw effect plots.
#       In case of an overall effect of river, check between which rivers specifically there are differences

fit1=lm(HEAVY_METAL_ACCUM~RIVER+MULTILOCUS_HETEROZYGOSITY_ALLOZYME,data=eel) 
summary(fit1) 
Anova(fit1, type="III") 
plot(allEffects(mod=fit1)) 
levels(eel$RIVER) #you can check the levels of the categorical variable

#check for pairwise differences between river:

contrast(emmeans(fit1, ~RIVER), method="pairwise",adjust="tukey")
# there are differences in heavy metal accumulation between IJZER and MAAS and between SCHELDE and MAAS,
# but not between IJZER and SCHELDE


#    c.  Now build a model that also includes the interaction between your predictors, again check
#        the Anova table and represent your model visually. Also check if there are any
#        differences in the effect of heterozygosity on heavy metal accumulation between the different rivers.

fit2=lm(HEAVY_METAL_ACCUM~RIVER*MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=eel)
summary(fit2) 
Anova(fit2, type="III") 
plot(allEffects(mod=fit2),multiline=T, ci.style="band") 

# check for differences in slopes:
contrast(emtrends(fit2, "RIVER", var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"), method="pairwise", adjust="tukey")
# there no pairwise differences in the effect of heterozygosity on heavy metal accumulation between any of the rivers

#    d.  Which model is the best? Check this with AICc. Are any other models worth
#        reporting?

AICc(fit1, fit2) #fit 1, WITHOUT the interaction term has lower AIC, so is the better model!

#The delta AICc of fit2 is not below 2 so no need to report on this model

#    e.  Test for outliers, influential observations, homogeneity of variance, linearity (of continous predictor) and normality of residuals.
#        Only do this for the best model.

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

#    f.  What are our conclusions based on our analysis?

# We conclude that heterozygosity negatively affects the heavy metal accumulation in minnows.
# We also detected overall differences in heavy metal concentration between the different 
# river systems, but there is no significant interaction. In other words, the effect of 
# heterozygosity on heavy metal accumulation is the same for all river systems.



#######################
# EXERCISE 3: MINNOWS #
#######################

#    For this dataset, the researchers measured the body length (in mm) of (male) minnows (Phoxinus 
#    laevis) depending on two (crossed) experimental conditions: The type of contaminant in 
#    the water (lead, manganese or chrome), and the type of stress induced (predator 
#    simulation, sound stress, control). For each combination of experimental conditions, 
#    there are two measurements. Load the minnow.csv dataset and check if everything 
#    was read OK. As always, code categorical predictors as factors.

minnow = read.xlsx("minnow.xlsx")
head(minnow)
str(minnow)
minnow$STRESS <- as.factor(minnow$STRESS)
minnow$CONTAMINATION <- as.factor(minnow$CONTAMINATION)

#   a.  As usual, start with some visual explorations.

par(mfrow=c(1,2))
plot(BODY.LENGTH ~ STRESS, data=minnow)
plot(BODY.LENGTH ~ CONTAMINATION, data=minnow)
par(mfrow=c(1,1))

#   b.  Now build a linear model with both predictors. Include only main effects.
#       Check if there is an overall effect of your variables and make effect plots. 

fit3 <- lm(BODY.LENGTH ~ STRESS+CONTAMINATION, data=minnow)
summary(fit3)
Anova(fit3, type="III")
plot(allEffects(fit3))
levels(minnow$CONTAMINATION)

#   c.  Build a linear model that includes the interaction as well.
#       Again check the Anova table and make effect plots. 

fit4 <- lm(BODY.LENGTH ~STRESS+CONTAMINATION+STRESS:CONTAMINATION, data=minnow)
fit4 <- lm(BODY.LENGTH ~STRESS*CONTAMINATION, data=minnow) #same model coded in a shorter way

Anova(fit4, type="III")
plot(allEffects(fit4))
#you can make the visualisation different if you switch position of the predictors (is still the same fit)
fit4b <- lm(BODY.LENGTH ~CONTAMINATION*STRESS, data=minnow)
plot(allEffects(fit4b))

#   d.  Which model is best? Base this on AICc. Do any other models warrant reporting?

AICc(fit3, fit4, fit4b)
#the model that includes interactions is better!
#the AICcs are about 7 apart, so no need to report on any other models

#   e.  Do some posthoc analysis. Specifically, check if there are differences between
#       the contamination treatments within each stress treatment.

contrast(emmeans(fit4, ~CONTAMINATION|STRESS), method="pairwise",adjust="tukey")
#in the control and predation treatments, we see a difference in body length between
#all pairwise combinations of the contamination treatments. However, in the sound
#stress treatment, we do not see a difference between the chrome and lead treatments
#(but we do see differences between the other pairs of contamination treatments).

contrast(emmeans(fit4, ~STRESS|CONTAMINATION), method="pairwise",adjust="tukey")
#we can also test pairwise the other way around: is there difference between stress treatment within each contamination

#   f.  Test assumptions for the best model

# Homogeneity of Variances
ncvTest(fit4)

# Normality: 
hist(rstudent(fit4), probability=T, col="lightgrey", xlim=c(-6,6), ylim=c(0,0.5),breaks=6,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2) 
#Shapiro-Wilk test:
shapiro.test(residuals(fit4))

# Outliers & influential observations
outlierTest(fit4)
outl=as.numeric(names(which(outlierTest(fit4)$bonf.p<0.05)))
outl # empty, so no outliers here
influenceIndexPlot(fit4,vars=c("Studentized","Bonf"))
cd=cooks.distance(fit4)
inflobs=which(cd>1) 
inflobs # empty, so no outliers here
influenceIndexPlot(fit4,vars="Cook")

#   g.  What are our conclusions based on this analysis?

# We conclude that contamination type and type of stress have significant effects 
# on the adult body length in minnows. On top of that, there is a significant interaction: the effects
# of contamination treatment is different between the stress treatments (or the other
# way around). Visually, it seems like there is a negative synergy: if there is already
# a strong negative effect of one of the treatments (especially STRESS), then the negative effect of the
# other treatment (contamination) is smaller.


###################################################################################################################


