##%#########################################################################%##
#                                                                             #
#             Remedying assumption violations - Zoja Manček Páli              #
#                              Date: 21.10.2025                               #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Remedying Assumptions")
getwd()

#Packages
remotes::install_version("car", version = "3.1-2", repos = "https://cloud.r-project.org")

library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(MuMIn)
library(afex)
library(emmeans)
library(dplyr)
library(nlme)
library(robustbase)
library(openxlsx)

set_sum_contrasts()

# EXERCISE 1: BLOOD GLUCOSE ----
#Glycemic control is a crucial aspect of managing diabetes and prediabetes, as 
#it reflects the average blood glucose levels over time and helps in preventing 
#long-term complications. This study investigates the effects of physical activity 
#and dietary carbohydrate intake on glycemic control, as measured by blood glucose
#levels, in individuals with varying levels of exercise and carbohydrate consumption. 
#The researchers were specifically interested in how carbohydrate consumption
#affects the benefits of exercise for lowering blood glucose.

#a. Read the data (blood_glucose), and start with the usual checks and visual explorations. 

blood <- read_xlsx('blood_glucose.xlsx')
head(blood)

blood <- blood %>% rename(exercise = weekly_exercise, 
                          carbs = carb_consumption, 
                          glucose = blood_glucose)

plot(glucose ~ carbs, data = blood)
boxplot(glucose ~ exercise, data = blood)

#b. Since the researchers were specifically interested in the interaction effect
#between weekly exercise and carbohydrate consumption, run a full factorial
#model to investigate this. Also provide a visualisation of the model.
#Based on this model, does the effect of weekly exercise on blood glucose 
#depend on the level of carbohydrate consumption?

model1 <- lm(glucose ~ carbs*exercise, data = blood)
plot(allEffects(model1), multiline = T)
summary(model1) #only carbs have an effect on blood glucose

#c. Check for heterogeneity of variances, normality of residuals, linearity, collinearity, 
#outliers and influential observations. Can we trust our conclusions?

shapiro.test(residuals(model1)) #no deviation from normality
ncvTest(model1) #no deviation from homogeneous variances
residualPlots(model1)  #no strong deviation from linearity
vif(model1) #we have clear collinearity! Let's residual center to address this:

#need to center:
model1c <- residualCenter(model1)

vif(model1c) #all about 1
summary(model1c) #now exercise also has an effect
#in case of collineatirity, we should interpret the p-values of the model with residual centering
Anova(model1c, type="III") #carbs and exercise have an effect, no interaction

#outliers:
outlierTest(model1c) #point n23 is an outlier with a p-value < 0.05
influenceIndexPlot(model1c, vars = c("Studentized","Bonf"))  #one significant outlier

cd <- cooks.distance(model1c)
(inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)

influenceIndexPlot(model1c, vars = c("Cook")) #point 23 has cd > 1


#d.Attempt to remedy any assumption violations. Again provide a model 
#visualization. Do your conclusions change? Carefully formulate your conclusions.
fit2 <- lmrob(blood_glucose ~ weekly_exercise * carb_consumption, data = d)
vif(fit2) #since we had high variance inflation factors before, we check it, and they are high
fit2b <- residualCenter(fit2) #remedy this by residual centering
vif(fit2b) #fixed

summary(fit2b)
Anova(fit2b, type="III")

model2 <- lmrob(glucose ~ exercise * carbs, data = blood)
vif(model2)
model2b <- residualCenter(model2)
vif(model2b) #all good
Anova(model2b, type="III")
summary(model2b) #all significant now

plot(allEffects(model2), multiline = T, confint = list(style = 'auto')) #now this is very different
# we now conclude that there is a strongly significant interaction. For people with
# a diet low in carbohydrates, weekly exercise has essentially no effect on their
# blood glucose. However, the higher the consumption of carbohydrates, the more 
# exercise helps to reduce blood glucose.

# EXERCISE 2: YARROW DEFENSE # ----
#We have a dataset on the use of different types of soil management techniques 
#and their effect on the concentration of phenolics (a plant defense compound)
#in yarrow (Achillea millefolium), a common herb. The study was carried out in 
#16 plots of 5x5 meters which were randomly assigned to one of four treatments: 
#1) surrounded by a fence exclosure to exclude herbivores; 
#2) fertilized with N-P-K fertilizer; 
#3) fenced AND fertilized; and 
#4) untreated control. 
#Then each plot was divided in two: one half was treated for the first
#ten years and then left to revert to the untreated state, while the other half
#was treated for the full 20 years of the experiment.

#a. Read the data (soilmanagement), and start with the usual checks and visual 
#explorations. Think carefully which variables should be coded as factors.

d = read.xlsx("soilmanagement.xlsx")
head(d)
str(d)

#we need to code plot, treatment and duration as factors! 
d$plot <- factor(d$plot)
d$treatment <- factor(d$treatment)
d$duration <- factor(d$duration)

par(mfrow=c(1,3))
plot(phenolics_conc ~ plot, data=d)
plot(phenolics_conc ~ treatment, data=d)
plot(phenolics_conc ~ duration, data=d)
par(mfrow=c(1,1))


#b. Run a linear  mixed-effects model to predict the effect of treatment, duration 
#and their interaction on phenolics concentration. What should be the random
#factor in this model? Visualize the model. 

#the random factor should be 'plot'. This is because we are not specifically interested
#in which plots lead to more phenolics - it is just a factor in the data that groups
#some data points together, and the levels of this factor (the plots) have been randomly drawn
#from a large number of possible plots. 
fit <- lmer(phenolics_conc ~ treatment * duration + (1|plot), data = d)
summary(fit)
Anova(fit, type="III")
plot(allEffects(fit))
plot(allEffects(fit), multiline=TRUE)
#to be completely correct, we should test an additive model and one with 
#interaction and check which is best with AICc


#c. Do some posthoc comparisons based on your model. Specifically, compare all
#treatments against the control for the same duration. To do this, first relevel 
#the treatment variable so that the reference level is the control treatment.
#Then run the model again (otherwise you will get nonsensical results).       
#Then do posthoc comparisons between the treatments using method 'trt.vs.ctrl'
#(see lecture 4) within each duration (see lecture 2). What do you conclude?

levels(d$treatment)
d$treatment = relevel(d$treatment, ref="control")
fit <- lmer(phenolics_conc ~ treatment * duration + (1|plot), data = d)
Anova(fit, type="III") ##stays the same, as it is the same fit
contrast(emmeans(fit, ~ treatment|duration), method='trt.vs.ctrl', adjust='Tukey')
plot(allEffects(fit), multiline=TRUE)
#We can see that the fertilizer treatment leads to lower phenolic concentrations for
#both durations, while we have no evidence that this is the case for the exclosure treatment
#(also for both durations). For the 'both' treatment, we find that the phenolics contentration
#is significantly lower than the control for the permanent duration, but have no evidence
#that this is also the case for the reverse duration.


#d. You cannot test for homogeneity of variances using our standard methods, 
#because we ran a mixed model. But we could run a mixed model that allows
#the variances between the treatments and durations (and both) to be different, 
#and then compare these models with our first model based on AICc. We have to do 
#this with the function lme. First reconstruct the model you made in (b) with 
#lme and assure yourself that it produces the same outcome as the model you 
#made under b. Then run three extra models: one that allows for different variances
#between treatments, one that allows for different variances between durations,
#and one that allows for different variances for each combination of treatment
#and duration (use 'form=~1|treatment*duration'). 

#Which model is the best? Do your conclusions change?
#Visualize the best model if you have not yet done so.

fit2 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, data = d)
?lme
summary(fit2)
Anova(fit2, type="III") 

AICc(fit, fit2) #it's indeed the same model as the original

fit3 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|treatment), data = d)
fit4 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|duration), data = d)
fit5 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|treatment*duration), data = d)

AICc(fit2, fit3, fit4, fit5) #our original fit has the best AICc, so we keep that one!


#e. Check assumptions: normality of residuals, outliers/influential observations.

shapiro.test(residuals(fit)) # no deviation from normality
outlierTest(fit) #no outliers (note that this test works for an lmer but not for a lme model)
max(cooks.distance(fit)) #no points with Cook's distance over 1
#NOTE: if a model with custom variances would be best we should have used lme to make the fit. testing outliers/influential observations
#with lme is not straightforward and not part of this course; So now we are just lucky we can use the original model
#which we can fit with lmer


#f.  What are your conclusions based on this model?
#We conclude that the effect of soil management treatment on phenolics in some cases depends on 
#the duration these treatments are applied for. The application of fertilizer (but no fencing)
#leads to lower phenolics compared to the control treatment, irrespective of the duration treatment.
#In contrast, we have no evidence that the application of exclosure (but no fertilizer) leads to 
#differences, again irrespective of the duration treatment. However, if BOTH are applied,
#then the duration matters: we see an effect if they are applied permanently, but not
#if they are only applied for 10 years and then let to reverse to their untreated state.

# EXERCISE 3: HYPERTENSION ----
#We have a dataset about blood pressure of 16 hypertension patients who have either been
#treated with a medicine or a placebo (control). Their blood pressure was measured
#every week for a total of 10 weeks.
#We would like to know if the medicine leads to a stronger decrease over time
#in blood pressure than the placebo.

#a.  Start with reading the data, checking everything and visually exploring.

d3 <- read.xlsx("bloodpressure.xlsx")
head(d3)
str(d3)
d3$subject <- as.factor(d3$subject)
d3$treatment <- as.factor(d3$treatment)

plot(bloodpressure ~ treatment, data = d3)
plot(bloodpressure ~ week, data = d3, col = d3$subject)

#b. Construct a linear mixed model to predict the effects of treatment and 
#time on blood pressure. Also include their interaction.
#Include the appropriate random effects. Visualize the model.

fit8 <- lmer(bloodpressure~treatment*week + (1|subject), data=d3)
summary(fit8)
Anova(fit8, type="III")

#looks like there is no difference in treatment alone, but the change over time
#is different between the medicine and the placebo
plot(allEffects(fit8), multiline=T, confint=list(style="auto"))
#the effects plot shows that the slope for the medicine is more downward than
#for the placebo - based on this, we would conclude that the medicine is effective.

#c. Because our measurements have been done over time, we might have 
#temporal autocorrelations in the data. 
#First check if there indeed is temporal autocorrelation in the previous model.
#Then fit a new model where you account for these temporal 
#autocorrelations and check if it is better.
#If so, visualize it. Do your conclusions change?

#autocorrelation plot
acf(residuals(fit8)) #we can see that for short lags there is a positive correlation

#first run lme WITHOUT the autocorrelation structure (should give the same as above)
fit9 <-lme(bloodpressure~treatment*week, random=~1|subject, data=d3)
summary(fit9)
Anova(fit9, type="III")
#and here we add temporal autocorrelation structure
fit10 <-lme(bloodpressure~treatment*week, random=~1|subject, correlation=corAR1(form=~week), data=d3)
summary(fit10)
Anova (fit10, type="III")
AICc(fit8, fit9, fit10) #the model that accounts for autocorrelation is much better!

plot(allEffects(fit10), multiline=T, confint=list(style="auto"))
#after accounting for temporal autocorrelation, we no longer have support that 
#bloodpressure declines faster in the patients who got the medicine than in 
#those who got the placebo

#d. we cannot test for homogeneity of variances with mixed models, but we could run a mixed model that allows
#the variances between the treatments to be different, 
#and then compare these models with our first model based on AICc.

fit11 <-lme(bloodpressure~treatment*week, random=~1|subject, weight=varIdent(form=~1|treatment), correlation=corAR1(form=~week), data=d3)
summary(fit11)
Anova (fit11, type="III")
AICc(fit8, fit9, fit10, fit11) #
# this is slightly worse than the previous one, so we keep the model without custom variances. 
#The difference in AICc is small (<2) so we can report this model, though our conclusions do not change

fit12 <-lme(bloodpressure~treatment*week, random=~1|subject, weight=varIdent(form=~1|treatment), data=d3)
AICc(fit8, fit9, fit10, fit11, fit12)
#the model with custom variances but not accounting for temporal autocorrelation, is clearly worse


#e. Check normality of residuals and collinearity on the best fit.
#Note: checking outliers, influential observations and linearity
#requires some different techniques than for lm/glm.
#We don't go into it in this course.

shapiro.test(residuals(fit10)) #no deviation (W>0.9, don't worry about the p-value)
vif(fit10) #no collinearity

#f.  What are your conclusions?

#we have no reliable support that the medicine is effective - bloodpressure does not
#change significantly differently over time in the patients that got the medicine vs
#the patients that got the placebo


