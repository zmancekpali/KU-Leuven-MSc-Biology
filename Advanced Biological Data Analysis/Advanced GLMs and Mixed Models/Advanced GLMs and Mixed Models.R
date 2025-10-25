##%#########################################################################%##
#                                                                             #
#         Advanced GLMs and Mixed Models Practical - Zoja Manček Páli         #
#                              Date: 14.10.2025                               #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Advanced GLMs and Mixed Models")
getwd()

#packages
library(afex) #also loads lme4
library(scales)
library(rockchalk)
library(emmeans)
library(nlme)
library(nnet)
library(readxl)
library(effects)
library(ggplot2)
library(car)
library(export)
library(MuMIn)
library(reshape2)

#Lecture ----
df <- read.csv("red_fox.csv")

# Histograms for each Prey Selection category, split by Vegetation Cover
ggplot(df, aes(x = Prox_Human_Activity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_grid(Diet ~ Veg_Cover) +
  labs(title = "Histograms of Proximity to Human Activity by Prey Selection and Vegetation Cover",
       x = "Proximity to Human Activity (km)",
       y = "Count") +
  theme_minimal()

# Fit the multinomial logistic regression model
# We need to ensure the outcome variable is a factor
df$Diet <- as.factor(df$Diet)

# Fit the model
fit <- multinom(Diet ~ Prox_Human_Activity + Veg_Cover, data = df)

# Summary of the model
summary(fit)
Anova(fit, type="III")

plot(allEffects(fit), multiline=TRUE, confint=list(style="auto"))


graph2ppt(file="effectplot_additive")

# Fit the model
fit2 <- multinom(Diet ~ Prox_Human_Activity * Veg_Cover, data = df)

# Summary of the model
summary(fit2)
Anova(fit2, type="III")

plot(allEffects(fit2), multiline=TRUE, confint=list(style="auto"))

AICc(fit, fit2)

#extract model effects for predictor 'Prox_Human_Activity'
#only extract the first four columns that have the actual probabilities
proximity_effect <- as.data.frame(Effect("Prox_Human_Activity", fit))[1:4]
#use 'melt' to turn the table from wide to long format
proximity_effect <- melt(proximity_effect, id.vars = "Prox_Human_Activity", variable.name = "Diet", value.name = "Probability")

#make a stacked effects plot
ggplot(proximity_effect, aes(x = Prox_Human_Activity, y = Probability, fill = Diet)) +
  geom_area() +
  labs(x = "Proximity to Human Activity (km)",
       y = "Probability") +
  theme_minimal()

#extract model effects for predictor 'Veg_Cover'
#only extract the first four columns that have the actual probabilities
veg_cover_effect <- as.data.frame(Effect("Veg_Cover", fit))[,1:4]
#use 'melt' to turn the table from wide to long format
veg_cover_effect <- melt(veg_cover_effect, id.vars = "Veg_Cover", variable.name = "Diet", value.name = "Probability")

#make a stacked effects plot
ggplot(veg_cover_effect, aes(x = Veg_Cover, y = Probability, fill = Diet)) +
  geom_bar(stat = "identity", position = 'stack') +
  labs(x = "Vegetation Cover",
       y = "Probability") +
  theme_minimal()


library(MASS)

df <- read.csv("butterflies.csv")
head(df)

df$Dev_Stage <- factor(df$Dev_Stage, levels = c("Egg", "Larva", "Pupa", "Adult"), ordered = TRUE)

# Fit the ordinal logistic regression model
fit3 <- polr(Dev_Stage ~ Temp + Host_Species + Week, data = df, Hess = TRUE)
Anova(fit3, type="III")

Temp_effect <- as.data.frame(Effect("Temp", fit3, xlevels=list(Temp=seq(min(df$Temp), max(df$Temp), length.out=100))))[,1:5]
#use 'melt' to turn the table from wide to long format
Temp_effect <- melt(Temp_effect, id.vars = "Temp", variable.name = "Dev_Stage", value.name = "Probability")

#make a stacked effects plot
ggplot(Temp_effect, aes(x = Temp, y = Probability, fill = Dev_Stage)) +
  geom_area() +
  labs(x = "Average Temperature",
       y = "Probability") +
  theme_minimal()

Host_effect <- as.data.frame(Effect("Host_Species", fit3))[,1:5]
#use 'melt' to turn the table from wide to long format
Host_effect <- melt(Host_effect, id.vars = "Host_Species", variable.name = "Dev_Stage", value.name = "Probability")

#make a stacked effects plot
ggplot(Host_effect, aes(x = Host_Species, y = Probability, fill = Dev_Stage)) +
  geom_bar(stat = "identity", position = 'stack') +
  labs(x = "Host_Species",
       y = "Probability") +
  theme_minimal()

Time_effect <- as.data.frame(Effect("Week", fit3, xlevels=list(Week=seq(min(df$Week), max(df$Week), length.out=100))))[,1:5]
#use 'melt' to turn the table from wide to long format
Time_effect <- melt(Time_effect, id.vars = "Week", variable.name = "Dev_Stage", value.name = "Probability")

#make a stacked effects plot
ggplot(Time_effect, aes(x = Week, y = Probability, fill = Dev_Stage)) +
  geom_area() +
  labs(x = "Time",
       y = "Probability") +
  theme_minimal()

#
set_sum_contrasts()

d = read.csv("fitness.csv")
head(d)
str(d)
#we now have to code FITNESS and TEST as factors, but also ID! Even though ID
#is in numbers, they represent different individuals so they should be coded as
#a factor
d$TEST <- as.factor(d$TEST)
d$FITNESS <- as.factor(d$FITNESS)
d$ID <- as.factor(d$ID)

#SLIDE 9: first try a 'regular' linear model...
fit = lm(PULSE ~ FITNESS + TEST, data=d)
Anova(fit, type="III")

#SLIDE 10: posthoc comparisons
contrast(emmeans(fit,~FITNESS), method="pairwise", adjust="Tukey")

#SLIDE 12: fit a mixed model
fit2 = lmer(PULSE ~ FITNESS + TEST + (1|ID), data=d)

#SLIDE 13:
Anova(fit2, type="III")

#SLIDE 14:
summary(fit2)

#SLIDE 15:
contrast(emmeans(fit2,~FITNESS), method="pairwise", adjust="Tukey")

#SLIDE 16:
AICc(fit, fit2)

#SLIDE 17: effect plots
plot(allEffects(fit2))

shapiro.test(residuals(fit2))
outlierTest(fit2)
max(cooks.distance(fit2))

# SLIDE 22: load data 'schools'
d2 <- read_xlsx("schools.xlsx")
head(d2)
str(d2)
#code class and school as factors (will be included as random factors)
d2$school <- as.factor(d2$school)
d2$class <- as.factor(d2$class)

#SLIDE 23: raw data plot
plot(test_result ~ breakfast_cal, data=d2, col=school, pch=c(0,4,19)[class])

#SLIDE 24: fit a mixed model with nested random effects:
fit3 <- lmer(test_result ~ breakfast_cal + (1|school/class), data=d2)

#SLIDE 25:
summary(fit3)

#SLIDE 26:
plot(allEffects(fit3))

#SLIDE 29: load queen pheromone data in both formats, but we will use the long
#dataset so we just overwrite the wide one
d3 <- read.csv("queenpheromone_long.csv")
head(d3)
str(d3)

#code 'colony' and 'treatment' as factors
d3$colony <- as.factor(d3$colony)
d3$treatment <- as.factor(d3$treatment)

#SLIDE 31: run a generalized linear mixed model
fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data = d3)

#SLIDE 32:
summary(fit3)

#SLIDE 33:
Anova(fit3, type="III")

#SLIDE 34:
plot(allEffects(fit3))

#SLIDE 36:
plot(allEffects(fit3), type="response")

#SLIDE 37:
d3$treatment = relevel(d3$treatment, ref="control")
#we have to run the fit again after releveling, otherwise the labels for the differen
#factor levels get mixed up
fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data=d3)

#SLIDE 38: posthoc comparisons
contrast(emmeans(fit3,~treatment), method="trt.vs.ctrl", adjust="Tukey")

#SLIDE 39: model selection
fit3 <- glmer(ovarydev~treatment+colonysize+(1|colony),family=binomial,data=d3)
fit4 <- glm (ovarydev~treatment+colonysize,family=binomial,data=d3)
AICc(fit3, fit4)

#SLIDE 42: create observation level factor variable
d3$obs <- factor(1:nrow(d3))

#SLIDE 43: run model that includes this variable to check for overdispersion
fit5 <- glmer(ovarydev~treatment+colonysize+(1|colony)+(1|obs),family=binomial,data=d3)
AICc(fit3, fit5) #original model is better, no overdispersion

#SLIDE 47: read data
darwin <- read.csv("darwin.csv")
head(darwin)

#SLIDE 48: original linear fit:
fit6 <- lm(height~type, data=darwin)
summary(fit6)
ncvTest(fit6) #unequal variances!

#SLIDE 49: new fit allowing for different variances between levels of 'type'
fit7 <- gls(height~type, data=darwin, weight=varIdent(form=~1|type))

#SLIDE 50
summary(fit7)

#SLIDE 51
plot(allEffects(fit7))

#SLIDE 52
#we cannot directly compare AICc of gls and lm models...
#but we can first rewrite the lm as a gls model and THEN compare the two

fit6b <- gls(height~type, data=darwin)
AICc(fit6, fit6b) #fit6 and fit6b are the same models in lm and gls, but have different
#AICc, even if they have the same estimates:
coef(fit6)
coef(fit6b)

#so if we want to compare both models (with and without allowing non-equal variances),
#we should compare both gls models: fit6b and fit7
AICc(fit6b, fit7)

#SLIDE 56
d4 <- read.csv("hawaiibirds.csv")
str(d4)
#we will only work with a subset of the data for this example
d5<-subset(d4, d4$Birds!="NA" & d4$Species==3 & d4$Island==1)
head(d5)
#no need to recode - we will only use 'Year' and 'Birds'

#SLIDE 57: raw data plot
plot(d5$Birds~d5$Year, type=c("l"))

#SLIDE 58
fit8 <- lm(Birds~Year, data=d5)
summary(fit8)

plot(fit8)

#SLIDE 60
acf(residuals(fit8))

#SLIDE 61
fit9 <- gls(Birds~Year, data=d5, correlation = corAR1(form=~Year))

#SLIDE 62
summary(fit9)

#SLIDE 63: effect plots - just made sure here that they are both on the same
#scale (y-axis between -100 and 300)
plot(allEffects(fit8, residuals=TRUE), smooth.residuals = FALSE, ylim=c(-100,300))
plot(allEffects(fit9, residuals=TRUE), smooth.residuals = FALSE, ylim=c(-100,300))

#SLIDE 64
#as before, to compare the gls model with the model that does not have a specific
#correlation structure, we need to code the first model (the lm) as gls
fit8b <- gls(Birds~Year, data=d5)
AICc(fit8b, fit9)

#Practical ----
library(car)
library(dplyr)
library(nnet)
library(effects)
library(reshape2)
library(ggplot2)
library(MuMIn)
library(lme4)
library(dplyr)
library(emmeans)
library(afex)
library(openxlsx)

set_sum_contrasts()

#EXERCISE 1: SQUIRRELS ----
#Red squirrels (Sciurus vulgaris) are known to host a species of parasitic nematode 
#(Strongyloides robustus), which can influence their health outcomes. Researchers 
#have observed that the severity of infection—measured by parasite load—can lead 
#to different health trajectories: recovery, chronic illness, or mortality. Additionally, 
#the impact of parasite load may vary depending on the age of the squirrels. In this study, 
#data has been collected on the age (in months), parasite load, and health outcomes 
#of a population of red squirrels. 

#a.Start with some visual inspection of the data. 
squirrels <- read_xlsx("Squirrels.xlsx")
head(squirrels)
str(squirrels)

squirrels <- squirrels %>% rename(age = Age, 
                                  parasites = Parasite_load, 
                                  outcome = Outcome)

plot(parasites ~ age, data = squirrels)
boxplot(parasites ~ outcome, data = squirrels)
boxplot(age ~ outcome, data = squirrels)

#b. Fit multinomial models to investigate the influence of age and parasite load on disease outcomes.
#Fit a model with and without an interaction and pick the best one based on AICc.
#NOTE: in including your predictor variables, list 'Age' first. Otherwise the effect plot is hard to interpret.
#Which variables have a significant effect?
fit1 <- multinom(outcome ~ age + parasites, data = squirrels)
fit2 <- multinom(outcome ~ age * parasites, data = squirrels)
AICc(fit1, fit2) #fit with interaction is much better
summary(fit2)
Anova(fit2, type = "III")



#c.Visualize the model with a regular effects plot. 
#Interpret the resulting graph carefully. What do you conclude?

plot(allEffects(fit2), multiline=TRUE, confint=list(style="auto"))

#dOPTIONAL: The effect plot is not an ideal visualization.
#As an alternative, try to produce 'stacked' plots with ggplot.
#For inspiration, you can check the R code from the lecture (but don't copy this one on one, it won't work!)
# To get you started, the following code creates a dataframe with values of age and parasite load. 

# create a sequence of parasite loads from 10 to 150 in 100 steps
parasite_loads <- seq(10, 150, length.out = 100)
# create a sequence of 3 ages: 10, 30 and 50
ages <- c(10, 30, 50)
# create a dataframe that has all combinations of ages and parasite loads
new_data <- expand.grid(parasites = parasite_loads, age = ages)

#You will still have to generate model predictions for these predictor values (using the 'predict' function) and then
#make a dataframe that includes both these predictions and the corresponding predictor values. Then, you have to
#use the 'melt' function to transform these predictions into 'long format' (see lecture code for an example).
#From that dataframe, you can then make the plots with ggplot. If you get stuck, google it or ask ChatGPT!

#generate model predictions for all these combinations
predicted_probs <- predict(fit2, newdata = new_data, type = "probs")
#append the new_data dataframe so that the predictions and their associated predictor values are all in one dataframe
predicted_df <- cbind(new_data, predicted_probs)
#transform the data to long format
predicted_long <- melt(predicted_df, id.vars = c("parasites", "age"), variable.name = "outcome", value.name = "Probability")

ggplot(predicted_long, aes(x = parasites, y = Probability, fill = outcome)) +
  geom_area() +
  facet_wrap(~ age, ncol = 1) +
  labs(x = "Parasite Load",
       y = "Probability") + 
  theme_classic()
#Testing for assumptions for multinomial models is not straightforward
#so we will not do this for multinomial/ordinal models in this course


# EXERCISE 2: BABBLERS # ----

#In this study, the researchers were interested in the cognitive performance in these birds. They were 
#interested in exploring whether cognitive performance varies between birds of different sex and age.
#They subjected 38 birds from 11 different social groups to three different cognitive tasks: an 
#associative learning task (“AL” in the dataset), and inhibitory control task (“IC”), and a reversal 
#learning task (“RL”). Next, they performed a principal component analysis on the scores on these three 
#tasks, from which they extracted the first principal component (“PC1”). The opposite of this first 
#principal component was then interpreted as a measure of “general cognitive performance” (“GCP”). Every
#individual was studied across all three tasks exactly once and has a single measure of general cognitive
#performance.


#a. Load the data, check if everything is ok and explore the data visually. Exclude birds with unknown sex
#SEX = "U" from the dataset (you can use the 'subset' command for this)

d1 <- read_xlsx("Babbler_Dataset.xlsx")
head(d1)
str(d1)
d1$ID <- as.factor(d1$ID)
d1$SEX <- as.factor(d1$SEX)
d1$GROUP_ID <- as.factor(d1$GROUP_ID)

d1 <- subset(d1, d1$SEX != "U")
plot(GCP~SEX, data=d1)
plot(GCP~AGE, data=d1)

#b. First construct models to predict cognitive performance of the birds based on age and sex that WITHOUT
#including any random effects. Run models for each possible combination of predictors and choose the best.
#Since the research question is exploratory, there is no need to keep any of the predictor variable in the
#model at all costs - just decide based on AICc. What would you conclude based on the best model?

fit1 <- lm(GCP ~ SEX * AGE, data=d1)
fit2 <- lm(GCP ~ SEX + AGE, data=d1)
fit3 <- lm(GCP ~ SEX, data=d1)
fit4 <- lm(GCP ~ AGE, data=d1)

AICc(fit1, fit2, fit3, fit4)

summary(fit4)
Anova(fit4, type="III")
#we would conclude that none of the predictors have a significant effect on cognitive performance

#c.Run your models again, but now include the appropriate random effects structure. Again choose
#the best model. Do you reach a different conclusion?

fit1b <- lmer(GCP ~ SEX * AGE + (1|GROUP_ID), data=d1)
fit2b <- lmer(GCP ~ SEX + AGE + (1|GROUP_ID), data=d1)
fit3b <- lmer(GCP ~ SEX + (1|GROUP_ID), data=d1)
fit4b <- lmer(GCP ~ AGE + (1|GROUP_ID), data=d1)


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

#d.Visualize the best model
plot(allEffects(fit4b))

#e. Check for normality of residuals, outliers, and influential observations. You can skip
#checking for homogeneity of variances (we will see how to deal with is in a next class).
#What final conclusion do you reach?

shapiro.test(resid(fit4b))
# W > 0.9 so no deviation from normality
outlierTest(fit4b)
cooks.distance(fit4b) > 1
# No problem

#checking for linearity for mixed models is not seen in this course

# We can conclude that age has a significant negative effect on cognitive performance,
# but we have no evidence for any effect of sex.

# EXERCISE 3: OWL CHICKS ----
#We have a dataset where the researchers counted number of 'sibling negotiation vocalizations'
#(a type of sound the owl chicks make) of owl chicks in a number of different nests. 
#We also have information on the sex of the feeding parent (male or female)
#and the experimental treatment: the chicks either got plenty of food before the experiment started 
#(satiated) or not (deprived).
#The researchers wanted to know whether the feeding parent and/or food deprivation
#had an impact on the number of sibling negotiation vocalizations that the chicks made.

#a.Load the data, check if everything is ok and explore the data visually.

d2 <- read_xlsx("owls.xlsx")
head(d2)
str(d2)
#we here use 'mutate_if' from the package 'dplyr' to change all character variables
#to factors at the same time, but you can of course also change them one by one.
d2<-mutate_if(d2,is.character, as.factor)
str(d2)

par(mfrow=c(1,2))
plot(Vocalizations~SexParent, data = d2)
plot(Vocalizations~FoodTreatment, data = d2)
par(mfrow=c(1,1))

#b. Construct a generalized linear mixed model to predict the number of vocalizations
#depending on the food treatment and the sex of the feeding parent. Include the appropriate
#random effects and specify the right error structure (and link function).
#Do not include the interaction between food treatment and feeding parent.
#Visualize the model with effect plots.

fit5=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=d2)
summary(fit5)   
Anova(fit5, type="III") #we detect a significant effect of food treatment and of the sex of the parent
plot(allEffects(fit5), type="response") 

#c. Now include the interaction between food treatment and feeding parent in the model.
#Is this model better? Visualize the best model with effect plots if you have not yet done so.

fit6=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest),family=poisson(link=log),data=d2)
summary(fit6) #the interaction is significant
Anova(fit6, type="III") 
AICc(fit5, fit6) #keep the model with the interaction
plot(allEffects(fit6), type="response", multiline=T, confint=list(style="auto"))

#d. Check if there is any overdispersion in the model. Recall that has to be done 
#in a different way than if you have a glm (without random factors).
#If the overdispersed model is better, check the summary table.
#Do any of your conclusions change? Does it make sense to try to fit any other 
#models based on your conclusions? Visualize the final version of your model.

#we must first include an observation-level variable
d2$obs <- factor(1:nrow(d2))
head(d2)

#then we run the same model, now including that new variable as a random factor:
fit6b=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest)+(1|obs),family=poisson(link=log),data=d2)

AICc(fit6, fit6b) #the model with overdispersion has much better AICc, keep it!
#make an effect plot
summary(fit6b)
Anova(fit6b, type="III") #interaction and effect of sex no longer significant!
plot(allEffects(fit6b), type="response", multiline=T, confint=list(style="auto"))

#since the interaction is now no longer significant, we may try to fit the
#model without interaction, but with accounting for overdispersion. 
fit7=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=d2)
fit7b=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest)+(1|obs),family=poisson(link=log),data=d2)

AICc(fit6, fit6b, fit7, fit7b) 
#it is close, but we drop the interaction from the model and keep the observation level random effect
summary(fit7b)
Anova(fit7b, type="III") #
plot(allEffects(fit7b), type="response")

#since the AICc models 6b and 7b are very close in AICc, we may report both. Our conclusions will remain largely
#the same between both models: the coefficients for the main effects are similar and the interaction
#is not significant in the model with the interaction.

#testing for outliers and influential observations for a glmer is not straigthforward and we do not do this in this course

#e.What are your conclusions based on this model?

#we conclude that the owl chicks do more 'sibling negotiation vocalizations' if they
#are food deprived than if they are satiated. Based on our data, we can not conclude
#that the sex of the parent matters for the number of vocalizations and we have no support
#for a significant interaction between the two.

