#########################################################
# EXERCISES PRACTICAL 4: ADVANCED GLMs AND MIXED MODELS #
#########################################################

library(car)
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
library(nlme)

#set your own working directory!
dir <- "C:/Users/Piet/GDrive/Teaching/Advanced Biological Data Analysis/Aanpassingen 2024/Practical 4 - advanced GLMs and mixed models"
setwd(dir)

set_sum_contrasts()

#########################
# EXERCISE 1: SQUIRRELS #
#########################

# Red squirrels (Sciurus vulgaris) are known to host a species of parasitic nematode (Strongyloides robustus), 
# which can influence their health outcomes. Researchers have observed that the severity of infection—measured 
# by parasite load—can lead to different health trajectories: recovery, chronic illness, or mortality. 
# Additionally, the impact of parasite load may vary depending on the age of the squirrels. In this study, 
# data has been collected on the age (in months), parasite load, and health outcomes of a population of red 
# squirrels. 

#    Read the data and code the categorical predictors as factors if necessary.
data <- read.xlsx("Squirrels.xlsx")
head(data)
str(data)
data$Outcome <- as.factor(data$Outcome)

#    a.  Start with some visual inspection of the data. 
plot(data)
plot(data$Outcome ~ data$Age)
plot(data$Outcome ~ data$Parasite_load)

#    b.  Fit multinomial models to investigate the influence of age and parasite load on disease outcomes.
#        Fit a model with and without an interaction and pick the best one based on AICc.
#        NOTE: in including your predictor variables, list 'Age' first. Otherwise the effect plot is hard to interpret.
#        Which variables have a significant effect?

fit1 <- multinom(Outcome ~ Age + Parasite_load, data = data)
fit2 <- multinom(Outcome ~ Age * Parasite_load, data = data)
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

########################
# EXERCISE 2: BABBLERS #
########################

#     In this study, the researchers were interested in the cognitive performance in these birds. They were 
#     interested in exploring whether cognitive performance varies between birds of different sex and age.
#     They subjected 38 birds from 11 different social groups to three different cognitive tasks: an 
#     associative learning task (“AL” in the dataset), and inhibitory control task (“IC”), and a reversal 
#     learning task (“RL”). Next, they performed a principal component analysis on the scores on these three 
#     tasks, from which they extracted the first principal component (“PC1”). The opposite of this first 
#     principal component was then interpreted as a measure of “general cognitive performance” (“GCP”). Every
#     individual was studied across all three tasks exactly once and has a single measure of general cognitive
#     performance.
#
#   a.  Load the data, check if everything is ok and explore the data visually. Exclude birds with unknown sex
#       SEX = "U" from the dataset (you can use the 'subset' command for this)

d1 <- read.xlsx("Babbler_Dataset.xlsx")
head(d1)
str(d1)
d1$ID <- as.factor(d1$ID)
d1$SEX <- as.factor(d1$SEX)
d1$GROUP_ID <- as.factor(d1$GROUP_ID)

d1 <- subset(d1, d1$SEX != "U")
plot(GCP~SEX, data=d1)
plot(GCP~AGE, data=d1)

#   b.  First construct models to predict cognitive performance of the birds based on age and sex that WITHOUT
#       including any random effects. Run models for each possible combination of predictors and choose the best.
#       Since the research question is exploratory, there is no need to keep any of the predictor variable in the
#       model at all costs - just decide based on AICc. 
#       What would you conclude based on the best model?

fit1 <- lm(GCP ~ SEX * AGE, data=d1)
fit2 <- lm(GCP ~ SEX + AGE, data=d1)
fit3 <- lm(GCP ~ SEX, data=d1)
fit4 <- lm(GCP ~ AGE, data=d1)

AICc(fit1, fit2, fit3, fit4)

summary(fit4)
Anova(fit4, type="III")
#we would conclude that none of the predictors have a significant effect on cognitive performance

#   c.  Run your models again, but now include the appropriate random effects structure. Again choose
#       the best model. Do you reach a different conclusion?

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



##########################
# EXERCISE 3: OWL CHICKS #
##########################

#     We have a dataset where the researchers counted number of 'sibling negotiation vocalizations'
#     (a type of sound the owl chicks make) of owl chicks in a number of different nests. 
#     We also have information on the sex of the feeding parent (male or female)
#     and the experimental treatment: the chicks either got plenty of food before the experiment started 
#     (satiated) or not (deprived).
#
#     The researchers wanted to know whether the feeding parent and/or food deprivation
#     had an impact on the number of sibling negotiation vocalizations that the chicks made.
#
#   a.  Load the data, check if everything is ok and explore the data visually.

d2 <- read.xlsx("owls.xlsx")
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

#   b.  Construct a generalized linear mixed model to predict the number of vocalizations
#       depending on the food treatment and the sex of the feeding parent. Include the appropriate
#       random effects and specify the right error structure (and link function).
#       Do not include the interaction between food treatment and feeding parent.
#       Visualize the model with effect plots.

fit5=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=d2)
summary(fit5)   
Anova(fit5, type="III") #we detect a significant effect of food treatment and of the sex of the parent
plot(allEffects(fit5), type="response") 

#   c.  Now include the interaction between food treatment and feeding parent in the model.
#       Is this model better? Visualize the best model with effect plots if you have not yet done so.

fit6=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest),family=poisson(link=log),data=d2)
summary(fit6) #the interaction is significant
Anova(fit6, type="III") 
AICc(fit5, fit6) #keep the model with the interaction
plot(allEffects(fit6), type="response", multiline=T, confint=list(style="auto"))

#   d.  Check if there is any overdispersion in the model.
#       Recall that has to be done in a different way than if you have a glm (without random factors).
#       If the overdispersed model is better, check the summary table.
#       Do any of your conclusions change?
#       Does it make sense to try to fit any other models based on your conclusions?
#       Visualize the final version of your model.

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

#   e.  What are your conclusions based on this model?

#we conclude that the owl chicks do more 'sibling negotiation vocalizations' if they
#are food deprived than if they are satiated. Based on our data, we can not conclude
#that the sex of the parent matters for the number of vocalizations and we have no support
#for a significant interaction between the two.

