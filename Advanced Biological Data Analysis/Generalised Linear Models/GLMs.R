##%#########################################################################%##
#                                                                             #
#            Generalised Linear Models Practical - Zoja Manček Páli           #
#                              Date: 9.10.2025                                #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Generalised Linear Models")
getwd()

#packages
library(car) 
library(tidyverse)
install.packages("rJava")
library(rJava)
library(glmulti)
library(rockchalk)
library(lmtest)
library(effects)
library(MuMIn)
library(afex)
library(openxlsx)

set_sum_contrasts() #effects coding

#data
blood <- read.xlsx("bloodcells.xlsx")
isolation <- read.xlsx("isolation.xlsx")
torts <- read.xlsx("tortoises.xlsx")


#EXERCISE 1: TORTOISES ---------------

#We have a dataset on tortoise carapace length and total clutch weight in tortoise.
#In this exercise we will ask whether total clutch weight in grams can be 
#predicted from the length of the carapace length in mm.

#a.Read the data check that everything is ok, and start with some visual inspections 
head(torts)
str(torts)

torts <- torts %>% rename(length = Length,
                          clutch = Clutch_weight)

plot(clutch ~ length, data = torts)

#b.Construct a simple linear model to see if there is an effect of carapace length 
#on clutch weight.Is there a significant effect? Also visualize your model with an 
#effect plot.

model1 <- lm(clutch ~ length, data = torts)
summary(model1) #no effect
plot(allEffects(model1)) #huge confidence interval

#c.Produce residualPlots of your model. Does the linear model you just made describe 
#the relationship between your predictor and your response variable well?
residualPlots(model1) #non-linear, no

#d.Start adding polynomial terms to your model, starting with second order, and stop 
#when AICc no longer improves. Visualize the best model based on AICc.
model2 <- lm(clutch ~ poly(length, 2), data = torts)
model3 <- lm(clutch ~ poly(length, 3), data = torts)
model4 <- lm(clutch ~ poly(length, 4), data = torts)
models_list <- list(model2, model3, model4)

AICc(model1, model2, model3, model4) #model 2 is best according to the AICc
model.sel(models_list) #model 2 is best

plot(allEffects(model2, residuals = TRUE), smooth.residuals = FALSE)
plot(allEffects(model2))

#e.Perform model diagnosis: make residual plots and see if predictions are biased, 
#test whether residuals are normally distributed, test homogeneity of variances, 
#and test whether there are outliers and/or influential observations.

#normality of residuals
hist(model3$residuals)

hist(rstudent(model3), probability = T, xlab = "Studentised residuals", 
     main = "Distribution of Studentised Residuals")

r = model3$residuals
h <- hist(model3$residuals, breaks = 10, density = 20,
          col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
xfit <- seq(min(r), max(r), length = 40) 
yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
yfit <- yfit * diff(h$mids[1:2]) * length(r) 
lines(xfit, yfit, col = "red", lwd = 2)

shapiro.test(residuals(model2)) #p not significant, W = 0.95822; assumption not violated

#homogeneity of variances
spreadLevelPlot(model2) #looks weird
ncvTest(model2) #p-value > 0.05, variances homogeneous; assummption not violated

#linearity and collinearity
residualPlots(model2) #pearson residuals look weird -> assumption appears violated

#outliers:
outlierTest(model2) #no residuals with p < 0.05
influenceIndexPlot(model2, vars = c("Studentized","Bonf"))  #no significant outliers 
#(no p-values < 0.05)
#points n114 and 153 are the furthest from the rest of the data, but not significant

cd <- cooks.distance(model2)
(inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)

influenceIndexPlot(model2, vars = c("Cook"))
#point n23 has the highest cook's distance but not > 1

#f. What are your conclusions based on this analysis?
summary(model2) #significant effect of height^2 on clutch size, p < 0.05
#adjusted r^2 = 0.3149
#intercept tells you the mean clutch length for the mean carpace length
  #because polynomial models are orthogonal
#first order =/ significant (relationship is not linear)
#polynomial term = significant (curved relationship)
  #negative coefficient for this poly term means the curve is concave downward

#A second-order polynomial model best explains the relationship between carapace 
#length and clutch weight in tortoises.The model indicates a significant non-linear 
#effect (p < 0.01) where clutch weight increases with carapace length up to an 
#intermediate size, after which it plateaus or decreases. Model assumptions were met, 
#The model explains ~31% of the variation in clutch weight. Ecologically, this 
#suggests that reproductive output scales with size but is constrained by 
#physiological or energetic limits at larger body sizes.
plot(allEffects(model2, residuals=TRUE), smooth.residuals = FALSE) #seed # decreases with seed weight

(tort_plot <- ggplot(torts, aes(x = length, y = clutch)) +
  geom_point(size = 3, alpha = 0.8, colour = "#1b9e77") +
    stat_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              colour = "#d95f02",
              fill = "#d95f02",
              alpha = 0.2,
              se = TRUE,       #adds confidence interval
              linewidth = 1.2) +
    theme_classic() +
  labs(
    x = "Carapace Length (mm)",
    y = "Total Clutch Weight (g)",
    caption = "Model: clutch ~ poly(length, 2)") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()))

ggsave("tortoises_poly_plot.png", tort_plot, width = 15, height = 11)

#EXERCISE 2: ISOLATION --------

#We have data ("isolation") on the presence or absence of a species on several 
#islands in an archipelago. For each island, we also have its size (km2) and the 
#distance to the mainland (in km). We are interested in whether the species' presence 
#on an island can be predicted by how big it is and how far it is from the mainland.

#a.As usual, start with reading the data, checking if everything is ok, and 
#making some graphs of the raw data. Note: because we now have a response variable 
#that only takes two values (0 or 1), those plots won't be as informative as before!

head(isolation)
str(isolation)

plot(presence ~ area, data = isolation)
plot(presence ~ distance, data = isolation)

par
xyplot(presence ~ area, data = isolation ,type = c("p","r")) #p = points, r = reg. line
xyplot(presence ~ distance, data = isolation, type = c("p","r"))
#we cannot base any conclusions on this!

#b.Run a generalized linear model including only the main effects of area and 
#distance on the presence of the species. Specify the appropriate error structure 
#and link function of the model. Visualize the results. 

model5 <- glm(presence ~ area + distance, family = binomial(link = logit), data = isolation)
plot(allEffects(model5)) #presence increases with island area and decreases
#with distance from shore
summary(model5) #both have a sig. interaction

#c.Now also include the interaction. Which model is best? Visualize the best model 
#if you haven't yet done so.

model6 <- glm(presence ~ area * distance, family = binomial(link = logit), data = isolation)
plot(allEffects(model6))

AICc(model5, model6) #model5 is better
models_list2 <- list(model5, model6)
model.sel(models_list2) #model5 has a higher AIkake weight

#d.According to our best model, what is the probability that the species occurs 
#on an island with a surface are of 3 km^2 and that is 8 km from the mainland? 
#How about an island of 6 km^2 that is 4 km from the mainland? Use predict()
(prediction1 <- predict(model5, list("area" = 3, "distance" = 8), type = "response")) #0.06967481

(prediction2 <- predict(model5, list("area" = 6, "distance" = 4), type = "response")) #0.9165

#or the faster way:
(preds <- predict(model5, list(area = c(3, 6), distance = c(8, 4)), type = "response"))


#e. Do the model diagnostics for our best model: linearity on the transformed 
#scale, collinearity, overdispersion, outliers/influential observations.
  plot(allEffects(model5), type = "response")
  
  #linearity and collinearity
  residualPlots(model5) #looks ok
  vif(model5) #VIF < 5; no collinearity
  
  #outliers:
  outlierTest(model5) #no residuals with p < 0.05
  influenceIndexPlot(model5, vars = c("Studentized","Bonf"))  #no significant outliers 
  #(no p-values < 0.05)
  #points n19 and 47 are the furthest from the rest of the data, but not significant
  
  #influential points:
  cd <- cooks.distance(model5)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model5, vars = c("Cook"))
  #points n19 and 47 have the highest cook's distance but not > 1
  
  #overdispersion?
  fit2quasi <- glm(presence ~ area + distance, family = quasibinomial(link = logit), data = isolation)
  summary(fit2quasi) #dispersion parameter: 0.6094858
  (overdispersion5 <- sum(residuals(model5, type = "pearson")^2) / df.residual(model5)) #0.6094412
    #all good, disp. parameter: 0.6094412
  

#f.What are your conclusions based on this analysis?
  
  summary(model5) #both predictors have a significant effect!
  
  #to get a graph on the linear scale, we need to include type="response":
  plot(allEffects(model5), type="response")
  
#For each 1-unit increase in island area, the log-odds of species presence 
  #increase by 0.58 (so presence probability increases).
#For each 1-unit increase in distance from the mainland, the log-odds of presence 
  #decrease by 1.37 (so presence probability decreases).
#A binomial GLM revealed significant effects of both island area (β = 0.58, 
  #p = 0.019) and distance from the mainland (β = −1.37, p = 0.004) on species 
  #presence. The model explained a substantial reduction in deviance (68.0 → 28.4), 
  #indicating strong predictive power. The results suggest that species are more 
  #likely to occur on larger and less isolated islands, consistent with classical 
  #island biogeography theory. Model diagnostics indicated mild underdispersion 
  #(ϕ = 0.60), confirming that the binomial variance assumption was adequate.

  
#EXERCISE 3: DAMAGED BLOOD CELLS ----
#We have a dataset ("bloodcells") with information on the number of damaged 
#bloodcells per mm^2 for a number of individuals. We also have information on 
#their weight, their sex, and whether they are a smoker.

#a.Read the data check that everything is ok, and start with some visual inspections 
head(blood)
str(blood)

blood <- blood %>% mutate(smoker = as.factor(smoker),
                          age = as.factor(age),
                          sex = as.factor(sex))
str(blood)

plot(cells ~ smoker, data = blood) #more damaged bloodcells in smokers
plot(cells ~ age, data = blood) #about equal with some outliars
plot(cells ~ sex, data = blood) #bigger outliars in women, but wider distribution in men

#b.We would like to construct a model to predict the effects of smoking, sex and weight on the number
#of damaged blood cells. What would be an appropriate type of model in this case? Why?
#Construct a model that includes all main effects but no interactions, and visualize the model.
head(blood)
hist(blood$cells) #poisson distribution, use poisson family glm
model7 <- glm(cells ~ smoker + weight + sex, family = poisson(link = log), data = blood)
plot(allEffects(model7))

summary(model7)   
Anova(model7, type="III")  #note that the p-values from the Anova table and the summary table are very similar, but not
#exactly the same. That is because they use slightly different tests - the summary table
#uses a Wald test, whereas the Anova uses a Likelihood Ratio test. The latter is more 
#exact but the two will yield very similar results. It is possible to use the Wald test in
#the Anova too, using the argument test.statistic="Wald" - if you do that, you'll see that
#the p-values are now exactly the same as in the summary table.

plot(allEffects(model7), ylab = "Number of damaged cells")
plot(allEffects(model7), ylab = "Number of damaged cells", type = "response") #use type = 
  #"response" to plot on the linear scale:


#c.  Now fit a model that also includes the interaction between smoker and weight. Is the interaction significant?
#What does it mean? Visualize the model. Which model is better, the one with or without the interaction?
levels(blood$smoker) #no = 0, yes = 1
levels(blood$sex) #female = 0, male = 1
model8 <- glm(cells ~ smoker*weight + sex, family = poisson(link = log), data = blood)
plot(allEffects(model8))
summary(model8) #interaction is significant
  #smoker1 (yes vs no) -> not significant
  #weight -> significant
    #people who weigh more have more damaged blood cells
  #sex1 (male vs female) -> men have higher counts of damaged blood cells, significant
  #male*weight interaction: negative, significant -> weight dampens the effect of smoking
Anova(model8, type = "III")
AICc(model7, model8) #model8 is better

plot(allEffects(model8), multiline = T, confint = list(style = "auto")) #stronger effect of weight on
#on blood cell damage counts in smokers
plot(allEffects(model8), multiline = T, confint = list(style = "auto"), type = "response")


#d.Now check all models with all possible combinations of one-way interactions. 
#Which has the best AICc? Present this model visually and perform model diagnosis: 
#linearity on the transformed scale, collinearity overdispersion, outliers/influential observations.

model7 = glm(cells~smoker+weight+sex + sex:weight, family = poisson, data = blood)
model8 = glm(cells~smoker+weight+sex + sex:smoker, family = poisson, data = blood)
model9 = glm(cells~smoker+weight+sex + sex:weight + sex:smoker, family = poisson, data = blood)
model10 = glm(cells~smoker+weight+sex + sex:weight + smoker:weight, family = poisson, data = blood)
model11 = glm(cells~smoker+weight+sex + sex:smoker + smoker:weight, family = poisson, data = blood)
model12 = glm(cells~smoker+weight+sex + sex:smoker + smoker:weight + sex:weight, family = poisson, data = blood)

AICc(model7, model8, model9, model10, model11, model12) #models 11 and 12 have the same AICc
models_list3 <- list(model7, model8, model9, model10, model11, model12)
model.sel(models_list3) #doesn't show weight?

  aics <- AICc(model7, model8, model9, model10, model11, model12)$AICc
  dAICc <- aics - min(aics)
  (w <- exp(-0.5*dAICc) / sum(exp(-0.5*dAICc))) #report model11 but 
  #also say model12 is equally supported

plot(allEffects(model11))
plot(allEffects(model11, multiline = T), type = "response") #non-linear relationship

summary(model11)
Anova(model11, type = "III") 

#between weight and cells

  #linearity and collinearity
  residualPlots(model11) #looks ok
  vif(model11) #GVIF > 5; collinearity
  model11c <- residualCenter(model11) #fix the correlation within the interaction
  summary(model11c)
    #intercept: expected count for a non-smoker, female, at average weight (e^estimate) ~ 0.17
    #smoker1: at average weight, smokers have about 45% fewer damaged cells than non-smokers.
      #e^(−0.593) ≈ 0.55
    #weight: for non-smokers, each 1-unit weight increase raises expected count by ~2.4%.
    #sex1: males have higher number of damaged blood cells
    #smoker*weight: the weight slope is weaker in smokers
  #Heavier people show more damaged cells, but that increase with weight is smaller 
    #in smokers. One sex shows ~15% higher counts than the other. At average weight, 
    #smokers have lower expected counts than non-smokers
  
  vif(model11c) #all < 5 so all good
    
  
  #outliers:
  outlierTest(model11) #no residuals with p < 0.05
  influenceIndexPlot(model11, vars = c("Studentized", "Bonf"))  #no significant outliers 
  #(no p-values < 0.05)
  #point n246 are the furthest from the rest of the data, but not significant
  
  #influential points:
  cd <- cooks.distance(model11)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model11, vars = c("Cook"))
  #points n246 and 294 have the highest cook's distance but not > 1
  
  #overdispersion?
  (overdispersion11 <- sum(residuals(model11, type = "pearson")^2) / df.residual(model11)) #1.014639
  fit11quasi <- glm(cells ~ smoker + weight + sex + sex:smoker + smoker:weight, family = quasipoisson, data = blood)
  summary(fit11quasi) #dispersion parameter is 0.9812321 (though WITH residualCentering)
  #slight overdispersion but ok

#e. What are your conclusions based on this analysis?
  #Heavier people show more damaged cells, but that increase with weight is smaller 
  #in smokers. One sex shows ~15% higher counts than the other (women > men). At average weight, 
  #smokers have lower expected counts than non-smokers
  plot(allEffects(model11), multiline = T, confint = list(style = "auto"), type = "response")

  #  We conclude that there are overall effects of smoking and weight on the damaged bloodcell count.
  #  Also,  being a smoker impacts this count more strongly in females than in males.
  #  Finally, the effect of weight is much stronger in smokers than in non-smokers.
  

###
install.packages("rJava")
library("rJava")
install.packages("glmulti")
library("glmulti")

best = glmulti(cells ~ smoker + sex + weight, family = "poisson", data = blood, confsetsize = 5, crit = "aicc") 
best@formulas
(bestfit = best@objects[[1]])
best #gives the models with AIC in ascending order
summary(best) #$bestmodel
#[1] "cells ~ 1 + smoker + sex + weight + sex:smoker + smoker:weight"

#so after this, you can check assumptions and then move on -> shortens step c from exercise 3 a lot


