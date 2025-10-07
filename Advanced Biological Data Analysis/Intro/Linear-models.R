##%#########################################################################%##
#                                                                             #
#                   Linear Models Practical - Zoja Manček Páli                #
#                              Date: 25.9.2025                                #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Linear Models")
getwd()

#packages
library(car)
library(tidyr)
library(dplyr)
library(ggplot2)
library(multcomp)
library(effects) 
library(emmeans)
library(openxlsx)

#data
metabolism <- read.xlsx("metabolism.xlsx")
yield <- read.xlsx("yields.xlsx")

#EXERCISE 1: resting metabolic rate ----
head(metabolism)
str(metabolism)
#bodyweight is numeric
#resting metabolic rate is numeric

#question = does bodyweight impact resting metabolic rate?

plot(resting_mr ~ bodyweight, data = metabolism) #looks linear

(model1 <- lm(resting_mr ~ bodyweight, data = metabolism))
summary(model1) #slope > 0, positive linear relationship
#p-value < 0.05, so the relationship appears significant -> significant positive effect
#adjusted r^2 is only 0.522, so the linear model only accounts for 52.2% of the variation within the data
#intercept = resting mr of a person at 0kg
#estimate of bodyweight: for each 1kg increase, you see a 7.437 calorie increase
#t-value is also extreme -> calculate the probability that the estimate =/ 0

#if we plot this a bit nicely:
(plot <- ggplot(metabolism, aes(bodyweight, resting_mr)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "red") +
  theme_classic() +
  labs(x = "Body weight (kg)", y = "Resting metabolic rate (cal)"))


plot(allEffects(model1)) #this shows the reg. line and the 95% conf interval
#estimate +- 1.96* SE = conf.interval
(esti1 <- 7.437 + 1.96 * 1.086) #9.56556
(esti2 <- 7.437 - 1.96 * 1.086) #5.30844
#so if we reran the experiment, we could say that our estimate would fall between 
  #5.30844 and 9.56556 95% of the time


# 'predict' for this: predict(MODELFIT, list("PREDICTORVARIABLE"=LEVEL))
predict(model1, list("bodyweight" = 60)) #resting mr of 1232.453 calories
predict(model1, list("bodyweight" = 80)) # 1381.194 calories


#assumptions tests
 #normality of residuals
  hist(model1$residuals)
  
  hist(rstudent(model1), probability = T, xlab = "Studentised residuals", 
       main = "Distribution of Studentised Residuals")

  r = model1$residuals
  h <- hist(model1$residuals, breaks = 10, density = 20,
            col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
  xfit <- seq(min(r), max(r), length = 40) 
  yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(r) 
  lines(xfit, yfit, col = "red", lwd = 2)
  
  shapiro.test(residuals(model1)) #p not significant, W = 0.96147; if W > 0.9, then normal distribution can be assumed
  #residuals are not exhibiting any deviation from a normal distribution, assuption not violated
  
 #homogeneity of variances
  spreadLevelPlot(model1) #looks normal-ish
  ncvTest(model1) #p-value > 0.05, variances homogeneous
  #no deviation from homogeneity, asummption not violated
  
 #linearity
  residualPlots(model1) #looks ok

  
 #outliars:
  outlierTest(model1) #outliars affecting the model are not significant 
  influenceIndexPlot(model1,vars=c("Studentized","Bonf"))  #no significant outliers (no p-values < 0.05)
  #point n40 is the furthest from the rest of the data, but not significant
  
  cd <- cooks.distance(model1)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model1,vars=c("Cook"))
  #point n40 has the highest cook's distance but not >1

  #no assumptions violated, model shows a significant effect of bodywieght on resting metabolic rate
  
#EXERCISE 2: crop yields ----
  head(yield)
  str(yield) #three numeric variables
  
  boxplot(yield)#you can see that there are three individual soil categories, but we want to see if soil type
  #affects crop yields so we need to transform the dataset:

  yield_long <- yield %>%
    pivot_longer(cols = c(clay, loam, sand),
                 names_to = "soil_type", 
                 values_to = "yield_value")  %>% 
    mutate(soil_type = factor(soil_type))

  str(yield_long)
  
  plot(yield_value ~ soil_type, data = yield_long)
    
  (model2 <- lm(yield_value ~ soil_type, data = yield_long))  
  summary(model2)
  Anova(model2, type="III") #soil type does have an effect (p-value < 0.05)
  #adjusted r^2 is only 0.1829
  
  plot(allEffects(model2)) 
  
  emmeans(model2, ~soil_type)
  contrast(emmeans(model2, ~soil_type), method='pairwise', adjust='Tukey') 
  #significant difference between loam and sand
  
  #normality of residuals:
  hist(model2$residuals)
  
  r2 = model2$residuals
  h2 <- hist(model2$residuals, breaks = 10, density = 10,
            col = "lightgray", xlab = "Residuals", ylab = "Frequency") 
  xfit2 <- seq(min(r2), max(r2), length = 40) 
  yfit2 <- dnorm(xfit2, mean = mean(r2), sd = sd(r2)) 
  yfit2 <- yfit2 * diff(h2$mids[1:2]) * length(r2) 
  lines(xfit2, yfit2, col = "black", lwd = 2) #looks normal
  
  shapiro.test(residuals(model2)) #p not significant, W = 0.99131; if W > 0.9, then normal distribution can be assumed
  
  #homogeneity of variances
  spreadLevelPlot(model2) #looks normal-ish
  ncvTest(model2) #p-value > 0.05, homogeneity
  
  #linearity
  residualPlot(model2) #looks ok
  
  #outliwrs:
  outlierTest(model2) #outliers affecting the model are not significant 
  influenceIndexPlot(model2,vars=c("Studentized","Bonf"))  #no significant outliers (no p-values < 0.05)
  
  cd2 <- cooks.distance(model2)
  inflobs = which(cd2>1);inflobs #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model2,vars=c("Cook")) #no values w cook's distance > 1
  
  