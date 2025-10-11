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
#points n2 and 16 are the furthest from the rest of the data, but not significant

cd <- cooks.distance(model2)
(inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)

influenceIndexPlot(model2, vars = c("Cook"))
#points n17 and 18 have the highest cook's distance but not > 1

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

#b.Run a generalized linear model including only the main effects of area and 
#distance on the presence of the species. Specify the appropriate error structure 
#and link function of the model. Visualize the results. 

model5 <- glm(presence ~ area + distance, family = binomial(link = logit), data = isolation)
plot(allEffects(model5)) #presence increases with island area and decreases
#with distance from shore

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
(prediction1 <- predict(model5, list("area" = 3, "distance" = 8), type = "response")) #-1.783
#probablity: 0.06967481

(prediction2 <- predict(model5, list("area" = 6, "distance" = 4), type = "response"))
exp(prediction2) #0.9165


#e. Do the model diagnostics for our best model: linearity on the transformed 
#scale, collinearity, overdispersion, outliers/influential observations.
plot(allEffects(model6), type = "response")


#overdispersion
(overdispersion6 <- sum(residuals(model6, type = "pearson")^2) / df.residual(model6)) #0.3647
#underdispersion

quasi6 <- glm(presence ~ area*distance, family = quasipoisson(link = log), data = isolation)
summary(quasi6) #overdispersion 0.3647
summary(model6) #overdispersion = 1

#linearity:
residualPlots(model6) #looks awful

#   f.  What are your conclusions based on this analysis?

