############################################################################
# EXERCISES PRACTICAL 3: GENERALIZED LINEAR MODELS (AND POLYNOMIAL MODELS) #
############################################################################

#set your own working directory!
dir <- "C:/Users/u0081311/OneDrive - KU Leuven/Paktijkassistent/1ste semester/advanced/Practicals2022/Practical3-afterupdates"
setwd(dir)

library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(MuMIn)
library(afex)
library(openxlsx)

set_sum_contrasts()

#########################
# EXERCISE 1: TORTOISES #
#########################

#     We have a dataset on tortoise carapace length and total clutch weight in tortoise (data: "tortoises").
#     In this exercise we will ask whether total clutch weight in grams can be predicted from the length of the carapace length in mm.
#
#   a.  Read the data check that everything is ok, and start with some visual inspections 

d1=read.xlsx("tortoises.xlsx")
str(d1)
head(d1) # check whether the data look OK
plot(Clutch_weight ~ Length, data = d1) # get a first impression of the relationship between carapace length and clutch size

#   b.  Construct a simple linear model to see if there is an effect of carapace length on clutch weight.
#       Is there a significant effect? Also visualize your model with an effect plot.

fit1=lm(Clutch_weight~Length,data=d1) # First produce a simple linear model
summary(fit1) # there is no significant effect of clutch size
Anova(fit1, type="III") #same results for single continuous variable, but the summary gives you the estimates
plot(allEffects(fit1))

#   c.  Produce residualPlots of your model. Does the linear model you just made describe the relationship
#       between your predictor and your response variable well?

residualPlots(fit1) #it looks like there is a strong curvilinear effect

#   d.  Start adding polynomial terms to your model, starting with second order, and stop when AICc no longer improves.
#       Visualize the best model based on AICc.

fit2=lm(Clutch_weight~poly(Length,2),data=d1) # include a second order polynomial term
summary(fit2)
AICc(fit1, fit2) # the model with the polynomial term is better!

fit3=lm(Clutch_weight~poly(Length,3),data=d1) # Third order polynomial model
summary(fit3)
AICc(fit1, fit2, fit3) #this model is worse - keep the model with only the second order polynomial term

plot(allEffects(fit2, residuals=TRUE), smooth.residuals = FALSE) #effect plot that also include the data points

#Note that the 'poly' function that we use to include polynomal terms automatically performs residual centering.
#This is necessary because the linear term (carapace length) and the polynomial term (carapace length squared)
#will usually be correlated, leading to collinearity. 

#   e.  Perform model diagnosis: make residual plots and see if predictions are biased, test whether residuals 
#       are normally distributed, test homogeneity of variances, and test whether there are outliers and/or 
#       influential observations.

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

#########################
# EXERCISE 2: ISOLATION #
#########################

#     We have data ("isolation") on the presence or absence of a species on several islands in an archipelago.
#     For each island, we also have its size (km2) and the distance to the mainland (in km).
#     We are interested in whether the species' presence on an island can be predicted by how big it is and how far
#     it is from the mainland.

#   a.  As usual, start with reading the data, checking if everything is ok, and making some graphs of the raw data.
#       Note: because we now have a response variable that only takes two values (0 or 1), those plots won't be as
#       informative as before!

d2 <- read.xlsx("isolation.xlsx")
head(d2)
str(d2)

plot(presence ~ area, data=d2)
plot(presence ~ distance, data=d2)

#another way is with 'xyplot' from the library 'lattice'. This allows us to also draw a quick-and-dirty linear
#fit in the plot to get an idea (by including type=c("p","r") we include both points ("p") and a linear regression line
#("r"). Of course that linear fit is just for illustration - we cannot based any conclusions on this!
library(lattice)
xyplot(presence ~ area, data=d2 ,type=c("p","r"))
xyplot(presence ~ distance, data=d2, type=c("p","r"))

#   b.  Run a generalized linear model including only the main effects of area and distance on the presence 
#       of the species. Specify the appropriate error structure and link function of the model. Visualize the results. 

#We need to run a binomial glm because our response variable is binomial (has 2 levels). The appropriate link function
#is the logit link, although we actually don't have to specify this - it's the default for a binomial model.
fit <- glm(presence ~ area + distance, family=binomial(link=logit), data=d2)
summary(fit) #both predictors have a significant effect!
plot(allEffects(fit))
#to get a graph on the linear scale, we need to include type="response":
plot(allEffects(fit), type="response")
#note that there are some rendering issues in the drawing of the graph - the lines go beyond 0 and 1. 
#The model doesn't actually predict this - it is just the graph. Later we will learn to make graphs with different
#functions that are not as vulnerable to these problems.

#   c.  Now also include the interaction. Which model is best? Visualize the best model if you haven't yet done so.

fit2 <- glm(presence ~ area * distance, family=binomial(link=logit), data=d2)
summary(fit2) #the interaction is not significant!
AICc(fit, fit2) #the fit without the interaction is better - keep the original model.
#We've already visualized the best model in the previous step.

#   d.  According to our best model, what is the probability that the species occurs on an island with a surface are of
#       3 km^2 and that is 8 km from the mainland? How about an island of 6 km^2 that is 4 km from the mainland?
#       Use the 'predict' function for this. 

#generate predictions from our model for the two islands...
preds <- predict(fit, list(area=c(3, 6), distance=c(8, 4)), type="response")
preds
#for the first island, we predict the probability for the species to be there at 7%.
#for the second island, we predict a 99% probability that the species occurs there.

#   e.  Do the model diagnostics for our best model: linearity on the transformed scale, collinearity
#       overdispersion, outliers/influential observations.

#linearity: check residual plots
residualPlots(fit) #no serious problems here

#collinearity: check if our variances are inflated:
vif(fit) #vifs are <5, so no collinearity

#overdispersion: make a quasibinomial fit and check the dispersion parameter
fitb <- glm(presence ~ area + distance, family=quasibinomial(link=logit), data=d2)
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

###################################
# EXERCISE 3: DAMAGED BLOOD CELLS #
###################################

#    We have a dataset ("bloodcells") with information on the number of damaged bloodcells per mm^2 for a 
#    number of individuals. We also have information on their weight, their sex, and whether they are a smoker.
#
#   a.  Read the data check that everything is ok, and start with some visual inspections 

d3 <- read.xlsx("bloodcells.xlsx")
str(d3) # Check the structure of your data (convert characters to factors if needed)
#conversion of all the 'str' variables in our dataset to factors can conveniently be done this way:
library(dplyr)
d3<- mutate_if(d3, is.character, as.factor)
#but we can of course also just code each of them as factor separately using as.factor

plot(cells~smoker, data=d3)
plot(cells~sex, data=d3)
plot(cells~weight, data=d3)

#   b.  We would like to construct a model to predict the effects of smoking, sex and weight on the number
#       of damaged blood cells. What would be an appropriate type of model in this case? Why?
#       Construct a model that includes all main effects but no interactions, and visualize the model.

# The dependent variable (cells) contains count data, so we need to construct a generalized linear model with 
# a poisson error distribution and a log link function. 

# Make the model
fit=glm(cells~smoker+sex+weight,family=poisson(link=log),data=d3)
# We actually don't have to specify the log link, as it is the default for a poisson error structure.
# So this would produce the exact same model:
fit=glm(cells~smoker+sex+weight,family=poisson,data=d3)

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

#   c.  Now fit a model that also includes the interaction between smoker and weight. Is the interaction significant?
#       What does it mean? Visualize the model. Which model is better, the one with or without the interaction?

fit2=glm(cells~smoker*weight+sex,family=poisson,data=d3)
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
fit3=glm(cells~smoker+weight+sex + sex:weight, family=poisson,data=d3)
fit4=glm(cells~smoker+weight+sex + sex:smoker, family=poisson,data=d3)
fit5=glm(cells~smoker+weight+sex + sex:weight + sex:smoker, family=poisson,data=d3)
fit6=glm(cells~smoker+weight+sex + sex:weight + smoker:weight, family=poisson,data=d3)
fit7=glm(cells~smoker+weight+sex + sex:smoker + smoker:weight, family=poisson,data=d3)
fit8=glm(cells~smoker+weight+sex + sex:smoker + smoker:weight + sex:weight, family=poisson,data=d3)

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


best = glmulti(cells ~ smoker + sex + weight, family="poisson", data = d3, confsetsize = 5, crit = "aicc") 
best@formulas
bestfit=best@objects[[1]]

 #glmulti came to the same conclusion as we did in c - the algorithm works! (though also here we still need to correct for overdispersion)



