#############################################################
# EXERCISES PRACTICAL 6: MODELLING NON-LINEAR RELATIONSHIPS #
#############################################################

#set your own working directory!
dir <- "C:/Users/pvdbe/Google Drive/Teaching/Advanced Biological Data Analysis/Practicals/Practical 5 - nonlinear models/"
setwd(dir)

library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(MuMIn)
library(afex)

set_sum_contrasts()

#####################
# EXERCISE 1: DNASE #
#####################

#     We have a dataset DNase (it is a dataset already available in R) that contains
#     the optical density that was measured for a number of different concentrations
#     of the enzyme DNase. In this exercise, we consider only the "Run = 1" assay.
#     We have all measurements in duplo.

library(nlstools)
library(nlsMicrobio) # contains some bacterial growth data and fit functions
library(investr) # for plotFit function

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



###################################
# EXERCISE 2: MEDICINAL TREATMENT #
###################################

#      In the dataset 'Theoph' we have the serum concentration of drug Theophylline 
#      for different individuals at various times after the drug has been administered.
#      The individuals all got somewhat different doses of the drug.

#      We are interested in predicting the concentration "conc" as a function of 
#      dose and time, while also accounting for random individual variation between 
#      the subjects.

#      For 'nlme', we must structure the data with "groupedData", but our data set
#      is already structured this way so we do not need to worry about it.

library(lattice)
library(Matrix)
library(afex) 
library(nlme)
library(nlstools)
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


################################
# EXERCISE 3: TRICEPS SKINFOLD #
################################

#     We have data of the 'triceps skin fold' of 892 women in West-Africa of different 
#     ages (this measurement gives an indication of their fat reserves). The data is 
#     available in the package 'MultiKink' which you'll have to (install and) load.

library(MultiKink)
library(splines)

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
