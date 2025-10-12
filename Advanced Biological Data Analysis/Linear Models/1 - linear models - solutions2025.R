########################################
# EXERCISES PRACTICAL 1: LINEAR MODELS #
########################################

dir <- setwd("set your own working directory")
setwd(dir)

library(car)
library(multcomp)
library(effects) 
library(emmeans)
library(openxlsx)


# Make sure your working directory is set.

##########################################
# EXERCISE 1: RESTING METABOLIC RATE #
##########################################

#     We have a dataset ('metabolism') on the resting metabolic rate (daily energy 
#     expenditure in calories when functioning at rest) for 43 women. We also have 
#     the bodyweight (in kilograms) of these women, and would like to ask whether 
#     body mass predicts resting metabolic rate (and if so, how).

#     a. Is the data well read? Check if there are 2 variables. Also check the structure of the data with
#     "str". 

#     A good place to start examining your data is usually by plotting. You can use the function
#         plot() for this. What is your first impression about the relationship between body weight and resting metabolic rate? 

metab <- read.xlsx("metabolism.xlsx", sheet=1)
head(metab)
str(metab)
plot(resting_mr~bodyweight, data=metab)

#     b. Construct a linear model to predict metabolic rate based on body weight. What do the estimates and p-values in the summary table mean?

fit1 <- lm(resting_mr~bodyweight, data=metab)
summary(fit1)

#the first estimate (of the intercept) indicates what would be the resting metabolism
#that our model predicts for a women of 0 kilograms (786 calories per day. This is 
#of course non-sensical. The estimate of the slope for bodyweight (the second estimate)
#tells us how many more calories of resting metabolism our model predicts for one 
#more kilogram of body weight (7.437 calories). The first P-value is again not so
#relevant: it tests whether the intercept is different from zero. However the second 
#P-value is interesting: it is the result of a test that checks whether the estimate
#of the slope on bodyweight is different from zero. This is the case (P<0.001) so this
#means there is a significant effect of bodyweight on resting metabolism.

#     c. Illustrate the model with an effect plot. See if you can recognize your estimates in the plot.

plot(allEffects(fit1))

#     d. Based on our model, what do we expect the resting metabolic rate to be for 
#        a woman of 60 kilos? What about a woman of 80 kg?
#        We can use the function 'predict' for this: predict(MODELFIT, list("PREDICTORVARIABLE"=LEVEL))
#        (Replace the text in capitals)

predict(fit1, list("bodyweight"=60)) #for a woman of 60 kg, we predict a resting metabolism of 1232 calories
predict(fit1, list("bodyweight"=80)) #for a woman of 80 kg, we predict a resting metabolism of 1381 calories

#     e. Check the assumptions. Start by checking if the residuals of your model are normally distributed, both visually and formally (with a test).

#first check visually by plotting the studentized residuals and the normal distribution
#this piece of code plots the residuals:
hist(rstudent(fit1), probability=T, ylim=c(0,0.4), 
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
#now we define a variable 'xfit' that just stores a vector of 100 numbers between -3 and 3, equally spaced
#you can check this by just executing 'xfit' after you run the following line
xfit=seq(-3,3,length=100)
#here we define a variable 'yfit' that calculates probabilities for the standard normal distribution (a normal
#distribution with mean 0 and standard deviation 1), for each of the x-values that we just created:
yfit=dnorm(xfit)
#and with this code we then just plot this normal distribution on top of our existing histogram
lines(xfit, yfit, col="red",lwd=2)

#Then do a Shapiro-Wilk W test
shapiro.test(residuals(fit1))  # W=0.96 > 0.9, so no deviation from normality

#     f. Test for homogeneity of variances, both visually and formally with a test.

#test for homogeneity of variances, first visually:
spreadLevelPlot(fit1)

#test for the homogeneity of variances
ncvTest(fit1) #the variances do not significantly diverge from homogeneous

#     g. Check visually for linearity

residualPlots(fit1) #lines are quite straight, not deviation from linearity

#     h. Check if there are any outliers, again both visually and formally.

outlierTest(fit1)     # there are no significant outliers, although observation 40 is very close

#check this visually
influenceIndexPlot(fit1, vars=c("Studentized","Bonf")) 

#     i. Finally, check for influential observations, both visually and formally.In cases where there is a violation of normality, this can be caused
#     by outliers and influential observations

#check for influential observations by making a vector of all the data points that have a Cook's distance >1
cd <- cooks.distance(fit1)
inflobs=which(cd>1)
inflobs #the vector is empty, so no influential observations

#check this visually
influenceIndexPlot(fit1, vars=c("Cook")) #again, point 40 pops out as the most influential,
#but its Cook's Distance is well below 1 so
#this is not a problem for us.

#     j. After doing the model diagnostics (e-i above), are you confident in the conclusions based on your model?

#we did not violate any of the model assumptions so we can be confident in our conclusions.


###########################
# EXERCISE 2: CROP YIELDS #
###########################

#     We have an experiment in which crop yields per unit area were measured from 10 randomly 
#     selected fields on each of three soil types. All fields were sown with the same variety 
#     of seed and provided with the same fertilizer and pest control inputs. The question is 
#     whether soil type significantly affects crop yield, and if so, to what extent.

#     Read the data ("yields") and check if everything has been read properly.
#     Inspect the data and make sure you understand what is on the rows and columns.



yields <- read.xlsx("yields.xlsx")
head(yields)

str(yields)

#     a.  Start with inspecting the data visually (that is always a good place to start). Use 'boxplot' for this.

boxplot(yields)
?boxplot

#     b.  Fit a linear model and test if soil type had a significant effect on yield overall.
#         Check the model summary. What do the estimates mean? What about the P-values?

#         Hint about data format: if you want to run a linear model, you need to
#         have the data in such a way that each variable is in one column (the 'stacked' 
#         format). Right now, there is one column for each soil type, but 'soil type' is 
#         just one variable. So we have to transform this dataset first so that we just have
#         two columns: one for the soil type (containing clay, loam or sand) and one for 
#         the yields. You can use the function stack() for this (only works for data frame objects)

#'Stack' the data to get the right format:
yields.stacked <- stack(yields)
names(yields.stacked) <- c("Yield", "Soil")
head(yields.stacked)

#### We have one categorical predictor (Soil) and this predictor should be coded as a 'factor'
#     so that we can use it in a linear model. Usually, text-variables (in this case, Soil can either
#     be "Clay", "Loam' or "Sand") are initially read in R as a character variable (chr). To change this, 
#     us the following code: DATASET$CATEGORICAL_PREDICTOR <- as.factor(DATASET$CATEGORICAL_PREDICTOR).


str(yields.stacked) #     In this case, due to the stacking as an in-between step, Soil is already coded as a factor

#Fit a linear model
fit2 <- lm(Yield ~ Soil, data=yields.stacked)  
summary(fit2)
levels(yields.stacked$Soil)

#the first estimate in the summary table (intercept) is the prediction for the reference level (clay, as this comes first alphabetically), which
#is 11.5. The next estimate (Soilloam) gives the predicted deviation from the reference level of yields on
#loam, whereas the last estimate (Soilsand) gives the predicted deviation from the reference level of yields on
#sand.

#the P-values in the table give the probability that the estimates are different from 0. In the case of the 
#intercept this just means that the yield on clay is different from 0. This is usually not something that we
#are particularly interested in. The next two p-values in the table give the probabilities that the yields on
#loam and sand are different from the reference level. The P-value all the way at the bottom (result of an F-test)
#tests the hypothesis that there is an OVERALL effect of soil type. This p = 0.025, so this means we conclude that
#soil type affects yield.

#We can also check the Anova table of our model
Anova(fit2, type="III")          #we again see the same P-value for the OVERALL effect of soil on yield
#note that we use 'type = "III"' in this code. For this model, the type of Anova that we run on the 
#fit is not important, because we have only one variable. 
#Also, normally we would have to use sum coding to run a type 3 Anova, but again this is not relevant
#if there is only one variable. More on this later in the course.

#     c.  Illustrate the model with an effects plot. Can you recognize the estimates from the summary
#         table in the plot?

#make an effect plot, we add "lty=0" so that there are no lines between the estimates
plot(effect("Soil", fit2),lty=0)
#or
plot(allEffects(fit2), lty=0)


#     d.  Test if the residuals of your model are normally distributed, both visually and formally (with a test).

#first plot the studentized residuals:
hist(rstudent(fit2), probability=T, ylim=c(0,0.4), 
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-3,3,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

#the residuals look quite normal. Let's actually test that with a Shapiro-Wilk W test:
shapiro.test(residuals(fit2))  # p = 0.996 and W=0.99 > 0.9, so no deviation from normality

#     e.  Test for homogeneity of variances, both visually and formally.

#test for homogeneity of variances, first visually:
spreadLevelPlot(fit2)

#test for the homogeneity of variances
ncvTest(fit2) #the variances do not significantly diverge from homogeneous

#     f.  Check if there are any outliers, again both visually and formally.

#test for outliers
outlierTest(fit2)     # there are no significant outliers, although observation 8 is somewhat close

#check this visually
influenceIndexPlot(fit2, vars=c("Studentized","Bonf")) #only observation 8 is close to being an outlier

#     g.  Finally, check for influential observations, both visually and formally.

#check for influential observations by making a vector of all the data points that have a Cook's distance >1
cd <- cooks.distance(fit2)
inflobs=which(cd>1)
inflobs #the vector is empty, so no influential observations

#check this visually
influenceIndexPlot(fit2, vars=c("Cook")) #even observation 8 has a Cook's distance of only ~0.25, nothing to worry about

#     h.  Next, we are interested in WHICH levels are different from each other (as we have >2 levels in our categorical predictor variable). 
#         We can do with posthoc-tests based on our linear model.
#         Do Tukey tests using the emmeans package. What are your conclusions based on this? 

# Tukey HSD posthoc tests:
contrast(emmeans(fit2, ~Soil), method='pairwise', adjust='Tukey')
#clay - loam     -2.8 1.53 27 -1.832  0.1785 
#clay - sand      1.6 1.53 27  1.047  0.5546 
#loam - sand      4.4 1.53 27  2.878  0.0204 *

#only the difference between loam and sand comes out as significant. 
#conclusion: plants growing on loam and sand lead to different yields, whereas there is no detectable difference between
#clay and loam or clay and sand.



