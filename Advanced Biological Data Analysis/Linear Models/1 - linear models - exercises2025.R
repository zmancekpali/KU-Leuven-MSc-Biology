##%#########################################################################%##
#                                                                             #
#                   Linear Models Practical - Zoja Manček Páli                #
#                             Date: 25.10.2025                                #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Linear Models")
getwd()

#packages
library(car)
library(multcomp)
library(effects) 
library(emmeans)
library(openxlsx)

#data
metabolism <- read.xlsx("metabolism.xlsx")
yield <- read.xlsx("yields.xlsx")

#EXERCISE 1: RESTING METABOLIC RATE ----
#We have a dataset ('metabolism') on the resting metabolic rate (daily energy 
#expenditure in calories when functioning at rest) for 43 women. We also have 
#the bodyweight (in kilograms) of these women, and would like to ask whether 
#body mass predicts resting metabolic rate (and if so, how).

#Is the data well read? Check if there are 2 variables. Also check the structure 
#of the data with "str". 
head(metabolism)
str(metabolism)

metabolism <- metabolism %>% rename(weight = bodyweight,
                                    mr = resting_mr)

#a. A good place to start examining your data is usually by plotting. You can use 
#the function plot() for this. What is your first impression about the relationship 
#between body weight and resting metabolic rate? 

plot(mr ~ weight, data = metabolism) #looks like mr increases with weight linearly

#b.Construct a linear model to predict metabolic rate based on body weight. 
#What do the estimates and p-values in the summary table mean?
model1 <- lm(mr ~ weight, data = metabolism)
summary(model1) #significant effect of weight on resting metabolic rate
#with every one unit weight increase, we get a 7.437 unit increase in resting mr

#c. Illustrate the model with an effect plot. See if you can recognize your estimates in the plot.
plot(allEffects(model1))

#d.Based on our model, what do we expect the resting metabolic rate to be for 
#a woman of 60 kilos? What about a woman of 80 kg?
#        We can use the function 'predict' for this: predict(MODELFIT, list("PREDICTORVARIABLE"=LEVEL))
predict(model1, list("weight" = 60)) #1232.453 
predict(model1, list("weight" = 80)) #1381.194

#e.Check if the residuals of your model are normally distributed, both visually 
#and formally (with a test).
hist(model1$residuals)
hist(rstudent(model1), probability=T, ylim=c(0,0.4), 
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-3,3,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

shapiro.test(model1$residuals) #p > 0.05, assumption not violated

#f. Check for homogeneity of variances, both visually and formally.
spreadLevelPlot(model1)
ncvTest(model1) #p > 0.05, assumption not violated

#g. Test visually for linearity
residualPlots(model1) #lines are quite straight, not deviation from linearity

#h. Check if there are any outliars, again both visually and formally.
outlierTest(model1) #no significant outliers, although observation 40 is very close
influenceIndexPlot(model1, vars = c("Studentized", "Bonf")) 


#i. Finally, check for influential observations, both visually and formally.
cd <- cooks.distance(model1)
inflobs=which(cd>1)
inflobs #no influential observations

influenceIndexPlot(model1, vars = c("Cook")) #point n40 is most influential but not significant

#j. After doing the model diagnostics (e-i above), are you confident in the conclusions based on your model?
#yes, all is good

#EXERCISE 2: CROP YIELDS ----
#We have an experiment in which crop yields per unit area were measured from 10 randomly 
#selected fields on each of three soil types. All fields were sown with the same variety 
#of seed and provided with the same fertilizer and pest control inputs. The question is 
#whether soil type significantly affects crop yield, and if so, to what extent.

#Read the data ("yields") and check if everything has been read properly.
#Inspect the data and make sure you understand what is on the rows and columns.
head(yield)
str(yield)

yields.stacked <- stack(yield)
names(yields.stacked) <- c("yield", "soil")
head(yields.stacked)
str(yields.stacked)

#a. Start with inspecting the data visually. Use 'boxplot' for this.
boxplot(yield ~ soil, data = yields.stacked)

#b.  Fit a linear model and test if soil type had a significant effect on yield overall.
#Check the model summary. What do the estimates mean? What about the P-values?
model2 <- lm(yield ~ soil, data = yields.stacked)
summary(model2) #overall p < 0.05, significant effect of soil on yield
levels(yields.stacked$soil)

Anova(model2, type="III") #we again see the same P-value for the OVERALL effect of soil on yield

#c.  Illustrate the model with an effects plot. Can you recognize the estimates from the summary
#table in the plot?
plot(allEffects(model2))
plot(allEffects(model2), lty = 0) #lty = 0 removes the line b/w the points

#d. Test if the residuals of your model are normally distributed, both 
#visually and formally (with a test).
hist(model2$residuals)
hist(rstudent(model2), probability=T, ylim=c(0,0.4), 
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-3,3,length=100)
yfit=dnorm(xfit)
lines(xfit, yfit, col="red",lwd=2)

shapiro.test(model2$residuals) #p > 0.05, assumption not violated

#e. Test for homogeneity of variances, both visually and formally.
spreadLevelPlot(model2)
ncvTest(model2) #all good

#f. Check if there are any outliers, again both visually and formally.
outlierTest(model2) #there are no significant outliers, although observation 8 is somewhat close
influenceIndexPlot(model2, vars = c("Studentized", "Bonf")) #only observation 8 is close to being an outlier

#g. Finally, check for influential observations, both visually and formally.
cd <- cooks.distance(model2)
inflobs=which(cd>1)
inflobs #the vector is empty, so no influential observations
influenceIndexPlot(model2, vars = c("Cook")) 
#even observation 8 has a Cook's distance of only ~0.25, nothing to worry about


#h.  Next, we are interested in WHICH levels are different from each other. 
#We can do with posthoc-tests based on our linear model.
#Do Tukey tests using the emmeans package. What are your conclusions based on this? 
contrast(emmeans(model2, ~soil), method = 'pairwise', adjust = 'Tukey')
#clay - loam     -2.8 1.53 27 -1.832  0.1785 
#clay - sand      1.6 1.53 27  1.047  0.5546 
#loam - sand      4.4 1.53 27  2.878  0.0204 *

#only the difference between loam and sand comes out as significant. 
#conclusion: plants growing on loam and sand lead to different yields, 
#whereas there is no detectable difference between clay and loam or clay and sand.





