############################################################################
# EXERCISES PRACTICAL 3: GENERALIZED LINEAR MODELS (AND POLYNOMIAL MODELS) #
############################################################################

#set your own working directory!
dir <- "D://GDrive/Teaching/Advanced Biological Data Analysis/Practicals/Practical 3 - generalized linear models (and polynomial models)/"
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

#   b.  Construct a simple linear model to see if there is an effect of carapace length on clutch weight.
#       Is there a significant effect? Also visualize your model with an effect plot.

#   c.  Produce residualPlots of your model. Does the linear model you just made describe the relationship
#       between your predictor and your response variable well?

#   d.  Start adding polynomial terms to your model, starting with second order, and stop when AICc no longer improves.
#       Visualize the best model based on AICc.

#   e.  Perform model diagnosis: make residual plots and see if predictions are biased, test whether residuals 
#       are normally distributed, test homogeneity of variances, and test whether there are outliers and/or 
#       influential observations.

#   f.  What are your conclusions based on this analysis?



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

#   b.  Run a generalized linear model including only the main effects of area and distance on the presence 
#       of the species. Specify the appropriate error structure and link function of the model. Visualize the results. 

#   c.  Now also include the interaction. Which model is best? Visualize the best model if you haven't yet done so.

#   d.  According to our best model, what is the probability that the species occurs on an island with a surface are of
#       3 km^2 and that is 8 km from the mainland? How about an island of 6 km^2 that is 4 km from the mainland?
#       Use the 'predict' function for this. 

#   e.  Do the model diagnostics for our best model: linearity on the transformed scale, collinearity,
#       overdispersion, outliers/influential observations.

#   f.  What are your conclusions based on this analysis?



###################################
# EXERCISE 3: DAMAGED BLOOD CELLS #
###################################

#     We have a dataset ("bloodcells") with information on the number of damaged bloodcells per mm^2 for a 
#     number of individuals. We also have information on their weight, their sex, and whether they are a smoker.
#
#   a.  Read the data check that everything is ok, and start with some visual inspections 

#   b.  We would like to construct a model to predict the effects of smoking, sex and weight on the number
#       of damaged blood cells. What would be an appropriate type of model in this case? Why?
#       Construct a model that includes all main effects but no interactions, and visualize the model.

#   c.  Now fit a model that also includes the interaction between smoker and weight. Is the interaction significant?
#       What does it mean? Visualize the model. Which model is better, the one with or without the interaction?
 
#   d.  Now check all models with all possible combinations of one-way interactions. Which has the best AICc?
#       Present this model visually and perform model diagnosis: linearity on the transformed scale, collinearity
#       overdispersion, outliers/influential observations.

#   e.  What are your conclusions based on this analysis?

#######################################################################################################################

#   f.  OPTIONAL: You just ran all possible models with all possible combinations of one-way interactions manually.
#       There are also ways to do this automatically - much more convenient. You can do this with the package
#       'glmulti' - install it with install.packages("glmulti") and then load it with library(glmulti).

#       NOTE: you need "RJava" for this package. Sometimes you already have it, sometimes it still needs to be installed.
#       On windows, you can just install it with "install.packages", but it is more difficult on mac. If you have a 
#       mac, you can try yourself to install it, but if it is too difficult, we suggest to skip this part of the 
#       exercise for now. glmulti will not be part of the exam.

#       Once you installed glmulti, you need to use the function that is also called glmulti. 
#       You will have to give a few input arguments. First, you need to specify a model (just like you would do in glm)
#       in the form "RESPONSE VARIABLE ~ PREDICTOR1 + PREDICTOR2 (etc), family = RESIDUAL_DISTRIBUTION". 
#       Don't include interactions - glmulti will automatically consider all one-way interactions between the predictors 
#       that you gave. Also specify your data like in glm (data = MYDATA). Then you should specify how many best models 
#       you want to save. In our case saving the 5 best models is enough. Do this with the argument confsetsize = NUMBER.
#       Finally, you should give the information criterion you want to use, in our case AICc. You can do this with 
#       crit= "aicc". Save the output of glmulti in a variable, something like BEST_MODEL_OUTPUT <- glmulti(ARGUMENTS).

#       After you run that, you can check the top 5 models with BEST_MODEL_OUTPUT@formulas
#       You can also save your best model like this: BESTFIT <- BEST_MODEL_OUTPUT@objects[[1]]
#       Then you can just check the summary and Anova and do effect plots of that model like normal.

#       Based on this algorithm, what is the best model? Is it the same model as you got before (under e)?



