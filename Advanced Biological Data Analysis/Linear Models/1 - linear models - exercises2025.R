########################################
# EXERCISES PRACTICAL 1: LINEAR MODELS #
########################################

setwd(setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Linear Models"))
getwd()

library(car)
library(multcomp)
library(effects) 
library(emmeans)
library(openxlsx)

# Make sure your working directory is set.

######################################
# EXERCISE 1: RESTING METABOLIC RATE #
######################################

#     We have a dataset ('metabolism') on the resting metabolic rate (daily energy 
#     expenditure in calories when functioning at rest) for 43 women. We also have 
#     the bodyweight (in kilograms) of these women, and would like to ask whether 
#     body mass predicts resting metabolic rate (and if so, how).

#  #  Is the data well read? Check if there are 2 variables. Also check the structure of the data with
#     "str". 

#     a.  A good place to start examining your data is usually by plotting. You can use the function
#         plot() for this. What is your first impression about the relationship between body weight and resting metabolic rate? 

#     b. Construct a linear model to predict metabolic rate based on body weight. What do the estimates and p-values in the summary table mean?

#     c. IIllustrate the model with an effect plot. See if you can recognize your estimates in the plot.

#     d. Based on our model, what do we expect the resting metabolic rate to be for 
#        a woman of 60 kilos? What about a woman of 80 kg?
#        We can use the function 'predict' for this: predict(MODELFIT, list("PREDICTORVARIABLE"=LEVEL))
#        (Replace the text in capitals)

#     e. Check if the residuals of your model are normally distributed, both visually and formally (with a test).

#     f. Check for homogeneity of variances, both visually and formally.

#     g. Test visually for linearity

#     h. Check if there are any outliers, again both visually and formally.

#     i. Finally, check for influential observations, both visually and formally.

#     j. After doing the model diagnostics (e-i above), are you confident in the conclusions based on your model?

###########################
# EXERCISE 2: CROP YIELDS #
###########################

#     We have an experiment in which crop yields per unit area were measured from 10 randomly 
#     selected fields on each of three soil types. All fields were sown with the same variety 
#     of seed and provided with the same fertilizer and pest control inputs. The question is 
#     whether soil type significantly affects crop yield, and if so, to what extent.

#     Read the data ("yields") and check if everything has been read properly.
#     Inspect the data and make sure you understand what is on the rows and columns.

#     a.  Start with inspecting the data visually. Use 'boxplot' for this.

#     b.  Fit a linear model and test if soil type had a significant effect on yield overall.
#         Check the model summary. What do the estimates mean? What about the P-values?

#         Hint about data format: if you want to run a linear model, you need to
#         have the data in such a way that each variable is in one column (the 'stacked' 
#         format). Right now, there is one column for each soil type, but 'soil type' is 
#         just one variable. So we have to transform this dataset first so that we just have
#         two columns: one for the soil type (containing clay, loam or sand) and one for 
#         the yields. You can use the function stack() for this (only works for data frame objects)


#### We have one categorical predictor (Soil) and this predictor should be coded as a 'factor'
#     so that we can use it in a linear model. Usually, text-variables (in this case, Soil can either
#     be "Clay", "Loam' or "Sand") are initially read in R as a character variable (chr). To change this, 
#     use the following code: DATASET$CATEGORICAL_PREDICTOR <- as.factor(DATASET$CATEGORICAL_PREDICTOR).

#     c.  Illustrate the model with an effects plot. Can you recognize the estimates from the summary
#         table in the plot?

#     d.  Test if the residuals of your model are normally distributed, both visually and formally (with a test).

#     e.  Test for homogeneity of variances, both visually and formally.

#     f.  Check if there are any outliers, again both visually and formally.

#     g.  Finally, check for influential observations, both visually and formally.

#     h.  Next, we are interested in WHICH levels are different from each other. 
#         We can do with posthoc-tests based on our linear model.
#         Do Tukey tests using the emmeans package. What are your conclusions based on this? 




