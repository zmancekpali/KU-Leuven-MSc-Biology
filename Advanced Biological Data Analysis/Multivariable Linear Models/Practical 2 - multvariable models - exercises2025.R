###############################################
# EXERCISES PRACTICAL 2: MULTIVARIABLE MODELS #
###############################################

#set your own working directory!
dir <- "C:/Users/Piet/GDrive/Teaching/Advanced Biological Data Analysis/Aanpassingen 2024/Practical 2 - multivariable models/"
setwd(dir)

library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(afex)
library(MuMIn)
library(emmeans)
library(openxlsx)

#we use sum coding (=effects coding) by default from now on - you can just always put this on the top of the code
set_sum_contrasts()

#####################
# EXERCISE 1: SEEDS #
#####################

#     Use the dataset seedset.xls, which contains data for individual plants in the two 
#     populations of Den Bood (DB) and Zaterde Woede (ZW). The variables are their production 
#     of pollen (pollen), their height at flowering (height), the number of flowers 
#     they produced over the season (flowers), the average weight of the seeds
#     (seed.weight) and the number of seeds that were produced (seed.number).

#     Read the data and code the categorical predictors as factors if necessary (use the following code:
#     DATASET$CATEGORICAL_PREDICTOR <- as.factor(DATASET$CATEGORICAL_PREDICTOR).

#    a.  Start with some visual inspection of the data. 
#        You can get a matrix of scatterplots by just using plot() and then providing the
#        columns of the dataset that you want to include (for example DATASET[,3:5] would include columns
#        3 to 5). Include variables flowers, seed.weight and seed.number
#        We are mostly interested in the relationships between seed number and the other variables.
#        Get a first impression of these relationships from the graphs.

#    b.  Which predictor variable best explains the variance in the number of seeds? Construct separate
#        linear models to check the effects of flowers and seed weight on seed number. 
#        Are the slopes significantly different from zero? What are the adjusted R^2 values of each? 
#        What does this mean? Are the coefficients positive or negative? Can you explain why?

#    c.  Now construct a linear model to predict the number of seeds by the number of
#        flowers over the season (flowers) and seed weight (seed.weight). First fit an additive model
#        (no interactions). What proportion of the variance in seed number is explained by this model?
#        Also make effect plots of your model.

#    d.  Now fit a model that also includes an interaction term between flowers and seed.weight. 
#        Also make an effect plot of the model. 

#    e.  Which model is the best, based on their AICc scores? Is it one of the univariable models,
#        the additive model, or the interaction model? Based on the differences in AICc, are any
#        of the other models worth reporting?

#    f.  Check the assumptions of your best model: linearity, homogeneity of variances,
#        normality of residuals, collinearity. Note the models with an interaction term often
#        suffer from collinearity because the main effects are correlated with the interaction
#        terms they appear in. You can avoid this by 'residual centering'. Instead of 
#        checking the colinearity of your fit, you can check the collinearity of 
#        residualCenter(FIT). This has the extra advantage that you can now also more clearly
#        interpret the main effects of your model as the parts of the main effects that are
#        not explained by your interaction. Also check for outliers.

#    g.  What are your general conclusions based on this analysis?



####################
# EXERCISE 2: EELS #
####################

#    In a study by Maes et al. (2005), the authors investigated the relationship 
#    between bioaccumulation of heavy metals, genetic variation and fitness in the 
#    Atlantic eel (Anguilla anguilla). We want to study the influence of genetic variation 
#    (measured as the 'multilocus allozyme heterozygosity' on bioaccumulation of heavy metals
#    in three different river basins (Maas, IJzer, Schelde). 
#    We want to know whether heterozygous individuals can better "detoxify" themselves 
#    compared to homozygous individuals, and we are also interested in how this relationship 
#    might be different between the different river systems.

#    Use the variables HEAVY_METAL_ACCUM, MULTILOCUS_HETEROZYGOSITY_ALLOZYMES and
#    RIVER. Load the dataset (eel) and, check if everything looks OK and code
#    categorical variables as factors.

#    a.  First and foremost, visually explore the data. Make some graphs that you think make sense 
#        to get a better idea of the relationships between the three variables that we're interested in.

#    b.  Build a linear model only including the main effects of river and allozyme heterozygosity. Check
#        the Anova table to see if there are overall effects of your predictors and draw effect plots.
#       In case of an overall effect of river, check between which rivers specifically there are differences

#    c.  Now build a model that also includes the interaction between your predictors, again check
#        the Anova table and represent your model visually. Also check if there are any
#        differences in the effect of heterozygosity on heavy metal accumulation between the different rivers.

#    d.  Which model is the best? Check this with AICc. Are any other models worth
#        reporting?

#    e.  Test for outliers, influential observations, homogeneity of variance, linearity (of continuous predictor), and normality of residuals.
#        Only do this for the best model.

#    f.  What are our conclusions based on our analysis?



#######################
# EXERCISE 3: MINNOWS #
#######################

#    For this dataset, the researchers measured the body length (in mm) of (male) minnows (Phoxinus 
#    laevis) depending on two (crossed) experimental conditions: The type of contaminant in 
#    the water (lead, manganese or chrome), and the type of stress induced (predator 
#    simulation, sound stress, control). For each combination of experimental conditions, 
#    there are two measurements. Load the minnow.csv dataset and check if everything 
#    was read OK. As always, code categorical predictors as factors.

#   a.  As usual, start with some visual explorations.

#   b.  Now build a linear model with both predictors. Include only main effects.
#       Check if there is an overall effect of your variables and make effect plots. 

#   c.  Build a linear model that includes the interaction as well.
#       Again check the Anova table and make effect plots. 

#   d.  Which model is best? Base this on AICc. Do any other models warrant reporting?

#   e.  Do some posthoc analysis. Specifically, check if there are differences between
#       the contamination treatments within each stress treatment.

#   f.  Test assumptions for the best model

#   g.  What are our conclusions based on this analysis?


###################################################################################################################


