##%#########################################################################%##
#                                                                             #
#            Multivariable Linear Models Practical - Zoja Manček Páli         #
#                              Date: 2.10.2025                                #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Multivariable Linear Models")
getwd()

#packages
library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(afex)
library(MuMIn)
library(emmeans)
library(openxlsx)

#data
minnow <- read.xlsx("minnow.xlsx")
eel <- read.xlsx("eel2.xlsx")
seed <- read.xlsx("seedset.xlsx")

#we use sum coding (=effects coding) by default from now on - 
#you can just always put this on the top of the code
set_sum_contrasts()

# Exercise 1: seeds -------------------------
#seeds contains data for individual plants in the two populations of Den Bood (DB) 
#and Zaterde Woede (ZW). The variables are their production of pollen (pollen), 
#their height at flowering (height), the number of flowers they produced over the 
#season (flowers), the average weight of the seeds (seed.weight), and the number 
#of seeds that were produced (seed.number).

head(seed)
str(seed)

seed <- seed %>% mutate(population = as.factor(population),
                        plant = as.factor(plant))
str(seed)

#a. Start with some visual inspection of the data. you can get a matrix of 
#scatterplots by just using plot() and then providing the columns of the dataset 
#that you want to include (for example DATASET[,3:5] would include columns (3 to 5). 
#Include variables flowers, seed.weight and seed.number. We are mostly interested 
#in the relationships between seed number and the other variables. Get a first 
#impression of these relationships from the graphs.

plot(seed.number ~ flowers, data = seed)
plot(seed.number ~ seed.weight, data = seed)
plot(seed.weight ~ flowers, data = seed)

#b.Which predictor variable best explains the variance in the number of seeds? 
#Construct separate linear models to check the effects of flowers and seed 
#weight on seed number. Are the slopes significantly different from zero? 
#What are the adjusted R^2 values of each? What does this mean? Are the
#coefficients positive or negative? Can you explain why?

model1 <- lm(seed.number ~ flowers, data = seed)
summary(model1)
#the number of flowers seems to have a significant effect on seed number
#slope = 164.42 #seed number increases as the number of flowers increases
#adjusted r^2 = 0.4874

model2 <- lm(seed.number ~ seed.weight, data = seed)
summary(model2)
#seed weight also seems to have a significant effect on seed number
#slope = -84.42 (seed number decreases as seed weight increases)
#adjusted r^2 = 0.3058

#c. Now construct a linear model to predict the number of seeds by the number of
#flowers over the season (flowers) and seed weight (seed.weight). First fit an 
#additive model (no interactions). What proportion of the variance in seed number 
#is explained by this model? Also make effect plots of your model.

model3 <- lm(seed.number ~ flowers + seed.weight, data = seed)
summary(model3) #both predictors have explanatory power (p < 0.05)
#both have an effect, and the additive model explains a more more of the variation
#71.44% of the variation in seed number
Anova(model3, type = "III")

png("effects_plot_additive_1.png", width = 1200, height = 600, res = 120)
plot(allEffects(model3))
dev.off()

#d.Now fit a model that also includes an interaction term between flowers and seed.weight. 
# Also make an effect plot of the model. 

model4 <- lm(seed.number ~ flowers*seed.weight, data = seed)
summary(model4)
#now we see a significant effect of flower number on seed number, but no effect of
#seed weight on seed number. We also see a significant interaction. 
#The r^2 = 75.81%, so this model explains the variation within the data better 
#than the additive model

png("effects_plot_interaction_1.png", width = 1200, height = 600, res = 120)
plot(allEffects(model4))
dev.off()
#each small panel corresponds to a particular value of seed.weight, and within 
#each panel you see how seed number changes as the number of flowers increases.
  #the slope of the line is upward in each facet, meaning that as the number of 
    #flowers increases, the seed number tends to increase -> more flowers = 
    #more seeds (which makes biological sense).
  #The intercepts (starting values on the y-axis) differ by panel. Panels with 
    #lower seed.weight (e.g. 9.7) show much higher predicted seed numbers overall — 
    #interestingly, that suggests that, in your data, plants with lighter seeds 
    #may produce more total seeds.Panels with higher seed.weight (25, 30) show 
    #flatter slopes — meaning flowers still have a positive effect, but the total 
    #seed number is smaller or grows more slowly.
  #If the slopes differ between panels, that implies an interaction effect between 
    #flowers and seed.weight. In your plot, the slope seems steeper at lower seed 
    #weight and flatter at higher seed weight.
    #Interpretation: The positive effect of flower number on seed number becomes 
    #weaker as seed weight increases. In other words, heavy seeds might come at 
    #the cost of producing many seeds.)
  #The relationship between flower number and seed number depends on seed weight.
    #When seed weight is low, the slope of flowers → seed.number is steep — more 
    #flowers strongly increase seed number.When seed weight is high, the slope 
    #flattens — adding more flowers doesn’t boost seed number much. This suggests a 
    #trade-off between seed size and seed quantity: plants investing in larger 
    #seeds produce fewer overall.


#e. Which model is the best, based on their AICc scores? Is it one of the univariable 
#models, the additive model, or the interaction model? Based on the differences in 
#AICc, are any of the other models worth reporting?

models_list <- list(model1, model2, model3, model4)
AICc(model1, model2, model3, model4)
model.sel(models_list)
#model4 is best, nothing extra to report because all the other models are more than 
#2 AICc scores away from it
#model4 has an Aikake weight of 1

#f. Check the assumptions of your best model: linearity, homogeneity of variances,
#normality of residuals, collinearity. Note the models with an interaction term often
#suffer from collinearity because the main effects are correlated with the interaction
#terms they appear in. You can avoid this by 'residual centering'. Instead of 
#checking the colinearity of your fit, you can check the collinearity of 
#residualCenter(FIT). This has the extra advantage that you can now also more clearly
#interpret the main effects of your model as the parts of the main effects that are
#not explained by your interaction. Also check for outliers.

  #normality of residuals
  hist(model4$residuals)
  
  hist(rstudent(model4), probability = T, xlab = "Studentised residuals", 
       main = "Distribution of Studentised Residuals")
  
  r = model4$residuals
  h <- hist(model4$residuals, breaks = 10, density = 20,
            col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
  xfit <- seq(min(r), max(r), length = 40) 
  yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(r) 
  lines(xfit, yfit, col = "red", lwd = 2)
  
  shapiro.test(residuals(model4)) #p not significant, W = 0.96147; assumption not violated
  
  #homogeneity of variances
  spreadLevelPlot(model4) #looks normal-ish
  ncvTest(model4) #p-value > 0.05, variances homogeneous; asummption not violated
  
  #linearity and collinearity
  residualPlots(model4) #looks ok
  vif(model4, type = 'predictor') #GVIF not > 5; no collinearity
  
  #outliers:
  outlierTest(model4) #no residuals with p < 0.05
  influenceIndexPlot(model4, vars = c("Studentized","Bonf"))  #no significant outliers 
  #(no p-values < 0.05)
  #point n15 is the furthest from the rest of the data, but not significant
  
  cd <- cooks.distance(model4)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model4, vars = c("Cook"))
  #point n89 has the highest cook's distance but not > 1
  

#g.What are your general conclusions based on this analysis?
  #Flower number is the strongest driver of seed production — plants with more 
  #flowers produce significantly more seeds. Seed weight alone shows a weak, 
  #negative relationship with seed number, but not independently significant in the full model.
  #There is a significant interaction between flower number and seed weight:
    #At low seed weights, the positive relationship between flowers and seed number is strong.
    #At high seed weights, this relationship weakens.
  #This supports a biological trade-off: plants investing in heavier seeds produce 
  #fewer seeds overall.
  #The interaction model (model4) provides the best statistical fit (lowest AICc, highest R² ≈ 76%).
  #All regression assumptions were met — the model is statistically sound and interpretable.

#Exercise 2: eels ------
#In a study by Maes et al. (2005), the authors investigated the relationship 
#between bioaccumulation of heavy metals, genetic variation and fitness in the 
#Atlantic eel (Anguilla anguilla). We want to study the influence of genetic variation 
#(measured as the 'multilocus allozyme heterozygosity' on bioaccumulation of heavy metals
#in three different river basins (Maas, IJzer, Schelde). We want to know whether 
#heterozygous individuals can better "detoxify" themselves compared to homozygous 
#individuals, and we are also interested in how this relationship might be different 
#between the different river systems.
  
head(eel)
str(eel)

eel <- eel %>% mutate(RIVER = as.factor(RIVER),
                      SUBPOPULATION = as.factor(SUBPOPULATION),
                      SUBPOPULATION_IN_RIVER = as.factor(SUBPOPULATION_IN_RIVER),
                      ind = as.factor(ind)) %>% 
              rename(river = RIVER, 
                     subpop = SUBPOPULATION,
                     river_subpop = SUBPOPULATION_IN_RIVER,
                     hm_conc = HEAVY_METAL_ACCUM,
                     cond_index = CONDITION_INDEX,
                     hema_index = HEMATOSOMATIC.INDEX,
                     allozyme = MULTILOCUS_HETEROZYGOSITY_ALLOZYME,
                     microsat = MULTILOCUS_HETEROZYGOSITY_MICROSAT) #renaming is
                     #not necessary but makes it easier

#a. First and foremost, visually explore the data. Make some graphs that you think make sense 
#to get a better idea of the relationships between the three variables that we're interested in.
boxplot(hm_conc ~ river, data = eel)
plot(hm_conc ~ allozyme, data = eel)
boxplot(allozyme ~ river, data = eel)


#b. Build a linear model only including the main effects of river and allozyme heterozygosity. Check
#the Anova table to see if there are overall effects of your predictors and draw effect plots.
#In case of an overall effect of river, check between which rivers specifically there are differences
model5 <- lm(hm_conc ~ allozyme + river, data = eel)
summary(model5)
#intercept = baseline hm_conc in River 3 when allozyme heterozygosity is 0
#allozyme = significant negative effect -> eels with higher heterozygosity have lower [hm]
#river1 = difference in [hm] between river 1 and 3 -> river 1's eels have lower [hm] than
  #those from river 3
#river 2 = difference in [hm] between river 2 and 3 -> river 3's eels have higher [hm]
  #than those from river 3
#the model itself is significant (p < 0.05) and explains 17.05% of the variation within data
Anova(model5, type = "III")
#both heterozygosity and river affect [hm]

png("effects_plot_additive_2.png", width = 1200, height = 600, res = 120)
plot(allEffects(model5))
dev.off()


#c. Now build a model that also includes the interaction between your predictors, again check
#the Anova table and represent your model visually. Also check if there are any
#differences in the effect of heterozygosity on heavy metal accumulation between the different rivers.

model6 <- lm(hm_conc ~ allozyme*river, data = eel)
summary(model6)
#heterozygosity has a significant negative effect on [hm] -> eels with higher 
  #heterozygosity have lower [hm]
#the heterozygosity (allozyme) = in river 3, higher heterozygosity = 
  #significantly lower heavy metal concentration
#rivers 1 and 2 dont differ significantly from river 3
#this model explains even less of the variation (15.52%), though it is still s
  #ignificantly explanatory of the trends we observe
#The effect of heterozygosity in River 1 is not significantly different from River 3
#The effect of heterozygosity in River 2 is not significantly different from River 3

Anova(model6, type = "III")
  #allozyme: Significant main effect — heterozygosity affects heavy metal concentration overall
  #river: Not significant — no overall difference between rivers after accounting for heterozygosity
  #allozyme*river :Not significant — the effect of heterozygosity does not differ between rivers

png("effects_plot_interaction_2.png", width = 1200, height = 600, res = 120)
plot(allEffects(model6))
dev.off()


#d. Which model is the best? Check this with AICc. Are any other models worth reporting?
AICc(model5, model6)
models_list2 <- list(model5, model6)
model.sel(models_list2)
#model 5 appears best based on the AICc and the AIkake weight (0.881)
#nothing else worth noting -> the AICc difference is > 2


#e. Test for outliers, influential observations, homogeneity of variance, 
#linearity (of continuous predictor), and normality of residuals. Only do this for the best model.
  #normality of residuals
  hist(model5$residuals)
  
  hist(rstudent(model5), probability = T, xlab = "Studentised residuals", 
       main = "Distribution of Studentised Residuals")
  
  r = model5$residuals
  h <- hist(model5$residuals, breaks = 10, density = 20,
            col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
  xfit <- seq(min(r), max(r), length = 40) 
  yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(r) 
  lines(xfit, yfit, col = "red", lwd = 2)
  
  shapiro.test(residuals(model5)) #p not significant, W = 0.96953; assumption not violated
  
  #homogeneity of variances
  spreadLevelPlot(model5) #looks normal-ish
  ncvTest(model5) #p-value > 0.05, variances homogeneous; assummption not violated
  
  #linearity and collinearity
  residualPlots(model5) #looks ok for the continuous predictor (allozyme)
  vif(model4, type = 'predictor') #GVIF not > 5; no collinearity
  
  #outliers:
  outlierTest(model5) #no residuals with p < 0.05
  influenceIndexPlot(model5, vars = c("Studentized","Bonf"))  #no significant outliers 
  #(no p-values < 0.05)
  #points n55 and 57 are the furthest from the rest of the data, but not significant
  
  cd <- cooks.distance(model5)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model5, vars = c("Cook"))
  #point n57 has the highest cook's distance but not > 1


#f.  What are our conclusions based on our analysis?
#model5 was best at determining the relationship between [hm] and heterozygosity
  #generally: eels with more heterozygosity show lower [hm]. The model is addtive,
  #so no interaction. 
  #Both predictors are significant: 
    #Heterozygosity: Negative relationship — eels with higher genetic diversity 
      #accumulate less heavy metal.
    #River: Significant overall differences in heavy metal concentration among rivers
  #The interaction model (model6) does not improve model fit (ΔAICc > 2, 
    #non-significant interaction): the effect of heterozygosity is consistent across rivers.
  #The model is robust and valid (no assumptions violated), although it only explains 
    #~21% of the variation within the data
  
  
#Exercise 3: minnows -------
# For this dataset, the researchers measured the body length (in mm) of (male) minnows (Phoxinus 
# laevis) depending on two (crossed) experimental conditions: The type of contaminant in 
# the water (lead, manganese or chrome), and the type of stress induced (predator 
# simulation, sound stress, control). For each combination of experimental conditions, 
# there are two measurements. Load the minnow.csv dataset and check if everything 
# was read OK. As always, code categorical predictors as factors.

head(minnow)
str(minnow)

minnow <- minnow %>% mutate(STRESS = as.factor(STRESS), 
                            #stress level 1: control
                            #stress level 2: predation
                            #stress level 3: sound
                      CONTAMINATION = as.factor(CONTAMINATION)) %>% 
                      #cont level 1: chrome
                      #cont level 2: lead
                      #cont level 3: manganese
      rename(stress = STRESS, 
         contaminant = CONTAMINATION,
         body_length = BODY.LENGTH) #renaming is not necessary but makes it easier

str(minnow)

#a. As usual, start with some visual explorations.
boxplot(body_length ~ contaminant, data = minnow)
boxplot(body_length ~ stress, data = minnow)

#b. Now build a linear model with both predictors. Include only main effects.
#Check if there is an overall effect of your variables and make effect plots. 

model7 <- lm(body_length ~ contaminant + stress, data = minnow)
summary(model7)
#intercept gives the baseline mean body length under chrome contamination with no stress (control)
#contaminant1: No significant difference in body length between lead and chrome exposure
#contaminant2: significant difference between manganese and chrome exposure body lengths
  #Minnows exposed to mercury are about 23.5 mm shorter than those exposed to chrome
#stress1: Under predation stress, minnows are on average ~42 mm longer than control fish
#stress2: no significant difference between sound stress and control
  #Sound stress doesn’t significantly affect body length compared to the control
#adjusted r^2 = 86.13% of variation explained
#model p-value < 0.05

Anova(model7, type = "III")
#both predictors have an effect on body length of minnows

png("effects_plot_additive_3.png", width = 1200, height = 600, res = 120)
plot(allEffects(model7))
dev.off()


#c. Build a linear model that includes the interaction as well. Again check the Anova 
#table and make effect plots. 

model8 <- lm(body_length ~ contaminant*stress, data = minnow)
summary(model8)
#Intercept: baseline minnow body length in chrome contamination and no stress
#cont1: no difference in body length between lead and chrome
#cont2: significant difference between chrome and manganese contamination body lengths
  #Manganese exposure causes ~23.5 mm shorter minnows than chrome
#stress1: significant difference between control and predation
  #Predation stress produces much longer minnows than control (~42 mm longer)
#stress2: no significant difference in body lengths in minnows under control or sound stress
#cont1*stress1: Not significant — predation’s effect same under lead and chrome
#cont2*stress1: significant negative interaction: predation increases length less under manganese
#cont1:stress2: not significant (lead/sound)
#cont2:stress2: not significant (manganese/sound)
#r^2 = 89.2%, model significant

#The baseline (chrome + control) minnows average about 90.7 mm in length.
#Contaminant effects:
  #Lead: no change vs chrome.
  #Manganese: strong reduction in body length (–23 mm).
#Stress effects:
  #Predation: strongly increases length (+42 mm).
  #Sound: no effect.
#Interaction:
  #Only the manganese × predation term is significant.
#That means predation stress normally increases body length, but this effect is 
#weaker when fish are exposed to manganese — manganese toxicity dampens the positive 
#growth response to predation pressure.

Anova(model8, type = "III")
#overall, both stress and contaminants have an effect, as does their interaction

png("effects_plot_interaction_3.png", width = 1200, height = 600, res = 120)
plot(allEffects(model8))
dev.off()



#d. Which model is best? Base this on AICc. Do any other models warrant reporting?
AICc(model7, model8)
models_list3 <- list(model7, model8)
model.sel(models_list3)
#model8 is better; nothing to report
#Aikake weight of model8 = 0.967

#e.Do some posthoc analysis. Specifically, check if there are differences between
#the contamination treatments within each stress treatment.
contrast(emmeans(model8, ~contaminant), method = 'pairwise', adjust = 'Tukey') #posthoc comparisons
#Minnows under chrome are ~22 mm longer than those under lead, significant
#Minnows under chrome are ~27 mm longer than those under manganese, significant
#Minnows under lead are ~49 mm longer than those under manganese, highly significant

#Chrome > Lead > Manganese in terms of mean body length.
  #So:
    #Manganese causes the greatest growth suppression,
    #Lead causes a moderate reduction,
    #Chrome (baseline) fish are largest overall.

#cant infer much from this because there is a significant interaction between contaminant 
  #and stress:
contrast(emmeans(model8, ~contaminant | stress), method = 'pairwise', adjust = 'Tukey') #posthoc comparisons
#Under control:
  #minnows under chrome are 33 mm longer than minnows under lead; significant
  #minnows under chrome are 39.5 mm shorter than minnows under manganese
  #minnows under lead are 72.5 mm shorter than minnows under lead
    #Under control (no stress): lead < chrome < manganese minnows

#Under predation:
  #minnows under chrome are 25.8 mm longer than minnows under lead, significant
  #minnows under chrome are 20.7 mm shorter than minnows under manganese, significant
  #minnows under lead are 46.5 mm shorter than minnows under manganese, significant
    #under predation: lead < chrome < manganese

#Under sound stress:
  #Under chrome, minnows are 7 mm longer than minnows under lead, not significant
  #under chrome, minnows are 19.8mm shorter than under manganese, significant
  #under lead, minnows are 26.8 mm shorter than under manganese, significant
    #under sound: lead < chrome < manganese 

  
#f.Test assumptions for the best model
  #normality of residuals
  hist(model8$residuals)
  
  hist(rstudent(model8), probability = T, xlab = "Studentised residuals", 
       main = "Distribution of Studentised Residuals")
  
  r = model8$residuals
  h <- hist(model8$residuals, breaks = 10, density = 20,
            col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
  xfit <- seq(min(r), max(r), length = 40) 
  yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(r) 
  lines(xfit, yfit, col = "red", lwd = 2)
  
  shapiro.test(residuals(model8)) #p not significant, W = 0.97521; assumption not violated
  
  #homogeneity of variances
  spreadLevelPlot(model8) #looks weird
  ncvTest(model8) #p-value > 0.05, variances homogeneous; asummption not violated
  
  #linearity and collinearity
  residualPlots(model8) #looks ok
  vif(model8, type = 'predictor') #GVIF not > 5; no collinearity
  
  #outliers:
  outlierTest(model8) #no residuals with p < 0.05
  influenceIndexPlot(model8, vars = c("Studentized","Bonf"))  #no significant outliers 
  #(no p-values < 0.05)
  #point n53 is the furthest from the rest of the data, but not significant
  
  cd <- cooks.distance(model8)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model8, vars = c("Cook"))
  #point n53 has the highest cook's distance but not > 1


#g.What are our conclusions based on this analysis?
#Across all stress conditions, body length differs significantly among contaminants. 
#Under control conditions, manganese-exposed minnows are the largest, chrome 
#intermediate, and lead smallest. This pattern remains under predation and sound stress, 
#though the differences are slightly reduced under predation and weakest under sound. 
#The consistently positive differences for “manganese vs others” suggest that, while 
#manganese exposure generally produced shorter fish on average in the main model, 
#the interaction with stress means manganese fish respond differently under varying 
#stress conditions—possibly showing compensatory or selective survival effects depending 
#on environment.
