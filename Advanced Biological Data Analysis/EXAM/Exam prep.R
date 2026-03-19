##%#########################################################################%##
#                                                                             #
#                       Exam prep - Zoja Manček Páli                          #
#                              Date: 25.1.2026                                #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/EXAM")
getwd()

#all packages
library(afex)
library(factoextra)
library(car)
library(tidyr)
library(dplyr)
library(effects) 
library(emmeans)
library(ggplot2)
library(ggpubr)
library(lattice)
library(lmtest)
library(multcomp)
library(MuMIn)
library(openxlsx)
library(rockchalk)
library(tidyverse)
library(rJava)
library(glmulti)
library(rockchalk)
library(scales)
library(nlme)
library(nnet)
library(reshape2)
library(robustbase)
library(MASS)
library(Matrix)
library(nlstools)
library(nlsMicrobio) # contains some bacterial growth data and fit functions
library(investr) # for plotFit function
library(MultiKink)
library(splines)
library(scales) #see through points on scatter plot
library(survival)
library(ggfortify)
library(KMsurv)
library(flexsurv)
library(Rmisc)
library(ggthemes)
library(ggsignif)
library(gridExtra)
library(ggprism)
library(scales)
library(svglite)
library(gtools) # for the stars.pval function
library(digest)
library(devtools)
library(ggbiplot)
library(vegan)
library(gclus)
library(ade4)

#Linear models ----
#data
metabolism <- read.xlsx("data/metabolism.xlsx")
yield <- read.xlsx("data/yields.xlsx")

#does metabolic rate change with weight?
  head(metabolism)
  str(metabolism)
  
  plot(resting_mr ~ bodyweight, data = metabolism) #looks linear
  (model1 <- lm(resting_mr ~ bodyweight, data = metabolism))
  summary(model1) #slope > 0, positive linear relationship
  #p < 0.05; significant effect of bw on rmr
  #r^2 = 0.522 *adjusted -> model explains 52.2% of the variance in the data
  #intercept = 786.229 (= rmr of person at 0kg -> nonsensical here)
  #bw estimate = 7.437 (= for every 1kg increase in weight, the rmr increases by 7.437 calories)
  #t-value = probability that estimate =/ 0
  
  #nicer plot:
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
  
  predict(model1, list("bodyweight" = 60)) #resting mr of 1232.453 calories
  predict(model1, list("bodyweight" = 80)) # 1381.194 calories
  
  #test assumptions:
    plot(model1)
    
    #normality of residuals
    ggqqplot(metabolism$bodyweight)
    ggqqplot(metabolism$resting_mr)
    
    plot(model1, which = 2)
    
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

    #homogeniety of variances
      spreadLevelPlot(model1) #looks normal-ish (nonconstant variance test)
      ncvTest(model1) #p-value > 0.05, variances homogeneous
      #no deviation from homogeneity, asummption not violated
      
      lmtest::bptest(model1) #Breusch-Pagan test; all ok
      plot(model1, which = 1) #looks ok-ish
      plot(model1, which = 3) #more sensitive than plot1; looks better
      
    #linearity
      residualPlots(model1) #looks ok
      plot(model1, which = 1) #looks ok-ish
  
    #outliers
      plot(model1, which = 4) #gives influential observations (40 most influential)
      outlierTest(model1) #outliers affecting the model are not significant  (marginally)
      influenceIndexPlot(model1,vars = c("Studentized", "Bonf"))  #no significant outliers (no p-values < 0.05)
      #point n40 is the furthest from the rest of the data, but not significant
      
      cd <- cooks.distance(model1)
      (inflobs = which(cd > 1)) #no influential points (none with Cook's distance > 1)
      
      influenceIndexPlot(model1,vars = c("Cook"))
      #point n40 has the highest cook's distance but not >1
      
  #conclusion: bodyweight has a significant positive linear effect on resting metabolic rate

#does soil type impact crop yields?
  head(yield)
  str(yield) #three numeric variables
  
  boxplot(yield) #you can see that there are three individual soil categories, 
  #but we want to see if soil type affects crop yields so we need to transform the dataset:
  yield_long <- yield %>%
    pivot_longer(cols = c(clay, loam, sand),
                 names_to = "soil_type", 
                 values_to = "yield_value")  %>% 
    mutate(soil_type = factor(soil_type))
  
  str(yield_long)
  
  plot(yield_value ~ soil_type, data = yield_long)

  (model2 <- lm(yield_value ~ soil_type, data = yield_long))  
  summary(model2)
  #adjusted r^2 is only 0.1829
  Anova(model2, type="III") #soil type does have an effect (p-value < 0.05)

  plot(allEffects(model2)) 
  
  emmeans(model2, ~soil_type)
  contrast(emmeans(model2, ~soil_type), method = 'pairwise', adjust = 'Tukey') 
  #significant difference only between loam and sand; otherwise insiginifcant
  
  #test assumptions:
  plot(model2)
  
    #normality of residuals
    ggqqplot(yield_long$yield_value)
    
    plot(model2, which = 2)
    
    hist(model2$residuals)
    
    hist(rstudent(model2), probability = T, xlab = "Studentised residuals", 
         main = "Distribution of Studentised Residuals")
    
    r = model2$residuals
    h <- hist(model2$residuals, breaks = 10, density = 20,
              col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
    xfit <- seq(min(r), max(r), length = 40) 
    yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
    yfit <- yfit * diff(h$mids[1:2]) * length(r) 
    lines(xfit, yfit, col = "red", lwd = 2)
    
    shapiro.test(residuals(model2)) #p not significant, W = 0.96147; if W > 0.9, then normal distribution can be assumed
    #residuals are not exhibiting any deviation from a normal distribution, assuption not violated

    #homogeniety of variances
    spreadLevelPlot(model2) #looks normal-ish (nonconstant variance test)
    ncvTest(model2) #p-value > 0.05, variances homogeneous
    #no deviation from homogeneity, asummption not violated
    
    lmtest::bptest(model2) #Breusch-Pagan test; all ok
    plot(model2, which = 1) #looks ok-ish
    plot(model2, which = 3) #more sensitive than plot1; looks better
    
    #linearity
    residualPlots(model2) #looks ok
    plot(model2, which = 1) #looks ok-ish
    
    #outliers
    plot(model2, which = 4) #gives influential observations (22 most influential)
    outlierTest(model2) #outliers affecting the model are not significant
    influenceIndexPlot(model2,vars = c("Studentized", "Bonf"))  #no significant outliers (no p-values < 0.05)
    #point n22 is the furthest from the rest of the data, but not significant
    
    cd <- cooks.distance(model2)
    (inflobs = which(cd > 1)) #no influential points (none with Cook's distance > 1)
    
    influenceIndexPlot(model2, vars = c("Cook"))
    #point n22 has the highest cook's distance but not >1
  
  #conclusion: soil type has a significant effect on yield
    

    

#Multivariate linear models ----
#data
minnow <- read.xlsx("Data/minnow.xlsx")
eel <- read.xlsx("Data/eel2.xlsx")
seed <- read.xlsx("Data/seedset.xlsx")

set_sum_contrasts()

#which predictor of flower success best describes the variation in seed number?
  head(seed)
  str(seed)
  
  seed <- seed %>% mutate(population = as.factor(population),
                          plant = as.factor(plant))
  str(seed)
  
  plot(seed[,6:8]) #includes all rows and only columns 6 - 8 (inclusive)
  #aka: flowers, seed.weight, and seed.number
  
  plot(seed.number ~ flowers, data = seed) #looks to be linearly increasing w flower number
  plot(seed.number ~ seed.weight, data = seed) #looks to decrease w seed weight
  plot(seed.weight ~ flowers, data = seed) #random but slight decrease with flower number
  
  #models
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
  
  model3 <- lm(seed.number ~ flowers + seed.weight, data = seed)
  summary(model3) #both predictors have explanatory power (p < 0.05)
  #both have an effect, and the additive model explains a more more of the variation
  #71.44% of the variation in seed number
  Anova(model3, type = "III")
  
  plot(allEffects(model3)) 

  #multiplicative model
  model4 <- lm(seed.number ~ flowers*seed.weight, data = seed)
  summary(model4) #interaction is significant; seed.weight not anymore
  #The r^2 = 75.81%, so this model explains the variation within the data better 
  #than the additive model
  
  plot(allEffects(model4), confint = list(style = "auto"))
  #each small panel corresponds to a particular value of seed.weight, and within 
  #each panel you see how seed number changes as the number of flowers increases.
  #the slope of the line is upward in each facet, meaning that as the number of 
  #flowers increases, the seed number tends to increase -> more flowers = 
  #more seeds (which makes biological sense).
  #The intercepts (starting values on the y-axis) differ by panel. Panels with 
  #lower seed.weight (e.g. 9.7) show much higher predicted seed numbers overall — 
  #interestingly, that suggests that plants with lighter seeds 
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
  plot(allEffects(model4), multiline = T, confint = list(style = "auto"))
  
  #AIC
  models_list <- list(model1, model2, model3, model4)
  AICc(model1, model2, model3, model4) #model4 has the lowest AICc
  model.sel(models_list) #model4 is best
  
  #assumptions:
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
    
    shapiro.test(residuals(model4)) #p not significant, W = 0.99091; assumption not violated
    
    #homogeneity of variances
    spreadLevelPlot(model4) #looks normal-ish
    ncvTest(model4) #p-value > 0.05, variances homogeneous; assummption not violated
    
    #linearity and collinearity
    residualPlots(model4) #looks ok
    vif(model4) #GVIF > 5; collinearity!
    
    #need to center:
    model4c <- residualCenter(model4)
    
    vif(model4c) #better
    summary(model4c)
    summary(model4) #compared to the initial interaction model, 
    #the main effect of seed.weight now has a significant effect
    
    #in case of collineatirity, we should interpret the p-values of the model with residual centering
    Anova(model4c, type = "III") #all significant
    
    
    #outliars:
    outlierTest(model4) #no residuals with p < 0.05
    influenceIndexPlot(model4, vars = c("Studentized","Bonf"))  #no significant outliars 
    #point n15 is the furthest from the rest of the data, but not significant
    
    cd <- cooks.distance(model4)
    (inflobs = which(cd > 1)) #no influential points (none with Cook's distance > 1)
    
    influenceIndexPlot(model4, vars = c("Cook"))
    #point n89 has the highest cook's distance but not > 1
  
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
    
    #The number of flowers produced over the season is a strong determinant of how 
    #many seeds will be produced, and there is also a significant main effect of 
    #the seed weight on the number of seeds. However, there is a trade-off between 
    #the number and weight of the seeds. Hence, you can see that the relationship 
    #between flower number and seed number is only apparent for light seeds and not for heavy ones.
    
  
#Effect of genetic heterozygosity on heavy metal bioaccumulation in eels
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
  
  eel <- mutate_if(eel, is.character, as.factor) #can also do this

  boxplot(hm_conc ~ river, data = eel)
  plot(hm_conc ~ allozyme, data = eel)
  boxplot(allozyme ~ river, data = eel)
  
  par(mfrow=c(1,3))
  plot(hm_conc ~ allozyme, data = subset(eel, eel$river ==  "IJZER"), col = "red", cex = 2, pch = 16)
  plot(hm_conc ~ allozyme, data = subset(eel, eel$river == "MAAS"), col = "blue", cex = 2, pch = 16)
  plot(hm_conc ~ allozyme, data = subset(eel, eel$river == "SCHELDE"), col = "green", cex = 2, pch = 16)
  
  par(mfrow=c(1,1))

  #lattice package
  xyplot(hm_conc ~ allozyme, data = eel, group = eel$river, type = c("p", "r"), 
         col = c(2, 3, 4), cex = 1.8, pch = 16, 
         key = list(text = list(levels(eel$river)), space = "right", 
                    points = list(pch = 16, cex = 1.8, col = c(2, 3, 4)))) #this is WAY better

  #additive model
  model5 <- lm(hm_conc ~ allozyme + river, data = eel)
  summary(model5)
  #intercept = baseline hm_conc in River 3 when allozyme heterozygosity is 0
  #allozyme = significant negative effect -> eels with higher heterozygosity have lower [hm]
  #river1 = difference in [hm] between river 1 and 3 -> river 1's eels have lower [hm] than
  #those from river 3
  #river 2 = difference in [hm] between river 2 and 3 -> river 2's eels have higher [hm]
  #than those from river 3
  #the model itself is significant (p < 0.05) and explains 17.05% of the variation within data
  Anova(model5, type = "III") #both river and heterozygosity have a significant effect
  
  contrast(emmeans(model5, ~river), method = "pairwise", adjust = "tukey")
  #sig differences between IJZER - MAAS (negative) & MAAS - SCHELD (positive)
  #no significant difference b/w IJZER - SCHELDE (negative effect)
  
  plot(allEffects(model5))
  
  #multiplicative model
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
  interaction.plot(eel$river, eel$allozyme, eel$hm_conc)
  #allozyme: Significant main effect — heterozygosity affects heavy metal concentration overall
  #river: Not significant — no overall difference between rivers after accounting for heterozygosity
  #allozyme*river: Not significant — the effect of heterozygosity does not differ between rivers
  
  contrast(emtrends(model6, "river", var = "allozyme"), method = "pairwise", adjust = "tukey")
  #no significant differences b/w the rivers
  
  plot(allEffects(model6), multiline = T, ci.style = "band") 
  plot(allEffects(model6))
  
  #AICc
  AICc(model5, model6)
  models_list2 <- list(model5, model6)
  model.sel(models_list2)
  #model 5 appears best based on the AICc and the AIkake weight (0.881)
  #nothing else worth noting -> the AICc difference is > 2
  
  #assumptions
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
    ncvTest(model5) #p-value > 0.05, variances homogeneous; assumption not violated
    
    spreadLevelPlot(model5, xlab = "log(fitted values)", 
                    ylab = "log(absolute studentized residuals)") # graphical test, 
    #there should be no strong correlation
    ncvTest(lm(model5, data = eel)) # variances don't deviate from homogeneity, but it's close
    
    
    #linearity 
    residualPlots(model5) #looks ok for the continuous predictor (allozyme)
    
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
    
    #We conclude that heterozygosity negatively affects the heavy metal accumulation in minnows.
    #We also detected overall differences in heavy metal concentration between the different 
    #river systems, but there is no significant interaction. In other words, the effect of 
    #heterozygosity on heavy metal accumulation is the same for all river systems.
  
#Minnows - 
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
  levels(minnow$stress)
  levels(minnow$contaminant)
  
  par(mfrow = c(1,2))
  plot(body_length ~ stress, data = minnow)
  plot(body_length ~ contaminant, data =minnow)
  par(mfrow = c(1,1))

  #additive model
  model7 <- lm(body_length ~ contaminant + stress, data = minnow)
  summary(model7)
  #intercept gives the baseline mean body length under chrome contamination with no stress (control)
  #contaminant1: No significant difference in body length between lead and chrome exposure
  #contaminant2: significant difference between manganese and chrome exposure body lengths
  #Minnows exposed to manganese are about 23.5 mm shorter than those exposed to chrome
  #stress1: Under predation stress, minnows exposed to chrome are longer than minnows exposed to chrome and control stress
  #stress2: no significant difference between sound stress and control
  #Sound stress doesn’t significantly affect body length compared to the control
  #adjusted r^2 = 86.13% of variation explained
  #model p-value < 0.05
  
  emmeans(model7, ~ stress)
  pairs(emmeans(model7, ~ stress))
  pairs(emmeans(model7, ~ contaminant))

  Anova(model7, type = "III")
  #both predictors have an effect on body length of minnows

  plot(allEffects(model7))

  #multiplicative model
  model8 <- lm(body_length ~ contaminant*stress, data = minnow)
  summary(model8)
  #Intercept: baseline minnow body length in chrome contamination and no stress
  #cont1: no difference in body length between lead and chrome (under no stress)
  #cont2: significant difference between chrome and manganese contamination body lengths
  #Manganese exposure causes ~23.5 mm shorter minnows than chrome exposure under no stress
  #stress1: significant difference between control and predation (under chrome)
  #Predation stress + chrome produces much longer minnows than control + chrome (~42 mm longer)
  #stress2: no significant difference in body lengths in minnows under control + chrome or sound stress + chrome
  #cont1*stress1: Not significant — predation effect same under lead and chrome
  #cont2*stress1: significant negative interaction: predation increases length less under manganese
    #predation effect is weaker under Mn + predation than in Chrome + predation
  #cont1:stress2: not significant (lead/sound)
  #cont2:stress2: not significant (manganese/sound)
  #r^2 = 89.2%, model significant
  
  #The effect of predation stress on body length depended on contaminant type. 
  #Under chrome and lead exposure, predation-stressed minnows were substantially 
  #larger than control fish, whereas under manganese exposure this effect 
  #was significantly reduced.
  
  #A linear model including a contaminant × stress interaction explained 89% of the 
  #variation in body length (adjusted R² = 0.89). There was a significant interaction 
  #between contaminant and stress, indicating that the effect of stress depended 
  #on contaminant type. In particular, the positive effect of predation stress on 
  #body length was significantly reduced under manganese exposure. These results 
  #indicate non-additive effects of environmental stressors on minnow growth.
  
  emmeans(model8, ~ stress | contaminant)
  emmeans(model8, ~ contaminant | stress)
  pairs(emmeans(model8, ~ stress | contaminant))
  pairs(emmeans(model8, ~ contaminant | stress))
  
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
  
  plot(allEffects(model8))

  AICc(model7, model8)
  models_list3 <- list(model7, model8)
  model.sel(models_list3)

  contrast(emmeans(model8, ~contaminant|stress), method = 'pairwise', adjust = 'Tukey') #posthoc comparisons
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
  
  #So:
  #Manganese causes the greatest growth suppression,
  #Lead causes a moderate reduction,
  #Chrome (baseline) fish are largest overall.

  #checking og assumptions
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
  
    #Across all stress conditions, body length differs significantly among contaminants. 
    #Under control conditions, manganese-exposed minnows are the largest, chrome 
    #intermediate, and lead smallest. This pattern remains under predation and sound stress, 
    #though the differences are slightly reduced under predation and weakest under sound. 
    #The consistently positive differences for “manganese vs others” suggest that, while 
    #manganese exposure generally produced shorter fish on average in the main model, 
    #the interaction with stress means manganese fish respond differently under varying 
    #stress conditions—possibly showing compensatory or selective survival effects depending 
    #on environment.
    
    # We conclude that contamination type and type of stress have significant effects 
    # on the adult body length in minnows. On top of that, there is a significant interaction: the effects
    # of contamination treatment is different between the stress treatments (or the other
    # way around). Visually, it seems like there is a negative synergy: if there is already
    # a strong negative effect of one of the treatments (especially STRESS), then the negative effect of the
    # other treatment (contamination) is smaller.
    

#Remedying assumptions ----
set_sum_contrasts()

#data
  blood <- read.xlsx('data/blood_glucose.xlsx')
  head(blood)
  
  blood <- blood %>% rename(exercise = weekly_exercise, 
                            carbs = carb_consumption, 
                            glucose = blood_glucose)
  
  plot(glucose ~ carbs, data = blood)
  boxplot(glucose ~ exercise, data = blood)
    
#model + assumptions
  model1 <- lm(glucose ~ carbs*exercise, data = blood)
  plot(allEffects(model1), multiline = T)
  summary(model1) #only carbs have an effect on blood glucose
  
  shapiro.test(residuals(model1)) #no deviation from normality
  ncvTest(model1) #no deviation from homogeneous variances
  residualPlots(model1)  #no strong deviation from linearity
  vif(model1) #we have clear collinearity! Let's residual center to address this:
  
  #need to center:
  model1c <- residualCenter(model1)
  
  vif(model1c) #all about 1
  summary(model1c) #now exercise also has an effect
  #in case of collineatirity, we should interpret the p-values of the model with residual centering
  Anova(model1c, type="III") #carbs and exercise have an effect, no interaction
  
  #outliers:
  outlierTest(model1c) #point n23 is an outlier with a p-value < 0.05
  influenceIndexPlot(model1c, vars = c("Studentized","Bonf"))  #one significant outlier
  
  cd <- cooks.distance(model1c)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model1c, vars = c("Cook")) #point 23 has cd > 1
  
#remedying
  model2 <- lmrob(glucose ~ exercise * carbs, data = blood)
  vif(model2)
  model2b <- residualCenter(model2)
  vif(model2b) #all good
  Anova(model2b, type="III")
  summary(model2b) #all significant now
  
  plot(allEffects(model2), multiline = T, confint = list(style = 'auto')) #now this is very different
  # we now conclude that there is a strongly significant interaction. For people with
  # a diet low in carbohydrates, weekly exercise has essentially no effect on their
  # blood glucose. However, the higher the consumption of carbohydrates, the more 
  # exercise helps to reduce blood glucose.
  
#
  d = read.xlsx("soilmanagement.xlsx")
  head(d)
  str(d)
  
  #we need to code plot, treatment and duration as factors! 
  d$plot <- factor(d$plot)
  d$treatment <- factor(d$treatment)
  d$duration <- factor(d$duration)
  
  par(mfrow=c(1,3))
  plot(phenolics_conc ~ plot, data=d)
  plot(phenolics_conc ~ treatment, data=d)
  plot(phenolics_conc ~ duration, data=d)
  par(mfrow=c(1,1))
  
  
  #b. Run a linear  mixed-effects model to predict the effect of treatment, duration 
  #and their interaction on phenolics concentration. What should be the random
  #factor in this model? Visualize the model. 
  
  #the random factor should be 'plot'. This is because we are not specifically interested
  #in which plots lead to more phenolics - it is just a factor in the data that groups
  #some data points together, and the levels of this factor (the plots) have been randomly drawn
  #from a large number of possible plots. 
  fit <- lmer(phenolics_conc ~ treatment * duration + (1|plot), data = d)
  summary(fit)
  Anova(fit, type="III")
  plot(allEffects(fit))
  plot(allEffects(fit), multiline=TRUE)
  #to be completely correct, we should test an additive model and one with 
  #interaction and check which is best with AICc
  
  
  #c. Do some posthoc comparisons based on your model. Specifically, compare all
  #treatments against the control for the same duration. To do this, first relevel 
  #the treatment variable so that the reference level is the control treatment.
  #Then run the model again (otherwise you will get nonsensical results).       
  #Then do posthoc comparisons between the treatments using method 'trt.vs.ctrl'
  #(see lecture 4) within each duration (see lecture 2). What do you conclude?
  
  levels(d$treatment)
  d$treatment = relevel(d$treatment, ref="control")
  fit <- lmer(phenolics_conc ~ treatment * duration + (1|plot), data = d)
  Anova(fit, type="III") ##stays the same, as it is the same fit
  contrast(emmeans(fit, ~ treatment|duration), method='trt.vs.ctrl', adjust='Tukey')
  plot(allEffects(fit), multiline=TRUE)
  #We can see that the fertilizer treatment leads to lower phenolic concentrations for
  #both durations, while we have no evidence that this is the case for the exclosure treatment
  #(also for both durations). For the 'both' treatment, we find that the phenolics contentration
  #is significantly lower than the control for the permanent duration, but have no evidence
  #that this is also the case for the reverse duration.
  
  
  #d. You cannot test for homogeneity of variances using our standard methods, 
  #because we ran a mixed model. But we could run a mixed model that allows
  #the variances between the treatments and durations (and both) to be different, 
  #and then compare these models with our first model based on AICc. We have to do 
  #this with the function lme. First reconstruct the model you made in (b) with 
  #lme and assure yourself that it produces the same outcome as the model you 
  #made under b. Then run three extra models: one that allows for different variances
  #between treatments, one that allows for different variances between durations,
  #and one that allows for different variances for each combination of treatment
  #and duration (use 'form=~1|treatment*duration'). 
  
  #Which model is the best? Do your conclusions change?
  #Visualize the best model if you have not yet done so.
  
  fit2 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, data = d)
  ?lme
  summary(fit2)
  Anova(fit2, type="III") 
  
  AICc(fit, fit2) #it's indeed the same model as the original
  
  fit3 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|treatment), data = d)
  fit4 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|duration), data = d)
  fit5 <- lme(phenolics_conc ~ treatment * duration, random=~1|plot, weight=varIdent(form=~1|treatment*duration), data = d)
  
  AICc(fit2, fit3, fit4, fit5) #our original fit has the best AICc, so we keep that one!
  
  
  #e. Check assumptions: normality of residuals, outliers/influential observations.
  
  shapiro.test(residuals(fit)) # no deviation from normality
  outlierTest(fit) #no outliers (note that this test works for an lmer but not for a lme model)
  max(cooks.distance(fit)) #no points with Cook's distance over 1
  #NOTE: if a model with custom variances would be best we should have used lme to make the fit. testing outliers/influential observations
  #with lme is not straightforward and not part of this course; So now we are just lucky we can use the original model
  #which we can fit with lmer
  
  
  #f.  What are your conclusions based on this model?
  #We conclude that the effect of soil management treatment on phenolics in some cases depends on 
  #the duration these treatments are applied for. The application of fertilizer (but no fencing)
  #leads to lower phenolics compared to the control treatment, irrespective of the duration treatment.
  #In contrast, we have no evidence that the application of exclosure (but no fertilizer) leads to 
  #differences, again irrespective of the duration treatment. However, if BOTH are applied,
  #then the duration matters: we see an effect if they are applied permanently, but not
  #if they are only applied for 10 years and then let to reverse to their untreated state.
  
  # EXERCISE 3: HYPERTENSION
  #We have a dataset about blood pressure of 16 hypertension patients who have either been
  #treated with a medicine or a placebo (control). Their blood pressure was measured
  #every week for a total of 10 weeks.
  #We would like to know if the medicine leads to a stronger decrease over time
  #in blood pressure than the placebo.
  
  #a.  Start with reading the data, checking everything and visually exploring.
  
  d3 <- read.xlsx("bloodpressure.xlsx")
  head(d3)
  str(d3)
  d3$subject <- as.factor(d3$subject)
  d3$treatment <- as.factor(d3$treatment)
  
  plot(bloodpressure ~ treatment, data = d3)
  plot(bloodpressure ~ week, data = d3, col = d3$subject)
  
  #b. Construct a linear mixed model to predict the effects of treatment and 
  #time on blood pressure. Also include their interaction.
  #Include the appropriate random effects. Visualize the model.
  
  fit8 <- lmer(bloodpressure~treatment*week + (1|subject), data=d3)
  summary(fit8)
  Anova(fit8, type="III")
  
  #looks like there is no difference in treatment alone, but the change over time
  #is different between the medicine and the placebo
  plot(allEffects(fit8), multiline=T, confint=list(style="auto"))
  #the effects plot shows that the slope for the medicine is more downward than
  #for the placebo - based on this, we would conclude that the medicine is effective.
  
  #c. Because our measurements have been done over time, we might have 
  #temporal autocorrelations in the data. 
  #First check if there indeed is temporal autocorrelation in the previous model.
  #Then fit a new model where you account for these temporal 
  #autocorrelations and check if it is better.
  #If so, visualize it. Do your conclusions change?
  
  #autocorrelation plot
  acf(residuals(fit8)) #we can see that for short lags there is a positive correlation
  
  #first run lme WITHOUT the autocorrelation structure (should give the same as above)
  fit9 <-lme(bloodpressure~treatment*week, random=~1|subject, data=d3)
  summary(fit9)
  Anova(fit9, type="III")
  #and here we add temporal autocorrelation structure
  fit10 <-lme(bloodpressure~treatment*week, random=~1|subject, correlation=corAR1(form=~week), data=d3)
  summary(fit10)
  Anova (fit10, type="III")
  AICc(fit8, fit9, fit10) #the model that accounts for autocorrelation is much better!
  
  plot(allEffects(fit10), multiline=T, confint=list(style="auto"))
  #after accounting for temporal autocorrelation, we no longer have support that 
  #bloodpressure declines faster in the patients who got the medicine than in 
  #those who got the placebo
  
  #d. we cannot test for homogeneity of variances with mixed models, but we could run a mixed model that allows
  #the variances between the treatments to be different, 
  #and then compare these models with our first model based on AICc.
  
  fit11 <-lme(bloodpressure~treatment*week, random=~1|subject, weight=varIdent(form=~1|treatment), correlation=corAR1(form=~week), data=d3)
  summary(fit11)
  Anova (fit11, type="III")
  AICc(fit8, fit9, fit10, fit11) #
  # this is slightly worse than the previous one, so we keep the model without custom variances. 
  #The difference in AICc is small (<2) so we can report this model, though our conclusions do not change
  
  fit12 <-lme(bloodpressure~treatment*week, random=~1|subject, weight=varIdent(form=~1|treatment), data=d3)
  AICc(fit8, fit9, fit10, fit11, fit12)
  #the model with custom variances but not accounting for temporal autocorrelation, is clearly worse
  
  
  #e. Check normality of residuals and collinearity on the best fit.
  #Note: checking outliers, influential observations and linearity
  #requires some different techniques than for lm/glm.
  #We don't go into it in this course.
  
  shapiro.test(residuals(fit10)) #no deviation (W>0.9, don't worry about the p-value)
  vif(fit10) #no collinearity
  
  #f.  What are your conclusions?
  
  #we have no reliable support that the medicine is effective - bloodpressure does not
  #change significantly differently over time in the patients that got the medicine vs
  #the patients that got the placebo
  
    
#Mixed models ----
set_sum_contrasts()

  d = read.csv("data/fitness.csv")
  head(d)
  str(d)
    
  
  #we now have to code FITNESS and TEST as factors, but also ID! Even though ID
  #is in numbers, they represent different individuals so they should be coded as
  #a factor
  d$TEST <- as.factor(d$TEST)
  d$FITNESS <- as.factor(d$FITNESS)
  d$ID <- as.factor(d$ID)
  
  fit = lm(PULSE ~ FITNESS + TEST, data=d)
  Anova(fit, type="III") #significant effect of both
  contrast(emmeans(fit,~FITNESS), method="pairwise", adjust="Tukey")
  
  fit2 = lmer(PULSE ~ FITNESS + TEST + (1|ID), data=d)
  Anova(fit2, type="III") #now only test is significant
  summary(fit2)
  contrast(emmeans(fit2,~FITNESS), method="pairwise", adjust="Tukey")
  
  AICc(fit, fit2) #fit2 is better
  
  plot(allEffects(fit2))
  
  shapiro.test(residuals(fit2)) #fine
  outlierTest(fit2) #fine
  max(cooks.distance(fit2)) #fine
  
  #schools
  d2 <- read.xlsx("data/schools.xlsx")
  head(d2)
  str(d2)
  #code class and school as factors (will be included as random factors)
  d2$school <- as.factor(d2$school)
  d2$class <- as.factor(d2$class)
  
  plot(test_result ~ breakfast_cal, data=d2, col=school, pch=c(0,4,19)[class])
  
  fit3 <- lmer(test_result ~ breakfast_cal + (1|school/class), data=d2)
  summary(fit3)
  
  plot(allEffects(fit3))
  
  #use long format!
  d3 <- read.csv("data/queenpheromone_wide.csv")
  head(d3)
  d3 <- read.csv("data/queenpheromone_long.csv")
  head(d3)
  str(d3)
  d3$colony <- as.factor(d3$colony)
  d3$treatment <- as.factor(d3$treatment)
  
  fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data=d3)
  summary(fit3)
  Anova(fit3, type="III")
  plot(allEffects(fit3))
  plot(allEffects(fit3), type="response")
  d3$treatment = relevel(d3$treatment, ref="control")
  #we have to run the fit again after releveling, otherwise the labels for the differen
  #factor levels get mixed up
  fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data=d3)
  contrast(emmeans(fit3,~treatment), method="trt.vs.ctrl", adjust="Tukey")
  
  fit3 <- glmer(ovarydev~treatment+colonysize+(1|colony),family=binomial,data=d3)
  fit4 <- glm (ovarydev~treatment+colonysize,family=binomial,data=d3)
  AICc(fit3, fit4)
  
  #create observation level factor variable
  d3$obs <- factor(1:nrow(d3))
  
  #run model that includes this variable to check for overdispersion
  fit5 <- glmer(ovarydev~treatment+colonysize+(1|colony)+(1|obs),family=binomial,data=d3)
  AICc(fit3, fit5) #original model is better, no overdispersion
  
  #darwin
  darwin <- read.csv("data/darwin.csv")
  head(darwin)
  
  fit6 <- lm(height~type, data=darwin)
  summary(fit6)
  ncvTest(fit6) #unequal variances!
  
  fit7 <- gls(height~type, data=darwin, weight=varIdent(form=~1|type))
  summary(fit7)
  plot(allEffects(fit7))
  
  #we cannot directly compare AICc of gls and lm models...
  #but we can first rewrite the lm as a gls model and THEN compare the two
  
  fit6b <- gls(height~type, data=darwin)
  AICc(fit6, fit6b) #fit6 and fit6b are the same models in lm and gls, but have different
  #AICc, even if they have the same estimates:
  coef(fit6)
  coef(fit6b)
  
  #so if we want to compare both models (with and without allowing non-equal variances),
  #we should compare both gls models: fit6b and fit7
  AICc(fit6b, fit7)
  
  #birds
  d4 <- read.csv("data/hawaiibirds.csv")
  str(d4)
  #we will only work with a subset of the data for this example
  d5<-subset(d4, d4$Birds!="NA" & d4$Species==3 & d4$Island==1)
  head(d5)
  #no need to recode - we will only use 'Year' and 'Birds'
  
  plot(d5$Birds~d5$Year, type=c("l"))
  fit8 <- lm(Birds~Year, data=d5)
  summary(fit8)
  plot(fit8)
  acf(residuals(fit8))
  
  fit9 <- gls(Birds~Year, data=d5, correlation = corAR1(form=~Year))
  
  summary(fit9)
  
  #effect plots - just made sure here that they are both on the same
  #scale (y-axis between -100 and 300)
  plot(allEffects(fit8, residuals=TRUE), smooth.residuals = FALSE, ylim=c(-100,300))
  plot(allEffects(fit9, residuals=TRUE), smooth.residuals = FALSE, ylim=c(-100,300))
  
  #as before, to compare the gls model with the model that does not have a specific
  #correlation structure, we need to code the first model (the lm) as gls
  fit8b <- gls(Birds~Year, data=d5)
  AICc(fit8b, fit9)
  
#foxes and vegetation cover
  df <- read.csv("data/red_fox.csv")
  
  # Histograms for each Prey Selection category, split by Vegetation Cover
  ggplot(df, aes(x = Prox_Human_Activity)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    facet_grid(Diet ~ Veg_Cover) +
    labs(title = "Histograms of Proximity to Human Activity by Prey Selection and Vegetation Cover",
         x = "Proximity to Human Activity (km)",
         y = "Count") +
    theme_minimal()
  
  # We need to ensure the outcome variable is a factor
  df$Diet <- as.factor(df$Diet)
  
  # Fit the model
  fit <- multinom(Diet ~ Prox_Human_Activity + Veg_Cover, data = df)
  
  # Summary of the model
  summary(fit)
  Anova(fit, type="III")
  
  plot(allEffects(fit), multiline=TRUE, confint=list(style="auto"))
  
  
  graph2ppt(file="effectplot_additive")
  
  # Fit the model
  fit2 <- multinom(Diet ~ Prox_Human_Activity * Veg_Cover, data = df)
  
  # Summary of the model
  summary(fit2)
  Anova(fit2, type="III")
  
  plot(allEffects(fit2), multiline=TRUE, confint=list(style="auto"))
  
  AICc(fit, fit2)
  
  #extract model effects for predictor 'Prox_Human_Activity'
  #only extract the first four columns that have the actual probabilities
  proximity_effect <- as.data.frame(Effect("Prox_Human_Activity", fit))[1:4]
  #use 'melt' to turn the table from wide to long format
  proximity_effect <- melt(proximity_effect, id.vars = "Prox_Human_Activity", variable.name = "Diet", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(proximity_effect, aes(x = Prox_Human_Activity, y = Probability, fill = Diet)) +
    geom_area() +
    labs(x = "Proximity to Human Activity (km)",
         y = "Probability") +
    theme_minimal()
  
  #extract model effects for predictor 'Veg_Cover'
  #only extract the first four columns that have the actual probabilities
  veg_cover_effect <- as.data.frame(Effect("Veg_Cover", fit))[,1:4]
  #use 'melt' to turn the table from wide to long format
  veg_cover_effect <- melt(veg_cover_effect, id.vars = "Veg_Cover", variable.name = "Diet", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(veg_cover_effect, aes(x = Veg_Cover, y = Probability, fill = Diet)) +
    geom_bar(stat = "identity", position = 'stack') +
    labs(x = "Vegetation Cover",
         y = "Probability") +
    theme_minimal()
  
  
  library(MASS)
  
  df <- read.csv("butterflies.csv")
  head(df)
  
  df$Dev_Stage <- factor(df$Dev_Stage, levels = c("Egg", "Larva", "Pupa", "Adult"), ordered = TRUE)
  
  # Fit the ordinal logistic regression model
  fit3 <- polr(Dev_Stage ~ Temp + Host_Species + Week, data = df, Hess = TRUE)
  Anova(fit3, type="III")
  
  Temp_effect <- as.data.frame(Effect("Temp", fit3, xlevels=list(Temp=seq(min(df$Temp), max(df$Temp), length.out=100))))[,1:5]
  #use 'melt' to turn the table from wide to long format
  Temp_effect <- melt(Temp_effect, id.vars = "Temp", variable.name = "Dev_Stage", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(Temp_effect, aes(x = Temp, y = Probability, fill = Dev_Stage)) +
    geom_area() +
    labs(x = "Average Temperature",
         y = "Probability") +
    theme_minimal()
  
  Host_effect <- as.data.frame(Effect("Host_Species", fit3))[,1:5]
  #use 'melt' to turn the table from wide to long format
  Host_effect <- melt(Host_effect, id.vars = "Host_Species", variable.name = "Dev_Stage", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(Host_effect, aes(x = Host_Species, y = Probability, fill = Dev_Stage)) +
    geom_bar(stat = "identity", position = 'stack') +
    labs(x = "Host_Species",
         y = "Probability") +
    theme_minimal()
  
  Time_effect <- as.data.frame(Effect("Week", fit3, xlevels=list(Week=seq(min(df$Week), max(df$Week), length.out=100))))[,1:5]
  #use 'melt' to turn the table from wide to long format
  Time_effect <- melt(Time_effect, id.vars = "Week", variable.name = "Dev_Stage", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(Time_effect, aes(x = Week, y = Probability, fill = Dev_Stage)) +
    geom_area() +
    labs(x = "Time",
         y = "Probability") +
    theme_minimal()

#GLMs and Mixed models (normal + advanced)----
#data
blood <- read.xlsx("data/bloodcells1.xlsx")
isolation <- read.xlsx("data/isolation.xlsx")
torts <- read.xlsx("data/tortoises.xlsx")

set_sum_contrasts() #effects coding

#Can we predict tortoise total clutch weight from carpace length?
  torts <- torts %>% rename(length = Length,
                          clutch = Clutch_weight)

  head(torts)
  str(torts)
  
  plot(clutch ~ length, data = torts)
  
  model1 <- lm(clutch ~ length, data = torts)
  summary(model1) #no effect
  plot(allEffects(model1)) #huge confidence interval
  residualPlots(model1) #non-linear
  
  model2 <- lm(clutch ~ poly(length, 2), data = torts)
  model3 <- lm(clutch ~ poly(length, 3), data = torts)
  model4 <- lm(clutch ~ poly(length, 4), data = torts)
  models_list <- list(model2, model3, model4)

  AICc(model1, model2, model3, model4) #model 2 is best according to the AICc
  model.sel(models_list) #model 2 is best
  
  plot(allEffects(model2, residuals = TRUE), smooth.residuals = FALSE)
  plot(allEffects(model2))

  #assumptions
    #normality of residuals
    hist(model2$residuals)
    
    hist(rstudent(model2), probability = T, xlab = "Studentised residuals", 
         main = "Distribution of Studentised Residuals")
    
    r = model2$residuals
    h <- hist(model2$residuals, breaks = 10, density = 20,
              col = "lightblue", xlab = "Residuals", ylab = "Frequency") 
    xfit <- seq(min(r), max(r), length = 40) 
    yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
    yfit <- yfit * diff(h$mids[1:2]) * length(r) 
    lines(xfit, yfit, col = "red", lwd = 2)
    
    shapiro.test(residuals(model2)) #p not significant, W = 0.95822; assumption not violated
    
    #homogeneity of variances
    spreadLevelPlot(model2) #looks weird
    ncvTest(model2) #p-value > 0.05, variances homogeneous; assummption not violated
    plot(model2, which = 3) #also weird
    
    #linearity and collinearity
    residualPlots(model2) #pearson residuals look weird -> assumption appears violated

    #outliers:
    outlierTest(model2) #no residuals with p < 0.05
    influenceIndexPlot(model2, vars = c("Studentized","Bonf"))  #no significant outliers 
    #(no p-values < 0.05)
    #points n16 and 2 are the furthest from the rest of the data, but not significant
    
    cd <- cooks.distance(model2)
    (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
    
    influenceIndexPlot(model2, vars = c("Cook"))
    #points 17 and 18 has the highest cook's distance but not > 1
  
    summary(model2) #significant effect of height^2 on clutch size, p < 0.05

    
    plot(allEffects(model2, residuals = TRUE), smooth.residuals = FALSE) #seed # decreases with seed weight
    
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
    
#Can we predict species present from island size and distance from mainland?
  #data
    head(isolation)
    str(isolation)
    
    plot(presence ~ area, data = isolation)
    plot(presence ~ distance, data = isolation)
    
    xyplot(presence ~ area, data = isolation ,type = c("p","r")) #p = points, r = reg. line
    xyplot(presence ~ distance, data = isolation, type = c("p","r"))
    #we cannot base any conclusions on this!
    
  #additive model
  model5 <- glm(presence ~ area + distance, family = binomial(link = logit), data = isolation)
  plot(allEffects(model5)) #presence increases with island area and decreases
  #with distance from shore
  summary(model5) #both have a sig. effect
  
  #interactive model
  model6 <- glm(presence ~ area * distance, family = binomial(link = logit), data = isolation)
  plot(allEffects(model6))
  
  AICc(model5, model6) #model5 is better
  models_list2 <- list(model5, model6)
  model.sel(models_list2) #model5 has a higher AIkake weight
  
  #predicting species presence:
  (prediction1 <- predict(model5, list("area" = 3, "distance" = 8), type = "response")) #0.06967481
  (prediction2 <- predict(model5, list("area" = 6, "distance" = 4), type = "response")) #0.9165
    #or the faster way:
    (preds <- predict(model5, list(area = c(3, 6), distance = c(8, 4)), type = "response"))
  
  plot(allEffects(model5), type = "response") #clearly non-linear
  
  #checking assumptions
    #linearity and collinearity
    residualPlots(model5) #looks ok
    vif(model5) #VIF < 5; no collinearity
    
    #outliers:
    outlierTest(model5) #no residuals with p < 0.05
    influenceIndexPlot(model5, vars = c("Studentized","Bonf"))  #no significant outliers 
    #(no p-values < 0.05)
    #points n19 and 47 are the furthest from the rest of the data, but not significant
    
    #influential points:
    cd <- cooks.distance(model5)
    (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
    
    influenceIndexPlot(model5, vars = c("Cook"))
    #points n19 and 47 have the highest cook's distance but not > 1
    
    #overdispersion?
    fit2quasi <- glm(presence ~ area + distance, family = quasibinomial(link = logit), data = isolation)
    summary(fit2quasi) #dispersion parameter: 0.6094858
    (overdispersion5 <- sum(residuals(model5, type = "pearson")^2) / df.residual(model5)) #0.6094412
    #all good, disp. parameter: 0.6094412
  
  
  #conclusions
  summary(model5) #both predictors have a significant effect!
  plot(allEffects(model5), type="response")
  
  #For each 1-unit increase in island area, the log-odds of species presence 
  #increase by 0.58 (so presence probability increases).
  #For each 1-unit increase in distance from the mainland, the log-odds of presence 
  #decrease by 1.37 (so presence probability decreases).
  #A binomial GLM revealed significant effects of both island area (β = 0.58, 
  #p = 0.019) and distance from the mainland (β = −1.37, p = 0.004) on species 
  #presence. The model explained a substantial reduction in deviance (68.0 → 28.4), 
  #indicating strong predictive power. The results suggest that species are more 
  #likely to occur on larger and less isolated islands, consistent with classical 
  #island biogeography theory. Model diagnostics indicated mild underdispersion 
  #(ϕ = 0.60), confirming that the binomial variance assumption was adequate.
  

#can we predict the effects of smoking, weight and sex on damaged blood cell counts?
  #data
  head(blood)
  str(blood)
  
  blood <- blood %>% mutate(smoker = as.factor(smoker),
                            age = as.factor(age),
                            sex = as.factor(sex))
  str(blood)
  
  plot(cells ~ smoker, data = blood) #more damaged bloodcells in smokers
  plot(cells ~ age, data = blood) #about equal with some outliars
  plot(cells ~ sex, data = blood) #bigger outliars in women, but wider distribution in men

  #model
  hist(blood$cells) #poisson distribution, use poisson family glm
  model7 <- glm(cells ~ smoker + weight + sex, family = poisson(link = log), data = blood)
  plot(allEffects(model7)) #damaged cells # increases with smoking and weight, and is lower in men
  
  summary(model7)   
  #significant negative effect of smoking; sig positive effect of increased weight; 
  #number of damaged blood cells higher in men
  Anova(model7, type="III") #note that the p-values from the Anova table and the summary table are very similar, but not
  #significant effect of smoking, weight, and sex on # of damaged blood cells
  
  plot(allEffects(model7), ylab = "Number of damaged cells")
  plot(allEffects(model7), ylab = "Number of damaged cells", type = "response") #use type = 
  #"response" to plot on the linear scale:
  
  #new model
  levels(blood$smoker) #no = 0, yes = 1
  levels(blood$sex) #female = 0, male = 1
  model8 <- glm(cells ~ smoker*weight + sex, family = poisson(link = log), data = blood)
  plot(allEffects(model8))
  summary(model8) #interaction is significant between smoking and weight (negative)
  #smoker1 (yes vs no) -> not significant
  #weight -> significant (positive - higher weight = higher number of damaged blood cells)
  #sex1 (male vs female) -> men have higher counts of damaged blood cells, significant
  #male*weight interaction: negative, significant -> weight dampens the effect of smoking
  Anova(model8, type = "III")
  #significant effect of weight and sex, and weight*smoker interaction, but not of smoking
  
  
  AICc(model7, model8) #model8 is better
  
  plot(allEffects(model8), multiline = T, confint = list(style = "auto")) #stronger effect of weight on
  #on blood cell damage counts in smokers
  plot(allEffects(model8), multiline = T, confint = list(style = "auto"), type = "response")
  
  
  #all possible models
  model7 = glm(cells~smoker+weight+sex + sex:weight, family = poisson, data = blood)
  model8 = glm(cells~smoker+weight+sex + sex:smoker, family = poisson, data = blood)
  model9 = glm(cells~smoker+weight+sex + sex:weight + sex:smoker, family = poisson, data = blood)
  model10 = glm(cells~smoker+weight+sex + sex:weight + smoker:weight, family = poisson, data = blood)
  model11 = glm(cells~smoker+weight+sex + sex:smoker + smoker:weight, family = poisson, data = blood)
  model12 = glm(cells~smoker+weight+sex + sex:smoker + smoker:weight + sex:weight, family = poisson, data = blood)
  
  AICc(model7, model8, model9, model10, model11, model12) #models 11 and 12 have the same AICc
  models_list3 <- list(model7, model8, model9, model10, model11, model12)
  model.sel(models_list3) #model11 best, model12 similarly good
  
  aics <- AICc(model7, model8, model9, model10, model11, model12)$AICc
  dAICc <- aics - min(aics)
  (w <- exp(-0.5*dAICc) / sum(exp(-0.5*dAICc))) #report model11 but 
  #also say model12 is equally supported
  
  plot(allEffects(model11))
  plot(allEffects(model11, multiline = T), type = "response") #non-linear relationship
  
  
  summary(model11)
  Anova(model11, type = "III") 
  
  #checking assumptions
  #linearity and collinearity
  residualPlots(model11) #looks ok
  vif(model11) #GVIF > 5; collinearity
  model11c <- residualCenter(model11) #fix the correlation within the interaction
  summary(model11c)
  #intercept: expected count for a non-smoker, female, at average weight (e^estimate) ~ 0.17
  #smoker1: at average weight, smokers have about 45% fewer damaged cells than non-smokers.
  #e^(−0.593) ≈ 0.55
  #weight: for non-smokers, each 1-unit weight increase raises expected count by ~2.4%.
  #sex1: males have higher number of damaged blood cells
  #smoker*weight: the weight slope is weaker in smokers
  #Heavier people show more damaged cells, but that increase with weight is smaller 
  #in smokers. One sex shows ~15% higher counts than the other. At average weight, 
  #smokers have lower expected counts than non-smokers
  
  vif(model11c) #all < 5 so all good
  
  #outliers:
  outlierTest(model11) #no residuals with p < 0.05
  influenceIndexPlot(model11, vars = c("Studentized", "Bonf"))  #no significant outliers 
  #(no p-values < 0.05)
  #point n246 are the furthest from the rest of the data, but not significant
  
  #influential points:
  cd <- cooks.distance(model11)
  (inflobs = which(cd>1)) #no influential points (none with Cook's distance > 1)
  
  influenceIndexPlot(model11, vars = c("Cook"))
  #points n246 and 294 have the highest cook's distance but not > 1
  
  #overdispersion?
  (overdispersion11 <- sum(residuals(model11, type = "pearson")^2) / df.residual(model11)) #1.014639
  fit11quasi <- glm(cells ~ smoker + weight + sex + sex:smoker + smoker:weight, family = quasipoisson, data = blood)
  summary(fit11quasi) #dispersion parameter is 0.9812321 (though WITH residualCentering)
  #slight overdispersion but ok
  
  #e. What are your conclusions based on this analysis?
  #Heavier people show more damaged cells, but that increase with weight is smaller 
  #in smokers. One sex shows ~15% higher counts than the other (women > men). At average weight, 
  #smokers have lower expected counts than non-smokers
  plot(allEffects(model11), multiline = T, confint = list(style = "auto"), type = "response")
  
  #  We conclude that there are overall effects of smoking and weight on the damaged bloodcell count.
  #  Also,  being a smoker impacts this count more strongly in females than in males.
  #  Finally, the effect of weight is much stronger in smokers than in non-smokers.
  
  #faster way to find the best glm:
  install.packages("rJava")
  library("rJava")
  install.packages("glmulti")
  library("glmulti")
  
  best = glmulti(cells ~ smoker + sex + weight, family = "poisson", data = blood, confsetsize = 5, crit = "aicc") 
  best@formulas
  (bestfit = best@objects[[1]])
  best #gives the models with AIC in ascending order
  summary(best) #$bestmodel
  #[1] "cells ~ 1 + smoker + sex + weight + sex:smoker + smoker:weight"
  
#advanced GLMs + Mixed models
  df <- read.csv("data/red_fox.csv")
  
  # Histograms for each Prey Selection category, split by Vegetation Cover
  ggplot(df, aes(x = Prox_Human_Activity)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    facet_grid(Diet ~ Veg_Cover) +
    labs(title = "Histograms of Proximity to Human Activity by Prey Selection and Vegetation Cover",
         x = "Proximity to Human Activity (km)",
         y = "Count") +
    theme_minimal()
  
  # Fit the multinomial logistic regression model
  # We need to ensure the outcome variable is a factor
  df$Diet <- as.factor(df$Diet)
  
  # Fit the model
  fit <- multinom(Diet ~ Prox_Human_Activity + Veg_Cover, data = df)
  
  # Summary of the model
  summary(fit)
  Anova(fit, type = "III") #veg_cover and human activity have a significant + effect
  #on counts
  
  plot(allEffects(fit), multiline = TRUE, confint = list(style = "auto"))
  #birds peak at intermediate proximate human activity; insects increase w human activity;
  #small mammals decrease w human activity
  #biirds peak at moderate vegetation cover; small mammals at dense, and insects at sparse
  
  # Fit the model
  fit2 <- multinom(Diet ~ Prox_Human_Activity * Veg_Cover, data = df)
  
  # Summary of the model
  summary(fit2)
  Anova(fit2, type = "III") #veg cover has an effect
  
  plot(allEffects(fit2), multiline=TRUE, confint=list(style="auto"))
  
  AICc(fit, fit2) #fit is better
  
  #extract model effects for predictor 'Prox_Human_Activity'
  #only extract the first four columns that have the actual probabilities
  proximity_effect <- as.data.frame(Effect("Prox_Human_Activity", fit))[1:4]
  #use 'melt' to turn the table from wide to long format
  proximity_effect <- melt(proximity_effect, id.vars = "Prox_Human_Activity", variable.name = "Diet", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(proximity_effect, aes(x = Prox_Human_Activity, y = Probability, fill = Diet)) +
    geom_area() +
    labs(x = "Proximity to Human Activity (km)",
         y = "Probability") +
    theme_minimal()
  
  #extract model effects for predictor 'Veg_Cover'
  #only extract the first four columns that have the actual probabilities
  veg_cover_effect <- as.data.frame(Effect("Veg_Cover", fit))[,1:4]
  #use 'melt' to turn the table from wide to long format
  veg_cover_effect <- melt(veg_cover_effect, id.vars = "Veg_Cover", variable.name = "Diet", value.name = "Probability")
  
  #make a stacked effects plot
  ggplot(veg_cover_effect, aes(x = Veg_Cover, y = Probability, fill = Diet)) +
    geom_bar(stat = "identity", position = 'stack') +
    labs(x = "Vegetation Cover",
         y = "Probability") +
    theme_minimal()
  
  #butterflies
    df <- read.csv("data/butterflies.csv")
    head(df)
    
    df$Dev_Stage <- factor(df$Dev_Stage, levels = c("Egg", "Larva", "Pupa", "Adult"), ordered = TRUE)
    
    # Fit the ordinal logistic regression model
    fit3 <- polr(Dev_Stage ~ Temp + Host_Species + Week, data = df, Hess = TRUE)
    Anova(fit3, type = "III")
    
    Temp_effect <- as.data.frame(Effect("Temp", fit3, xlevels=list(Temp=seq(min(df$Temp), max(df$Temp), length.out=100))))[,1:5]
    #use 'melt' to turn the table from wide to long format
    Temp_effect <- melt(Temp_effect, id.vars = "Temp", variable.name = "Dev_Stage", value.name = "Probability")
    
    #make a stacked effects plot
    ggplot(Temp_effect, aes(x = Temp, y = Probability, fill = Dev_Stage)) +
      geom_area() +
      labs(x = "Average Temperature",
           y = "Probability") +
      theme_minimal()
    
    Host_effect <- as.data.frame(Effect("Host_Species", fit3))[,1:5]
    #use 'melt' to turn the table from wide to long format
    Host_effect <- melt(Host_effect, id.vars = "Host_Species", variable.name = "Dev_Stage", value.name = "Probability")
    
    #make a stacked effects plot
    ggplot(Host_effect, aes(x = Host_Species, y = Probability, fill = Dev_Stage)) +
      geom_bar(stat = "identity", position = 'stack') +
      labs(x = "Host_Species",
           y = "Probability") +
      theme_minimal()
    
    Time_effect <- as.data.frame(Effect("Week", fit3, xlevels=list(Week=seq(min(df$Week), max(df$Week), length.out=100))))[,1:5]
    #use 'melt' to turn the table from wide to long format
    Time_effect <- melt(Time_effect, id.vars = "Week", variable.name = "Dev_Stage", value.name = "Probability")
    
    #make a stacked effects plot
    ggplot(Time_effect, aes(x = Week, y = Probability, fill = Dev_Stage)) +
      geom_area() +
      labs(x = "Time",
           y = "Probability") +
      theme_minimal()
    
    
    set_sum_contrasts()
    
  #fitness
    d = read.csv("data/fitness.csv")
    head(d)
    str(d)
    #we now have to code FITNESS and TEST as factors, but also ID! Even though ID
    #is in numbers, they represent different individuals so they should be coded as
    #a factor
    d$TEST <- as.factor(d$TEST)
    d$FITNESS <- as.factor(d$FITNESS)
    d$ID <- as.factor(d$ID)
    
    #SLIDE 9: first try a 'regular' linear model...
    fit = lm(PULSE ~ FITNESS + TEST, data=d)
    Anova(fit, type="III")
    
    #SLIDE 10: posthoc comparisons
    contrast(emmeans(fit,~FITNESS), method="pairwise", adjust="Tukey")
    
    #SLIDE 12: fit a mixed model
    fit2 = lmer(PULSE ~ FITNESS + TEST + (1|ID), data=d)
    
    #SLIDE 13:
    Anova(fit2, type="III")
    
    #SLIDE 14:
    summary(fit2)
    
    #SLIDE 15:
    contrast(emmeans(fit2,~FITNESS), method="pairwise", adjust="Tukey")
    
    #SLIDE 16:
    AICc(fit, fit2)
    
    #SLIDE 17: effect plots
    plot(allEffects(fit2))
    
    shapiro.test(residuals(fit2))
    outlierTest(fit2)
    max(cooks.distance(fit2))
    
    # SLIDE 22: load data 'schools'
    d2 <- read.xlsx("data/schools.xlsx")
    head(d2)
    str(d2)
    #code class and school as factors (will be included as random factors)
    d2$school <- as.factor(d2$school)
    d2$class <- as.factor(d2$class)
    
    #SLIDE 23: raw data plot
    plot(test_result ~ breakfast_cal, data=d2, col=school, pch=c(0,4,19)[class])
    
    #SLIDE 24: fit a mixed model with nested random effects:
    fit3 <- lmer(test_result ~ breakfast_cal + (1|school/class), data=d2)
    
    #SLIDE 25:
    summary(fit3)
    
    #SLIDE 26:
    plot(allEffects(fit3))
    
    #SLIDE 29: load queen pheromone data in both formats, but we will use the long
    #dataset so we just overwrite the wide one
    d3 <- read.csv("data/queenpheromone_long.csv")
    head(d3)
    str(d3)
    
    #code 'colony' and 'treatment' as factors
    d3$colony <- as.factor(d3$colony)
    d3$treatment <- as.factor(d3$treatment)
    
    #SLIDE 31: run a generalized linear mixed model
    fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data = d3)
    
    #SLIDE 32:
    summary(fit3)
    
    #SLIDE 33:
    Anova(fit3, type="III")
    
    #SLIDE 34:
    plot(allEffects(fit3))
    
    #SLIDE 36:
    plot(allEffects(fit3), type="response")
    
    #SLIDE 37:
    d3$treatment = relevel(d3$treatment, ref="control")
    #we have to run the fit again after releveling, otherwise the labels for the differen
    #factor levels get mixed up
    fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data=d3)
    
    #SLIDE 38: posthoc comparisons
    contrast(emmeans(fit3,~treatment), method="trt.vs.ctrl", adjust="Tukey")
    
    #SLIDE 39: model selection
    fit3 <- glmer(ovarydev~treatment+colonysize+(1|colony),family=binomial,data=d3)
    fit4 <- glm (ovarydev~treatment+colonysize,family=binomial,data=d3)
    AICc(fit3, fit4)
    
    #SLIDE 42: create observation level factor variable
    d3$obs <- factor(1:nrow(d3))
    
    #SLIDE 43: run model that includes this variable to check for overdispersion
    fit5 <- glmer(ovarydev~treatment+colonysize+(1|colony)+(1|obs),family=binomial,data=d3)
    AICc(fit3, fit5) #original model is better, no overdispersion
    
    #SLIDE 47: read data
    darwin <- read.csv("data/darwin.csv")
    head(darwin)
    
    #SLIDE 48: original linear fit:
    fit6 <- lm(height~type, data=darwin)
    summary(fit6)
    ncvTest(fit6) #unequal variances!
    
    #SLIDE 49: new fit allowing for different variances between levels of 'type'
    fit7 <- gls(height~type, data=darwin, weight=varIdent(form=~1|type))
    
    #SLIDE 50
    summary(fit7)
    
    #SLIDE 51
    plot(allEffects(fit7))
    
    #SLIDE 52
    #we cannot directly compare AICc of gls and lm models...
    #but we can first rewrite the lm as a gls model and THEN compare the two
    
    fit6b <- gls(height~type, data=darwin)
    AICc(fit6, fit6b) #fit6 and fit6b are the same models in lm and gls, but have different
    #AICc, even if they have the same estimates:
    coef(fit6)
    coef(fit6b)
    
    #so if we want to compare both models (with and without allowing non-equal variances),
    #we should compare both gls models: fit6b and fit7
    AICc(fit6b, fit7)
    
    #SLIDE 56
    d4 <- read.csv("data/hawaiibirds.csv")
    str(d4)
    #we will only work with a subset of the data for this example
    d5<-subset(d4, d4$Birds!="NA" & d4$Species==3 & d4$Island==1)
    head(d5)
    #no need to recode - we will only use 'Year' and 'Birds'
    
    #SLIDE 57: raw data plot
    plot(d5$Birds~d5$Year, type=c("l"))
    
    #SLIDE 58
    fit8 <- lm(Birds~Year, data=d5)
    summary(fit8)
    
    plot(fit8)
    
    #SLIDE 60
    acf(residuals(fit8))
    
    #SLIDE 61
    fit9 <- gls(Birds~Year, data = d5, correlation = corAR1(form =~Year))
    
    #SLIDE 62
    summary(fit9)
    
    #SLIDE 63: effect plots - just made sure here that they are both on the same
    #scale (y-axis between -100 and 300)
    plot(allEffects(fit8, residuals=TRUE), smooth.residuals = FALSE, ylim = c(-100,300))
    plot(allEffects(fit9, residuals=TRUE), smooth.residuals = FALSE, ylim = c(-100,300))
    
    #SLIDE 64
    #as before, to compare the gls model with the model that does not have a specific
    #correlation structure, we need to code the first model (the lm) as gls
    fit8b <- gls(Birds~Year, data=d5)
    AICc(fit8b, fit9)

#What is the effect of age and parasite load on disease outcome?
  #data
    squirrels <- read.xlsx("data/Squirrels.xlsx")
    head(squirrels)
    str(squirrels)
    
    squirrels <- squirrels %>% rename(age = Age, 
                                      parasites = Parasite_load, 
                                      outcome = Outcome)
    
    plot(parasites ~ age, data = squirrels)
    boxplot(parasites ~ outcome, data = squirrels)
    boxplot(age ~ outcome, data = squirrels)

  #multinomial models
    fit1 <- multinom(outcome ~ age + parasites, data = squirrels)
    fit2 <- multinom(outcome ~ age * parasites, data = squirrels)
    AICc(fit1, fit2) #fit2 with interaction is much better
    summary(fit2)
    Anova(fit2, type = "III") #significant interaction + sig effect of parasaites
    
    plot(allEffects(fit2), multiline = TRUE, confint = list(style = "auto"))

    #stacked plot
    parasite_loads <- seq(10, 150, length.out = 100)
    ages <- c(10, 30, 50)
    new_data <- expand.grid(parasites = parasite_loads, age = ages)
    predicted_probs <- predict(fit2, newdata = new_data, type = "probs")
    predicted_df <- cbind(new_data, predicted_probs)
    predicted_long <- melt(predicted_df, id.vars = c("parasites", "age"), variable.name = "outcome", value.name = "Probability")
    
    ggplot(predicted_long, aes(x = parasites, y = Probability, fill = outcome)) +
      geom_area() +
      facet_wrap(~ age, ncol = 1) +
      labs(x = "Parasite Load",
           y = "Probability") + 
      theme_classic()

#Babblers - does their cognitive performance vary with age and sex?
  d1 <- read.xlsx("data/Babbler_Dataset.xlsx")
  head(d1) #PC1 = principal component 1; GCP = general cognitive performance
  str(d1)
  d1$ID <- as.factor(d1$ID)
  d1$SEX <- as.factor(d1$SEX)
  d1$GROUP_ID <- as.factor(d1$GROUP_ID)
  
  d1 <- subset(d1, d1$SEX != "U") #removes birds with unknown sex
  plot(GCP~SEX, data=d1)
  plot(GCP~AGE, data=d1)

  #models
  fit1 <- lm(GCP ~ SEX * AGE, data=d1)
  fit2 <- lm(GCP ~ SEX + AGE, data=d1)
  fit3 <- lm(GCP ~ SEX, data=d1)
  fit4 <- lm(GCP ~ AGE, data=d1)
  
  AICc(fit1, fit2, fit3, fit4)
  
  summary(fit4) #no effect
  Anova(fit4, type="III") #same; no effect of age on cognitive performance
  
  #mixed models
  fit1b <- lmer(GCP ~ SEX * AGE + (1|GROUP_ID), data=d1)
  fit2b <- lmer(GCP ~ SEX + AGE + (1|GROUP_ID), data=d1)
  fit3b <- lmer(GCP ~ SEX + (1|GROUP_ID), data=d1)
  fit4b <- lmer(GCP ~ AGE + (1|GROUP_ID), data=d1)
  
  AICc(fit1b, fit2b, fit3b, fit4b)
  
  summary(fit4b)
  Anova(fit4b, type="III")
  #now we conclude that age has a significant negative effect on cognitive performance
  
  AICc(fit4, fit4b) #the model with random effect is also better based on AICc
  
  summary(fit3b)
  Anova(fit3b, type="III") #check because AICc was close to model4b; but we see no effect of sex
  
  # since fit3b had an AICc that was very close to the one of fit4b, we also consider the conclusions
  # of this model. Here, the outcome is that sex does not have a significant effect on cognitive performance.
  # Taken together, we can conclude that age has a significant negative effect and we have not found any
  # evidence for a positive effect of sex on cognitive performance.
  
  plot(allEffects(fit4b)) #GCP declines with age
  
  #checking assumptions:
  shapiro.test(resid(fit4b))
  outlierTest(fit4b)
  cooks.distance(fit4b) > 1
  #all good
  
#do owl chicks change vocalisation based on food abundance/shortage and feeding parent sex?
  d2 <- read.xlsx("data/owls.xlsx")
  head(d2)
  str(d2)
  #we here use 'mutate_if' from the package 'dplyr' to change all character variables
  #to factors at the same time, but you can of course also change them one by one.
  d2<-mutate_if(d2,is.character, as.factor)
  str(d2)
  
  par(mfrow=c(1,2))
  plot(Vocalizations~SexParent, data = d2)
  plot(Vocalizations~FoodTreatment, data = d2)
  par(mfrow=c(1,1))

  #models
  fit5=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=d2)
  summary(fit5)   
  Anova(fit5, type="III") #we detect a significant effect of food treatment and of the sex of the parent
  plot(allEffects(fit5), type="response") 

  fit6=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest),family=poisson(link=log),data=d2)
  summary(fit6) #the interaction is significant
  Anova(fit6, type="III") 
  AICc(fit5, fit6) #keep the model with the interaction
  plot(allEffects(fit6), type="response", multiline=T, confint=list(style="auto"))
  #vocalisations decrease with food for both chicks w male and female feeding parents; 
  #but the decrease is more stark in female parents (which start with higher vocalisation numbers)

    
  #d. Check if there is any overdispersion in the model. Recall that has to be done 
  #in a different way than if you have a glm (without random factors).
  #If the overdispersed model is better, check the summary table.
  #Do any of your conclusions change? Does it make sense to try to fit any other 
  #models based on your conclusions? Visualize the final version of your model.
  
  #we must first include an observation-level variable
  d2$obs <- factor(1:nrow(d2))
  head(d2)
  
  #then we run the same model, now including that new variable as a random factor:
  fit6b=glmer(Vocalizations~FoodTreatment*SexParent+(1|Nest)+(1|obs),family=poisson(link=log),data=d2)
  
  AICc(fit6, fit6b) #the model with overdispersion has much better AICc, keep it!
  #make an effect plot
  summary(fit6b)
  Anova(fit6b, type="III") #interaction and effect of sex no longer significant!
  plot(allEffects(fit6b), type="response", multiline=T, confint=list(style="auto"))
  
  #since the interaction is now no longer significant, we may try to fit the
  #model without interaction, but with accounting for overdispersion. 
  fit7=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest),family=poisson(link=log),data=d2)
  fit7b=glmer(Vocalizations~FoodTreatment+SexParent+(1|Nest)+(1|obs),family=poisson(link=log),data=d2)
  
  AICc(fit6, fit6b, fit7, fit7b) 
  #it is close, but we drop the interaction from the model and keep the observation level random effect
  summary(fit7b)
  Anova(fit7b, type="III") #
  plot(allEffects(fit7b), type="response")
  #since the AICc models 6b and 7b are very close in AICc, we may report both. Our conclusions will remain largely
  #the same between both models: the coefficients for the main effects are similar and the interaction
  #is not significant in the model with the interaction.
  
  #testing for outliers and influential observations for a glmer is not straigthforward and we do not do this in this course

  #we conclude that the owl chicks do more 'sibling negotiation vocalizations' if they
  #are food deprived than if they are satiated. Based on our data, we can not conclude
  #that the sex of the parent matters for the number of vocalizations and we have no support
  #for a significant interaction between the two.
  
  
  
  
        
    
        
    

#Modelling non-linear relationships ----

#DNase - the optical density that was measured for a number of different concentrations
  #of the enzyme DNase. In this exercise, we consider only the "Run = 1" assay.
  #We have all measurements in duplo.
  
  d1 <- subset(DNase, Run == 1)
  head(d1)
  str(d1)
  plot(density~conc, data=d1)
  
  #optical density ~ max_density/(1+exp((xmid-log(concentration))/scale))
    #In that equation, max_density, xmid and scale are parameters that 
    #need to be estimated by our nonlinear model, while 'concentration' is 
    #our predictor variable (named 'conc' in our dataset).

  fit <- nls(density ~ max_density/(1+exp((xmid-log(conc))/scale)), 
             start = list(max_density=1, xmid=1, scale=1), data=d1)
  
  fitx <- lm(density ~ conc, data=d1) #if you would fit a simple lm, you would see 
  # a strong deviation from the linearity assumption
  residualPlots(fitx)
  
  summary(fit)#the parameters estimates are given under 'Estimate' in the summary table
  
  # Make two plots: one of the fit with 95% confidence intervals and
  #one of the fit with 95% prediction intervals
  
  par(mfrow = c(1,2)) #this allows us to make two plots next to each other 
  plotFit(fit, interval="confidence") # interpretation: if you ran the experiment again, you have 95%
  # confidence that your fit will fall within the interval
  plotFit(fit, interval="prediction") # interpretation: if you have a new observation, you have 95% 
  # confidence that it will fall within the interval
  par(mfrow = c(1,1)) # go back to just one graph in the graphics device
  
  preds <- predict(fit, list(conc=4)) #we can make predicions now based on our fit
  preds
  
  shapiro.test(residuals(fit)) # residuals do not diverge from normal distribution
  plot(fit) #no clear systematic deviations - the non-linear function that we used
  #seems to have been appropriate for fitting the data

  #Our model estimated that the maximum optical density is 2.34. In other words, our
  #model estimates based on this nonlinear function that no matter how high we raise
  #the DNAse concentration, the optical density will approach but not exceed 2.34.
  #Also, our model estimates xmid to be 1.48, meaning that half of the maximum optical
  #density is reached at a log(concentration) of 1.48.
  
  
#Medicinal treatment (predict concentration based on dose and time)
  d2 <- Theoph
  
  head(d2)
  str(d2)
  plot(d2) #because we used groupedData, this plots separate graphs for all subjects.
  
  #We want to predict the concentration as a function of time and dose, while
  #also accounting for random variation between subjects. Assume that you knew
  #beforehand that the self-starting function 'SSfol' is the appropriate
  #function to model this relationship.
  #there are five arguments: Dose, input, lKe, lKa and lCl. The first two are predictor
  #variables from our data, while the other three are parameters that are to be estimated
  #by our model.
  
  fit2 = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKa + lCl + lKe ~ 1|Subject)
  
  summary(fit2)

  plot(augPred(fit2, level=c(0,1))) #model predictions for each subject
  #the fixed parts are different between the individuals, because the individuals
  #all got a different dose. So even without the random individual variation between
  #Subjects, our model arrives at different predictions for them.
  
  #check visually if there seems to be any temporal autocorrelation in the model.
  plot(ACF(fit2)) #there does not seem to be any temporal autocorrelation (there is
  #only a serious correlation at lag 0, which is 1 by definition, 
  #because points correlate perfectly with themselves)
  
  fit2b = nlme(
    conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
    data=d2,
    fixed = lKe + lKa + lCl ~ 1, 
    random = lKa + lCl + lKe ~ 1|Subject,
    correlation = corCAR1(form = ~Time)) #time autocorrelation
  
  AICc(fit2, fit2b) #the first model is better, so we keep it!
  
  shapiro.test(residuals(fit2)) #it's close, but W is just over 0.9 so we are OK
  plot(fit2) #there are no very obvious systematic deviations from our predictions here.
  
#triceps
  d3 <- triceps
  head(d3)
  str(d3)
  plot(triceps~age, d3)

  fit3 <- lm(triceps ~ ns(age, df=10), data=d3)
  summary(fit3)
  
  plot(triceps~age, d3, col=alpha("black", 0.2))
  xvals=0:550/10
  preds=predict(fit3,data.frame(age=xvals),se=T)
  lines(xvals, preds$fit, col="red", lwd=2)
  lines(xvals, preds$fit + preds$se.fit * 1.96, col="red", lty=2)
  lines(xvals, preds$fit - preds$se.fit * 1.96, col="red", lty=2)
  
  
  #best number of knots
  
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
  
  #assumptions
  
  hist(residuals(fit4), breaks=-15:25)
  shapiro.test(residuals(fit4)) #deviation from normality...
  ncvTest(fit4) #clear deviation from homogeneous variances
  spreadLevelPlot(fit4) 
  outlierTest(fit4)#there are a number of significant outliers
  max(cooks.distance(fit4))#but no influential observations
  
  fit_rob<- lmrob(triceps ~ ns(age, df=5), data=d3) #lmrob does not converge to a better, more robust model
  
  #  specifying custom variance along a continuous predictor requires specification of how you want to let your residuals vary
  # along this continuous predictor. This is not required for this course
  
  
#Survival analysis ----
set_sum_contrasts()
  
  data(heart, package = "survival")
  
  stanford2=stanford2[complete.cases(stanford2),]
  rownames(stanford2)=1:nrow(stanford2)
  stanford=stanford2
  head(stanford)
  str(stanford)
    # variables in dataset:
  # id = patient ID
  # time = survival time or censoring time (in days)
  # status = censoring status (1=died, i.e. complete, 0=didn't die, ie censored)
  # age = age of patient when heart transplant was performed
  # t5 = degree of mismatch in immune system
  
  #What kind of censoring do we have?
  
  # the data is right censored, and we would specify this in the model by using
  # outcome variable Surv(time, status, type = "right")
  fit = survreg(Surv(time, status, type = "right") ~ age + t5, data=stanford)
  
  #Determine the best fitting hazard distribution for the dataset (AICc)
  fit0 = flexsurvreg(Surv(time, status, type = "right") ~ age + t5, dist = "exponential", data=stanford)
  fit1 = flexsurvreg(Surv(time, status, type = "right") ~ age + t5, dist = "weibull", data=stanford)
  fit2 = flexsurvreg(Surv(time, status, type = "right") ~ age + t5, dist = "gengamma", data=stanford)
  AICc(fit0,fit1,fit2)
  #      df     AICc
  # fit0  3 1639.231
  # fit1  4 1577.398 # lowest, so best
  # fit2  5 1577.912
  # ---> Weibull= best (lowest AICc)
  
  # corresponding survreg model (Anova type III tests only support survreg models,
  # but flexsurvreg easier to plot)
  fit = survreg(Surv(time, status, type = "right") ~ age + t5, dist = "weibull", data=stanford) 
  
  
  #Plot mean survivorship of patients and hazard in function of time
  plot(fit1, ylab = "Survivorship", xlab = "time (days)", main = "Weibull")
  plot(fit1, ylab = "Hazard", xlab = "time (days)", main = "Weibull", type="hazard")

  # Which of the covariates (age, t5) significantly affects survival when used in
  # a predictive model?
  fit1$res
  #                 est          L95%          U95%           se
  # shape  5.550420e-01    0.47074906  6.544286e-01 4.664666e-02
  # scale  1.896503e+04 2784.31566553  1.291780e+05 1.856472e+04
  # age   -5.670125e-02   -0.09697416 -1.642834e-02 2.054778e-02
  # t5    -3.130435e-01   -0.95591740  3.298304e-01 3.280029e-01
  
  Anova(fit, type ='III')
  # Analysis of Deviance Table (Type III tests)
  #       LR Chisq Df Pr(>Chisq)   
  #   age   8.5037  1   0.003544 **
  #   t5    0.8907  1   0.345282  
  
  # age negatively affects survival (p = 0,0035), immune mismatch t5 has no sign
  # effect on survival (p = 0,345)
  
  #Check the quality of the fit based on the deviance
  
  # Deviance is a measure of departure from optimal model fit & should not
  # vary systematically as a function of any of the factors or covariates
  dev = residuals(fit,type="deviance")
  par(mfrow = c(1, 2))
  for (f in c("age","t5")) {
    plot(stanford[,f],dev,xlab=f,ylab="Deviance") 
    abline(h = 0, lty = 2)
  }
  par(mfrow = c(1, 1))
  # flat, so OK
  
  #Plot survivorship curves for 40 and 50 year old patients based on your model fit. Which die faster?
  newstanford_40 = data.frame(age = 40, t5 = mean(stanford$t5))
  newstanford_50 = data.frame(age = 50, t5 = mean(stanford$t5))
  plot(fit1, newdata=newstanford_40,ylab = "survivorship", xlab = "time (days)", main = "Survivorship after heart transplant (blue=40 yr old, red=50 yr old)", 
       ci=T, col="blue", col.obs=rgb(0,0,0,0))
  plot(fit1, newdata=newstanford_50,ylab = "survivorship", xlab = "time (days)", 
       ci=T, col="red", col.obs=rgb(0,0,0,0), add=T)
  #40 year old patients live longer than 50 year old patients
  
  #vg. survival time of patients of average age & their 95% CIs?
  confint(emmeans(fit, ~age), type="response")
  #  age response  SE  df lower.CL upper.CL
  # 41.7     1255 231 153      872     1806
  # for an average age of 41.7, the average survival time (in days) is 1255, 
  # with 95% conf int = (872, 1806)
  
  # we can also make specific predictions based on our fit (e.g. for an age of 24
  # and a mean t5 value), though the survivorship curves are more informative
  predict(fit, list(age=40, t5= mean(stanford$t5)))
  # 1384.023 
  
  
  #Are posthoc tests useful here? What are they used for?
  # They are not useful here as there are no factors included in the analysis, we
  # only have continuous covariates. Posthoc tests are useful to compare the mean
  # survival time among different groups. E.g. : sex, age groups, different
  # treatments, ...
  
  
  
#NONPARAMETRIC COX PROPORTIONAL HAZARDS REGRESSION & KAPLAN-MEIER SURVIVAL ANALYSIS
  #(MODELS HAZARD RATE AND HAZARD RATIOS)
  data(heart,package="survival")
  
  stanford2=stanford2[complete.cases(stanford2),]
  rownames(stanford2)=1:nrow(stanford2)
  stanford=stanford2
  head(stanford)
  str(stanford)
  
  # A) Make a new model using cox proportional hazards with factors age and t5
  # included as covariates (use main effects only). Based on an Anova table, which
  # of the variables is significant? What is their influence on the hazard
  # function? Check the quality of the fit by looking at the deviance in function
  # of each covariate. Is there any indication of a bad fit?
  
  #Make a new model using a Cox proportional hazards with factors age and t5 as covariate. 
  #Interpret summary coefficient table. #
  head(stanford)
  fit = coxph(Surv(time, status, type = "right") ~ age + t5, data=stanford)
  
  fit   # SUMMARY TABLE
  #        coef exp(coef) se(coef)     z       p
  # age 0.02961   1.03006  0.01136 2.608 0.00911
  # t5  0.17041   1.18579  0.18326 0.930 0.35243
  # Age is significant, unlike t5. when age increases with 1, the hazard function
  # increases with exp(1.0301)
  
  
  # We can check the quality of the fit (based on the so-called deviance, which is
  # a measure of departure from optimal model fit). Deviance should not vary
  # systematically as a function of any of the factors or covariates
  dev = residuals(fit,type="deviance")
  par(mfrow = c(1, 2))
  for (f in c("age","t5")) {
    plot(stanford[,f],dev,xlab=f,ylab="Deviance") 
    abline(h = 0, lty = 2)
  }
  par(mfrow = c(1, 1))
  #  Flat -> ok
  
  
  #Plot the overall Kaplan Meier survival curve & then plot the Kaplan Meier survival curve separately for 40 and 50 year olds. Compare their survival.
  
  autoplot(survfit(fit), ylab = "survivorship", xlab = "time (days)", main = "Heart transplant patients") 
  
  newstanford_40=with(stanford,data.frame(age = 40, t5 = mean(t5)))
  newstanford_50=with(stanford,data.frame(age = 50, t5 = mean(t5)))
  p1 = autoplot( survfit(fit, newdata = newstanford_40), surv.colour = "blue", 
                 ylab = "survivorship", xlab = "time (days)", main = "40 year old heart transplant patients" ) 
  p2 = autoplot( survfit(fit, newdata = newstanford_50), surv.colour = "red", 
                 ylab = "survivorship", xlab = "time (days)", main = "50 year old heart transplant patients" )
  multiplot(p1,p2,cols=1) # 50 year old patients have lower survival

#Visualisation ----
set_sum_contrasts() # use effect coding for all fitted models
  
#EXERCISE 1
  
  # We have data ("isolation.xlsx") on the presence or absence of a species on
  # several islands in an archipelago. For each island, we also have its size
  # (km2) and the distance to the mainland (in km). We are interested in whether
  # the species" presence on an island can be predicted by how big it is and how
  # far it is from the mainland. To investigate this, we fit a binomial GLM and
  # make effect plots to show the effect of both area and distance :
  
  d1 = read.xlsx("data/isolation.xlsx")
  fit_ex1 = glm(presence ~ area + distance, family=binomial(link=logit), data=d1)
  
  plot(allEffects(fit_ex1, xlevels=100), type="response") # NOTE: xlevels=100 added here to show effect plot for 100 X values; 
  #For a continuous predictor, xlevels contains a sequence of values at which you want to evaluate the effects.
  
  #step a: making a dataframe of the effect of area and distance on the probability of presence
  
    # First we need to extract the marginal means for both area and distance over 
    # approximately the range that they vary in the dataset (here slightly extended).
    # To do this we will need the "at=" argument in which we specify a list with a
    # sequence of the desired values of each variable (using seq with length.out=100
    # to get 100 values between those extremes) :
    
    # for the effect of area:
    fit1_ex1_area_df = data.frame(emmeans(fit_ex1, ~ area, at = list(area = seq(0, max(d1$area)+1, length.out=100)), 
                                          type="response"))
    head(fit1_ex1_area_df) # check column names to use in ggplot
    colnames(fit1_ex1_area_df)
    
    # for the effect of distance:
    fit1_ex1_distance_df = data.frame(emmeans(fit_ex1, ~ distance, at = list(distance = seq(0, max(d1$distance)+2, length.out=100)), 
                                              type="response"))
    head(fit1_ex1_distance_df) # check column names to use in ggplot
    colnames(fit1_ex1_distance_df)
    
    # ggplot2 plots:
    (plot1 = ggplot(data=fit1_ex1_area_df, aes( x=area , y=prob ))+                           # specify dataframe & x & y variables
      geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha = 0.1)+                         # show 95% confidence intervals using a semitransparent ribbon
      geom_line(linewidth=1)+                                                                # show marginal mean with linewidth of 1
      # Important for plotting from the original dataset is to include it in the front and then use the mapping so the function knows where to get the data
      geom_point(data=d1, mapping=aes(x=area, y=presence), shape=16, color="red2", size=3, alpha=0.3)+ # show original datapoints as semitransparent red disks
      xlab(expression("Island area (km"^2*")"))+                                             # X axis title, using expression to show ^2 as superscript
      ylab("Probability of species being present")+                                          # Y axis title
      theme_few(base_size=16))                                                                # theme_few with base text size of 16

    (plot2 = ggplot(data=fit1_ex1_distance_df, aes( x=distance , y=prob ))+                   # specify dataframe & x & y variables
      geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha = 0.1)+                         # show 95% confidence intervals using a semitransparent ribbon
      geom_line(linewidth=1)+                                                                # show marginal mean with linewidth of 1
      # Important for plotting from the original dataset is to include it in the front and then use the mapping so the function knows where to get the data
      geom_point(data=d1, mapping=aes(x=distance, y=presence), shape=16, color="red2", size=3, alpha=0.3)+ # show original datapoints as semitransparent red disks
      xlab("Distance from mainland (km)")+                                                   # X axis title, .
      ylab("Probability of species being present")+                                          # Y axis title
      theme_few(base_size=16))                                                                # theme_few with base text size of 16

    # grid.arrange allows you to put multiple plots in a specific arrangement of your choice
    # (here in a single column) & allows you to specify a common spanning Y axis title using
    # argument left with a correctly specified textGrob (with rot = 90, gp = gpar(fontsize = 17)).
    # Before adding the common Y axis title you can remove the previously provided separate Y axis titles
    # by passing plot1 + rremove("ylab") & plot2 + rremove("ylab") as your ggplot objects  :
    (plot3 = grid.arrange(plot1 + rremove("ylab"), 
                         plot2 + rremove("ylab"), 
                         left = text_grob("Probability of species being present", # common spanning Y axis title
                                         rot = 90, size = 17), 
                         ncol=1)) # single column (one figure below the other)
    #The textGrob function in R is part of the grid package, which is used for creating graphical objects in a grid layout. The textGrob function specifically is 
    #used to create a text graphical object (a "grob") that can be placed on a plot. It allows you to specify various attributes of the text, 
    #such as its content, position, font, color, and alignment.

    
#EXERCISE 2
  
  # In a study by Maes et al. (2005), the authors investigated influence of
  # genetic variation (measured as the "multilocus allozyme heterozygosity") on
  # bioaccumulation of heavy metals in three different river basins (Maas, IJzer,
  # Schelde) in the Atlantic eel (Anguilla anguilla). The hypothesis was that
  # individuals with a greater multilocus heterozygosity score
  # (MULTILOCUS_HETEROZYGOSITY_ALLOZYME) can better "detoxify" themselves compared
  # to homozygous individuals, and we are also interested in how this relationship
  # might be different between the different river systems. To this end, we fitted
  # a homogeneity-of-slopes linear model & made an effect plot showing the
  # relationship between heavy metal accumulation & multilocus heterozygosity in
  # the different river basins. The first aim is to remake this effect plot in a more
  # beautiful way using emmeans & ggplot2. A second aim is to plot the estimated
  # slopes of the heavy metal accumulation in function of multilocus heterozygosity
  # across the different rivers plus overall (calculated using emtrends) & annotate
  # the significance of the difference from zero.
  eel = read.xlsx("data/eel2.xlsx")
  eel = mutate_if(eel, is.character, as.factor)
  # specify desired order & custom labels (this will be order in later plots,
  # here ordered from highest avg heavy metal accumulation to lowest) :
  eel$RIVER = factor(eel$RIVER, 
                     levels=c("MAAS", "SCHELDE", "IJZER"),
                     labels=c("Maas", "Schelde", "Ijzer"))
  eel$RIVER # all caps looked a bit ugly in later plot, so changed labels
  
  # this was our homogeneity of slopes model
  fit_ex2 = lm(HEAVY_METAL_ACCUM ~ RIVER * MULTILOCUS_HETEROZYGOSITY_ALLOZYME, data=eel) 
  Anova(fit_ex2, type="III")
  
  # effect plot of combined effects of river & multilocus heterozygosity 
  plot(allEffects(fit_ex2))
  
  #here we already made the effect plot a bit nicer:
  plot(Effect(focal.predictors=c("MULTILOCUS_HETEROZYGOSITY_ALLOZYME","RIVER"),
              mod=fit_ex2, residuals=T), 
       smooth.residuals=F,
       residuals.color=adjustcolor("blue",alpha.f=0.2), 
       residuals.pch=16,
       band.colors="grey2")
  
  
  #Make more beautiful effect plot illustrating the effect of river and
  # multilocus heterozygosity using ggplot2, using marginal means calculated with
  # the emmeans package, and coerce output to a dataframe before plotting
  # using as.data.frame. 
  
  # Use a geom_ribbon ggplot2 layer to illustrate the 95%
  # confidence intervals and a geom_line layer to illustrate the marginal means.
  # Facet by river & show the different river systems in a single row. 
  # Colour points & lines by RIVER & give them the colours red2, purple & blue.
  # Use a semi-transparent fill in the same colours for the confidence interval
  # ribbons & omit the outlines by specifying color=NA in the geom_ribbon layer.
  # Also superimpose the actual datapoints from the dataset (in dataframe eel),
  # using a semi-transparent colour and solid disks (shape=16). 
  # Also provide appropriate axis titles. 
  # Use theme_par with a base_size of 8. 
  # Also provide appropriate options via theme to show the X axis labels
  # rotated 45 degrees and adjust your axis title size to use a font size of 12.
  # Suppress the legend, which is here superfluous.
  # Export the figure in Powerpoint .pptx and .svg formats at a size of 6.5 x 4.5 inches.
  
  
  # First: calculate the marginal means with emmeans (try it yourself this time):
  
  # Calculate marginal means by river & multilocus heterozygosity (RIVER * MULTILOCUS_HETEROZYGOSITY_ALLOZYME) for 100 values  of 
  # multilocus heterozygosity, ranging between the extremes (min and max) observed in the dataset
  # using emmeans (and again the "at" argument) & convert output to a dataframe using as.data.frame
  
  
  fit_ex2_emmeans_df = data.frame( emmeans(fit_ex2, ~ RIVER * MULTILOCUS_HETEROZYGOSITY_ALLOZYME, 
                                           at = list(MULTILOCUS_HETEROZYGOSITY_ALLOZYME = seq(min(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME),
                                                                                              max(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME),
                                                                                              length.out=100))) )
  head(fit_ex2_emmeans_df) # check column names to use in ggplot
  colnames(fit_ex2_emmeans_df)
  
  # Make ggplot:
  (plot4 = ggplot(data = fit_ex2_emmeans_df,                                                # dataframe with emmeans
                 aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, y = emmean, color = RIVER))+    # specify x & y & color points and lines by RIVER
    facet_wrap(~RIVER, nrow=1)+                                                            # facet by RIVER
    geom_ribbon(aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, ymin = lower.CL, ymax = upper.CL, fill = RIVER), 
                alpha = 0.1, color = NA)+                                                  # add 95% confidence intervals ribbon, using ribbon transparancy of 0.1 and color=NA (or linetype=0) to remove outline
    geom_line(linewidth = 1)+                                                              # marginal means shown as lines with line thicknes of 1
    geom_point(eel,mapping=aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME,
                               y = HEAVY_METAL_ACCUM, color = RIVER), 
               shape = 16, alpha = 0.3)+                                                   # original data points shown as semi-transparent solid disks
    scale_color_manual(values=c("red2", "purple", "blue"))+                                # manual colours for lines & points
    scale_fill_manual(values=c("red2", "purple", "blue"))+                                 # manual colours for ribbon fill
    ylab("Heavy metal accumulation")+                                                      # Y axis title
    xlab("Allozyme multilocus heterozygosity")+                                            # X axis title
    theme_par(base_size=8) +                                                               # use theme theme_par with base_size for text of 8
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),                            # rotate X axis labels 45 degrees & adjust placement with hjust & vjust
          axis.title=element_text(size=12)) +                                              # adjust axis titles to font size of 12
    theme(legend.position="none"))                                                         # leave out legend (redundant)
  
  
  
  # #EXTRA: PREDICTION INTERVALS
  #
  # # NOTE: In the same way we can also plot total model predictions. In that case, we can
  # # include both confidence & prediction intervals and superimpose them as two
  # # semi-transparent geom_ribbon layers. 
  # 
  # # We then first have to calculate predictions & CIs & PIs over a grid of specific covariate values : 
  # 
  # # Dataframe with values of independent variables for which you would like to 
  # # get predicted values for :
  newdata = expand.grid(MULTILOCUS_HETEROZYGOSITY_ALLOZYME = seq(min(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME), 
                                                                 max(eel$MULTILOCUS_HETEROZYGOSITY_ALLOZYME)+0.05, 
                                                                 length.out=100),
                        RIVER = levels(eel$RIVER))
  
  # # Dataframe with model predictions & 95% confidence & prediction intervals
  # # for covariate values in newdata :
  preds = newdata %>% # we are using dplyr here; we are adding predictions & CIs & PIs to newdata
    mutate(data.frame(predict(fit_ex2, # dataframe with confidence intervals
                              newdata = newdata, 
                              interval = "confidence", 
                              level = 0.95))) %>% # mutate = add columns
    rename(lower.CI = lwr, upper.CI = upr) %>% # rename columns: NEW_NAME = OLD_NAME
    mutate(data.frame(predict(fit_ex2, # dataframe with prediction intervals
                              newdata = newdata, 
                              interval = "prediction", 
                              level = 0.95))) %>% # mutate = add columns
    rename(lower.PI = lwr, upper.PI = upr) # rename columns: NEW_NAME = OLD_NAME
  head(preds)
  # 
  # # Make ggplot:
  plot4B = ggplot(data = preds,                                                # dataframe with model predictions & CIs & PIs
                  aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, y = fit, color = RIVER))+     # specify x & y & color points and lines by RIVER
    facet_wrap(~RIVER, nrow=1)+                                                            # facet by RIVER
    geom_ribbon(aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, ymin = lower.CI, ymax = upper.CI, fill = RIVER), 
                alpha = 0.4, color = NA)+                                                  # add 95% confidence intervals ribbon, using ribbon transparancy of 0.4 and color=NA (or linetype=0) to remove outline
    geom_ribbon(aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME, ymin = lower.PI, ymax = upper.PI, fill = RIVER), 
                alpha = 0.2, color = NA)+                                                  # add 95% prediction intervals ribbon, using ribbon transparancy of 0.2 and color=NA (or linetype=0) to remove outline
    geom_line(linewidth = 1)+                                                              # model predictions shown as lines with line thicknes of 1
    geom_point(eel,mapping=aes(x = MULTILOCUS_HETEROZYGOSITY_ALLOZYME,
                               y = HEAVY_METAL_ACCUM, color = RIVER), 
               shape = 16)+                                                                # original data points shown as non-transparent solid disks
    scale_color_manual(values=c("red2", "purple", "blue"))+                                # manual colours for lines & points
    scale_fill_manual(values=c("red2", "purple", "blue"))+                                 # manual colours for ribbon fill
    ylab("Heavy metal accumulation")+                                                      # Y axis title
    xlab("Allozyme multilocus heterozygosity")+                                            # X axis title
    theme_grey(base_size=10) +                                                             # here using default grey theme with base_size for text of 10
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),                            # rotate X axis labels 45 degrees & adjust placement with hjust & vjust
          axis.title=element_text(size=12)) +                                              # adjust axis titles to font size of 12
    coord_cartesian(expand=FALSE) +                                                        # make ribbon stretch to side of plot & don"t extend axis range
    theme(legend.position="none")                                                          # leave out legend (redundant)
  
  
  #Plot the marginal mean trends of the heavy metal accumulation in function of
  # eterozygosity. 
  
  
  # for the plot:
  # Use red2, purple, blue and black for the estimates for the 3 rivers & the overall mean slope. 
  # Use a geom_errorbar layer for the confidence intervals (using a width of 0.2 and a
  # linewidth of 0.5) and a geom_point layer for the actual estimated slopes (use
  # a point size of 2). 
  
  # Use theme_par with a base_size of 10. 
  # Add an appropriate Y axis label & use a blank X axis
  # title (not needed here). Rotate the X axis labels 45 degree & play with hjust
  # & vjust to have them correctly positioned. Remove the legend as that one is
  # superfluous. Export figure at 4 x 4.5 inches in .pptx Powerpoint and .svg
  # formats.
  
  
  
  # first, we need to calculate the marginal trends for the different rivers as well as overall,
  # i.e. the slopes of heavy metal accumulation in function of heterozygosity plus 95% confidence intervals.
  # we use emtrends (from the emmeans package) to estimate trends (= slopes) of the fit
  # and store them in a dataframe fit_ex2_emtrends_df 
  
  fit_ex2_emtrends_df = rbind(data.frame(confint(emtrends(fit_ex2, ~ RIVER, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"))),
                              data.frame(confint(emtrends(fit_ex2, ~ 1, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"))))
  fit_ex2_emtrends_df # this gives the slope per river and overall in a dataframe
  
  
  #####EXTRA/OPTIONAL: we can also plot in ggplot asterisks of significances. In this case you can try it for slopes significantly different from 0, 
  # to test if every slope is significant different from 0 we use 'test' (instead of 'contrast') and null=0 (instead of e.g. method="pairwise") also adjust
  # for multiple testing with adjust="sidak"
 
  # the significances of the difference of these slopes from zero based on Sidak posthoc tests (using 'test' and adjust="sidak"):
  fit_ex2_emtrends_signif_df = rbind(data.frame(test(emtrends(fit_ex2, ~ RIVER, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"), null=0, adjust="sidak")), 
                                     data.frame(test(emtrends(fit_ex2, ~ 1, var="MULTILOCUS_HETEROZYGOSITY_ALLOZYME"), null=0) %>% rename(RIVER=1) )) %>%
    mutate(stars.pval = stars.pval(p.value)) # convert p value to sign asterisks & add it to dataframe, see ?gtools::stars.pval
  fit_ex2_emtrends_signif_df
  
  # we added the significance asterisk for the overall difference in slope across all rivers to our earlier dataframe
  fit_ex2_emtrends_df$stars.pval = fit_ex2_emtrends_signif_df$stars.pval
  fit_ex2_emtrends_df
  #     RIVER MULTILOCUS_HETEROZYGOSITY_ALLOZYME.trend        SE df   lower.CL    upper.CL stars.pval
  # 1    Maas                               -0.1269687 0.1774889 66 -0.4813366  0.22739932           
  # 2 Schelde                               -0.3317813 0.1794692 66 -0.6901032  0.02654061           
  # 3   Ijzer                               -0.3108771 0.2147662 66 -0.7396717  0.11791738           
  # 4 overall                               -0.2565424 0.1104717 66 -0.4771063 -0.03597845          *
  
  
  # ggplot code:
  plot5 = ggplot(fit_ex2_emtrends_df, aes(x=RIVER, y=MULTILOCUS_HETEROZYGOSITY_ALLOZYME.trend, color=RIVER))+ # specify input dataframe & x and y variables and colour by RIVER
    scale_color_manual(values=c("red2","purple","blue","black"))+                                             # specify custom colours
    geom_errorbar(aes(x=RIVER, ymin=lower.CL, ymax=upper.CL), width=0.2, linewidth=0.5)+   # add errorbars , change width and linewidth of the errorbars to 0.2 and 0.5
    geom_point(size=2)+                                                                    # plot estimated marginal slopes and change point size to 2
    geom_text(aes(label=stars.pval, y=upper.CL), vjust=-0.5, size=4) +                     # add significance labels for difference from zero using font size of 4 and place it above upper.CL   
    theme_par(base_size=10) +                                                              # use theme_par with base size of 10   
    ylab("Slope of Heavy Metal Accumulation\nvs. Multilocus Heterozygosity")+              # add Y axis title (use \n for break)
    xlab("")+                                                                              # use blank X axis title
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),                            # rotate X axis labels 45 degrees & adjust placement with hjust & vjust
          axis.title=element_text(size=10)) +                                              # adjust axis titles to font size of 12
    theme(legend.position="none")                                                          # legend not required here
  
  plot5
  

  
  
  #EXERCISE 3
  
  # We have a dataset ("bloodcells") with information on the number of damaged
  # red blood cells per mm^2 in a microscope image counting grid/chamber for a number of
  # individuals in function of their weight (in kg), their sex and
  # whether or not they are a smoker. The determine whether those variables were 
  # predictive of the number of damaged blood cells observed we fitted a Poisson glm.
  # The aim is to recreate the standard effect plots for the sex:smoker and smoker:weight
  # interaction effects in a prettier way using emmeans and ggplot2.
  
  d3 = read.xlsx("data/bloodcells1.xlsx")
  d3 = mutate_if(d3, is.character, as.factor)
  d3$smoker = factor(d3$smoker, levels=c("no","yes"), labels=c("non-smoker","smoker")) # recode factor levels to later allow us to omit legend title
  
  # log link Poisson glm fit (best fitting model) :
  fit_ex3 = glm(cells ~ smoker + weight + sex + sex:smoker + smoker:weight, family=poisson, data=d3)
  
  # standard effect plot
  plot(allEffects(fit_ex3), type="response", rug=F)
  
  
  # effect plot also showing partial residuals
  plot(allEffects( fit_ex3, residuals=T, xlevels=100) , type="response" , multiline=F,
       smooth.residuals=F,
       residuals.color=adjustcolor("red2",alpha.f=0.02), 
       residuals.pch=16,
       band.colors="blue",
       ylim=c(0, 15) )   
  
  
  # instead of using these rather ugly effect plots, we would like to plot the
  # following marginal means calculated using emmeans to make a more beautiful
  # multipanel effect plot using ggploT
  
  
  
  
  #Recreate the plot(allEffects(fit_ex3)) effect plots for the effects of
  # the sex:smoker and smoker:weight interaction effects based on the calulated emmeans
  
  
  # emmeans for effect of smoker & sex :
  em_smoker_sex_df = data.frame(emmeans(fit_ex3, ~ smoker*sex, type="response"))
  head(em_smoker_sex_df)
  colnames(em_smoker_sex_df) # use these column names in your ggplot2 code
  
  # emmeans for effect of smoker & weight :
  em_smoker_weight_df = data.frame(emmeans(fit_ex3, ~ smoker*weight, type="response", 
                                           at = list(weight = seq(round(min(d3$weight)),
                                                                  round(max(d3$weight)),
                                                                  by=1))))
  head(em_smoker_weight_df)
  colnames(em_smoker_weight_df) # use these column names in your ggplot2 code
  
  
  
  ### EXTRA
  # in the smoker x sex interaction effect effect plot we would also like to show
  # significance asterisks for the contrasts in the red blood cells damage
  # between smoking & nonsmoking men & woman based on the following
  # posthoc tests
  contrasts_df = data.frame(contrast(emmeans(fit_ex3, ~ smoker|sex, type="response"), method="revpairwise")) %>%
    mutate(stars.pval = stars.pval(p.value), # convert p value to sign asterisks & add it to dataframe, see ?gtools::stars.pval
           rate = c(2.75, 1.5),              # Y value where you would like significance asterisks to appear
           smoker = c(NA, NA))               # NA column for smoker (required when this dataframe is plotted in a geom_text layer alongside the other layers)    
  contrasts_df  # use these column names in your ggplot2 code
  

  
  
  # For the sex:smoker effect plot use a grouped column plot with error bars
  # (geom_linerange layer) and place sex on the X axis. 
  # Fill & group by smoker. 
  # In the geom_col layer use columns with a width of 0.6 and use
  # position=position_dodge(0.75) to place the columns in a staggered way to make
  # a grouped column plot. 
  # Use geom_linerange to display the 95% confidence intervals and use a linewidth of 0.5 and the same position specification as
  # for the columns. 
  # 
  # Use theme_few with a base_size font size of 16.
  # Use scale_fill_manual to specify the fill colour of the columns as grey40 and red3 and use name="" to omit legend title. 
  # Add custom X and Y axis titles and use expression to show the 2 in mm^2 in superscript. 
  # Use a floating legend at position c(0.75, 0.9) for smoker (fill aesthetic) 
  # and use a blank transparent legend background by specifying the right options using theme.
  
  
  # For the smoker:weight effect plot use a geom_ribbon layer for the 95%
  # confidence intervals (disable the outline by using color=NA) 
  # and a geom_line layer for the marginal means. 
  # Use the same manual fill & colour colors using scale_fill_manual and scale_colour_manual as in the first plot. 
  # Specify Y axis breaks at values 0 to 11 using scale_y_continuous. 
  # Specify custom X and Y axis titles.
  # Disable all legends in this plot, as the legend is the same as in the first
  # plot & we will show both below each other in a multipanel plot, so we just need
  # 1 legend. 
  # Use coord_cartesian to make the Y axis go between 0 and 11.
  
  # Finally, use grid.arrange with ncol=1 and argument left to place both plots below
  # each other in a multipanel plot (leave out the Y axes specified in the individual
  # plots by passing plot6 + rremove("ylab") & plot7 + rremove("ylab")).
  # (cf EXERCISE 1)
  
  # Make ggplot2 effect plot for effect of smoker & sex :
  plot6 = ggplot(data=em_smoker_sex_df, aes(x = sex, y=rate, fill=smoker, group=smoker ))+            # specify dataframe, show sex on X axis and rate on Y axis, fill by smoker & group by sex to make position dodging below (we use a grouped column plot)
    geom_col(width=0.6, position=position_dodge(0.75))+                                               # in the geom_col column layer we use different alpha values for the 2 sexes, using a width of 0.6 for the columns & use position_dodge to get a staggered grouped column plot
    geom_linerange(aes(x=sex,ymin=asymp.LCL,ymax=asymp.UCL), linewidth=0.5, position=position_dodge(0.75))+  # add 95% confidence intervals, here using geom_linerange & using a linewidth of 0.3 & also using positidion_dodge for position to get error bars in correct place
    geom_text(data=contrasts_df, aes(label=stars.pval, y=rate, fill=NULL),  vjust=-0.5, size=4) +   # Adding the asterisks of significances
    theme_few(base_size=16)+                                                                          # use theme_few theme                                                                                                                                 
    scale_fill_manual(name="", values=c("grey40","red3"))+                                            # specify custom fill colors & suppress legend title using name=""
    ylab(expression(paste("Number of damaged red blood cells per  ",  mm^2)))+                        # specify Y axis title with mm^2 shown in superscript using expression
    xlab("Sex")+                                                                                      # specify X axis title
    theme(legend.position = "inside", legend.position.inside = c(0.75, 0.9),                                                             # use a floating legend for sex & put it at position (0.75, 0.9) using a transparent background
          legend.background = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE))                                                       # show smoker first in legend (to match visual order in plot below)
  plot6
  
  
  # Make ggplot2 effect plot for effect of smoker * weight :
  
  
  plot7 = ggplot(data=em_smoker_weight_df, aes(x=weight, y=rate, color=smoker))+                      # specify dataframe, show weight on X axis and rate on Y axis, color by smoker
    geom_ribbon(aes(x=weight, ymin=asymp.LCL, ymax=asymp.UCL, fill=smoker), alpha=0.2, color=NA)+     # add 95% confidence intervals using geom_ribbon layer using semi-transparency of 0.2 and color=NA to remove outline
    geom_line(linewidth=1)+                                                                           # show marginal means using line with linewidth of 1
    theme_few(base_size=16)+                                                                          # use theme_few with a base_size of 16
    scale_y_continuous(breaks=c(0:11))+                                                               # put labels on the y-axis for specific positions
    scale_fill_manual(values=c("grey40","red3"))+                                                     # specify custom fill colours 
    scale_colour_manual(values=c("grey40","red3"))+                                                   # specify custom line colours 
    ylab(expression(paste("Number of damaged red blood cells per  ",  mm^2)))+                        # custom Y axis label with ^2 shown in superscript
    xlab("Weight (kg)") +                                                                             # custom X axis label
    coord_cartesian(ylim=c(0,11), expand=T) + 
    theme(legend.position="none")                                                          # leave out legend (redundant)
  
  plot7
  
  # grid.arrange allows you to put multiple plots in a specific arrangement of your choice
  # (here in a single column) & allows you to specify a common spanning Y axis title using
  # argument left with a correctly specified textGrob (with rot = 90, gp = gpar(fontsize = 17)).
  # Before adding the common Y axis title you can remove the previously provided separate Y axis titles
  # by passing plot6 + rremove("ylab") & plot7 + rremove("ylab") as your ggplot objects  :
  plot8 = grid.arrange(plot6 + rremove("ylab"), 
                       plot7 + rremove("ylab"), 
                       left = text_grob(expression(paste("Number of damaged red blood cells per  ",  mm^2)), # common spanning Y axis title
                                       rot = 90, size = 17), 
                       ncol=1) # single column (one figure below the other)
  plot8
  

#PCA ----
  #### Exercise 1 - diploid-hexaploid contact zone of Aster amellus
  aster=read.xlsx("data/Aster.xlsx")
  
  str(aster)
  aster$Ploidy=as.factor(aster$Ploidy)
  which(is.na(aster))
  asternew<-na.omit(aster) #a PCA on a dataset with missing data gives errors
  which(is.na(asternew))
  aster.data=asternew[,2:ncol(asternew)]  # Selection of the columns that contain data, leaving out the "Ploidy" level of the individuals
  head(aster.data)
  
  pairs(aster.data, cex=0.8, pch=21, col=c("blue","green","red")[asternew$Ploidy])
  cor(aster.data, use="complete.obs", method="pearson") #complete.obs is to include only the individuals where all data is present (omitting those with missing values)
  #cor function gives correlation coefficients
  # Variables that are correlated are: 
  #ligule.length and Nr.stem.leaves
  #ligule.length and Stem.length
  #Nr.stem.leaves and Stem.length
  #Nr.stem.leaves and Ligule.width
  
  #yes, the PCA will be usefull to reduce the dimensions as quite some variables are correlated (however rather low correlation coefficients)
  
  # You should use a correlation matrix, since not all data are in the same unit (mm, cm, count data) -> scale.=TRUE; center always has to be TRUE
  pca.aster = prcomp(aster.data, center=T, scale.=T)
  
  
  #
  pca.aster #s.d and eigenvectors --> Show which variables contribute most to each component (eigenvectors of which elements are loadings)
  #eigenvectors needed for interpretation of the data (see later)
  
  eigenvalues = pca.aster$sdev^2 #amount of variance explained per PC
  eigenvalues #amount of variance of the data along the principal axes; one eigenvalue per PC
  sum(eigenvalues)  #total amount of variation of the variables; sum eigenvectors = total variability
  summary(pca.aster)  # Shows how much variance is explained by the different principal components (in proportions)
  screeplot(pca.aster, main = "Scree plot", type="lines") ##PC1 and PC2 important, rest not that important
  
  #
  plot(pca.aster$x[,c(1,2)],col ="black", cex=1) #pca.aster$x contains the scores --> position on graph per object (F=U*Yc)
  plot(pca.aster$x[,c(1,3)],col ="black", cex=1)
  plot(pca.aster$x[,c(2,3)],col ="black", cex=1)
  
  # Different color for each group: blue=diploid, green=hexaploid, red=tetraploid
  # use the Ploidy column of the asternew dataset, because pca.aster uses the dataset without the NA point 
  levels(asternew$Ploidy) #check the order of the factor to interpret colours
  plot(pca.aster$x[,c(1,2)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  legend("topleft",c("diploid", "hexaploid", "tetraploid"), fill=c("blue","green","red"))
  text(pca.aster$x[,c(1,2)], labels=row.names(asternew), pos=3) #add ID's of objects; pos=3 meansadding the text above the coordinates

  plot(pca.aster$x[,c(1,3)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(2,3)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(1,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(2,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  plot(pca.aster$x[,c(3,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
  
  # Diploids and hexaploids are clearly separated on the first PC axis, tetraploids (the current hybrid of the two cytotypes)
  # are not morphologically very distinct from the other two groups.
  # In the literature the diploids and hexaploids are considered to be two different subspecies.
  # Since intermediate hybrids (tetraploids) are easily formed, all three groups still belong to the same species.
  
  ####compare graph with the loadings (eigenvectors) to check which original variables contribute most to the variability on the axes:
  pca.aster
  
  #these variables can also visualised on biplots:
  #
  par(mfrow = c(1, 2))
  biplot(pca.aster, scale=0)           # Distance biplot - distances among objects are ~ Euclidian (cfr PCAplot above)
  biplot(pca.aster, pc.biplot=TRUE)    # Correlation biplot - angles between descriptors reflect their correlation
  
  par(mfrow = c(1, 1))
  
  # a nicer way to make correlation biplots with using ggbiplot
  # use the Ploidy column of the asternew dataset, because pca.aster uses the dataset without the NA point 
  g <- ggbiplot(pca.aster, obs.scale = 1, var.scale = 1, groups = asternew$Ploidy, ellipse = TRUE, circle = FALSE); 
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g
  
  # Hexaploids are generally bigger; higher stem length, larger ligules and a higher number of stem leaves
  
  
  #### FYI: nicer biplot (see code in theory slides)
  # ggfortify::autoplot() is a wrapper that produces ggplot2 graphics; autoplot returns ggplot objects so you can add ggplot layers

  biplot2 <- autoplot(pca.aster, data=asternew, colour='Ploidy', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot2
  
  
  ##plotting other PC's can be done by specifying the axes with x= and y= (default are PC1 and PC2):
  biplot2b <- autoplot(pca.aster, x=3, y=4, data=asternew, colour='Ploidy', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot2b
  
  
  ##once you loaded ggfortify, making an 'ordinary ggbiplot will give errors.
  #unload ggfortify in order to ggbiplot to work again
  
  ##for your interest: visualisation with fviz_pca from package factoextra 
  fviz_pca(pca.aster)#with biplot
  fviz_pca_ind(pca.aster)#without biplot
  fviz_pca_ind(pca.aster, habillage=asternew$Ploidy, invisible="quali", geom= c ("point", "text"), 
               pointsize=2, labelsize=4)#different colours for different groups
  #the argument invisible=quali is to remove the mean point for each habillage that is generated
  fviz_pca_ind(pca.aster, axes = c(1,2), habillage=asternew$Ploidy, invisible="quali", label="var",
               addEllipses=T, ellipse.type="convex",title="PCA aster", 
               palette=c("blue","green","red"))
  fviz_pca(pca.aster, axes = c(1,2), habillage=asternew$Ploidy, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA aster", 
           palette=c("blue","green","red"))
  # habillage: convex hulls around groups
  #axes: define which PC axes to visualise
  fviz_pca_ind(pca.aster, axes = c(1,3), habillage=asternew$Ploidy, invisible="quali", label="var",
               addEllipses=T, ellipse.type="convex",title="PCA aster", 
               palette=c("blue","green","red"))
  
  
### Exercise 2 - Queen pheromones in social wasps
  
  # Social wasp datasets - cuticular hydrocarbon data was log-ratio transformed to account for differences in concentrations among samples
  dolicho<-read.xlsx("data/Dolichovespula saxonica log-ratio.xlsx", rowNames = T)
  str(dolicho)
  dolicho<-mutate_if(dolicho, is.character, as.factor)
  dolicho.data = dolicho[,6:ncol(dolicho)]
  which(is.na(dolicho.data)) #no missing data so ok
  
  # PCA Dolichovespula saxonica
  # Use a covariance matrix because all data is in the same unit (log ratio transformed cuticular hydrocarbon data) -> scale.=FALSE
  pca.dolicho = prcomp(dolicho.data, center=T, scale.=F)  
  
  pca.dolicho #it is not the case that some components stand out in contributing to the variance explained; it is more the total combination of components 
  # that explain variability (and explain differences between groups)
  
  eigenvalues=pca.dolicho$sdev^2 #amount of variance of the data along the principal axes; one eigenvalue per PC
  sum(eigenvalues)#total amount of variation of the variables; sum eigenvectors = total variability
  eigenvalues
  summary(pca.dolicho) #proportion variance explained per PC
  screeplot(pca.dolicho, main= "Scree plot",type="lines") #first 4 PC axes are meaningfull
  
  levels(dolicho$Caste)
  
  # PCA plots of Dolichovespula saxonica - queen=blue, reproductive worker=green, sterile worker=red
  plot(pca.dolicho$x[,c(1,2)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
  legend("bottomleft", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  text(pca.dolicho$x, labels=row.names(dolicho), pos=3) 
  plot(pca.dolicho$x[,c(1,3)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
  legend("topright", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  plot(pca.dolicho$x[,c(2,3)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
  legend("topright", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  # PC1 shows that queens smell different than sterile workers
  # when workers become reproductive they start smelling like the queen
  # The Queen Pheromone is a fertility signal in D. saxonica
  
  par(mfrow = c(1, 2))
  # biplot
  biplot(pca.dolicho,cex=0.9, scale=0) # Distance biplot - distances among objects are ~ Euclidian (cfr PCAplot above)
  biplot(pca.dolicho, pc.biplot=TRUE) # Correlation biplot - angles between descriptors reflect their correlation
  par(mfrow = c(1, 1))
  
  #  biplot using ggbiplot
  #  if the package ggfortify is loaded, you might have to unload it first for ggbiplot to work
  g <- ggbiplot(pca.dolicho, obs.scale = 1, var.scale = 1, groups = dolicho$Caste, ellipse = TRUE, circle = FALSE);
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g

  
  #### FYI: nicer biplots (see code in theory slides)
  biplot2 <- autoplot(pca.dolicho, data=dolicho, colour='Caste', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot2;
  
  
  #biplot using factoextra
  fviz_pca(pca.dolicho, axes = c(1,2), habillage=dolicho$Caste, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA dolicho", 
           palette=c("blue","green","red"))
  
  
  
## Vespula vulgaris

  vespula<-read.xlsx("data/Vespula vulgaris log-ratio.xlsx", rowNames = T)
  vespula<-mutate_if(vespula, is.character, as.factor)
  vespula.data = vespula[,3:ncol(vespula)]
  which(is.na(vespula.data)) 
  # PCA Vespula vulgaris
  # Use a covariance matrix because all data is in the same unit (log ratio transformed cuticular hydrocarbon data) -> scale.=FALSE
  pca.vespula = prcomp(vespula.data,center=T,scale.=F)  
  
  eigenvalues=pca.vespula$sdev^2
  sum(eigenvalues)
  eigenvalues
  summary(pca.vespula)
  screeplot(pca.vespula, main= "Scree plot",type="lines")
  
  pca.vespula
  
  
  
  # PCA plots of Vespula vulgaris - queen=blue, reproductive worker=green, sterile worker=red
  levels(vespula$Caste) #check the order of the different groups to link the colours
  plot(pca.vespula$x[,c(1,2)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
  legend("topleft", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
  text(pca.vespula$x[,c(1,2)], labels= row.names(vespula), pos=2) 
  plot(pca.vespula$x[,c(1,3)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
  text(pca.vespula$x[,c(1,3)], labels=row.names(vespula), pos=2) 
  plot(pca.vespula$x[,c(2,3)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
  text(pca.vespula$x[,c(2,3)], labels=row.names(vespula), pos=2) 
  # PC1 shows that queens smell different than all workers
  #SW4 is probably an outlier that needs to be checked
  # when workers become reproductive they do not smell like the queen.
  # The Queen Pheromone is not a fertility signal in V. vulgaris
  
  #  biplot using ggbiplot
  g <- ggbiplot(pca.vespula, obs.scale = 1, var.scale = 1, groups = vespula$Caste, ellipse = TRUE, circle = FALSE);
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g
  
  
  #  biplot using autoplot
  biplot3 <- autoplot(pca.vespula, data=vespula, colour='Caste', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position.inside= c(0.85, 0.15)) +
    theme(legend.title = element_text(colour="black", size=14)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    scale_fill_manual(values=c("blue","green","red")) +
    scale_color_manual(values=c("blue","green","red")) +
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
          axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);
  
  biplot3;
  
  
## Exercise 3 - Fish in ponds

  fish<-read.xlsx("data/Data_Ponds.xlsx")
  str(fish)
  fish$VIS=as.factor(fish$VIS)
  fish$Poel.Id.=as.factor(fish$Poel.Id.)

  fish.data=fish[,c(2:17,19:ncol(fish))] # Remove the variables fish and Poel(=pond) from the dataset 
  #(poel = ID and is not data to enter in the PCA)
  #vis (fish) is our categorizing variable to examine differences for (~ predictor variable)
  
  # Use a correlation matrix because not all variables have the same unit
  pca.fish=prcomp(fish.data,center=T,scale.=T)
  eigenvalues=pca.fish$sdev^2
  sum(eigenvalues)
  summary(pca.fish)
  
  pca.fish
  
  #
  screeplot(pca.fish, main= "Scree plot",type="lines")
  #the first  three PC's seem informative, containing most of the variability
  
  
  #yellow = no fish
  levels(fish$VIS)
  plot(pca.fish$x[,c(1,2)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
  plot(pca.fish$x[,c(1,3)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
  plot(pca.fish$x[,c(2,3)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
  
  #
  biplot(pca.fish,scale=0)
  biplot(pca.fish,pc.biplot=TRUE)
  
  #
  g <- ggbiplot(pca.fish, obs.scale = 1, var.scale = 1, groups = fish$VIS, ellipse = TRUE, circle = FALSE);
  g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
  g;
  
  #
  fviz_pca(pca.fish, axes = c(1,2), habillage=fish$VIS, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA Fish", 
           palette=c("yellow", "blue"))
  fviz_pca(pca.fish, axes = c(3,4), habillage=fish$VIS, invisible="quali", label="var",
           addEllipses=T, ellipse.type="convex",title="PCA Fish", 
           palette=c("yellow", "blue"))
  
  #EXTRA: Test whether the principal components differ significantly between the two groups.
  
  hist(pca.fish$x[,1])        # Check whether your variables (the positions of the points on the PC's: columns of matrix F) are normally distributed (to perform following analyses)
  shapiro.test(pca.fish$x[,1])
  hist(pca.fish$x[,2])
  shapiro.test(pca.fish$x[,2])
  
  set_sum_contrasts()
  fish.data.pc=cbind(fish,pca.fish$x[,1:4]) # Adding the first four PCs to the original dataset
  mod1=lm(PC1~VIS,data=fish.data.pc)
  summary(mod1)#The  presence/absence of fish has a significant effect on the position on PC1
  Anova(mod1, type="III")
  plot(fish.data.pc$VIS,fish.data.pc$PC1, xlab="VIS", ylab="PC1")
  
  #The relationship between pc1 and the presence/absence of fish is significant, however, because we really don't see clear 
  # structuring on the PCA, we doubt the biological relevance of this 'statistical difference'
  
  mod2=lm(PC2~VIS,data=fish.data.pc)
  summary(mod2)  
  Anova(mod2, type="III") # Not significant
  plot(fish.data.pc$VIS,fish.data.pc$PC2,  xlab="VIS", ylab="PC2")
  
  
  
#NMDS and decision trees ----
#NMDS
  # Exercise 1: NMDS
  mycor.all<-read.xlsx("data/Epipactis_fungi.xlsx", rowNames = T) #rowNames=True allows for the labels of the specimens to be used
  str(mycor.all)
  mycor.all<-mutate_if(mycor.all, is.character, as.factor)
  mycor = mycor.all[,c(2:84)] #only select the numerical data to work with

  #creating the Dissimilarity matrix
  #for non-binary data, you could use the following methods
  mycor.euc <- vegdist(mycor, method = "euclidean")
  mycor.man <- vegdist(mycor, method = "manhattan")
  mycor.bc <- vegdist(mycor, method = "bray")
  mycor.chord <- vegdist(decostand(mycor, "norm"), method="euclidean")
  #for binary data (which we have)
  library(ade4)
  mycor.jac<-dist.binary(mycor, method=1)
  # Dissimilarity matrix visualization, pink=similar, blue is dissimilar
  ##build coldiss function: see code on Toledo to create the function
  library(gclus)
  coldiss(mycor.bc, byrank=FALSE, diag=TRUE) 
  coldiss(mycor.jac, byrank=FALSE, diag=TRUE) 
  
  par(mfrow=c(1,1))
  ### NMDS - Euclidean (Don't use the Euclidean distance matrix because it cannot handle zeros very well)
  mycor.nmds.euc <- metaMDS(mycor, distance = "euclidean") #use original dataset as input, metaMDS calculates distance matrix
  mycor.nmds.euc #stress = 0.12
  plot(mycor.nmds.euc, type="text", main=paste("NMDS/Euclidean -Stress =", round(mycor.nmds.euc$stress,3)))
  
  # Stress: 0.05 = excellent; 0.1 = great; 0.2 = ok; 0.3 = poor representation
  
  # Orchid species are colored
  levels(mycor.all$Species)
  plot(mycor.nmds.euc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Euclidean - Stress =", round(mycor.nmds.euc$stress,3  )))
  legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"), text.font=8, text.width =0.5, bty="n" )
  
  ## $points: plotting only the plant objects; if you want to plot the columns (OTU's in this case) you can put $species
  text(mycor.nmds.euc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
  ## display the mycor. OTU's as text
  ##if you want to plot the text of the plant objects (the rows) you put display="sites"
  
  #shepards plot compares the distances in the ordination plot with the original distances
  stressplot(mycor.nmds.euc, main=paste("Shepard plot - Euclidean", round(mycor.nmds.euc$stress,3)))
  gof=goodness(mycor.nmds.euc)
  plot(mycor.nmds.euc, type="t", main="goodness-of-fit")
  points(mycor.nmds.euc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  ### NMDS - Bray-Curtis (You could use the Bray-Curtis distance matrix since it can also handle binary data very well)
  mycor.nmds.bc <- metaMDS(mycor, distance = "bray")
  mycor.nmds.bc #stress=0.1264
  plot(mycor.nmds.bc, type="t", main=paste("NMDS/Bray-Curtis - Stress =", round(mycor.nmds.bc$stress,3)))
  
  
  # Stress: 0.05 = excellent; 0.1 = great; 0.2 = ok; 0.3 = poor representation
  
  # Orchid species are colored
  plot(mycor.nmds.bc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Bray-Curtis - Stress =", round(mycor.nmds.bc$stress,3  )))
  legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"),text.font=8, text.width =0.5, bty="n")
  ##$points: plotting only the plant objects
  
  text(mycor.nmds.bc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
  ## display the mycor. OTU's as text
  
  # Stress and goodness of fit
  stressplot(mycor.nmds.bc, main=paste("Shepard plot - Bray-Curtis", round(mycor.nmds.bc$stress,3)))
  gof=goodness(mycor.nmds.bc)
  plot(mycor.nmds.bc, type="t", main="goodness-of-fit")
  points(mycor.nmds.bc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  ### NMDS - Jaccard (The Jaccard distance matrix is also very suitable for binary data)
  mycor.nmds.jacc <- metaMDS(mycor, distance = "jaccard")
  plot(mycor.nmds.jacc, type="t", main=paste("NMDS/Jaccard - Stress =", round(mycor.nmds.jacc$stress,3)))
  
  # Orchid species are colored
  
  plot(mycor.nmds.jacc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Jaccard - Stress =", round(mycor.nmds.jacc$stress,3  )))
  text(mycor.nmds.jacc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
  legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"),text.font=8, text.width =0.2, bty="n")
  
  
  # Stress and goodness of fit
  stressplot(mycor.nmds.jacc, main=paste("Shepard plot - Jaccard", round(mycor.nmds.jacc$stress,3)))
  gof=goodness(mycor.nmds.jacc) 
  plot(mycor.nmds.jacc, type="t", main="goodness-of-fit")
  points(mycor.nmds.jacc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit
  
  ####in this case, with this dataset, completely the same results as with the BRAY_CURTIS distance matrix
  ################## Other distance matrices are also well suited for binary data. Not only the ones mentioned above.e.g. Sorenssen
  
  
  
#Exercise 2: Clustering #
  plants<-read.xlsx("data/meadow_plant_species.xlsx")
  str(plants)
  
  ### Distance matrix 
  #p.dist = vegdist(plants,"euclidean") # Euclidean - Not good for this dataset due to the zero's
  #p.dist = vegdist(plants,"bray") # Bray-Curtis distance 
  p.dist = vegdist(decostand(plants,"norm"),method="euclidean") # Chord distance - Good option 
  
  # Chord distance is especially useful for community data
  
  
  ### Different clustering methods
  par(mfrow=c(2,2))
  # Single linkage agglomerative clustering
  p.dist.single <- hclust(p.dist, method="single")
  plot(p.dist.single, main="Single linkage")
  
  # Complete linkage agglomerative clustering
  p.dist.complete <- hclust(p.dist, method="complete");
  plot(p.dist.complete, main="Complete linkage")
  
  # Unweighted average linkage agglomerative clustering (UPGMA)
  p.dist.UPGMA <- hclust(p.dist, method="average");
  plot(p.dist.UPGMA, main="UPGMA")
  
  
  # Ward's minimum variance method
  p.dist.ward <- hclust(p.dist, method="ward.D");
  plot(p.dist.ward,main="Ward")
  #p.dist.ward$height <-sqrt(p.dist.ward$height);
  #plot(p.dist.ward)
  
  
  ### Assessing which clustering method works best
  # Single linkage agglomerative clustering
  p.dist.single.coph <- cophenetic(p.dist.single)
  plot(p.dist, p.dist.single.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("Single linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.single.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.single.coph), col="red")
  gow.dist.single <- sum((p.dist-p.dist.single.coph)^2); 
  gow.dist.single # Small values indicate a high fit
  
  
  # Complete linkage agglomerative clustering
  p.dist.complete.coph <- cophenetic(p.dist.complete);
  plot(p.dist, p.dist.complete.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("complete linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.complete.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.complete.coph), col="red")
  gow.dist.complete <- sum((p.dist-p.dist.complete.coph)^2); 
  gow.dist.complete # Small values indicate a high fit
  
  # Unweighted average linkage agglomerative clustering (UPGMA)
  p.dist.UPGMA.coph <- cophenetic(p.dist.UPGMA);
  plot(p.dist, p.dist.UPGMA.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("UPGMA linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.UPGMA.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.UPGMA.coph), col="red")
  gow.dist.UPGMA <- sum((p.dist-p.dist.UPGMA.coph)^2); 
  gow.dist.UPGMA # Small values indicate a high fit
  
  # Ward's minimum variance method
  p.dist.ward.coph <- cophenetic(p.dist.ward);
  plot(p.dist, p.dist.ward.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
       main = c("ward linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.ward.coph),3))))
  abline(0,1)
  lines(lowess(p.dist,p.dist.ward.coph), col="red")
  gow.dist.ward <- sum((p.dist-p.dist.ward.coph)^2); 
  gow.dist.ward # Small values indicate a high fit
  
  ##UPGMA is best
  ##for illustrative purposes, for the subsequent analyses we give the codes for all different clustering methods. But actually the next steps need
  # to be only done for the best clustering method (UPGMA in this case)
  par(mfrow=c(1,1))
  
  ### The optimal amount of clusters - fusion plots
  # Single linkage
  plot(p.dist.single$height, nrow(plants):2, type="S", main="Fusion levels - Single",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.single$height, nrow(plants):2,nrow(plants):2, col="red", cex = 0.8)  
  
  # Complete linkage
  plot(p.dist.complete$height, nrow(plants):2, type="S", main="Fusion levels - Complete",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.complete$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  
  
  # UPGMA
  plot(p.dist.UPGMA$height, nrow(plants):2, type="S", main="Fusion levels - UPGMA",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.UPGMA$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  
  
  # Ward's D
  plot(p.dist.ward$height, nrow(plants):2, type="S", main="Fusion levels - ward",
       ylab="k (number of clusters)", xlab="h (node height)", col="grey")
  text(p.dist.ward$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  
  
  # It is quite difficult and arbitrary to choose the optimal number of clusters, but based on 
  # the fusion plots (and the information given with the exercise) I'd choose 5 clusters
  par(mfrow=c(1,2))
  par(mfrow=c(1,1))
  ### The optimal amount of clusters - silhouette width approach
  library(cluster)
  
  # Single linkage
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.single, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.single <- which.max(asw);
  k.best.single
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, single",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.single, paste("optimum",k.best.single,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.single, max(asw), pch=16, col="red", cex=1.5)  
  
  # Complete linkage
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.complete, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.complete <- which.max(asw);
  k.best.complete
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, complete",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.complete, paste("optimum",k.best.complete,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.complete, max(asw), pch=16, col="red", cex=1.5)  
  
  
  # UPGMA
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.UPGMA, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }
  k.best.upgma <- which.max(asw);
  k.best.upgma
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, UPGMA",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.upgma, paste("optimum",k.best.upgma,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.upgma, max(asw), pch=16, col="red", cex=1.5)  
  
  
  # Ward
  asw <- numeric(nrow(plants));
  for (k in 2:(nrow(plants)-1)) {
    sil <- silhouette(cutree(p.dist.ward, k=k), p.dist)
    asw[k] <- summary(sil)$avg.width
  }  
  k.best.ward <- which.max(asw);
  
  k.best.ward
  plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, Ward",
       xlab="k (number of groups)", ylab="Average silhouette width")
  axis(1, k.best.ward, paste("optimum",k.best.ward,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best.ward, max(asw), pch=16, col="red", cex=1.5)  
  
  # The optimal amount of clusters - Mantel approach
  
  ##first define the function to make the binary matrix:
  grpdist <- function(X)
  {require(cluster)
    gr <- as.data.frame(as.factor(X))
    distgr <- daisy(gr, "gower")
    distgr}
  
  kt <- data.frame(k=1:nrow(plants), r=0)
  
  
  # Single linkage
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.single, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, single",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  # Complete linkage
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.complete, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, complete",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  # UPGMA
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.UPGMA, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, UPGMA",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  # Ward
  for (i in 2:(nrow(plants)-1)){
    gr <- cutree(p.dist.ward, i)
    distgr <- grpdist(gr)
    mt <- cor(p.dist, distgr, method="pearson")
    kt[i,2] <- mt}
  k.best <- which.max(kt$r)
  plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, Ward",
       xlab="k (number of groups)", ylab="Pearson's correlation")
  axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
  points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
  
  par(mfrow=c(1,1))
  
  
  ### Silhouette plots # Use the highest number of optimal clusters found in the last exercise
  cutg1 <- cutree(p.dist.single, k=7);
  sil1 <- silhouette(cutg1, p.dist);
  silo1 <- sortSilhouette(sil1);
  rownames(silo1) <- row.names(plants)[attr(silo1,"iOrd")];
  plot(silo1, main="Silhouette plot - Single", cex.names=0.8,col=silo1+1, nmax.lab=100)
  
  cutg2 <- cutree(p.dist.complete, k=7);
  sil2 <- silhouette(cutg2, p.dist);
  silo2 <- sortSilhouette(sil2);
  rownames(silo2) <- row.names(plants)[attr(silo2,"iOrd")];
  plot(silo2, main="Silhouette plot - Complete", cex.names=0.8,col=silo2+1, nmax.lab=100)
  
  cutg3 <- cutree(p.dist.UPGMA, k=7); #you can try different numbers of clusters (e.g the max k=7, or the intermediate k=5)
  sil3 <- silhouette(cutg3, p.dist);
  silo3 <- sortSilhouette(sil3); ##orders the rows of sil by clusters and decreasing silhouette width
  rownames(silo3) <- row.names(plants)[attr(silo3,"iOrd")];#also order the plant numbers accordingly
  plot(silo3, main="Silhouette plot - UPGMA", cex.names=0.8,col=silo3+1,  nmax.lab=100)
  
  
  cutg4 <- cutree(p.dist.ward, k=7);
  sil4 <- silhouette(cutg4, p.dist);
  silo4 <- sortSilhouette(sil4);
  rownames(silo4) <- row.names(plants)[attr(silo4,"iOrd")];
  plot(silo4, main="Silhouette plot - Ward", cex.names=0.8,col=silo4+1, nmax.lab=100)
  
  
  ### Plotting the different clusters on the dendrogram
  # Single
  plants.final.single <- reorder(p.dist.single, p.dist);
  plot(plants.final.single, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - Single (reordered)", labels=cutree(plants.final.single,k=25));
  rect.hclust(plants.final.single,k=5)  
  
  # Complete
  plants.final.complete <- reorder(p.dist.complete, p.dist);
  plot(plants.final.complete, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - Complete (reordered)", labels=cutree(plants.final.complete,k=25));
  rect.hclust(plants.final.complete,k=5)  
  
  # UPGMA
  plants.final.upgma <- reorder(p.dist.UPGMA, p.dist);#reorder: making sure nearby object pairs are adjacent
  plot(plants.final.upgma, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - UPGMA (reordered)", labels=cutree(plants.final.upgma,k=25));
  ##you could also use labels = row.names(plants) insead of labels=cutree(plants.final.upgma,k=25)
  rect.hclust(plants.final.upgma,k=5) #you can test here the difference between 5 and 7 clusters 
  #or with hcoplot
  source("hcoplot.R")
  hcoplot(p.dist.UPGMA, p.dist, k=5)
  
  
  # Ward
  plants.final.ward <- reorder(p.dist.ward, p.dist);
  plot(plants.final.ward, hang = -1, xlab="5 groups", sub="", ylab="Height",
       main="Chord - Ward (reordered)", labels=cutree(plants.final.ward,k=25));
  rect.hclust(plants.final.ward,k=5)  
  
  
  ### Combine the NMDS with the cluster results
  plants.nmds <- metaMDS(decostand(plants,"norm"),distance="euclidean") # Compare to the same distance matrix - currently Chord
  #metaMDS does not have an in-built"Chord" distance matrix to use. You need to use distance=euclidean on a standardized (decostand) dataset), 
  #similar as for constructing the distance matrix itself (see Ex1)
  plants.nmds 
  plot(plants.nmds, type="n", main=paste("NMDS/Chord - Stress =", round(plants.nmds$stress,3)))
  text(plants.nmds,display=c("sites")) #(fyi: display=c("species") plots the columns)
  #or
  plot(plants.nmds, type="text", main=paste("NMDS/Chord - Stress =", round(plants.nmds$stress,3)))
  
  # give colours based on the the clusters of the cluster analysis
  plants.groups <- cutree(p.dist.UPGMA, k=5)
  str(plants.groups)
  plants.groups<-as.factor(plants.groups)
  grp.lev <- levels((plants.groups)) #for the legend
  p <- ordiplot(plants.nmds, type="n", main="NMDS/Chord - clusters UPGMA")
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  points(plants.nmds$points, pch=21, cex=1.5, bg=plants.groups)
  legend("bottomleft",legend=grp.lev, pch=16, col=grp.lev)
  
  
  text(plants.nmds$points, row.names(plants), pos=4, cex =0.7)
  text(plants.nmds,display=c("species")) #not very aesthetically
  
  
  # Add the dendrogram 
  ordicluster(p, p.dist.UPGMA, col="dark grey")
  
  
#RDA and permanova ----
  bryo<-read.xlsx("data/Bryophyte_community.xlsx",rowNames = T)    # Bryophyte communities
  env<-read.xlsx("data/Environment_Vegetation.xlsx",rowNames = T)  # Environmental variables (including: chemical compounds, slope and two NMDS axes that represent the surrounding vegetation)
  
  # NMDS scores here are a proxy of the communities of higher plants in the environment
  # At the bottom of this script the  NMDS analysis of the vascular plant data to obtain this data is shown
  
#Exercise 1.1
  # RDA on untransformed bryophyte data
  bryo.rda <- rda(bryo~.,env) # the dot refers to including all variables of the dataset env; 
  #bryo = response matrix; env = explanatory matrix
  summary(bryo.rda)
  RsquareAdj(bryo.rda)$r.squared                # R^2 = proportion of constrained variance
  RsquareAdj(bryo.rda)$adj.r.squared            # adjusted R^2
  
  # RDA on Hellinger transformed bryophyte data
  bryo.hel <- decostand(bryo,"hellinger")
  bryo.hel.rda <- rda(bryo.hel~.,env) 
  summary(bryo.hel.rda)
  RsquareAdj(bryo.hel.rda)$r.squared                # R^2
  RsquareAdj(bryo.hel.rda)$adj.r.squared            # adjusted R^2
  # adjusted R²is % of variance explained, adjusted is to compensate for the fact that more variables lead to higher R²
  
  # Use the hellinger transformed dataset for further analyses (it has the highest Adj. R^2)
  
  ##### Exercise 1.2
  anova.cca(bryo.hel.rda, step=1000)                
  anova.cca(bryo.hel.rda, by="axis", step=1000)  
  #model significant --> bryophyte explained by env
  
  
  ##### Exercise 1.3
  # Distance triplot: distances between sites, between species or between sites and species approximate their euclidean distances.
  par(mar=c(5,5,5,5)) #set plot margins
  plot(bryo.hel.rda, scaling=1,  main="Triplot RDA bryophytes scaling = 1",type="none") # empty plot  
  bryo.sc1 <- scores(bryo.hel.rda, scaling=1) #retrieve the scores for sites and species of bryophytes
  points(bryo.sc1$sites,col="black",pch=21,cex=0.7) #plot the site scores in points
  text(bryo.sc1$species, row.names(bryo.sc1$species), col="red",cex=0.8)#plot the bryo species in text (response variables as labels)
  ##we could also add lines (or arrows) to these labels:
  arrows(0,0,bryo.sc1$species[,1], bryo.sc1$species[,2], length =0.05, lty=1, col="red")
  #length=0.05 is length of the arrow triangle (tips)
  text(bryo.hel.rda, display="bp",col="blue", scaling=1)#display biplot --> gives the environmental characteristics in arrows (arrows for continuous explanatiory variables))
  
  
  #display: bp= biplot, cn= centroids, wa or sites= sites, sp= species
  
  #####R code from theory
  plot(bryo.hel.rda, scaling=1,  main="Triplot RDA bryophytes scaling = 1") #everything at once
  ##we can also add lines for the response variables
  bryo.sc1 <- scores(bryo.hel.rda, choices= 1:2, display="sp", scaling=1)
  arrows(0,0,bryo.sc1[,1], bryo.sc1[,2], length =0, lty=1, col="red")
  
  
  # Correlation triplot: the angles in the biplot between response (abundance data) and explanatory (environment) variables, and 
  # between response variables themselves or explanatory variables themselves, reflect their correlations.
  par(mar=c(5,5,5,5))
  plot(bryo.hel.rda, scaling=2,  main="Triplot RDA bryophytes scaling = 2",type="none")   
  bryo.sc1 <- scores(bryo.hel.rda, scaling=2)
  points(bryo.sc1$sites,col="black",pch=21,cex=0.7)
  text(bryo.sc1$species, row.names(bryo.sc1$species),col="red",cex=0.8)
  arrows(0,0,bryo.sc1$species[,1], bryo.sc1$species[,2], length =0.05, lty=1, col="red")
  text(bryo.hel.rda,display="bp",col="blue")
  
  ##### Exercise 1.4
  vif.cca(bryo.hel.rda)
  # Correlations among the environmental variables are <10
  
  ##### Exercise 1.5
  # Criterion is P < 0.05
  step.forward <- ordistep(rda(bryo.hel ~ 1, data = env), scope=formula(bryo.hel.rda), direction="forward", pstep=1000)
  # Final model:  rda(bryo.hel ~ NMDS1 + NMDS2 + NH3 + Si + Corg + Ca + pH ,data=env)
  # The final model can differ (!!)
  step.forward$call
  
  # Criterion is to stop when adj.R2 of full model is exceeded
  step.forward.R2 <- ordiR2step(rda(bryo.hel ~ 1, data = env), scope=formula(bryo.hel.rda), direction="forward", pstep=1000)
  step.forward.R2$call
  # Final model:  rda(bryo ~ NMDS1 + NMDS2 + Si + NH3 + Corg + Ca ,data=env)
  # In this case the final model without ph is better (the R² of the reduced model cannot exceed the R² of the full model)
  
  # Let's continue with the model with ph in ex 1.6: NMDS1 + NMDS2 + Si + NH3 + Corg + Ca + pH
  
  ##### Exercise 1.6
  bryo.hel.rda2 = rda(bryo.hel ~ NMDS1 + NMDS2 + Si + NH3 + Corg + Ca + pH ,data=env)
  RsquareAdj(bryo.hel.rda2)$adj.r.squared 
  
  
  ##### Exercise 1.7
  #distance triplot
  par(mar=c(5,5,5,5))
  plot(bryo.hel.rda2, scaling=1,  main="Triplot RDA bryophytes (forward selection) - scaling = 1",type="none")
  bryo.sc.new <- scores(bryo.hel.rda2, scaling = 1)
  points(bryo.sc.new$sites,col="black",pch=21,cex=0.7)
  text(bryo.sc.new$species, row.names(bryo.sc.new$species),col="red",cex=0.8)
  arrows(0,0,bryo.sc.new$species[,1], bryo.sc.new$species[,2], length =0.05, lty=1, col="red")
  text(bryo.hel.rda2, display="bp",col="blue")
  
  #label sites: use text instead of points: 
  text(bryo.sc.new$sites,col="black")
  #or 
  text(bryo.hel.rda2,scaling=1, display="wa",col="black", cex=0.8)
  
  #correlation triplot
  par(mar=c(5,5,5,5))
  plot(bryo.hel.rda2, scaling=2,  main="Triplot RDA bryophytes (forward selection) - scaling = 2",type="none")
  bryo.sc.new <- scores(bryo.hel.rda2, scaling = 2)
  points(bryo.sc.new$sites,col="black",pch=21,cex=0.7)
  text(bryo.sc.new$species, row.names(bryo.sc.new$species),col="red",cex=0.8)
  arrows(0,0,bryo.sc.new$species[,1], bryo.sc.new$species[,2], length =0.05, lty=1, col="red")
  text(bryo.hel.rda2, display="bp",col="blue")
  
  ##### Exercise 1.8 - partial RDA
  env2 <- env[,c(1:15)] # Environmental variables
  veg <- env[,c(16:17)] # NMDS axes representing the vegetation data
  
  bryo.pRDA <- rda(bryo.hel, veg, env2)
  bryo.pRDA
  # Conditioned: Variance explained by the covariable 'env2' and removed
  # Constrained: Variance explained purely by 'veg'
  # Unconstrained: Residual variance
  RsquareAdj(bryo.pRDA)$adj.r.squared 
  
  anova.cca(bryo.pRDA, step = 1000) # Vascular plant communities (i.e. NMDS axes) significantly affect the bryophyte community even though the constrained variance is low
  
  bryo.pRDA2 <- rda(bryo.hel, env2, veg)
  bryo.pRDA2
  RsquareAdj(bryo.pRDA2)$adj.r.squared 
  # Conditioned: Variance explained by 'veg' and removed
  # Constrained: Variance explained purely by 'env2'
  # Unconstrained: Residual variance
  
  anova.cca(bryo.pRDA2, step = 1000) # The environmental variables also significantly affect the bryophyte community
  
  #### Exercise 1.9
  bryo.varpart <- varpart(bryo.hel, veg, env2)
  par(mar=c(1,1,1,1))
  plot(bryo.varpart, bg=c("blue","green"),Xnames=c("Veg","Env2"),cex=1.5)
  
  bryo.varpart
  
  #visualise outcomes-->use these values in function below (draw.pairwise.venn)
  #the values in the venndiagram obtained from 'varpart' are the R²adjusted values
  #the outcomes (conditioned, constrained and unconstrained) obtained from the partial RDA are the
  #unadjusted R² values
  
  # Or you could use:
  library(VennDiagram)
  par(mar=c(1,1,1,1))
  plot.new()
  draw.pairwise.venn(0.26,0.28, 0.21, c("Vegetation","Environment"), col=c("blue","green"), 
                     fill=c("light blue","lightgreen"),cex=1.3, cat.cex = 1.5)
  
  ## the numbers you need to fill in are the [a+b], [b+c] and [b] results respectively from the varpart
  
  # FYI:  NMDS on the vascular plant community: an illustration of how the columns of the NMDS axes have been obtained
  
  plant = read.xlsx("data/Vascular_plant_community.xlsx",rowNames = T)
  head(plant)
  
  nmds = metaMDS(decostand(plant,"nor"),distance="euclidean") # Chord distance
  nmds
  scores(nmds) ##these are the values in the dataset 'Environment_vegetation'
  plot(nmds$points,pch=21,col="black",cex=1,main=paste("NMDS Stress =", round(nmds$stress,3  )))
  text(nmds, display = "species",cex=0.7) # Adding plant species

# Exercise 2: MANOVA
  manova_data <- read.xlsx("data/manova_data.xlsx")
  str(manova_data)
  manova_data$IC<-as.factor(manova_data$IC)
  manova_data$year<-as.factor(manova_data$year)
  levels(manova_data$IC)
  
  
  # Exercise 2.2
  MSP.manova <- manova(cbind(MSP1, MSP2, MSP3)~IC, data=manova_data) 
  summary(MSP.manova, test="Wilks")
  
  
  # Exercixe 2.3
  summary(MSP.manova, test="Wilks")
  summary(MSP.manova, test="Pillai")
  summary(MSP.manova, test="Hotelling-Lawley")
  summary(MSP.manova, test="Roy")
  
  # Exercise 2.4
  summary(aov(cbind(MSP1, MSP2, MSP3)~IC, data=manova_data))
  # MSP-2 is most affected by inbreeding, but MSP-1 also affected
  
  
  #gives same results:
  set_sum_contrasts()
  fit1<-lm(MSP1~IC, data=manova_data)
  fit2<-lm(MSP2~IC, data=manova_data)
  fit3<-lm(MSP3~IC, data=manova_data)
  
  Anova(fit1, type="III")
  Anova(fit2, type="III")
  Anova(fit3, type="III")
  
  # Exercise 2.5 --> two-way MANOVA
  IC.two.manova <- manova(cbind(MSP1,MSP2,MSP3)~IC*year, data=manova_data)
  summary(IC.two.manova,test="Wilks")
  summary(IC.two.manova, test="Pillai")
  
  # MSP production did not differ significantly between years. However, we found a significant interaction of 'level of inbreeding' and 'year' on MSP production. 
  # This indicates that MSP production varied slightly between years (but not significantly) and this variation was stronger in some of the IC groups. 
  
  
  # Exercise 2.6
  flight.manova <- manova(cbind(drythor,dryabdo,dryhead)~IC*year, data=manova_data)
  summary(flight.manova,test="Wilks")
  summary(flight.manova,test="Pillai")
  # Inbreeding significantly affects flight performance and flight performance also differs between years, and the effect of inbreeding is different over the different years
  summary(aov(cbind(drythor, dryabdo, dryhead)~IC*year, data=manova_data))
  # Inbreeding mainly affects size of the thorax and the size of the head
  # Furthermore, the size of the head differed between years and we found a significant interaction between inbreeding and year on the size of the thorax.
  
  #in a model containing many response variables, the model could be simplified by removing the response variables that are not affected by inbreeding, year or an interaction between inbreeding and year
  
  
#Exercise 3: PERMANOVA
  # Exercise 3.1
  OTU = read.xlsx("data/OTU.xlsx")
  id = read.xlsx("data/id_variables.xlsx")
  
  # Exercise 3.2
  m1 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="bray")
  m1 #only gives an oveall p-value for the model
  m1 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="bray", by="terms")
  m1
  # There is a significant effect of location and a marginally significant (0.05<P<0.1) interaction effect 
  # Exercise 3.3
  m2 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="euclidean", by="terms")
  m2
  m3 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="jaccard", by="terms")
  m3 
  m4 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="manhattan", by="terms")
  m4 
  m5 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="canberra", by="terms")
  m5
  
  # Exercise 3.4 try additive models as interactions term is not or only marginally significant
  m6 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="bray", by="terms")
  m6
  m7 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="euclidean", by="terms")
  m7
  m8 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="jaccard", by="terms")
  m8
  m9 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="manhattan", by="terms")
  m9
  m10 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="canberra", by="terms")
  m10
  
  # Exercise 3.5
  m11 <- adonis2(OTU ~ location, data=id, permutations=1000, method="bray" , by="terms")
  m11
  
  # Exercise 3.6
  # Bacteria are rather generalists. They are not really associated with specific bug species, but they are location specific.
  
#PCA vs NMDS ----
  env <- data.frame(
    temp = c(12, 14, 18, 20, 22),
    pH = c(6.5, 6.8, 7.0, 7.2, 7.4),
    nitrate = c(2, 5, 10, 15, 20)
  )
  
  pca <- prcomp(env, scale. = TRUE)
  summary(pca)  
  #plot:
  biplot(pca)
  
  #or:
  library(vegan)
  pca <- rda(env, scale = TRUE)
  plot(pca)
  
  
  comm <- data.frame(
    sp1 = c(5, 0, 3, 1, 0),
    sp2 = c(0, 2, 1, 0, 4),
    sp3 = c(1, 1, 0, 3, 2)
  )
  
  nmds <- metaMDS(
    comm, 
    distance = "bray",
    k = 2, 
    trymax = 100
  )
  plot(nmds)
  nmds$stress
  
  envfit_res <- envfit(nmds, env)
  plot(nmds)
  plot(envfit_res)
  
  
  data(varechem)
  head(varechem)
  pca <- prcomp(varechem, scale. = TRUE)
  biplot(pca)
  summary(pca)  
  
  data(varespec)
  nmds <- metaMDS(varespec, distance = "bray", k = 2)
  plot(nmds)
  
#Summary ----
  #LM - response is continuous with constant variance and relationships approx. linear
  #Poly LM - relationship is curved
  #Multivariate model - additive or interaction
    #Manova: multiple responses
    m4 <- manova(cbind(biomarker1, biomarker2, biomarker3) ~ treatment, data = data)
    summary(m4, test = "Pillai")
  #Polynomial multivariate model - at least one of the multiple predictors shows curvature
  #GLM - when response not Gaussian (counts, proportions, binary outcomes)
    #Binary - binomial
    #Counts - poisson (if disp > 1; consider overdispersion and thus negative binomial glm.nb)
    #Proportions - binomial w/ cbind
      g3 <- glm(cbind(infected, total - infected) ~ treatment, data = data, family = binomial)
  #Mixed effects
    #LMM (repeated measures per individual)
    #GLMM (counts with random site)
  #Robust linear model - heavy outliers/influential observations
      
      
#Past questions ----
  # Honeybee Queen Pheromones
      bees <- read.csv("data/queenpheromone_long.csv")
      bees_wide <- read.csv("data/queenpheromone_wide.csv")
      head(bees_wide)
      
      unique(bees_wide$treatment)
      unique(bees$treatment)
      
      bees_wide$treatment <- relevel(bees_wide$treatment, ref = "control")

      model <- glm(colonysize ~ as.factor(treatment) + (1|colony), data = bees_wide, family = poisson)
      
      
      #spaghetti plot
      library(lattice)
      
      xyplot(size_oocyte ~ treatment | caste, groups = ID, data = data,
             type = c("p", "l"),
             auto.key = FALSE,
             xlab = "Treatment",
             ylab = "Largest oocyte size")
      
      library(ggplot2)
      
      ggplot(data, aes(x = treatment, y = size_oocyte, group = ID)) +
        geom_line(alpha = 0.5) +
        geom_point(size = 2) +
        facet_wrap(~caste) +
        theme_classic() +
        labs(x = "Treatment", y = "Largest oocyte size")
      
      #for both, each line is one colony/sib-group paid, the slope is within-ID effects of phermonone vs control; 
      #separately for queens and workers
      
      
      #models
      library(lme4)
      
      model_add <- lmer(size_oocyte ~ treatment + caste + (1|ID), data = data, REML = FALSE)
      model_int <- lmer(size_oocyte ~ treatment * caste + (1|ID), data = data, REML = FALSE)
      
      #effect plots
      library(emmeans)
      best <- if (AIC(model_int) < AIC(model_add)) model_int else model_add
      emm <- emmeans(best, ~ treatment * caste)
      plot(emm, comparison = TRUE)
      
      #post hoc tests
      anova(model_add, model_int) #does interaction improve fit
      m_caste <- lmer(size_oocyte ~ caste + (1|ID), data = data, REML = FALSE) #reduced model
      m_treat <- lmer(size_oocyte ~ treatment + (1|ID), data = data, REML = FALSE) #reduced model
      
      anova(model_add, m_caste)
      anova(model_add, m_treat)
      
      anova(best)
      
      emm_tc <- emmeans(best, ~ treatment|caste)
      pairs(emm_tc, adjust = "Tukey")
      
      
      #assumptions
      res <- resid(best)
      hist(res)
      qqnorm(res)
      qqline(res)
      shapiro.test(res)
        
      library(DHARMa)
      sim <- simulateResiduals(best)
      plot(sim)
      testDispersion(sim)
      
      
#leaf PCA practice
  leaf <- read.csv("data/Leafshape.csv")
  head(leaf)
  Species <- leaf$Species
  leaf_data <- leaf[, 2:6] #only the desired variables selected

  leaf_pca <- princomp(leaf_data, cor = FALSE)  
  plot(leaf_pca$scores, pch = 16, col = as.factor(Species))
  legend(0, 0.4, c("Species A", "Species B"), pch = 16, col = c("black", "red"))
  
  summary(leaf_pca)  
  screeplot(leaf_pca, type = "lines") #2 is enough
  
  loadings(leaf_pca)
  biplot(leaf_pca)

  leaf_pca1 <- prcomp(leaf_data, scale. = TRUE)
  q <- autoplot(leaf_pca1, data = leaf, color = "Species", loadings = TRUE,
                loadings.color = "blue", loadings.label = TRUE)
  ggplotly(q)
  
  
#herbs NMDS practice
  herbs <- read.csv("data/Herbivore_specialisation.csv", header = TRUE)
  head(herbs)

  herb_comm <- herbs[5:11] #columns 5 - 11 (only abundance values)
  
  library(vegan)
  nmds <- metaMDS(comm = herb_comm, distance = "bray", trace = FALSE, autotransform = FALSE)
  plot(nmds)    
  nmds_xy <- data.frame(nmds$points)  
  nmds_xy$habitat <- herbs$Habitat
  nmds_xy$daynight <- herbs$DayNight
  
  ggplot(nmds_xy, aes(MDS1, MDS2, color = habitat)) +
    geom_point() +
    theme_classic()

  nmds$stress  #acceptable
  