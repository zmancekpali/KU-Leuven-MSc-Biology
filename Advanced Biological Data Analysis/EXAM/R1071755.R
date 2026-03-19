##%#########################################################################%##
#                                                                             #
#                     Exam - Zoja Manček Páli (r1071755)                      #
#                         Van den Berg/Wenseleers                             #
#                          Date: 26.1.2026, 13:00                             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/EXAM")
getwd()

#packages
library(readxl)
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

#data
comm <- read_xlsx("Exam data/Comm_data.xlsx")
env <- read_xlsx("Exam data/Env_data.xlsx")
peng <- read.csv("Exam data/Penguins.csv")

#context
#penguins cannot moult and mate at the same time - significant life history event
  #Age and time of breeding may influence moult timing - individuals who breed later
    #may delay moulting 
#Repeated measurements of same individual over different years (data points not independent)
  #Recorded: age, breeding outcome, time of moult (0 = earliest moult, 10 = 10 days after earliest moult)

#data inspection
set_sum_contrasts()
head(peng)
str(peng)
unique(peng$breeding_status)

peng <- peng %>% mutate(breeding_status = as.factor(breeding_status),
                        bird_id = as.factor(bird_id)) #change character + integer values to factors
peng <- peng %>% dplyr::rename(status = breeding_status,
                               ID = bird_id,
                               moult = moult_start_days)
                  #renamed for clarity
(summ_peng <- summarise_all(peng, mean)) #gives mean of all numerical categories (some columns are non-numerical so it gives an error)
    #mean age: 9.5806; mean start date: 17.96129

plot(moult ~ age, data = peng) #seems to be a slight positive trend
plot(moult ~ status, data = peng) #moult time longest in successful breeders - in line with hypothesis

#analysis:
#y = moult time = from 0 - counts (poisson regression best)
  #x = age = counts data
  #x = status = categorical
  #random effect: bird ID (because data points not independent) -> mixed poisson GLM best

peng$status <- relevel(peng$status, ref = "Non-breeder")

#Poisson GLMM
model_null <- glmer(moult ~ 1 + (1|ID), data = peng, family = poisson) #null model
model_status <- glmer(moult ~ status + (1|ID), data = peng, family = poisson) #reproductive status only
model_age <- glmer(moult ~ age + (1|ID), data = peng, family = poisson) #age only
model_add <- glmer(moult ~ status + age + (1|ID), data = peng, family = poisson) #additive model
model_int <- glmer(moult ~ status * age + (1|ID), data = peng, family = poisson) #interactive model

#to determine best model:
MuMIn::AICc(model_null, model_status, model_age, model_add, model_int) #interactive model = best

models_list <- list(model_null, model_status, model_age, model_add, model_int)
model.sel(models_list) #model_int has the lowest AICc and delta > 2

plot(allEffects(model_int), multiline = T, confint = list(style = "auto"))
#we can see that non-breeders have the most flat slope; successful breeders the steepest positive slope
  #(meaning that time to moulting event increases with age); and failed breeders show a negative relationship
  #between age and moult, suggesting they moult earlier as they age. 
  #This all being on the log scale
plot(allEffects(model_int), multiline = T, confint = list(style = "auto"), type = 'response')
#With the actual responses, we see that the successful breeder curve looks non-linear (convex) and with a big
  #confidence interval, while for non-breeders and failed breeders the relationships stay the same as above

summary(model_int)
Anova(model_int, type = 3)
vif(model4) #GVIF < 5; no collinearity!

par(mfrow = c(1, 2))
plot(residuals(model_int) ~ fitted(model_int), main = "Residuals vs. Fitted")
qqnorm(residuals(model_int)) #looks ok
par(mfrow = c(1, 1))

#overdispersion check
peng$obs <- factor(1:nrow(peng))
model_overdisp <- glmer(moult ~ status * age + (1|ID) + (1|obs), data = peng, family = poisson)

simulationOutput <- DHARMa::simulateResiduals(fittedModel = model_int, plot = F)
DHARMa::testDispersion(simulationOutput) #no sign of overdisperson, statistically

#plots
Status <- peng$status
(peng_plot <- ggplot(peng, aes(x = age, y = moult, col = status)) +
    geom_point(size = 3, alpha = 0.8) +
    theme_classic() +
    labs(
      x = "Penguin age",
      y = "Time to moulting event (days)",
      caption = "Model: moult ~ status * age + (1|ID), data = peng, family = poisson") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()))
ggpredict(model_int, se = TRUE, type = "count", interactive = TRUE)
