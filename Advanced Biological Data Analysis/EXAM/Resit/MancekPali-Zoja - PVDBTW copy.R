#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/EXAM/Resit")
getwd()

#Libraries
library(readxl)
library(afex)
library(lme4)
library(MuMIn)
library(DHARMa)
library(car)
library(effects)
library(emmeans)
library(multcomp)
library(ggeffects)

#Data
leaf <- read_xlsx("leafminer_data.xlsx")

#QUESTION: How does initial leaf phenolic concentration affect new mine abundance, 
#and does this differ between canopy positions?
  #Mine_count = counts -> need poisson GLM
  #Each tree sampled multiple times -> tree as random factor -> Poisson GLMM

head(leaf)
str(leaf)
leaf$tree <- as.factor(leaf$tree)
leaf$canopy_position <- as.factor(leaf$canopy_position)

plot(mine_count ~ phenolics_mg_g, data = leaf)
plot(mine_count ~ canopy_position, data = leaf)

#Analysis
set_sum_contrasts() 

#5 models to start with:
model_null <- glmer(mine_count ~ 1 + (1|tree), data=leaf, family=poisson)
model_A <- glmer(mine_count ~ phenolics_mg_g + (1|tree), data=leaf, family=poisson)
model_B <- glmer(mine_count ~ canopy_position + (1|tree), data=leaf, family=poisson)
model_add <- glmer(mine_count ~ phenolics_mg_g + canopy_position + (1|tree), data=leaf, family=poisson)
model_int <- glmer(mine_count ~ phenolics_mg_g * canopy_position + (1|tree), data=leaf, family=poisson)

AICc(model_null, model_A, model_B, model_add, model_int) #additive model = lowest AICc
model.sel(list(model_null, model_A, model_B, model_add, model_int))
# Best model = lowest AICc; ΔAIC > 2 from next = clearly best
  #Additive model lowest AICc, interactive model <2 difference in AICc -> prefer simpler

#Overdispersion check - DHARMa:
sim_out <- simulateResiduals(fittedModel = model_add, plot = FALSE)
testDispersion(sim_out)    # p > 0.05 = no overdispersion
plot(sim_out)              # visual residual diagnostics -> no overdispersion

#Overdispersion check - obs random effect:
leaf$obs <- factor(1:nrow(leaf))
head(leaf)

model_add_b = glmer(mine_count ~ phenolics_mg_g + canopy_position + (1|tree) + (1|obs),family=poisson(link=log),data=leaf)
summary(model_add_b)
AICc(model_add, model_add_b) #regular model = lower AICc; keeping it

#Collinearity:
vif(model_add) #VIF < 5; no collinearity!

#Summary of chosen model:
summary(model_add) #upper canopy leaves = higher proportion
Anova(model_add, type = "III") #initial phenolics conc and canopy position significant
levels(leaf$canopy_position) #0 = lower; 1 = upper

#Marginal means:
emm <- emmeans(model_add, ~ canopy_position, type="response")
pairs(emm)                                 # pairwise contrasts; significant
cld(emm, Letters = letters)               # compact letter display

#Visualisation:
plot(allEffects(model_add), multiline=TRUE, confint=list(style="auto"))
plot(allEffects(model_add), multiline=TRUE, confint=list(style="auto"), type="response")

(leaf_plot <- ggplot(leaf, aes(x = phenolics_mg_g, y = mine_count, col = canopy_position)) +
    geom_point(size = 3, alpha = 0.8) +
    theme_classic() +
    labs(x = "Initial leaf phenolics concentration (mg/g)",
      y = "Mine count",
      caption = "Model: Mine count ~ [Phenolics] + Canopy position + (1|tree), data = leaf, family = poisson") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), panel.grid.minor = element_blank()) +
    facet_wrap(~canopy_position) +
    geom_smooth(method="glm", method.args=list(family="poisson"), 
                fullrange=TRUE, se=TRUE))
