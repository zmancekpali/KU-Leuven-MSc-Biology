#This is the R script belonging to lecture 5 ('mixed models') 
#of Advanced Biological Data Analysis

library(afex) #also loads lme4
library(car)
library(effects)
library(MuMIn)
library(scales)
library(rockchalk)
library(emmeans)
library(nlme)

set_sum_contrasts()

# SLIDE 8: Start by downloading the datafile "fitness.csv" from Toledo
# Set your own working directory! (the directory where the datafile is located)
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Mixed Models")
d = read.xlsx("fitness.xlsx")
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
d2 <- read.xlsx("schools.xlsx")
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
d3 <- read.xlsx("queenpheromone_wide.xlsx")
head(d3)
d3 <- read.xlsx("queenpheromone_long.xlsx")
head(d3)
str(d3)
#code 'colony' and 'treatment' as factors
d3$colony <- as.factor(d3$colony)
d3$treatment <- as.factor(d3$treatment)

#SLIDE 31: run a generalized linear mixed model
fit3 <- glmer(ovarydev ~ treatment + colonysize + (1|colony), family=binomial, data=d3)

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
darwin <- read.xlsx("darwin.xlsx")
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
d4 <- read.xlsx("hawaiibirds.xlsx")
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
fit9 <- gls(Birds~Year, data=d5, correlation = corAR1(form=~Year))

#SLIDE 62
summary(fit9)

#SLIDE 63: effect plots - just made sure here that they are both on the same
#scale (y-axis between -100 and 300)
plot(allEffects(fit8, residuals=TRUE), smooth.residuals = FALSE, ylim=c(-100,300))
plot(allEffects(fit9, residuals=TRUE), smooth.residuals = FALSE, ylim=c(-100,300))

#SLIDE 64
#as before, to compare the gls model with the model that does not have a specific
#correlation structure, we need to code the first model (the lm) as gls
fit8b <- gls(Birds~Year, data=d5)
AICc(fit8b, fit9)

