setwd("/Users/zojamancekpali/Desktop/KU Leuven/EEED")
getwd()

#data
bugs <- read.csv("Heverlee - Kleine poel - 2018A.csv")

head(bugs)
str(bugs)

bugs <- bugs %>% rename(individual = Individual, 
                        status = Status) %>% mutate(
                      bodysize = (BodySize1 + BodySize2) / 2,
                      antennae = (Antenna1 + Antenna2) / 2) %>% 
                 mutate(antennae_z = scale(antennae),
                        bodysize_z = scale(bodysize)) %>% 
                 mutate(fitness = case_when(status == "M" ~ 1, status == "U" ~ 0)) %>% 
                 mutate(mean_w = 52/292) %>% 
                 mutate(rw = fitness/mean_w)

#packages
library(mgcv)
library(ggplot2)
library(car)
library(rockchalk)
library(lmtest)
library(effects)
library(MuMIn)
library(afex)
library(emmeans)
library(dplyr)
library(nlme)
library(robustbase)
library(openxlsx)
library(mgcv)

#Univariate
fit1 <- lm(rw ~ antennae_z, , data = bugs)
plot(allEffects(fit1))
#poly rescales so beta is rescaled
fit2 <- lm(rw ~ poly(antennae_z, 2, raw = T), data = bugs)
plot(allEffects(fit2))
fit3 <- glm(fitness ~ antennae_z, family = binomial(link=logit), data=bugs)
plot(allEffects(fit3))
fit4 <- glm(fitness ~ poly(antennae_z,2), family = binomial(link=logit), data=bugs)
plot(allEffects(fit4))
AICc(fit3, fit4)
summary(fit4)
#REPORT ALL p values of ALL MODELS use glm aicc cause fits data better due to binomial family
#VIF ON POLY GIVES GVIF (1/2) not the same as vif, square the gvif
#NEW PACKAGE ncetst run incdiag(fit) will return vif
#VIF BELOW 10 IS FINE


model_bs <- lm(rw ~ bodysize_z, data = bugs)
summary(model_bs) #bodysize_z = 0.6950 (p-value = 0.0154)*
plot(allEffects(model_bs))

model_an <- lm(rw ~ antennae_z, data = bugs)
summary(model_an) #antennae length b = 0.3601 (p-value = 0.225)
plot(allEffects(model_an))

AICc(model_bs, model_an)

#Quadratic univariate
bugs_na <- na.omit(bugs)
quad_bs <- lm(rw ~ poly(bodysize_z, 2, raw = T), data = bugs_na)
summary(quad_bs) #bodysize_z = 6.0901 (p-value = 0.0302)*

quad_al <- lm(rw ~ poly(antennae_z, 2, raw = T), data = bugs_na)
summary(quad_al) #quad term significant!
plot(allEffects(quad_al))

AICc(quad_bs, quad_al)

AICc(model_bs, model_an, quad_al, quad_bs)


###Multivariate
fit1 <- lm(rw ~ antennae_z + bodysize_z, data = bugs)
fit2 <- lm(rw ~ antennae_z * bodysize_z, data = bugs)
fit3 <- lm(rw ~ poly(antennae_z, 2, raw = T) + poly(bodysize_z, 2, raw = T) + antennae_z:bodysize_z, data = bugs)

#multi glm
fit4 <- glm(fitness ~ antennae_z + bodysize_z, family = binomial(link=logit), data = bugs)
fit5 <- glm(fitness~ antennae_z * bodysize_z, family = binomial(link=logit), data = bugs)
fit6 <- glm(fitness~ poly(antennae_z, 2, raw = T) + poly(bodysize_z, 2, raw = T),family = binomial(link=logit), data = bugs)
fit7 <- glm(fitness~ poly(antennae_z, 2, raw = T) + poly(bodysize_z, 2, raw = T) + antennae_z:bodysize_z,family = binomial(link=logit), data = bugs)
AICc(fit4,fit5,fit6,fit7)
# df     AICc
# fit4  3 122.9682
# fit5  4 122.5189
# fit6  5 113.0677
# fit7  6 110.1605
#ALLE UITKOMSTEN NOTEREN ALTIJD AICC ZELFS HIER
summary(fit7)
#ANTENNA squared is not significant
#install.packages('mctest')
library(mctest)
imcdiag(fit7)


model_multi <- lm(rw ~ antennae_z + bodysize_z, data = bugs)
summary(model_multi)
model_multi_in <- lm(rw ~ antennae_z * bodysize_z, data = bugs)
summary(model_multi_in)

AICc(model_multi, model_multi_in)
vif(model_multi)
vif(model_multi_in)

model_quad_multi <- lm(rw ~ poly(antennae_z, 2, raw = T) + poly(bodysize_z, 2, raw = T), data = bugs_na)
summary(model_quad_multi)

model_quad_multi_in <- lm(rw ~ poly(antennae_z, 2, raw = T) * poly(bodysize_z, 2, raw = T), data = bugs_na)
summary(model_quad_multi_in) 

AICc(model_quad_multi, model_quad_multi_in)
imcdiag(model_quad_multi)
imcdiag(model_quad_multi_in)


#GLMs
glm_bs <- glm(fitness ~ bodysize_z, data = bugs, family = binomial)
summary(glm_bs)
#significant selection for bodysize_z (p = 0.0199)

glm_an <- glm(fitness ~ antennae_z, data = bugs, family = binomial)
summary(glm_an)
#no significant selestion for antennae length (p = 0.224)

#Multivariate
model_logit <- glm(fitness ~ bodysize_z * antennae_z, 
                   data = bugs, 
                   family = binomial)
summary(model_logit)
#estimates = β: bodysize_z = 0.6147 (selection favours larger males; p-value = 0.0492), 
                #antennae_z = -0.1755 (n.s.)
                #interaction = n.s.
vif(model_logit)

library(mctest)
imcdiag(model_logit)

#Splines
#### SPLINES #####
spline_body_size <- gam(fitness ~ s(bodysize) + antennae, family = "binomial", data = bugs)
spline_antennae_size <- gam(fitness ~ bodysize + s(antennae), family = "binomial", data = bugs)
#### predictions body size s(size) + weight
predict_range <- data.frame(bodysize = seq(min(bugs_na$bodysize), max(bugs_na$bodysize), length.out = 50),
                            antennae = mean(bugs_na$antennae))
estimates_covariatemodel <- cbind(predict_range,
                                  estimate_covariatemodel = predict(spline_body_size, newdata = predict_range, type = "response"))
ggplot() +
  geom_line(data = estimates_covariatemodel, aes(x = bodysize, y = estimate_covariatemodel)) +
  geom_point(data = bugs, aes(x = bodysize, y = fitness))

bootstraps <- do.call(rbind,
                      replicate(2000,
                                predict(gam(fitness ~ antennae + s(bodysize), family = "binomial",
                                            data = bugs_na[sample(1:nrow(bugs_na), replace = T),],
                                            sp = spline_body_size$sp),
                                        newdata = predict_range,
                                        type = "response"),
                                simplify = FALSE))

estimates_covariatemodel <- cbind(estimates_covariatemodel,
                                  lower_ci = apply(bootstraps, 2, quantile, 0.025),
                                  upper_ci = apply(bootstraps, 2, quantile, 0.975))

ggplot() + geom_ribbon(data = estimates_covariatemodel, aes(x = bodysize, ymin = lower_ci, ymax = upper_ci), alpha = 0.5) +
  geom_line(data = estimates_covariatemodel, aes(x = bodysize, y = estimate_covariatemodel)) +
  geom_point(data = bugs_na, aes(x = bodysize, y = fitness))

#### predictions antennae

predict_range <- data.frame(antennae = seq(min(bugs_na$antennae), max(bugs_na$antennae), length.out = 50),
                            bodysize = mean(bugs_na$antennae))
estimates_covariatemodel <- cbind(predict_range,
                                  estimate_covariatemodel = predict(spline_antennae_size, newdata = predict_range, type = "response"))
ggplot() +
  geom_line(data = estimates_covariatemodel, aes(x = antennae, y = estimate_covariatemodel)) +
  geom_point(data = bugs_na, aes(x = antennae, y = fitness))

bootstraps <- do.call(rbind,
                      replicate(2000,
                                predict(gam(fitness ~ s(antennae) + bodysize, family = "binomial",
                                            data = bugs_na[sample(1:nrow(bugs_na), replace = T),],
                                            sp = spline_antennae_size$sp),
                                        newdata = predict_range,
                                        type = "response"),
                                simplify=FALSE))
estimates_covariatemodel <- cbind(estimates_covariatemodel,
                                  lower_ci = apply(bootstraps, 2, quantile, 0.025),
                                  upper_ci = apply(bootstraps, 2, quantile, 0.975))

ggplot() + geom_ribbon(data = estimates_covariatemodel, aes(x = antennae, ymin = lower_ci, ymax = upper_ci), alpha = 0.5) +
  geom_line(data = estimates_covariatemodel, aes(x = antennae, y = estimate_covariatemodel)) +
  geom_point(data = bugs_na, aes(x = antennae, y = fitness))

