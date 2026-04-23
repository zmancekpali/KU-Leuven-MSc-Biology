##%#########################################################################%##
#                                                                             #
#               Behavioural Ecology - Practical course (G0G34a)               #
#                                 2025/2026                                   #
#                             Cricket group 1                                 #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/Desktop/KU Leuven/Behavioural Ecology")
getwd()

#Libraries
library(car)
library(DHARMa)
library(effectsize)
library(emmeans)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(lmerTest)
library(MuMIn)
library(patchwork)
library(survival)
library(survminer)
library(tidyverse)
library(VGAM)

#Data ----
crickets <- read.csv("cricket_autotomy_raw_dataset_v2.csv",
                     na = c("", "NA", "NaN"))

crickets_long <- read.csv("cricket_autotomy_long.csv",
                          na = c("", "NA", "NaN"))

crickets_long <- crickets_long %>%
  rename(ID = individual_ID,
         age = age_days_post_moult,
         mass = body_mass_g,
         regime = autotomy_regime,
         personality = personality_category,
         PC1_pre = personality_PC1_pre,
         PC1_post = personality_PC1_post,
         a_latency = autotomy_latency_s,
         a_censored = autotomy_censored,
         a_occured = autotomy_occurred,
         pinch_dur = sham_pinch_duration_s,
         FM_latency = OFT_latency_first_move_s,
         total_distance = OFT_total_distance_cm,
         time_exp = OFT_time_exposed_zone_s,
         time_central = OFT_time_central_zone_s,
         freezing_n = OFT_freezing_bouts_n,
         resume_latency = PCRT_latency_resume_s,
         total_freeze_time = PCRT_total_freeze_duration_s,
         shelter_latency = PCRT_shelter_seeking_latency_s) %>%
  mutate(ID = factor(ID),
         sex = factor(sex),
         regime = factor(regime, levels = c("sham", "autotomy")),
         personality = factor(personality, levels = c("Shy", "Intermediate", "Bold")),
         d_boldness = PC1_post - PC1_pre)

crickets_analysis <- crickets %>%
  mutate(individual_ID = factor(individual_ID),
    sex = factor(sex),
    autotomy_regime = factor(autotomy_regime, levels = c("sham", "autotomy")),
    personality_category = factor(personality_category, levels = c("Shy", "Intermediate", "Bold")),
    autotomy_occurred = case_when(autotomy_occurred == "True"  ~ TRUE,
      autotomy_occurred == "False" ~ FALSE, TRUE ~ NA),
    autotomy_censored = case_when(autotomy_censored == "True"  ~ TRUE,
      autotomy_censored == "False" ~ FALSE,
      TRUE ~ NA),
    delta_boldness = personality_PC1_post - personality_PC1_pre,
    surv_status = case_when(autotomy_regime == "autotomy" & autotomy_occurred == TRUE  ~ 1,
      autotomy_regime == "autotomy" & autotomy_occurred == FALSE ~ 0,
      TRUE ~ NA_integer_))

#Plot aestetics: ----
pal_personality <- c(Shy = "yellow2", Intermediate = "orange", Bold = "red3")

pal_regime <- c(sham = "lightblue3", autotomy = "red")

pal_timepoint <- c(pre = "#888780", post = "#C04828")

theme_cricket <- function() {
  theme_classic() +
    theme(axis.title = element_text(size = 10),
      legend.position = c(0.1, 0.1),
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text = element_text(face = "bold", size = 9))
}

#RQ1: does boldness predict autotomy latency ----
#Bodymass and sex as additional variables
#Additive models 
rq1_lm <- lm(autotomy_latency_s ~ personality_PC1_pre + body_mass_g + sex,
  data = filter(crickets_analysis, autotomy_regime == "autotomy"))
summary(rq1_lm) #significant effect of pre_PC1 and sex on latency to autotomise
#Model = significant; adjusted R^2 = 0.7054
#No random effect because this first test had no repeated measures

  #Diagnostics for rq1_lm
  shapiro.test(resid(rq1_lm)) #normally distributed
  qqnorm(residuals(rq1_lm)); qqline(residuals(rq1_lm), col = "red", lwd = 2) #ok
  
  vif(rq1_lm) #all good
  
  spreadLevelPlot(rq1_lm) #looks weird
  ncvTest(rq1_lm) #significantly off
  
  residualPlots(rq1_lm) #non linear for PC1_pre and Tukey Test
  outlierTest(rq1_lm) #ok
  influenceIndexPlot(rq1_lm, vars = c("Studentized", "Bonf")) #point 7 significant outlier but not with Bonferroni correction
  
  #Quadratic additive model:
  rq1_lm2 <- lm(autotomy_latency_s ~ personality_PC1_pre + I(personality_PC1_pre^2) + body_mass_g + sex,
                data = filter(crickets_analysis, autotomy_regime == "autotomy"))
  summary(rq1_lm2) #PC_1, PC_1^2 and sex all significant
  #adjusted R^2 slightly better (0.7423); model significant
  
  rq1_lm3 <- lm(autotomy_latency_s ~ personality_PC1_pre + I(personality_PC1_pre^3) + body_mass_g + sex,
                data = filter(crickets_analysis, autotomy_regime == "autotomy"))
  summary(rq1_lm3) #all significant predictors except body mass
  #Adjusted R^2 even beter (0.7844); model significant

  residualPlots(rq1_lm3) #GOOD
  
  rq1_lm_poly <- lm(autotomy_latency_s ~ personality_PC1_pre + 
                      I(personality_PC1_pre^2) +
                      I(personality_PC1_pre^3) +
                      body_mass_g + sex,
                    data = filter(crickets_analysis, autotomy_regime == "autotomy")) #good practice to include both poly terms
  summary(rq1_lm_poly) #sex, PC1_pre^3, and PC1_pre significant
  #R^2 adjusted: 0.7831, model significant
    #As PC1 (boldness) increases, latency to autotomise (s) decreases (time to autotomy lower)
    #As PC1^2 (boldness^2) increases, latency to autotomise increases
    #Male crickets autotomise faster
  residualPlots(rq1_lm_poly) 
  

#Interactive models 
rq1_lm_int <- lm(autotomy_latency_s ~ personality_PC1_pre * body_mass_g * sex,
             data = filter(crickets_analysis, autotomy_regime == "autotomy"))
summary(rq1_lm_int) #nothing significant
#Model = significant; adjusted R^2 = 0.7042

  #Diagnostics for rq1_lm
  shapiro.test(resid(rq1_lm_int)) #normally distributed
  qqnorm(residuals(rq1_lm_int)); qqline(residuals(rq1_lm_int), col = "red", lwd = 2) #ok
  
  vif(rq1_lm_int, type = "predictor") #?
  
  spreadLevelPlot(rq1_lm_int) #looks weird
  ncvTest(rq1_lm_int) #ok
  
  residualPlots(rq1_lm_int) #non linear for PC1_pre
  outlierTest(rq1_lm_int) #ok
  influenceIndexPlot(rq1_lm_int, vars = c("Studentized", "Bonf")) #point 7 significant outlier but not with Bonferroni correction
  
  #Quadratic model:
  rq1_lm2_int <- lm(autotomy_latency_s ~ I(personality_PC1_pre^2) + personality_PC1_pre * body_mass_g * sex,
                data = filter(crickets_analysis, autotomy_regime == "autotomy"))
  summary(rq1_lm2_int) #PC_1^2 significant
  #adjusted R^2 better (0.7299); model significant
  
  rq1_lm3_int <- lm(autotomy_latency_s ~ I(personality_PC1_pre^3) + personality_PC1_pre * body_mass_g * sex,
                data = filter(crickets_analysis, autotomy_regime == "autotomy"))
  summary(rq1_lm3_int) #PC1_pre*sex significant
  #Adjusted R^2 worse (0.7827); model significant
  
  MuMIn::AICc(rq1_lm2_int, rq1_lm3_int, rq1_lm_int) #quadratic is best
  
  residualPlots(rq1_lm2_int) #GOOD
  
  rq1_lm_poly_int <- lm(autotomy_latency_s ~ 
                      I(personality_PC1_pre^2) *
                      I(personality_PC1_pre^3) * 
                      personality_PC1_pre *
                      body_mass_g * sex,
                    data = filter(crickets_analysis, autotomy_regime == "autotomy"))
  rq1_lm_poly_int2 <- lm(autotomy_latency_s ~ I(personality_PC1_pre^2) +
                           I(personality_PC1_pre^3) +
                           personality_PC1_pre * body_mass_g * sex,
                         data = filter(crickets_analysis, autotomy_regime == "autotomy"))
  MuMIn::AICc(rq1_lm_poly_int, rq1_lm_poly_int2) #fewer interactions = better
  MuMIn::AICc(rq1_lm_poly_int2, rq1_lm3_int) #cubic still better
  
  summary(rq1_lm3_int) #PC1_pre^3 significant
  #Adjusted R^2 worse (0.7738); model significant
  residualPlots(rq1_lm_poly_int) 
  

  MuMIn::AICc(rq1_lm_poly_int, rq1_lm2_int, rq1_lm2, rq1_lm, rq1_lm_int, rq1_lm_poly, rq1_lm3, rq1_lm3_int) #Cubic alone, no interactions better fit (delta AICc < 2)
    #rq1_lm_poly (first polynomial also similar in AICc, worth noting)
  
#BEST MODEL:
summary(rq1_lm3) #sex, PC1_pre, and PC1_pre^3 significant
#In conclusion: relationship between PC1_pre and latency to autotomise best explained by a cubic tranformation
  #Bolder individuals = autotomise faster
  #Males autotomise faster
  #Cubic term: relationship curves at the extremes
shapiro.test(resid(rq1_lm3))
qqnorm(residuals(rq1_lm3)); qqline(residuals(rq1_lm3), col = "red", lwd = 2) #ok

vif(rq1_lm3, type = "predictor") #?

spreadLevelPlot(rq1_lm3) #looks ok
ncvTest(rq1_lm3) #ok

residualPlots(rq1_lm3) #non linear for PC1_pre
outlierTest(rq1_lm3) #ok
influenceIndexPlot(rq1_lm3, vars = c("Studentized", "Bonf")) #point 7 significant outlier but not with Bonferroni correction

#In the linear model
plot(autotomy_latency_s ~ personality_PC1_pre, data = crickets)
#Low PC1 values = shy (high latency (s) to autotomise); intermediate values: latency decreases and boldness increases
#At high boldness, latency (s) to autotomise lowest (most likely to autotomise)
plot(body_mass_g ~ personality_PC1_pre, data = crickets) #spread out but some general trend of increased boldness with increased body mass
plot(autotomy_latency_s ~ body_mass_g, data = crickets) #latency highest (takes longest) at intermediate body mass
boxplot(autotomy_latency_s ~ sex, data = crickets)
  
    
  
#Tobit regression
rq1_tobit <- vglm(
  autotomy_latency_s ~ personality_PC1_pre + body_mass_g + sex,
  tobit(Upper = 30),
  data = filter(crickets_analysis, autotomy_regime == "autotomy"))
summary(rq1_tobit) #Significant effect of sex and PC1_pre on latency to autotomise (same trends as above)

#Cox model 
cox_model <- coxph(Surv(autotomy_latency_s, surv_status) ~ personality_PC1_pre,
                   data = crickets_analysis) 
summary(cox_model)

rq1_cox <- coxph(
  Surv(autotomy_latency_s, surv_status) ~ personality_PC1_pre + body_mass_g + sex,
  data = filter(crickets_analysis, autotomy_regime == "autotomy"))
summary(rq1_cox) #this one explains more; sex and PC1_pre significant

standardize_parameters(rq1_lm3)


#Plots for RQ1: ----
tobit_pc1_p  <- "p < 0.001"
tobit_mass_p <- "p = 0.005"
lm_r2        <- round(summary(rq1_lm3)$adj.r.squared, 3)
cox_hr       <- round(summary(rq1_cox)$coefficients["personality_PC1_pre", "exp(coef)"], 2)

#To go with the Cox model:
km_fit <- survfit(Surv(autotomy_latency_s, surv_status) ~ personality_category,
                  data = filter(crickets_analysis, autotomy_regime == "autotomy"))

(cox_plot <- ggsurvplot(km_fit,
           data        = filter(crickets_analysis, autotomy_regime == "autotomy"),
           palette     = unname(pal_personality),
           conf.int    = TRUE,
           pval.coord   = c(25, 0.95),
           legend.labs = c("Shy", "Intermediate", "Bold"),
           legend = c(0.1, 0.1),
           legend.title = "Personality",
           xlab        = "Time (s)",
           ylab        = "Probability of not autotomising",
           ggtheme     = theme_cricket()))

cox_plot$plot <- cox_plot$plot +
  annotate("text", x = -Inf, y = Inf, label = "b)", 
           size = 6, fontface = "bold", hjust = -0.5, vjust = 1.5)
png("Plots/cox_KM_plot.png", width = 2000, height = 2100, res = 300)
print(cox_plot)
dev.off()

lr_test <- survdiff(Surv(autotomy_latency_s, surv_status) ~ personality_category,
                    data = filter(crickets_analysis, autotomy_regime == "autotomy"))
lr_p <- round(1 - pchisq(lr_test$chisq, length(lr_test$n) - 1), 4)

(cox_plot_stats <- ggsurvplot(km_fit,
           data         = filter(crickets_analysis, autotomy_regime == "autotomy"),
           palette      = unname(pal_personality),
           conf.int     = TRUE,
           pval         = paste0("Log-rank p < 0.0001",
                                 "\nCox HR(PC1) = 6.57, p < 0.001",
                                 "\nConcordance = 0.882"),
           pval.coord   = c(17, 0.8),
           legend       = c(0.15, 0.1),
           legend.labs  = c("Shy", "Intermediate", "Bold"),
           legend.title = "Personality",
           xlab         = "Time (s)",
           ylab         = "Probability of not autotomising",
           ggtheme      = theme_cricket()))

png("Plots/cox_KM_plot_stats.png", width = 2400, height = 1800, res = 300)
print(cox_plot_stats)
dev.off()

#To go with the Tobit + linear regression: 
pred_data <- data.frame(
  personality_PC1_pre = seq(
    min(filter(crickets_analysis, autotomy_regime == "autotomy")$personality_PC1_pre, na.rm = TRUE),
    max(filter(crickets_analysis, autotomy_regime == "autotomy")$personality_PC1_pre, na.rm = TRUE),
    length.out = 100),
  body_mass_g = mean(filter(crickets_analysis, autotomy_regime == "autotomy")$body_mass_g, na.rm = TRUE),
  sex = c("F", "M"))

pred_data$predicted <- predict(rq1_lm3, newdata = pred_data)

(tb_plot <- ggplot() +
    geom_point(data = filter(crickets_analysis, autotomy_regime == "autotomy"),
               aes(x = personality_PC1_pre, y = autotomy_latency_s,
                   colour = personality_category,
                   shape = if_else(autotomy_occurred, "Autotomised", "Censored")),
               size = 2.5, alpha = 1) +
    geom_line(data = pred_data,
              aes(x = personality_PC1_pre, y = predicted,
                  linetype = sex),
              colour = "grey30", linewidth = 1.2) +
    scale_linetype_manual(values = c(F = "solid", M = "dashed"),
                          name = "Sex",
                          labels = c(F = "Female", M = "Male")) +
    geom_hline(yintercept = 30, linetype = "dashed", colour = "red3") +
    annotate("text", x = 2, y = 30.8,
             label = "Censor (30 s)", colour = "red3", size = 3, hjust = 0, fontface = "bold") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_shape_manual(values = c(Autotomised = 16, Censored = 2), name = "Outcome") +
    scale_linetype_manual(values = c(F = "solid", M = "dashed"), 
                          name = "Sex",
                          labels = c(F = "Female", M = "Male")) +
    labs(x = "Pre-trial boldness (PC1)", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.24),
          legend.background =element_blank(),
          legend.key = element_blank(),
          legend.key.width = unit(2, "cm")) +
    annotate("text", x = -3.4, y = 38, label = "a)", size = 6, fontface = "bold"))
ggsave("Plots/tb_plot.png", tb_plot, width = 10, height = 8, dpi = 300)


(tb_plot_stats <- ggplot() +
    geom_point(data = filter(crickets_analysis, autotomy_regime == "autotomy"),
               aes(x = personality_PC1_pre, y = autotomy_latency_s,
                   colour = personality_category,
                   shape  = if_else(autotomy_occurred, "Autotomised", "Censored")),
               size = 2.5, alpha = 0.8) +
    geom_line(data = pred_data,
              aes(x = personality_PC1_pre, y = predicted),
              colour = "grey30", linewidth = 1.2) +
    geom_hline(yintercept = 30, linetype = "dashed", colour = "red3") +
    annotate("text", x = -3, y = 30.8,
             label = "Censor (30 s)", colour = "red3", size = 3, hjust = 0) +
    annotate("text", x = 0.8, y = 28,
             label = paste0("Tobit: β(PC1) = -11.98, ", tobit_pc1_p,
                            "\nβ(mass) = +18.64, ", tobit_mass_p,
                            "\nLM adj. R² = ", lm_r2,
                            "\nCox HR = ", cox_hr, ", p < 0.001"),
             hjust = 0, vjust = 1, size = 3, colour = "grey20",
             lineheight = 1.4, fontface = "bold") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_shape_manual(values = c(Autotomised = 16, Censored = 2), name = "Outcome") +
    labs(x = "Pre-trial boldness (PC1)", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = c(0.15, 0.2)))
ggsave("Plots/tb_plot_stats.png", tb_plot_stats, width = 8, height = 8, dpi = 300)

gridRQ1 <- tb_plot | cox_plot
ggsave("Plots/RQ1_grid.png", gridRQ1, width = 18, height = 7, dpi = 300)



#Body mass as a predictor plot: 
(bm_plot <- ggplot(filter(crickets_analysis, autotomy_regime == "autotomy"),
                         aes(x = body_mass_g, y = autotomy_latency_s,
                             colour = personality_category,
                             shape  = if_else(autotomy_occurred, "Autotomised", "Censored"))) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey30",
                fill = "grey80", linewidth = 1,
                inherit.aes = FALSE,
                aes(x = body_mass_g, y = autotomy_latency_s,
                    data = filter(crickets_analysis, autotomy_regime == "autotomy"))) +
    geom_hline(yintercept = 30, linetype = "dashed", colour = "red3") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_shape_manual(values = c(Autotomised = 16, Censored = 2), name = "Outcome") +
    labs(x = "Body mass (g)", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = "bottom"))
ggsave("Plots/bm_plot.png", bm_plot, width = 8, height = 8, dpi = 300)

(bm_plot_stats <- ggplot(filter(crickets_analysis, autotomy_regime == "autotomy"),
                        aes(x = body_mass_g, y = autotomy_latency_s,
                            colour = personality_category,
                            shape  = if_else(autotomy_occurred, "Autotomised", "Censored"))) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "grey30", fill = "grey80", linewidth = 1,
              inherit.aes = FALSE,
              aes(x = body_mass_g, y = autotomy_latency_s),
              data = filter(crickets_analysis, autotomy_regime == "autotomy")) +
  geom_hline(yintercept = 30, linetype = "dashed", colour = "red3") +
  annotate("text", x = 0.75, y = 34,
           label = "Tobit: β = +18.64, p = 0.005\nCox HR = 0.04, p = 0.008",
           hjust = 0, vjust = 1, size = 3, colour = "grey20", lineheight = 1.4, fontface = "bold") +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  scale_shape_manual(values = c(Autotomised = 16, Censored = 2), name = "Outcome") +
  labs(x = "Body mass (g)", y = "Latency to autotomise (s)") +
  theme_cricket() +
  theme(legend.position = "bottom"))
ggsave("Plots/bm_plot_stats.png", bm_plot_stats, width = 8, height = 8, dpi = 300)

#RQ2: does personality change after autotomy? ----
dat_long <- crickets_analysis %>%
  select(individual_ID, autotomy_regime, personality_category,
         body_mass_g, sex, personality_PC1_pre, personality_PC1_post) %>%
  # keep PC1_pre as a separate column BEFORE pivoting
  mutate(PC1_pre_score = personality_PC1_pre) %>%
  pivot_longer(cols      = c(personality_PC1_pre, personality_PC1_post),
               names_to  = "time",
               values_to = "boldness") %>%
  mutate(
    time = factor(time,
                  levels = c("personality_PC1_pre", "personality_PC1_post"),
                  labels = c("pre", "post")),
    autotomy_regime      = factor(autotomy_regime, levels = c("sham", "autotomy")),
    personality_category = factor(personality_category,
                                  levels = c("Shy", "Intermediate", "Bold"))
  )

rq2_model <- lmer(
  boldness ~ autotomy_regime * time + PC1_pre_score + (1 | individual_ID),
  data = dat_long)
summary(rq2_model) #PC1_pre significant; interaction between regime and time

    #Diagnostics for rq2_model
    shapiro.test(resid(rq2_model)) #non-normal 
    qqnorm(resid(rq2_model)); qqline(resid(rq2_model), col = "red", lwd = 2) #looks bad
    
    vif(rq2_model) #all good
    
    leveneTest(resid(rq2_model) ~ dat_long$time) #non-homogeneous
    leveneTest(resid(rq2_model) ~ dat_long$autotomy_regime) #also not
    leveneTest(resid(rq2_model) ~ interaction(dat_long$time, dat_long$autotomy_regime)) #also not 
    
    
    residualPlots(rq2_model) #doesnt work?
    outlierTest(rq2_model) #some significant outliers (points 166, 132)
    influenceIndexPlot(rq2_model, vars = c("Studentized", "Bonf")) #points 166 and 132 significant outliers
    
    
    #Transformation to avoid violating the assumptions:
    rq2_model_log <- lmer(
      log(boldness + abs(min(dat_long$boldness, na.rm = TRUE)) + 1) ~ 
        autotomy_regime * time + PC1_pre_score + (1 | individual_ID),
      data = dat_long) #log-transformation
    shapiro.test(resid(rq2_model_log)) #still bad
    leveneTest(resid(rq2_model_log) ~ dat_long$time) #still bad but better
    leveneTest(resid(rq2_model_log) ~ dat_long$autotomy_regime) #still bad but better
    
    
    #GlmmTMB without dispformula
    rq2_glmm2 <- glmmTMB::glmmTMB(
      boldness ~ autotomy_regime * time + PC1_pre_score + (1 | individual_ID),
      data = dat_long)
    summary(rq2_glmm2)
    
    sim_resid <- simulateResiduals(rq2_glmm2) #still issues
    plot(sim_resid)
    testDispersion(sim_resid)
    
    #Robust lmer with 1000 bootstraps for confidence intervals
    rq2_model_robust <- lmer(
      boldness ~ autotomy_regime * time + PC1_pre_score + (1 | individual_ID),
      data = dat_long) #gives singular warning, could use an LM alone 
    boot_ci <- confint(rq2_model, method = "boot", nsim = 1000)
    boot_ci
    summary(rq2_model_robust)
    
    vif(rq2_model)
    
    #LM without PC1 (use this one)
    rq2_model_lm <- lmer(
      boldness ~ body_mass_g + sex + autotomy_regime * time + (1 | individual_ID),
      data = dat_long)  
    summary(rq2_model_lm) #significant effect of time*regime

    sim_resid <- simulateResiduals(rq2_model_lm) #still issues
    plot(rq2_model_lm)
    testDispersion(rq2_model_lm)
    
    shapiro.test(resid(rq2_model_lm)) #still bad
    vif(rq2_model_lm) #all good
    outlierTest(rq2_model) #some significant outliers (points 166, 132)
    influenceIndexPlot(rq2_model, vars = c("Studentized", "Bonf")) #points 166 and 132 significant outliers
    
    qqPlot(resid(rq2_model_lm))
    shapiro.test(resid(rq2_model_lm))
    qqnorm(resid(rq2_model_lm)); qqline(resid(rq2_model_lm), col = "red", lwd = 2) #looks bad
    
    
    emm    <- emmeans(rq2_model_lm, ~ autotomy_regime * time)
    emm_df <- as.data.frame(emm)
    standardize_parameters(rq2_model_lm)
    confint(rq2_model_lm, method = "boot", nsim = 1000)
    
    
#Post-hoc tests:
    pairs(emm, simple = "time")
    pairs(emm, simple = "autotomy_regime")
    pairs(emm, simple = "autotomy_regime")
    pairs(emm, simple = "autotomy_regime")
    
    
#Plots for RQ2: ----
(emm_plot <- ggplot(emm_df, aes(x = time, y = emmean,
                   group = autotomy_regime,
                   colour = autotomy_regime)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.08, linewidth = 0.8) +
  scale_colour_manual(values = c(sham = "lightblue3", autotomy = "red3"),
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name = "Treatment") +
   coord_cartesian(ylim = c(-1.5, 1.5)) + #zooming in
  labs(y = "Estimated Boldness (PC1)\n← Shyer | Bolder →", x = "Timepoint") +
  theme_cricket() +
  theme(legend.position = c(0.06, 0.08)) +
   geom_point(data = dat_long,
              aes(x = time, y = boldness, colour = autotomy_regime),
              position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.3),
              alpha = 0.3, size = 1.5) +
   annotate("text", x = 0.5, y = 1.5, label = "a)", size = 6, fontface = "bold"))
ggsave("Plots/emm_plot.jpg", emm_plot, width = 8, height = 8, dpi = 300)

label_df <- data.frame(
  time = factor("Pre", levels = c("Pre", "Post")),
  PC1 = 2.4,
  autotomy_regime = factor("Sham", levels = c("Sham", "Autotomy")),
  label = "b)")
(ind_plot2 <- crickets_analysis %>%
    select(individual_ID, personality_category, autotomy_regime,
           personality_PC1_pre, personality_PC1_post) %>%
    drop_na() %>%
    pivot_longer(cols      = c(personality_PC1_pre, personality_PC1_post),
                 names_to  = "time", values_to = "PC1") %>%
    mutate(time = factor(time,
                         levels = c("personality_PC1_pre", "personality_PC1_post"),
                         labels = c("Pre", "Post")),
           autotomy_regime = factor(autotomy_regime, 
                                    levels = c("sham", "autotomy"),
                                    labels = c("Sham", "Autotomy"))) %>%
    ggplot(aes(x = time, y = PC1, colour = personality_category)) +
    geom_line(aes(group = individual_ID), alpha = 0.2, linewidth = 0.4) +
    geom_point(aes(group = individual_ID), alpha = 0.3, size = 1.5) +
    coord_cartesian(ylim = c(-2.5, 2.5)) +
    stat_summary(aes(group = personality_category),
                 fun = mean, geom = "line", linewidth = 1.5) +
    stat_summary(aes(group = personality_category),
                 fun = mean, geom = "point", size = 4, shape = 18) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    facet_wrap(~ autotomy_regime,
               labeller = labeller(autotomy_regime = c(sham = "Sham",
                                                       autotomy = "Autotomy"))) +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    labs(x = "Timepoint", y = "Boldness (PC1)\n← Shyer | Bolder →", x = "Timepoint") +
    theme_cricket() +
    theme(legend.position = c(0.07, 0.1)) +
    geom_text(data = label_df,
              aes(x = time, y = PC1, label = label),
              hjust = 5, fontface = "bold", size = 6,
              inherit.aes = FALSE))
ggsave("Plots/ind_plot.jpg", ind_plot2, width = 8, height = 8, dpi = 300)

(rq2_combined <- emm_plot | ind_plot2)
ggsave("Plots/RQ2grid1.jpg", rq2_combined, width = 19, height = 8, dpi = 300)


(d_boldness_plot <- ggplot(crickets_analysis,
       aes(x = personality_category, y = delta_boldness,
           fill = autotomy_regime, colour = autotomy_regime)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(alpha = 0.6, position = position_dodge(0.8),
               width = 0.25, outlier.shape = NA, colour = "grey30") +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             alpha = 0.6, size = 1.8) +
  scale_fill_manual(values = c(sham = "lightblue3", autotomy = "red3"),
                    labels = c(sham = "Sham", autotomy = "Autotomy"),
                    name = "Treatment") +
  scale_colour_manual(values = c(sham = "lightblue3", autotomy = "red3"),
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name = "Treatment") +
  labs(x = "Personality", y = "Δ Boldness (PC1 post − pre)") +
  theme_cricket() +
  theme(legend.position = c(0.1, 0.1)))
ggsave("Plots/d_boldness_plot.jpg", d_boldness_plot, width = 8, height = 8, dpi = 300)

(p_rq2_scatter <- crickets_analysis %>%
  drop_na(personality_PC1_pre, personality_PC1_post) %>%
  ggplot(aes(x = personality_PC1_pre, y = personality_PC1_post,
             colour = personality_category)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50") +
  geom_point(aes(shape = autotomy_regime), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              aes(fill = personality_category), alpha = 0.15) +
  facet_wrap(~ autotomy_regime,
             labeller = labeller(autotomy_regime = c(sham = "Sham",
                                                     autotomy = "Autotomy"))) +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  scale_fill_manual(values = pal_personality, guide = "none") +
  scale_shape_manual(values = c(sham = 1, autotomy = 16),
                     labels = c(sham = "Sham", autotomy = "Autotomy"),
                     name = "Treatment") +
  labs(x = "Pre-trial boldness (PC1)",
       y = "Post-trial boldness (PC1)") +
  theme_cricket() +
  theme(legend.position = c(0.9, 0.2)))
ggsave("Plots/boldness_pre_post_plot.jpg", p_rq2_scatter, width = 8, height = 8, dpi = 300)

#Cricket personality plots: ----
(p1 <- crickets_long %>%
   filter(timepoint == "pre") %>%
   group_by(ID, personality) %>%
   summarise(across(c(FM_latency, total_distance, time_central,
                      freezing_n, resume_latency, total_freeze_time,
                      shelter_latency),
                    \(x) mean(x, na.rm = TRUE)),
             .groups = "drop") %>%
   mutate(across(c(FM_latency, total_distance, time_central,
                   freezing_n, resume_latency, total_freeze_time,
                   shelter_latency), scale)) %>%
   pivot_longer(cols = c(FM_latency, total_distance, time_central,
                         freezing_n, resume_latency, total_freeze_time,
                         shelter_latency),
                names_to = "metric", values_to = "z_score") %>%
   group_by(personality, metric) %>%
   summarise(mean_z = mean(z_score, na.rm = TRUE),
             se_z   = sd(z_score, na.rm = TRUE) / sqrt(n()),
             .groups = "drop") %>%
   mutate(metric = dplyr::recode(metric,
                          FM_latency        = "OFT: Latency\nto first move",
                          total_distance    = "OFT: Total\ndistance",
                          time_central      = "OFT: Central\nzone time",
                          freezing_n        = "OFT: Freezing\nbouts",
                          resume_latency    = "PCRT: Latency\nto resume",
                          total_freeze_time = "PCRT: Freeze\nduration",
                          shelter_latency   = "PCRT: Shelter\nlatency")) %>%
   ggplot(aes(x = metric, y = mean_z,
              fill = personality, colour = personality)) +
   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
   geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
   geom_errorbar(aes(ymin = mean_z - se_z, ymax = mean_z + se_z),
                 position = position_dodge(0.75), width = 0.25, linewidth = 0.7) +
   scale_fill_manual(values = pal_personality, name = "Personality") +
   scale_colour_manual(values = pal_personality, name = "Personality") +
   labs(x = NULL, y = "Mean z-score") +
   theme_cricket() +
   theme(axis.text.x = element_text(size = 8)) +
   theme(legend.position = c(0.85, 0.9))) #pre
ggsave("Plots/pre_personality.png", p1, width = 10, height = 5.5, dpi = 300)

(p2 <- crickets_long %>%
    filter(timepoint == "post") %>%
    group_by(ID, personality) %>%
    summarise(across(c(FM_latency, total_distance, time_central,
                       freezing_n, resume_latency, total_freeze_time,
                       shelter_latency),
                     \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(across(c(FM_latency, total_distance, time_central,
                    freezing_n, resume_latency, total_freeze_time,
                    shelter_latency), scale)) %>%
    pivot_longer(cols = c(FM_latency, total_distance, time_central,
                          freezing_n, resume_latency, total_freeze_time,
                          shelter_latency),
                 names_to = "metric", values_to = "z_score") %>%
    group_by(personality, metric) %>%
    summarise(mean_z = mean(z_score, na.rm = TRUE),
              se_z   = sd(z_score, na.rm = TRUE) / sqrt(n()),
              .groups = "drop") %>%
    mutate(metric = dplyr::recode(metric,
                                  FM_latency        = "OFT: Latency\nto first move",
                                  total_distance    = "OFT: Total\ndistance",
                                  time_central      = "OFT: Central\nzone time",
                                  freezing_n        = "OFT: Freezing\nbouts",
                                  resume_latency    = "PCRT: Latency\nto resume",
                                  total_freeze_time = "PCRT: Freeze\nduration",
                                  shelter_latency   = "PCRT: Shelter\nlatency")) %>%
    ggplot(aes(x = metric, y = mean_z,
               fill = personality, colour = personality)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
    geom_errorbar(aes(ymin = mean_z - se_z, ymax = mean_z + se_z),
                  position = position_dodge(0.75), width = 0.25, linewidth = 0.7) +
    scale_fill_manual(values = pal_personality, name = "Personality") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    labs(x = NULL, y = "Mean z-score") +
    theme_cricket() +
    theme(axis.text.x = element_text(size = 8)) +
    theme(legend.position = c(0.85, 0.9))) #post
ggsave("Plots/post_personality.png", p2, width = 10, height = 5.5, dpi = 300)

(personality_change_plot <- crickets_long %>%
    group_by(ID, personality, regime, timepoint) %>%
    summarise(across(c(FM_latency, total_distance, time_central,
                       freezing_n, resume_latency, total_freeze_time,
                       shelter_latency),
                     \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(across(c(FM_latency, total_distance, time_central,
                    freezing_n, resume_latency, total_freeze_time,
                    shelter_latency), scale)) %>%
    pivot_longer(cols = c(FM_latency, total_distance, time_central,
                          freezing_n, resume_latency, total_freeze_time,
                          shelter_latency),
                 names_to = "metric", values_to = "z_score") %>%
    group_by(regime, timepoint, metric) %>%
    summarise(mean_z = mean(z_score, na.rm = TRUE),
              se_z   = sd(z_score, na.rm = TRUE) / sqrt(n()),
              .groups = "drop") %>%
    mutate(
      metric = dplyr::recode(metric,
                             FM_latency        = "OFT: Latency\nto first move",
                             total_distance    = "OFT: Total\ndistance",
                             time_central      = "OFT: Central\nzone time",
                             freezing_n        = "OFT: Freezing\nbouts",
                             resume_latency    = "PCRT: Latency\nto resume",
                             total_freeze_time = "PCRT: Freeze\nduration",
                             shelter_latency   = "PCRT: Shelter\nlatency"),
      timepoint = factor(timepoint, levels = c("pre", "post")),
      regime    = factor(regime, levels = c("sham", "autotomy"),
                         labels = c("Sham", "Autotomy"))
    ) %>%
    ggplot(aes(x = metric, y = mean_z,
               fill = timepoint, colour = timepoint)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    geom_errorbar(aes(ymin = mean_z - se_z, ymax = mean_z + se_z),
                  position = position_dodge(0.7), width = 0.2, linewidth = 0.7) +
    scale_fill_manual(values   = c(pre = "#888780", post = "#C04828"),
                      labels   = c(pre = "Pre", post = "Post"),
                      name     = "Timepoint") +
    scale_colour_manual(values = c(pre = "#888780", post = "#C04828"),
                        labels = c(pre = "Pre", post = "Post"),
                        name   = "Timepoint") +
    facet_wrap(~ regime, ncol = 1) +
    labs(x = NULL, y = "Mean z-score") +
    theme_cricket() +
    theme(axis.text.x = element_text(size = 8)))
ggsave("Plots/change_in_personality.png", personality_change_plot, width = 10, height = 10, dpi = 300)

#Supplementary plots ----
#What drives the boldness: 
corr_data <- crickets_long %>%
  filter(timepoint == "pre") %>%
  group_by(ID) %>%
  summarise(across(c(FM_latency, total_distance, time_central,
                     freezing_n, resume_latency, total_freeze_time,
                     shelter_latency),
                   \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") %>%
  select(-ID)

colnames(corr_data) <- c("OFT: Latency\nfirst move", "OFT: Total\ndistance",
                         "OFT: Central\nzone time", "OFT: Freezing\nbouts",
                         "PCRT: Latency\nresume", "PCRT: Freeze\nduration",
                         "PCRT: Shelter\nlatency")

(cor_matrix <- cor(corr_data, use = "complete.obs", method = "pearson"))

png("Plots/fig_corrplot.png", width = 2400, height = 2000, res = 300)
corrplot(cor_matrix,
         method   = "color",
         type     = "upper",
         tl.col   = "black",
         tl.srt   = 45,
         tl.cex   = 0.8,
         addCoef.col = "black",
         number.cex  = 0.7,
         col      = colorRampPalette(c("red3", "white", "steelblue3"))(200),
         diag     = FALSE)
dev.off()

(crickets_long %>%
  filter(timepoint == "pre") %>%
  group_by(ID, personality, regime) %>%
  summarise(across(c(FM_latency, total_distance, time_central,
                     freezing_n, resume_latency, total_freeze_time,
                     shelter_latency, PC1_pre),
                   \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(cols = c(FM_latency, total_distance, time_central,
                        freezing_n, resume_latency, total_freeze_time,
                        shelter_latency),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = dplyr::recode(metric,
                                FM_latency        = "OFT: Latency\nto first move",
                                total_distance    = "OFT: Total\ndistance",
                                time_central      = "OFT: Central\nzone time",
                                freezing_n        = "OFT: Freezing\nbouts",
                                resume_latency    = "PCRT: Latency\nto resume",
                                total_freeze_time = "PCRT: Freeze\nduration",
                                shelter_latency   = "PCRT: Shelter\nlatency")) %>%
  ggplot(aes(x = value, y = PC1_pre, colour = personality)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30",
              fill = "grey80", linewidth = 0.8,
              inherit.aes = FALSE,
              aes(x = value, y = PC1_pre)) +
  facet_wrap(~ metric, scales = "free_x", ncol = 4) +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  labs(x = "Metric value", y = "Boldness (PC1)") +
  theme_cricket() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(legend.position = c(0.9, 0.2)))

ggsave("Plots/fig_pc1_drivers.png", last_plot(),
       width = 14, height = 8, dpi = 300, bg = "white")

#Confounding variables:
#Body mass vs latency to autotomise (confounding variable)
(p_bodymass <- crickets_analysis %>%
    filter(autotomy_regime == "autotomy") %>%
    mutate(outcome = if_else(autotomy_occurred, "Autotomised", "Censored")) %>%
    ggplot(aes(x = body_mass_g, y = autotomy_latency_s,
               colour = personality_category, shape = outcome)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
                aes(group = 1), colour = "grey30") +
    geom_hline(yintercept = 30, linetype = "dashed", colour = "grey50") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_shape_manual(values = c(Autotomised = 16, Censored = 2),
                       name = "Outcome") +
    labs(x = "Body mass (g)", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = c(0.8, 0.2)))

ggsave("fig8_mass_confound.png", p_bodymass, width = 7, height = 5, dpi = 300)

#Sex vs latency to autotomise
(p_sex <- crickets_analysis %>%
  filter(autotomy_regime == "autotomy") %>%
  ggplot(aes(x = sex, y = autotomy_latency_s, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_manual(values = c(F = "#E07CA6", M = "#5B8DB8"),
                    labels = c(F = "Female", M = "Male"),
                    name = "Sex") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 30, linetype = "dashed", colour = "red3") +
  labs(x = "Sex", y = "Latency to autotomise (s)") +
  theme_cricket())
ggsave("Plots/sex_confound.png", p_sex, width = 7, height = 5, dpi = 300)

#Sample composition 
(p9a <- crickets_analysis %>%
    count(personality_category, autotomy_regime) %>%
    ggplot(aes(x = personality_category, y = n,
               fill = autotomy_regime)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    geom_text(aes(label = n),
              position = position_dodge(0.7), vjust = -0.4, size = 3.2) +
    scale_fill_manual(values = pal_regime,
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name = "Treatment") +
    labs(x = "Personality", y = "n") +
    theme_cricket() +
    theme(legend.position = "right"))

(p9b <- crickets_analysis %>%
    count(personality_category, sex) %>%
    ggplot(aes(x = personality_category, y = n, fill = sex)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    geom_text(aes(label = n),
              position = position_dodge(0.7), vjust = -0.4, size = 3.2) +
    scale_fill_manual(values = c(M = "steelblue3", F = "salmon3"), name = "Sex") +
    labs(x = "Personality", y = "n") +
    theme_cricket() +
    theme(legend.position = "right"))

(p9 <- (p9a | p9b) +
  plot_annotation(theme = theme(plot.title = element_text(face = "bold", size = 12))))
ggsave("Plots/fig9_sample_composition.png", p9, width = 11, height = 4.5, dpi = 300)

#Change in mean boldness per personality
(p5b <- crickets_long %>%
    filter(trial == 1) %>%
    select(ID, regime, personality, PC1_pre, PC1_post) %>%
    distinct() %>%
    group_by(regime, personality) %>%
    summarise(Pre  = mean(PC1_pre,  na.rm = TRUE),
              Post = mean(PC1_post, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols = c(Pre, Post), names_to = "time", values_to = "PC1") %>%
    mutate(time = factor(time, levels = c("Pre", "Post"))) %>%
    ggplot(aes(x = time, y = PC1,
               group = interaction(regime, personality),
               colour = personality,
               linetype = regime)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 4) +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_linetype_manual(values = c(sham = "dashed", autotomy = "solid"),
                          labels = c(sham = "Sham", autotomy = "Autotomy"),
                          name   = "Regime") +
    labs(x = "Timepoint", y = "Mean Boldness PC1") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.2)))

ggsave("Plots/boldness_pre_post.png", p5b,
       width = 8, height = 8, dpi = 300, bg = "white")

#Behaviour profiles (7)
(p7 <- crickets_long %>% #pre
    filter(timepoint == "pre") %>%
    group_by(ID, personality) %>%
    summarise(across(c(FM_latency, total_distance, time_central,
                       freezing_n, resume_latency, total_freeze_time,
                       shelter_latency),
                     mean, na.rm = TRUE),
              .groups = "drop") %>%
    # z-score each metric
    mutate(across(c(FM_latency, total_distance, time_central,
                    freezing_n, resume_latency, total_freeze_time,
                    shelter_latency), scale)) %>%
    pivot_longer(cols = c(FM_latency, total_distance, time_central,
                          freezing_n, resume_latency, total_freeze_time,
                          shelter_latency),
                 names_to  = "metric",
                 values_to = "z_score") %>%
    group_by(personality, metric) %>%
    summarise(mean_z = mean(z_score, na.rm = TRUE),
              se_z = sd(z_score, na.rm = TRUE) / sqrt(n()),
              .groups = "drop") %>%
    mutate(metric = dplyr::recode(metric,
                           FM_latency = "OFT: Latency\nto first move",
                           total_distance = "OFT: Total\ndistance",
                           time_central = "OFT: Central\nzone time",
                           freezing_n = "OFT: Freezing\nbouts",
                           resume_latency = "PCRT: Latency\nto resume",
                           total_freeze_time = "PCRT: Freeze\nduration",
                           shelter_latency = "PCRT: Shelter\nlatency")) %>%
    ggplot(aes(x = metric, y = mean_z,
               fill = personality, colour = personality)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
    geom_errorbar(aes(ymin = mean_z - se_z, ymax = mean_z + se_z),
                  position = position_dodge(0.75), width = 0.25,
                  linewidth = 0.7) +
    scale_fill_manual(values   = pal_personality, name = "Personality") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    labs(x = NULL, y = "Mean z-score") +
    theme_cricket() +
    theme(axis.text.x = element_text(size = 8)) +
    theme(legend.position = c(0.85, 0.9)))

ggsave("Plots/z-score_personality.png", p7,
       width = 10, height = 7, dpi = 300, bg = "white")




#Grids ----
#Pannels + more grids (H2)

plot_data <- crickets_long %>%
  filter(trial == 1) %>%
  select(ID, regime, personality, PC1_pre, PC1_post) %>%
  distinct() %>%
  pivot_longer(cols      = c(PC1_pre, PC1_post),
               names_to  = "timepoint",
               values_to = "PC1") %>%
  mutate(timepoint = factor(dplyr::recode(timepoint,
                                   PC1_pre  = "Pre",
                                   PC1_post = "Post"),
                            levels = c("Pre", "Post")),
         regime = factor(regime,
                         levels = c("sham", "autotomy"),
                         labels = c("Sham", "Autotomy")))

(pA <- ggplot(plot_data, aes(x = timepoint, y = PC1, group = ID)) +
    geom_line(aes(colour = personality), alpha = 0.3, linewidth = 0.5) +
    geom_point(aes(colour = personality), alpha = 0.4, size = 1.5) +
    stat_summary(aes(group = 1), fun = mean,
                 geom = "line", linewidth = 2,
                 colour = "black") +
    stat_summary(aes(group = 1), fun = mean,
                 geom = "point", size = 4,
                 colour = "black") +
    facet_wrap(~ regime) +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    labs(x = NULL, y = "Boldness PC1") +
    theme_cricket())

summary_data <- plot_data %>%
  group_by(regime, personality, timepoint) %>%
  summarise(mean_PC1 = mean(PC1, na.rm = TRUE),
            se       = sd(PC1, na.rm = TRUE) / sqrt(n()),
            .groups  = "drop")

(pB <- ggplot(summary_data,
              aes(x = timepoint, y = mean_PC1,
                  colour = personality,
                  group  = personality)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_errorbar(aes(ymin = mean_PC1 - se,
                      ymax = mean_PC1 + se),
                  width = 0.12, linewidth = 0.7) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 4) +
    facet_wrap(~ regime) +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    labs(x = NULL, y = "Mean Boldness PC1") +
    theme_cricket())

(pC <- ggplot(plot_data,
              aes(x = timepoint, y = PC1, fill = personality)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_boxplot(position = position_dodge(0.75), width = 0.6,
                 outlier.shape = 21, outlier.size = 1.5,
                 alpha = 0.8, colour = "grey30") +
    facet_wrap(~ regime) +
    scale_fill_manual(values = pal_personality, name = "Personality") +
    labs(x = NULL, y = "Boldness PC1") +
    theme_cricket())

(fig_prepost <- pA | pB | pC +
    plot_annotation(
      theme = theme(plot.title = element_text(face = "bold", size = 14))))

ggsave("Plots/fig_prepost_personality.png", fig_prepost,
       width = 18, height = 8, dpi = 300, bg = "white")

#Boxplot grids
boxplot_data <- crickets_long %>%
  group_by(ID, regime, personality, timepoint) %>%
  summarise(
    a = mean(FM_latency,        na.rm = TRUE),
    b = mean(total_distance,    na.rm = TRUE),
    c = mean(time_exp,          na.rm = TRUE),
    d = mean(time_central,      na.rm = TRUE),
    e = mean(freezing_n,        na.rm = TRUE),
    f = mean(resume_latency,    na.rm = TRUE),
    g = mean(total_freeze_time, na.rm = TRUE),
    h = mean(shelter_latency,   na.rm = TRUE),
    .groups = "drop") %>%
  pivot_longer(cols      = a:h,
               names_to  = "panel",
               values_to = "value") %>%
  mutate(
    timepoint = factor(timepoint,
                       levels = c("pre", "post"),
                       labels = c("Pre", "Post")),
    regime    = factor(regime,
                       levels = c("sham", "autotomy"),
                       labels = c("Sham", "Autotomy")),
    panel_label = dplyr::recode(panel,
                         a = "a)  OFT: Latency to first move (s)",
                         b = "b)  OFT: Total distance moved (cm)",
                         c = "c)  OFT: Time in exposed zone (s)",
                         d = "d)  OFT: Time in central zone (s)",
                         e = "e)  OFT: Freezing bouts (n)",
                         f = "f)  PCRT: Latency to resume activity (s)",
                         g = "g)  PCRT: Total freeze duration (s)",
                         h = "h)  PCRT: Shelter-seeking latency (s)"),
    panel_label = factor(panel_label, levels = c(
      "a)  OFT: Latency to first move (s)",
      "b)  OFT: Total distance moved (cm)",
      "c)  OFT: Time in exposed zone (s)",
      "d)  OFT: Time in central zone (s)",
      "e)  OFT: Freezing bouts (n)",
      "f)  PCRT: Latency to resume activity (s)",
      "g)  PCRT: Total freeze duration (s)",
      "h)  PCRT: Shelter-seeking latency (s)")))

(fig_boxgrid <- ggplot(boxplot_data,
                       aes(x    = timepoint,
                           y    = value,
                           fill = regime)) +
    geom_boxplot(
      position = position_dodge(0.7),
      width = 0.55,
      outlier.size = 0.8,
      outlier.shape = 21,
      outlier.alpha = 0.5,
      alpha = 0.8,
      colour = "grey30",
      linewidth = 0.4) +
    facet_wrap(~ panel_label,
               scales = "free_y",
               ncol = 4) +
    scale_fill_manual(values = c(Sham = "lightblue3", Autotomy = "red"),
                      name   = "Regime") +
    labs(x = NULL, y = "Mean value (session average)") +
    theme_classic() +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      strip.text       = element_text(face = "bold", size = 9, hjust = 0),
      strip.background = element_rect(fill = "grey95", colour = NA),
      axis.text.x      = element_text(size = 9),
      axis.text.y      = element_text(size = 8),
      axis.title.y     = element_text(size = 9),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.spacing    = unit(1, "lines")))

ggsave("Plots/fig_boxplot_grid.png",  fig_boxgrid,
       width = 17, height = 8, dpi = 300, bg = "white")

(fig_boxgrid_pers <- ggplot(boxplot_data,
                            aes(x    = timepoint,
                                y    = value,
                                fill = personality)) +
    geom_boxplot(position = position_dodge(0.75),
                 width = 0.6,
                 outlier.size = 0.8,
                 outlier.shape = 21,
                 outlier.alpha = 0.5,
                 alpha = 0.8,
                 colour = "grey30",
                 linewidth = 0.4) +
    facet_wrap(~ panel_label,
               scales = "free_y",
               ncol   = 4) +
    scale_fill_manual(
      values = pal_personality,
      name = "Personality") +
    labs(x = NULL, y = "Mean value (session average)") +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      strip.text = element_text(face = "bold", size = 9, hjust = 0),
      strip.background = element_rect(fill = "grey95", colour = NA),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.spacing = unit(1, "lines")))


ggsave("Plots/fig_boxplot_grid_personality.png", fig_boxgrid_pers,
       width = 17, height = 8, dpi = 300, bg = "white")

