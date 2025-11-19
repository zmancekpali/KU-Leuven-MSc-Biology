setwd('~/Desktop/KU Leuven/Stress Ecology & Ecotoxicology')
getwd()

#libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggeffects)
library(drc)
library(MASS)
library(MuMIn)
library(DHARMa)
library(effects)
library(cowplot)
library(car)
library(emmeans)
library(RColorBrewer)


#data
acute <- read.xlsx("Report/Acutetox_2025.xlsx") #acute toxicity
chronic <- read.xlsx("Report/dataset_chronic_2025.xlsx")  #combined stressors
daph <- read.xlsx("daphneas_pt1.xlsx") #EC50 calculations

head(acute)
head(acute_48h)
head(chronic)

#EC50 ----
daph %>%
  count(conc)

(EC48h <- drm(immobility ~ conc, data = daph, fct =
                LL.4(names = c("Slope","Lower Limit","Upper Limit", "EC50"))))

summary(EC48h) #gives lower + upper limits, EC50, and slope

ED(EC48h, c(50),interval='delta') #this gives the 95% confidence interval

plot(EC48h, broken = TRUE, xlab = "Concentration", ylab = "Immobility")

#Dose-response curve
daph_1 <- data.frame(conc = exp(seq(log(min(daph$conc[daph$conc>0])),
                                    log(max(daph$conc)), length=100)))
daph_1$fit <- predict(EC48h, daph_1)

EC50 <- 0.364
CI_low <- 0.3200
CI_high <- 0.408

conc_breaks <- c(0.1, 0.2, 0.4, 0.8, 1.6)

(daph_plot <- ggplot() +
    geom_point(data = daph,
               aes(x = conc, y = immobility, colour = "Observed data"),
               size = 2) +
    geom_line(data = daph_1,
              aes(x = conc, y = fit, colour = "Dose-response model fit"),
              linewidth = 1) +
    geom_vline(xintercept = EC50, linetype = "dashed", colour = "red") +
    geom_point(aes(x = EC50, y = 50), colour = "red", size = 3) +
    annotate("text", x = EC50, y = 45,
             label = paste0("EC50 = ", EC50, 
                            " mg/L\n(95% CI: ", 
                            CI_low, "–", CI_high, ")"),
             hjust = -0.1, vjust = 0, colour = "red") +
    scale_x_log10(breaks = conc_breaks,
                  labels = conc_breaks) +    
    scale_colour_manual(values = c("Observed data" = "cornflowerblue", "Dose-response model fit" = "blue2")) +
    labs(x = expression("ZnO concentration (mg·L"^-1*")"),
         y = expression("Immobility of "*italic("Daphnia magna")*" (%)"),
         color = NULL) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank(), 
          legend.position = "none"))

ggsave("Report/dose_response_plot.jpg", daph_plot, units = "cm", 
       width = 15, height = 10)


#Acute ----
acute <- acute %>% mutate(
  temp_f = factor(temperature, levels = c(20,24), labels = c("20°C","24°C")),
  zinc_f = factor(zinc, levels = c(0,1), labels = c("No Zn","Zn")),
  total = alive + dead,
  prop_alive = alive / total)
head(acute)
acute %>%
  count(temperature)


acute_48h <- filter(acute, time == 48)
acute_48h$immo_count <- round(acute_48h$immo * acute_48h$total)
head(acute_48h)

boxplot(immo ~ temperature, data = acute_48h)
boxplot(immo ~ zinc, data = acute_48h)

#data is a proportion, so need to use glm (with wide format)
glm_wide <- glm(cbind(immo_count, total - immo_count) ~ zinc_f * temp_f, 
                  family = binomial(link = logit), data = acute_48h)
summary(glm_wide) #significant effect of zn on immobility

Eglmm_wide <- glmer(cbind(immo_count, total - immo_count) ~ zinc_f * temp_f + 
                     (1|group), family = binomial, data = acute_48h)
summary(glmm_wide)

AIC(glm_wide, glmm_wide) #no real difference, will keep the glm; delta = 0.56128
anova(glm_wide, glmm_wide, test = "Chisq")

em <- emmeans(glm_wide, ~ zinc_f * temp_f, type = "response")
summary(em)
cont <- pairs(em)
summary(cont)

plot(allEffects(glm_wide), multiline = T, confint = list(style = 'auto'), types = "response") #right

vif(glm_wide, type = "terms") #all good
outlierTest(glm_wide) #all good
residualPlots(glm_wide) #looks fine


#Visualisation
(acute_boxplot <- ggplot(acute, aes(x = temp_f, y = immo, fill = zinc_f)) +
  scale_fill_brewer(palette = "Paired", direction = 1, name = "nZnO treatment", 
                    labels = c(expression("No nZnO"), bquote(EC[50*","*48]*" nZnO"))) +
  geom_boxplot() +
  labs(x = "Acclimation Temperature", y = "Proportion immobile", fill = "ZnO") +
  theme_classic())
ggsave("Report/acute_boxplot.png", acute_boxplot, units = "cm", width = 20, height = 15)


#Chronic ----
head(chronic)

chronic <- chronic %>% rename(temp = temperature, 
                              h560 = "Absorbance(560nm)Haem",
                              h576 = "Absorbance(576nm)Haem",
                              h600 = "Absorbance(600nm)Haem",
                              h_nn = "Haemo.non.normalized",
                              p593 = "Absorbance(593nm)Prot") %>%  mutate(
                                temp_f = factor(temp, levels = c(20,24), 
                                                labels = c("20°C","24°C")),
                                zinc_f = factor(zinc, levels = c(0,1), 
                                                labels = c("No Zn","Zn")))
chronic <- chronic %>% mutate(
      treatment = case_when(
      ((grepl("20°C", temp_f, ignore.case = TRUE) & grepl("No Zn", zinc_f, ignore.case = TRUE))) ~ "Control",
      ((grepl("20°C", temp_f, ignore.case = TRUE) & grepl("Zn", zinc_f, ignore.case = TRUE))) ~ "Toxicant",
      ((grepl("24°C", temp_f, ignore.case = TRUE) & grepl("No Zn", zinc_f, ignore.case = TRUE))) ~ "Warming",
      ((grepl("24°C", temp_f, ignore.case = TRUE) & grepl("Zn", zinc_f, ignore.case = TRUE))) ~ "Warming + Toxicant"))
      
head(chronic)

boxplot(ctmax ~ zinc, data = chronic)
boxplot(ctmax ~ temp, data = chronic)
boxplot(ctmax ~ treatment, data = chronic)

shapiro.test(chronic$ctmax) #all good
bartlett.test(ctmax ~ treatment, chronic) #fine


(chronic_boxplot <- ggplot(chronic, aes(x = treatment, y = ctmax, fill = treatment)) +
    scale_fill_brewer(palette = "Paired", direction = 1, name = "Treatment") +
    geom_boxplot() +
    labs(x = "Treatment", y = expression("CT"["max"])) +
    theme_classic() +
    theme(legend.position = "none"))
ggsave("Report/chronic_boxplot1.png", chronic_boxplot, units = "cm", width = 20, height = 15)


model_ctmax <- lmer(ctmax ~ temp_f * zinc_f + (1|group), data = chronic)
model_ctmax2 <- lm(ctmax ~ temp_f * zinc_f, data = chronic)

AICc(model_ctmax, model_ctmax2) #first model is better
anova(model_ctmax, model_ctmax2, test = "Chisq")

summary(model_ctmax)
plot(allEffects(model_ctmax), multiline = T, confint = list(style = 'auto'), types = "response") #right


vif(model_ctmax, type = "terms") #all good
outlierTest(model_ctmax) #all good

emm <- emmeans(model_ctmax, ~ temp_f * zinc_f)
plot(emm, comparison = TRUE)

re <- ranef(model_ctmax)$group
re_df <- data.frame(group = rownames(re), intercept = re[,1])
ggplot(re_df, aes(x = group, y = intercept)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Group", y = "Random intercept deviation") +
  theme_classic() #random effects plot

emm_df <- as.data.frame(emm)
ggplot(emm_df, aes(x = temp_f, y = emmean, fill = zinc_f)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(0.9), width = 0.2) +
  labs(x = "Temperature", y = "Predicted CTmax (°C)", fill = "ZnO treatment") +
  theme_classic()

#Visualisation
(chronic_boxplot2 <- ggplot(chronic, aes(x=temp_f, y=ctmax, fill=zinc_f)) +
  geom_boxplot(position=position_dodge()) +
  scale_fill_brewer(palette = "Paired", direction = 1, name = "nZnO treatment", 
                    labels = c(expression("No nZnO"), bquote(EC[50*","*48]*" nZnO"))) +
  labs(x="Temperature", y="CTmax (°C)", fill="ZnO") +
  theme_classic())
ggsave("Report/chronic_boxplot2.png", chronic_boxplot2, units = "cm", width = 20, height = 15)


#Haemoglobin
boxplot(hemo ~ treatment, data = chronic)

shapiro.test(chronic$hemo) #borderline normal
hist(chronic$hemo) #looks slightly skewed to the right
bartlett.test(hemo ~ treatment, chronic) #all good

hemo_model <- lm(hemo ~ zinc_f * temp_f, data = chronic)
hemo_model2 <- lmer(hemo ~ zinc_f * temp_f + (1|group), data = chronic)

AICc(hemo_model, hemo_model2) #model2 is much better
anova(hemo_model2, hemo_model, method = "Chisq")

summary(hemo_model2)
plot(allEffects(hemo_model2), multiline = T, confint = list(style = 'auto'), types = "response") #right

#Visualisation
(chronic_boxplot3 <- ggplot(chronic, aes(x = temp_f, y = hemo, fill = zinc_f)) +
  geom_boxplot(position = position_dodge()) +
  scale_fill_brewer(palette = "Paired", direction = 1, name = "nZnO treatment", 
                    labels = c(expression("No nZnO"), bquote(EC[50*","*48]*" nZnO"))) +
  labs(x = "Acclimated temperature", y = "Normalised haemoglobin content", fill = "nZnO treatment") +
  theme_classic())
ggsave("Report/chronic_boxplot3.png", chronic_boxplot3, units = "cm", width = 20, height = 15)


#CTmax + hemoglobin
model <- lm(ctmax ~ hemo, data = chronic)
summary(model)

model2 <- lmer(ctmax ~ hemo + (1|group), data = chronic)
summary(model2)
AICc(model, model2)
anova(model2, model)

cor.test(chronic$hemo, chronic$ctmax)

ctmax_hemo_lm_add <- lm(ctmax ~ hemo + temp_f + zinc_f, data = chronic)
ctmax_hemo_lm_int <- lm(ctmax ~ hemo * temp_f * zinc_f, data = chronic)
model_ctmax_hemo_add <- lmer(ctmax ~ hemo + temp_f + zinc_f + (1 | group), data = chronic, REML = FALSE)
model_ctmax_hemo_int <- lmer(ctmax ~ hemo * temp_f * zinc_f + (1 | group), data = chronic, REML = FALSE)

AICc(ctmax_hemo_lm_add, ctmax_hemo_lm_int, model_ctmax_hemo_add, model_ctmax_hemo_int) #mixed full model is better
anova(model_ctmax_hemo_add, model_ctmax_hemo_int, ctmax_hemo_lm_add, ctmax_hemo_lm_int)
anova(model_ctmax_hemo_add, model_ctmax_hemo_int)


summary(model_ctmax_hemo_add) #significant effect of temperature, no effect of hemo
plot(allEffects(model_ctmax_hemo_add), multiline = T, confint = list(style = 'auto'), types = "response") #right

vif(model_ctmax_hemo_add) #all good
outlierTest(model_ctmax_hemo_add) #all good

emm <- emmeans(model_ctmax_hemo_add, ~ hemo)
plot(emm, comparison = TRUE)

#Visualisation
(haem_plot <- ggplot(chronic, aes(x = hemo, y = ctmax)) +
    geom_point(col = "lightblue4") +
    stat_smooth(method = "lm", se = TRUE, color = "black") +
    labs(x = "Normalised haemoglobin Content", y = "CTmax (°C)") +
    theme_classic())
ggsave("Report/haem_plot.png", haem_plot, units = "cm", width = 20, height = 15)

(treatment_ctmax <- ggplot(chronic, aes(x = hemo, y = ctmax, color = treatment)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_brewer(palette = "Paired", direction = 1, name = "Treatment") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Normalised Hemoglobin Content", y = "CTmax (°C)", color = "Treatment") +
  theme_classic())
ggsave("Report/treatment_plot.png", treatment_ctmax, units = "cm", width = 20, height = 15)
