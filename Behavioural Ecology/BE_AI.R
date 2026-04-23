#Crickets group 1 - Behavioural Ecology 2026

#WD
setwd("~/Desktop/KU Leuven/Behavioural Ecology")
getwd()

#Packages ----
library(tidyverse)
library(patchwork)    
library(ggbeeswarm)
library(ggpubr) 
library(gridExtra)

#Data & wrangling ----
crickets <- read.csv("cricket_autotomy_raw_dataset_v2.csv",
                     na = c("", "NA", "NaN"))

head(crickets)
str(crickets)

crickets_long <- read.csv("cricket_autotomy_long.csv",
                           na = c("", "NA", "NaN")) #changed to long format
head(crickets_long)

crickets_long <- crickets_long %>% rename(ID = individual_ID,
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
                                          shelter_latency = PCRT_shelter_seeking_latency_s) #renaming for ease

crickets_long <- crickets_long %>%
  mutate(ID = factor(ID),
         sex = factor(sex),
         regime = factor(regime, levels = c("sham", "autotomy")),
         personality = factor(personality, levels = c("Shy", "Intermediate", "Bold")),
         d_boldness = PC1_post - PC1_pre) #d<0 = individual became shyer

head(crickets_long)

crickets_analysis <- crickets %>%
  mutate(
    individual_ID = factor(individual_ID),
    sex = factor(sex),              
    autotomy_regime = factor(autotomy_regime,
                                 levels = c("sham", "autotomy")),
    personality_category = factor(personality_category,
                                  levels = c("Shy", "Intermediate", "Bold")),
    autotomy_occurred = as.factor(autotomy_occurred),  #1 = autotomised
    autotomy_censored = as.factor(autotomy_censored),  #1 = censored (no autotomy)
    delta_boldness = personality_PC1_post - personality_PC1_pre, #gives boldness value (H2)
    surv_status = case_when( #1 = autotomy, 0 = censored (only for autotomised individuals)
      autotomy_regime == "autotomy" & autotomy_occurred == 1 ~ 1,
      autotomy_regime == "autotomy" & autotomy_occurred == 0 ~ 0,
      TRUE ~ NA_integer_
    )
  )

#Colours + palettes ----
pal_personality <- c(Shy = "yellow2",
                     Intermediate = "orange",
                     Bold = "red3")

pal_regime <- c(sham = "lightblue3",
                autotomy = "red")

pal_timepoint <- c(pre = "#888780",
                   post = "#C04828")

theme_cricket <- function() {
  theme_classic() +
    theme(
      axis.title = element_text(size = 10),
      legend.position = c(0.1, 0.1),
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text = element_text(face = "bold", size = 9)
    )
}
#Final plots ----
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
   mutate(metric = recode(metric,
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

ggsave("fig1_personality_profiles.png", p1, width = 10, height = 5, dpi = 300)
message("✓ Figure 1 saved")

(p1 <- crickets_long %>%
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
    mutate(metric = recode(metric,
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


(p2 <- crickets_long %>%
   filter(timepoint %in% c("pre","post")) %>%
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
   group_by(personality, regime, timepoint, metric) %>%
   summarise(mean_z = mean(z_score, na.rm = TRUE),
             se_z   = sd(z_score, na.rm = TRUE) / sqrt(n()),
             .groups = "drop") %>%
   mutate(
     metric = dplyr::recode(metric,
                     FM_latency        = "OFT: Latency\nfirst move",
                     total_distance    = "OFT: Total\ndistance",
                     time_central      = "OFT: Central\nzone time",
                     freezing_n        = "OFT: Freezing\nbouts",
                     resume_latency    = "PCRT: Latency\nresume",
                     total_freeze_time = "PCRT: Freeze\nduration",
                     shelter_latency   = "PCRT: Shelter\nlatency"),
     timepoint = factor(timepoint, levels = c("pre","post"))
   ) %>%
   ggplot(aes(x = metric, y = mean_z,
              fill = personality, colour = personality,
              alpha = timepoint)) +
   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
   geom_col(position = position_dodge(0.75), width = 0.65) +
   geom_errorbar(aes(ymin = mean_z - se_z, ymax = mean_z + se_z),
                 position = position_dodge(0.75), width = 0.25, linewidth = 0.6) +
   scale_fill_manual(values = pal_personality, name = "Personality") +
   scale_colour_manual(values = pal_personality, name = "Personality") +
   scale_alpha_manual(values = c(pre = 0.9, post = 0.4), name = "Timepoint") +
   facet_wrap(~ regime, labeller = labeller(regime = c(sham = "Sham", autotomy = "Autotomy"))) +
   labs(x = NULL, y = "Mean z-score") +
   theme_cricket() +
   theme(axis.text.x = element_text(size = 7))) #both together

ggsave("fig2_pre_post_profiles.png", p2, width = 13, height = 5.5, dpi = 300)
message("✓ Figure 2 saved")


#Latency to autotomise by personality
(p3 <- filter(crickets, autotomy_regime == "autotomy") %>%
   mutate(
     personality_category = factor(personality_category, levels = c("Shy","Intermediate","Bold")),
     outcome = if_else(autotomy_latency_s >= 30, "Censored", "Autotomised")
   ) %>%
   ggplot(aes(x = personality_category, y = autotomy_latency_s,
              fill = personality_category, shape = outcome)) +
   geom_boxplot() +
   geom_jitter(width = 0.15, size = 2.5, alpha = 0.8) +
   geom_hline(yintercept = 30, linetype = "dashed", colour = "red3") +
   annotate("text", x = 0.7, y = 30.8, label = "Censor (30 s)",
            colour = "red3", size = 3) +
   scale_fill_manual(values = pal_personality, guide = "none") +
   scale_shape_manual(values = c(Autotomised = 16, Censored = 2), name = "Outcome") +
   labs(x = "", y = "Latency to autotomise (s)") +
   theme_cricket() +
   theme(legend.position = c(0.1, 0.1)))

ggsave("fig3_autotomy_latency.png", p3, width = 7, height = 5.5, dpi = 300)
message("✓ Figure 3 saved")


#Survival analysis plot by personality
surv_data <- crickets_analysis %>%
  filter(autotomy_regime == "autotomy",
         !is.na(autotomy_latency_s), !is.na(surv_status))

surv_obj <- Surv(time   = surv_data$autotomy_latency_s,
                 event  = surv_data$surv_status)

km_fit <- survfit(surv_obj ~ personality_category, data = surv_data)
(p4 <- ggsurvplot(
  km_fit,
  data         = surv_data,
  palette      = unname(pal_personality[levels(surv_data$personality_category)]),
  conf.int     = TRUE,
  pval         = TRUE,
  risk.table   = FALSE,
  legend = c(0.1, 0.1), 
  legend.labs  = levels(surv_data$personality_category),
  legend.title = "Personality",
  xlab         = "Time (s)",
  ylab         = "Probability of NOT autotomising",
  ggtheme      = theme_cricket()))

# Save via ggsurvplot's own method
png("fig4_survival_curves.png", width = 2400, height = 1800, res = 300)
print(p4)
dev.off()
message("✓ Figure 4 saved")


#Pre- boldness vs latency to autotomise
(p5 <- crickets_analysis %>%
   filter(autotomy_regime == "autotomy") %>%
   mutate(outcome = if_else(autotomy_occurred, "Autotomised", "Censored")) %>%
   ggplot(aes(x = personality_PC1_pre, y = autotomy_latency_s,
              colour = personality_category)) +
   geom_point(aes(shape = outcome), alpha = 0.8, size = 2.8) +
   geom_smooth(data = . %>% filter(autotomy_occurred),
               method = "lm", se = TRUE, linewidth = 1,
               aes(fill = personality_category), alpha = 0.15) +
   geom_hline(yintercept = 30, linetype = "dashed", colour = "grey50") +
   scale_colour_manual(values = pal_personality, name = "Personality") +
   scale_fill_manual(values = pal_personality, guide = "none") +
   scale_shape_manual(values = c(Autotomised = 16, Censored = 2),
                      name = "Outcome") +
   labs(x = "Pre-trial boldness (PC1)", y = "Latency to autotomise (s)") +
   theme_cricket() +
   theme(legend.position = c(0.1, 0.2)))

ggsave("fig5_boldness_vs_latency.png", p5, width = 8, height = 5.5, dpi = 300)
message("✓ Figure 5 saved")


#Change in boldness by personality and treatment
delta_data <- crickets_analysis %>%
  select(individual_ID, personality_category, autotomy_regime, delta_boldness) %>%
  drop_na(delta_boldness)

(p6 <- ggplot(delta_data,
              aes(x = personality_category, y = delta_boldness,
                  fill = autotomy_regime, colour = autotomy_regime)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_boxplot(alpha = 0.6, position = position_dodge(0.8), width = 0.7,
                 outlier.shape = NA, colour = "grey30") +
    geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
               alpha = 0.6, size = 1.8) +
    scale_fill_manual(values = pal_regime,
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name = "Treatment") +
    scale_colour_manual(values = pal_regime,
                        labels = c(sham = "Sham", autotomy = "Autotomy"),
                        name = "Treatment") +
    labs(x = "", y = "Δ Boldness (PC1 post − pre)") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.1)))

ggsave("fig6_delta_boldness.png", p6, width = 8, height = 5.5, dpi = 300)
message("✓ Figure 6 saved")


#Pre vs post treatment PC1
(p7 <- crickets_analysis %>%
   select(individual_ID, personality_category, autotomy_regime,
          personality_PC1_pre, personality_PC1_post) %>%
   drop_na() %>%
   ggplot(aes(x = personality_PC1_pre, y = personality_PC1_post,
              colour = personality_category)) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
   geom_segment(aes(xend = personality_PC1_pre, yend = personality_PC1_pre),
                colour = "grey80", linewidth = 0.3) +
   geom_point(size = 2.5, alpha = 0.8) +
   facet_wrap(~ autotomy_regime,
              labeller = labeller(autotomy_regime = c(sham = "Sham", autotomy = "Autotomy"))) +
   scale_colour_manual(values = pal_personality, name = "Personality") +
   labs(x = "Pre-trial boldness (PC1)", y = "Post-trial boldness (PC1)") +
   theme_cricket() +
   theme(legend.position = c(0.1, 0.1)))

ggsave("fig7_pc1_scatter.png", p7, width = 9, height = 5, dpi = 300)
message("✓ Figure 7 saved")


#Body mass vs latency to autotomise (confounding variable)
(p8 <- crickets_analysis %>%
  filter(autotomy_regime == "autotomy") %>%
  mutate(outcome = if_else(autotomy_occurred, "Autotomised", "Censored")) %>%
  ggplot(aes(x = body_mass_g, y = autotomy_latency_s,
             colour = personality_category, shape = outcome)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8,
              aes(group = 1), colour = "grey30") +
  geom_hline(yintercept = 30, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  scale_shape_manual(values = c(Autotomised = 16, Censored = 2),
                     name = "Outcome") +
  labs(title = "Figure 8 — Body mass vs autotomy latency (confound check)",
       subtitle = "Overall regression line in grey. Use as covariate in LMM if correlated.",
       x = "Body mass (g)", y = "Latency to autotomise (s)") +
  theme_cricket())

ggsave("fig8_mass_confound.png", p8, width = 7, height = 5, dpi = 300)
message("✓ Figure 8 saved")

#Sex vs latency to autotomise
(p8 <- crickets_analysis %>%
    filter(autotomy_regime == "autotomy") %>%
    mutate(outcome = if_else(autotomy_occurred, "Autotomised", "Censored")) %>%
    ggplot(aes(x = sex, y = autotomy_latency_s,
               colour = personality_category, shape = outcome)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
                aes(group = 1), colour = "grey30") +
    geom_hline(yintercept = 30, linetype = "dashed", colour = "grey50") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_shape_manual(values = c(Autotomised = 16, Censored = 2),
                       name = "Outcome") +
    labs(x = "Sex", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.2)))
ggsave("Plots/sex_confound.png", p8, width = 7, height = 5, dpi = 300)


#Samples
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
   labs(subtitle = "Sample sizes per cell",
        x = "Personality", y = "n") +
   theme_cricket() +
   theme(legend.position = "right"))

(p9b <- crickets_analysis %>%
  count(personality_category, sex) %>%
  ggplot(aes(x = personality_category, y = n, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
  geom_text(aes(label = n),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c(M = "steelblue3", F = "salmon3"), name = "Sex") +
  labs(subtitle = "Sex distribution per personality",
       x = "Personality", y = "n") +
  theme_cricket() +
  theme(legend.position = "right"))

p9 <- (p9a | p9b) +
  plot_annotation(theme = theme(plot.title = element_text(face = "bold", size = 12)))

ggsave("Plots/fig9_sample_composition.png", p9, width = 11, height = 4.5, dpi = 300)
message("✓ Figure 9 saved")



(p_pc1 <- crickets %>%
    mutate(autotomy_regime = factor(autotomy_regime, levels = c("sham","autotomy"))) %>%
    select(individual_ID, autotomy_regime,
           personality_PC1_pre, personality_PC1_post) %>%
    pivot_longer(cols = c(personality_PC1_pre, personality_PC1_post),
                 names_to = "timepoint", values_to = "PC1") %>%
    mutate(timepoint = factor(if_else(timepoint == "personality_PC1_pre", "pre", "post"),
                              levels = c("pre","post"))) %>%
    group_by(autotomy_regime, timepoint) %>%
    summarise(mean_PC1 = mean(PC1, na.rm = TRUE),
              se_PC1   = sd(PC1, na.rm = TRUE) / sqrt(n()),
              .groups  = "drop") %>%
    ggplot(aes(x = timepoint, y = mean_PC1,
               colour = autotomy_regime, group = autotomy_regime)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_PC1 - se_PC1,
                      ymax = mean_PC1 + se_PC1),
                  width = 0.08, linewidth = 0.8) +
    scale_colour_manual(values = c(sham = "cadetblue3", autotomy = "salmon"),
                        name = "Treatment") +
    labs(title = "Boldness (PC1) Change Pre→Post",
         x = "Time", y = "Estimated Boldness (PC1)") +
    theme_cricket()) #checking w wies 

#All Plots ----
#Distributions (1): 
(p1a <- crickets_long %>%
  filter(timepoint == "pre", trial == 1) %>%   
  ggplot(aes(x = PC1_pre, fill = personality)) +
  geom_histogram(bins = 20, colour = "white", linewidth = 0.3, alpha = 0.85) +
  scale_fill_manual(values = pal_personality, name = "Personality") +
  labs(x = "Boldness PC1 score", y = "Count") +
  theme_cricket())
ggsave("histogramPC1.png", p1a,
       width = 10, height = 10, dpi = 300, bg = "white")

(p1b <- crickets_long %>%
  filter(trial == 1) %>%
  pivot_longer(cols = c(PC1_pre, PC1_post),
               names_to  = "time",
               values_to = "PC1") %>%
  mutate(time = recode(time, PC1_pre = "pre", PC1_post = "post"),
         time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(x = PC1, fill = time, colour = time)) +
  geom_density(alpha = 0.45, linewidth = 0.8) +
  scale_fill_manual(values   = pal_timepoint, name = "Timepoint") +
  scale_colour_manual(values = pal_timepoint, name = "Timepoint") +
  labs(x = "Boldness PC1 score", y = "Density") +
  theme_cricket())
ggsave("histogramPC1_2.png", p1b,
       width = 10, height = 10, dpi = 300, bg = "white")

(p1c <- crickets_long %>%
  filter(trial == 1) %>%
  pivot_longer(cols = c(PC1_pre, PC1_post),
               names_to  = "time",
               values_to = "PC1") %>%
  mutate(time = recode(time, PC1_pre = "pre", PC1_post = "post"),
         time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(x = PC1, fill = time, colour = time)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  facet_wrap(~ regime, labeller = labeller(regime = c(sham = "Sham", autotomy = "Autotomy"))) +
  scale_fill_manual(values   = pal_timepoint, name = "Timepoint") +
  scale_colour_manual(values = pal_timepoint, name = "Timepoint") +
  labs(x = "Boldness PC1", y = "Density") +
  theme_cricket())
ggsave("histogramPC1_3.png", p1c,
       width = 10, height = 10, dpi = 300, bg = "white")

(pc1_grid <- p1a / p1b /p1c +
    plot_annotation(
      theme = theme(plot.title = element_text(face = "bold", size = 14),
                       plot.subtitle = element_text(size = 10, colour = "grey40"))))
ggsave("PC1grid.png", pc1_grid,
       width = 10, height = 15, dpi = 300, bg = "white")

#Boxplots (2)
(p2a <- crickets_long %>%
  filter(trial == 1, timepoint == "pre") %>%
  ggplot(aes(x = regime, y = d_boldness, fill = regime)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(width = 0.5, outlier.shape = 21, outlier.size = 2,
               alpha = 0.8, colour = "grey30") +
  scale_fill_manual(values = pal_regime, name = "Regime") +
  scale_x_discrete(labels = c(sham = "Sham", autotomy = "Autotomy")) +
  labs(x = "", y = "Δ Boldness (PC1 post − pre)") +
  theme_cricket() +
  theme(legend.position = c(0.1, 0.1)))
ggsave("p2a.png", p2a,
       width = 8, height = 8, dpi = 300, bg = "white")


(p2b <- crickets_long %>%
  filter(trial == 1, timepoint == "pre") %>%
  ggplot(aes(x = personality, y = d_boldness, fill = regime)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = pal_regime,
                    labels = c(sham = "Sham", autotomy = "Autotomy"),
                    name   = "Regime") +
  labs(x = "", y = "Δ Boldness (post − pre)") +
  theme_cricket() +
  theme(legend.position = c(0.1, 0.1)))

ggsave("p2b.png", p2b,
       width = 8, height = 5, dpi = 300, bg = "white")


(p2c <- crickets_long %>%
  filter(timepoint == "pre") %>%
  ggplot(aes(x = personality, y = total_distance,
             fill = personality, colour = personality)) +
  geom_boxplot(side = "r", alpha = 0.5, trim = FALSE) +
  geom_jitter(width = 0.05, alpha = 0.3, size = 1.2) +
  scale_fill_manual(values   = pal_personality) +
  scale_colour_manual(values = pal_personality) +
  labs(x = "Personality category", y = "Total distance moved (cm)") +
  theme_cricket() +
  theme(legend.position = "none"))


p2_grid <- grid.arrange(p2a, p2b, p2c, nrow = 1)
ggsave("boldness_grid.png", p2_grid,
       width = 20, height = 10, dpi = 300, bg = "white")

#Scatterplots (3)
(p3a <- crickets_long %>%
  filter(trial == 1, timepoint == "pre") %>%
  ggplot(aes(x = PC1_pre, y = d_boldness,
             colour = regime, fill = regime)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.2) +
  scale_colour_manual(values = pal_regime,
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name = "Regime") +
  scale_fill_manual(values = pal_regime,
                    labels = c(sham = "Sham", autotomy = "Autotomy"),
                    name = "Regime") +
  labs(x = "Pre-trial boldness (PC1)", y = "Δ Boldness (post − pre)") +
  theme_cricket() +
  theme(legend.position = c(0.1, 0.1)))

ggsave("change_in_boldness.png", p3a,
       width = 8, height = 8, dpi = 300, bg = "white")


(p3b <- crickets_long %>%
  filter(trial == 1, timepoint == "pre") %>%
  ggplot(aes(x = PC1_pre, y = d_boldness,
             colour = regime, fill = regime)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.65, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1) +
  facet_wrap(~ personality) +
  scale_colour_manual(values = pal_regime,
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name = "Regime") +
  scale_fill_manual(values = pal_regime,
                    labels = c(sham = "Sham", autotomy = "Autotomy"),
                    name = "Regime") +
  labs(x = "Pre-trial boldness (PC1)", y = "Δ Boldness") +
  theme_cricket())

(p3c <- crickets_long %>%
  filter(trial == 1) %>%
  ggplot(aes(x = PC1_pre, y = d_boldness, 
             colour = personality, linetype = regime)) +
  geom_point(aes(shape = regime), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1) +
  facet_wrap(~ personality, scales = "fixed") +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  scale_linetype_manual(values = c("sham" = "dashed", "autotomy" = "solid"),
                        name = "Regime") +
  scale_shape_manual(values = c("sham" = 1, "autotomy" = 16),
                     name = "Regime") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(x = "Pre-trial boldness (PC1)", y = "Δ Boldness") +
  theme_cricket() +
  theme(legend.position = c(0.1, 0.2)))


ggsave("boldness_change_RP.png", p3c,
       width = 8, height = 5, dpi = 300, bg = "white")




(p3d <- crickets_long %>%
    filter(trial == 1, timepoint == "pre") %>%
    ggplot(aes(x = mass, y = PC1_pre, colour = personality)) +
    geom_point(alpha = 0.7, size = 2.5) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    labs(x = "Body mass (g)", y = "Pre-trial boldness (PC1)") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.1)))


ggsave("boldnes_P_pre.png", p3d,
       width = 8, height = 8, dpi = 300, bg = "white")

p3_grid <- grid.arrange(p3a, p3c, nrow = 1)
ggsave("boldness_grid2.png", p3_grid,
       width = 20, height = 10, dpi = 300, bg = "white")


#Bar charts (4)
(p4a <- crickets_long %>%
    filter(trial == 1,
           timepoint == "pre",
           regime == "autotomy",
           !is.na(a_occured)) %>%
    mutate(autotomised_num = as.integer(a_occured == "True")) %>%
    group_by(personality) %>%
    summarise(
      n = n(),
      rate = mean(autotomised_num, na.rm = TRUE),
      se = sqrt(rate * (1 - rate) / n),
      ci_lo = pmax(0, rate - 1.96 * se),
      ci_hi = pmin(1, rate + 1.96 * se),
      .groups = "drop") %>%
    ggplot(aes(x = personality, y = rate, fill = personality)) +
    geom_col(width = 0.55, colour = "white", alpha = 0.9) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                  width = 0.18, linewidth = 0.8) +
    scale_fill_manual(values = pal_personality) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1)) +
    labs(x = "Personality category", y = "Proportion autotomised") +
    theme_cricket() +
    theme(legend.position = "none"))

(p4b <- crickets_long %>%
  filter(trial == 1, timepoint == "pre") %>%
  group_by(regime, personality) %>%
  summarise(
    n = n(),
    mean_d = mean(d_boldness, na.rm = TRUE),
    se_d = sd(d_boldness,   na.rm = TRUE) / sqrt(n),
    .groups = "drop") %>%
  ggplot(aes(x = personality, y = mean_d,
             fill = regime, colour = regime)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_col(position = position_dodge(0.7), width = 0.6,
           alpha = 0.85, colour = "white") +
  geom_errorbar(aes(ymin = mean_d - se_d, ymax = mean_d + se_d),
                position = position_dodge(0.7), width = 0.2,
                linewidth = 0.8) +
  scale_fill_manual(values = pal_regime,
                    labels = c(sham = "Sham", autotomy = "Autotomy"),
                    name = "Regime") +
  scale_colour_manual(values = pal_regime,
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name   = "Regime") +
  labs(x = "Personality category", y = "Mean Δ Boldness") +
  theme_cricket())

p4_grid <- grid.arrange(p4a, p4b, nrow = 1)
ggsave("autotomy_gridx.png", p4_grid,
       width = 20, height = 10, dpi = 300, bg = "white")


#Scatters (5)
(p5a <- crickets_long %>%
  filter(trial == 1) %>%
  select(ID, regime, personality, PC1_pre, PC1_post) %>%
  distinct() %>%
  pivot_longer(cols = c(PC1_pre, PC1_post),
               names_to  = "time",
               values_to = "PC1") %>%
  mutate(time = factor(dplyr::recode(time, PC1_pre = "Pre", PC1_post = "Post"),
                       levels = c("Pre", "Post"))) %>%
  ggplot(aes(x = time, y = PC1, group = ID, colour = regime)) +
  geom_line(alpha = 0.35, linewidth = 0.6) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_summary(aes(group = regime), fun = mean,
               geom = "line", linewidth = 2, alpha = 0.9) +
  stat_summary(aes(group = regime), fun = mean,
               geom = "point", size = 4, alpha = 0.9) +
  scale_colour_manual(values = pal_regime,
                      labels = c(sham = "Sham", autotomy = "Autotomy"),
                      name   = "Regime") +
  labs(x = "Timepoint", y = "Boldness PC1") +
  theme_cricket())

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

p5_grid <- grid.arrange(p5a, p5b, nrow = 1)
ggsave("boldness_grid3.png", p5_grid,
       width = 20, height = 10, dpi = 300, bg = "white")



#Repeatablity (??; 6)
(p6a <- crickets_long %>%
  filter(timepoint == "pre") %>%
  select(ID, personality, trial, total_distance) %>%
  pivot_wider(names_from = trial, values_from = total_distance,
              names_prefix = "T") %>%
  ggplot(aes(x = T1, y = T2, colour = personality)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey30",
              linewidth = 1, inherit.aes = FALSE,
              aes(x = T1, y = T2)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  labs(x = "Trial 1 — Total distance (cm)", y = "Trial 2 — Total distance (cm)") +
  theme_cricket())

trial_pairs <- crickets_long %>%
  filter(timepoint == "pre") %>%
  select(ID, personality, trial, total_distance) %>%
  pivot_wider(names_from = trial, values_from = total_distance,
              names_prefix = "T")

(p6b <- bind_rows(
  trial_pairs %>% mutate(pair = "T1 vs T2", x = T1, y = T2),
  trial_pairs %>% mutate(pair = "T1 vs T3", x = T1, y = T3),
  trial_pairs %>% mutate(pair = "T2 vs T3", x = T2, y = T3)) %>%
  ggplot(aes(x = x, y = y, colour = personality)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey30",
              linewidth = 0.9, inherit.aes = FALSE, aes(x = x, y = y)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60") +
  facet_wrap(~ pair) +
  scale_colour_manual(values = pal_personality, name = "Personality") +
  labs(x = "Distance (cm) — Trial A", y = "Distance (cm) — Trial B") +
  theme_cricket())

p6_grid <- grid.arrange(p6a, p6b, nrow = 1)
ggsave("distance_grid.png", p6_grid,
       width = 20, height = 10, dpi = 300, bg = "white")

#Behaviour profiles (7)
(p7 <- crickets_long %>%
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
  mutate(metric = recode(metric,
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

ggsave("z-score_personality.png", p7,
       width = 10, height = 7, dpi = 300, bg = "white")

(p8.1 <- crickets_long %>%
    filter(timepoint == "post") %>%
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
    mutate(metric = recode(metric,
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

#latency to autotomise (8)
crickets_long <- crickets_long %>%
  mutate(
    a_occured = case_when(
      regime == "autotomy" & a_latency < 30  ~ "Autotomised",
      regime == "autotomy" & a_latency == 30 ~ "Censored",
      TRUE ~ NA_character_
    ),
    a_occured = factor(a_occured, levels = c("Autotomised", "Censored"))
  )

(p8a <- crickets_long %>%
    filter(trial == 1,
           timepoint == "pre",
           regime == "autotomy",
           !is.na(a_latency)) %>%
    ggplot(aes(x = personality, y = a_latency, colour = personality)) +
    geom_boxplot(data = . %>% filter(a_occured == "Autotomised"),
                 width = 0.35, outlier.shape = NA,
                 colour = "grey40", fill = "white", alpha = 0.6) +
    geom_jitter(aes(shape = a_occured),
                width = 0.12, size = 3.5, alpha = 0.85) +
    geom_hline(yintercept = 30, linetype = "dashed",
               colour = "#C04828", linewidth = 0.6) +
    annotate("text", x = 0.5, y = 29.2,
             label = "Censor (30 s)", hjust = 0,
             size = 3, colour = "#C04828") +
    scale_colour_manual(values = pal_personality, guide = "none") +
    scale_shape_manual(values = c("Autotomised" = 16, "Censored" = 2),
                       name   = "Outcome") +
    labs(x = "", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.1)))

ggsave("autotomy_latency_pre.png", p8a,
       width = 8, height = 8, dpi = 300, bg = "white")

(p8b <- crickets_long %>%
    filter(trial == 1,
           timepoint == "post",
           regime == "autotomy",
           !is.na(a_latency)) %>%
    ggplot(aes(x = personality, y = a_latency, colour = personality)) +
    geom_boxplot(data = . %>% filter(a_occured == "Autotomised"),
                 width = 0.35, outlier.shape = NA,
                 colour = "grey40", fill = "white", alpha = 0.6) +
    geom_jitter(aes(shape = a_occured),
                width = 0.12, size = 3.5, alpha = 0.85) +
    geom_hline(yintercept = 30, linetype = "dashed",
               colour = "#C04828", linewidth = 0.6) +
    annotate("text", x = 0.5, y = 29.2,
             label = "Censor (30 s)", hjust = 0,
             size = 3, colour = "#C04828") +
    scale_colour_manual(values = pal_personality, guide = "none") +
    scale_shape_manual(values = c("Autotomised" = 16, "Censored" = 2),
                       name   = "Outcome") +
    labs(x = "", y = "Latency to autotomise (s)") +
    theme_cricket() +
    theme(legend.position = c(0.1, 0.1)))

ggsave("autotomy_latency_pre.png", p8a,
       width = 8, height = 8, dpi = 300, bg = "white")


(p8b <- crickets_long %>%
    filter(trial == 1,
           timepoint == "pre",
           regime    == "autotomy",
           !is.na(a_latency)) %>%
    ggplot(aes(x = PC1_pre,
               y = a_latency,
               colour = personality,
               shape = a_occured)) +
    geom_point(size = 3, alpha = 0.85) +
    geom_smooth(
      data = crickets_long %>%
        filter(trial     == 1,
               timepoint == "pre",
               regime    == "autotomy",
               a_occured == "Autotomised",
               !is.na(a_latency)),
      aes(x = PC1_pre, y = a_latency),
      method    = "lm",
      se        = TRUE,
      colour    = "grey30",
      fill      = "grey80",
      linewidth = 1,
      inherit.aes = FALSE) +
    geom_hline(yintercept = 30, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    annotate("text", x = -3, y = 28.5,
             label = "Censor (30 s)", hjust = 0,
             size = 3, colour = "grey50") +
    scale_colour_manual(values = pal_personality, name = "Personality") +
    scale_shape_manual(values = c("Autotomised" = 16, "Censored" = 2),
                       name   = "Outcome") +
    labs(x = "Pre-trial boldness (PC1)",
         y = "Latency to autotomy (s)") +
    theme(legend.position = "topleft") +
    theme_cricket())

p8_grid <- grid.arrange(p8a, p8b, nrow = 1)
ggsave("autotomy_latency_grid.png", p8_grid,
       width = 14, height = 6, dpi = 300, bg = "white")


#PCA loadings plots - what drives the behaviour? 
pca_input <- crickets %>%
  transmute(
    OFT_latency_fm  = (OFT_pre_T1_latency_first_move_s +
                         OFT_pre_T2_latency_first_move_s +
                         OFT_pre_T3_latency_first_move_s) / 3,
    OFT_distance    = (OFT_pre_T1_total_distance_cm +
                         OFT_pre_T2_total_distance_cm +
                         OFT_pre_T3_total_distance_cm) / 3,
    OFT_centre_time = (OFT_pre_T1_time_central_zone_s +
                         OFT_pre_T2_time_central_zone_s +
                         OFT_pre_T3_time_central_zone_s) / 3,
    OFT_freezing    = (OFT_pre_T1_freezing_bouts_n +
                         OFT_pre_T2_freezing_bouts_n +
                         OFT_pre_T3_freezing_bouts_n) / 3,
    PCRT_resume     = (PCRT_pre_T1_latency_resume_s +
                         PCRT_pre_T2_latency_resume_s +
                         PCRT_pre_T3_latency_resume_s) / 3,
    PCRT_freeze     = (PCRT_pre_T1_total_freeze_duration_s +
                         PCRT_pre_T2_total_freeze_duration_s +
                         PCRT_pre_T3_total_freeze_duration_s) / 3,
    PCRT_shelter    = (PCRT_pre_T1_shelter_seeking_latency_s +
                         PCRT_pre_T2_shelter_seeking_latency_s +
                         PCRT_pre_T3_shelter_seeking_latency_s) / 3
  )

pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)

if (pca_result$rotation["OFT_distance", 1] < 0) {
  pca_result$rotation[, 1] <- -pca_result$rotation[, 1]
} #bold = positive PC value

loadings_df <- data.frame(
  metric   = rownames(pca_result$rotation),
  loading  = pca_result$rotation[, 1],
  var_exp  = round(summary(pca_result)$importance[2, 1] * 100, 1)
) %>%
  mutate(
    direction = ifelse(loading > 0, "Bold", "Shy"),
    label = recode(metric,
                   OFT_latency_fm = "OFT: Latency to\nfirst move",
                   OFT_distance = "OFT: Total\ndistance",
                   OFT_centre_time = "OFT: Central\nzone time",
                   OFT_freezing = "OFT: Freezing\nbouts",
                   PCRT_resume = "PCRT: Latency\nto resume",
                   PCRT_freeze = "PCRT: Freeze\nduration",
                   PCRT_shelter = "PCRT: Shelter-seeking\nlatency"),
    label = fct_reorder(label, abs(loading)))

#PC plots
(p_loadings <- ggplot(loadings_df,
                     aes(x = loading, y = label, fill = direction)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.6) +
  geom_col(width = 0.65, colour = "white", alpha = 0.9) +
  geom_text(aes(label = round(loading, 3),
                hjust = ifelse(loading > 0, -0.15, 1.15)),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(Bold = "red", Shy = "lightblue3"),
                    name   = "Drives:") +
  scale_x_continuous(limits = c(-0.55, 0.55),
                     breaks = seq(-0.5, 0.5, 0.25)) +
  labs(x = "PC1 loading", y = NULL) +
  theme_cricket() +
  theme(legend.position = "right",
        panel.grid.major.x = element_line(colour = "grey92")))

metric_means <- crickets_long %>%
  group_by(ID, regime, personality, timepoint) %>%
  summarise(
    OFT_latency_fm  = mean(FM_latency,         na.rm = TRUE),
    OFT_distance    = mean(total_distance,      na.rm = TRUE),
    OFT_centre_time = mean(time_central,        na.rm = TRUE),
    OFT_freezing    = mean(freezing_n,          na.rm = TRUE),
    PCRT_resume     = mean(resume_latency,      na.rm = TRUE),
    PCRT_freeze     = mean(total_freeze_time,   na.rm = TRUE),
    PCRT_shelter    = mean(shelter_latency,     na.rm = TRUE),
    .groups = "drop") %>%
  group_by(ID) %>%
  mutate(across(OFT_latency_fm:PCRT_shelter, scale)) %>%
  ungroup()

metric_long <- metric_means %>%
  pivot_longer(cols      = OFT_latency_fm:PCRT_shelter,
               names_to  = "metric",
               values_to = "z_score") %>%
  mutate(label = recode(metric,
                   OFT_latency_fm  = "OFT: Latency to\nfirst move",
                   OFT_distance    = "OFT: Total\ndistance",
                   OFT_centre_time = "OFT: Central\nzone time",
                   OFT_freezing    = "OFT: Freezing\nbouts",
                   PCRT_resume     = "PCRT: Latency\nto resume",
                   PCRT_freeze     = "PCRT: Freeze\nduration",
                   PCRT_shelter    = "PCRT: Shelter-seeking\nlatency"),
    timepoint = factor(timepoint, levels = c("pre", "post")),
    regime    = factor(regime,
                       levels = c("sham", "autotomy"),
                       labels = c("Sham", "Autotomy")))

(p_change <- metric_long %>%
  group_by(regime, label, timepoint) %>%
  summarise(mean_z = mean(z_score, na.rm = TRUE),
            se_z   = sd(z_score,   na.rm = TRUE) / sqrt(n()),
            .groups = "drop") %>%
  ggplot(aes(x = timepoint, y = mean_z,
             colour = timepoint, group = label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_line(colour = "grey60", linewidth = 0.7) +
  geom_errorbar(aes(ymin = mean_z - se_z,
                    ymax = mean_z + se_z),
                width = 0.1, linewidth = 0.7) +
  geom_point(size = 3.5) +
  facet_grid(label ~ regime, switch = "y") +
  scale_colour_manual(values = c(pre = "#888780", post = "#C04828"),
                      labels = c(pre = "Pre", post = "Post"),
                      name   = "Timepoint") +
  labs(x = NULL, y = "Mean z-score") + #what changes after autotomy?
  theme_cricket() +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8),
        strip.placement   = "outside",
        panel.spacing.y   = unit(0.3, "lines")))

delta_metrics <- metric_means %>%
  select(ID, regime, personality, timepoint,
         OFT_latency_fm:PCRT_shelter) %>%
  pivot_longer(OFT_latency_fm:PCRT_shelter,
               names_to = "metric", values_to = "z") %>%
  pivot_wider(names_from = timepoint, values_from = z) %>%
  mutate(delta = post - pre,
         label = recode(metric,
                        OFT_latency_fm  = "OFT: Latency to first move",
                        OFT_distance    = "OFT: Total distance",
                        OFT_centre_time = "OFT: Central zone time",
                        OFT_freezing    = "OFT: Freezing bouts",
                        PCRT_resume     = "PCRT: Latency to resume",
                        PCRT_freeze     = "PCRT: Freeze duration",
                        PCRT_shelter    = "PCRT: Shelter-seeking latency"),
         regime = factor(regime,
                         levels = c("sham", "autotomy"),
                         labels = c("Sham", "Autotomy")))

(p_delta_metrics <- delta_metrics %>% #change in behavioural metrics (pre/post trial)
  group_by(regime, label) %>%
  summarise(mean_d = mean(delta, na.rm = TRUE),
            se_d   = sd(delta,   na.rm = TRUE) / sqrt(n()),
            .groups = "drop") %>%
  mutate(label = fct_reorder(label, mean_d)) %>%
  ggplot(aes(x = mean_d, y = label,
             colour = ifelse(mean_d > 0, "Increase", "Decrease"))) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = mean_d - se_d,
                     xmax = mean_d + se_d),
                 height = 0.25, linewidth = 0.7) +
  geom_point(size = 4) +
  facet_wrap(~ regime) +
  scale_colour_manual(values = c(Increase = "green3", Decrease = "red2"),
                      name   = "Direction of change") +
  labs(x = "Mean Δ z-score (post − pre)", y = NULL) + #negative z-score = becomes shyer
  theme_cricket() + 
  theme(panel.grid.major.x = element_line(colour = "grey92")))

ggsave("boldness_drivers.png", p_delta_metrics,
       width = 8, height = 8, dpi = 300, bg = "white")


(fig_drivers <- p_loadings / p_delta_metrics +
  plot_annotation(
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10, colour = "grey40"))))
ggsave("fig_boldness_drivers.png", fig_drivers,
       width = 11, height = 10, dpi = 300, bg = "white")


#Grids ----
#Pannels + more grids (H1 and H2)
(fig_H1 <- (p4a | p8b) +
   plot_annotation(
     theme = theme(plot.title = element_text(face = "bold", size = 14),
                   plot.subtitle = element_text(size = 10, colour = "grey40"))))

ggsave("H1.png", fig_H1,
       width = 14, height = 6, dpi = 300, bg = "white")

(fig_H2 <- (p3a / (p5b | p2b)) +
    plot_annotation(
      theme = theme(plot.title = element_text(face = "bold", size = 14),
                    plot.subtitle = element_text(size = 10, colour = "grey40"))))
ggsave("H2.png", fig_H2,
       width = 14, height = 8, dpi = 300, bg = "white")

#More grids
plot_data <- crickets_long %>%
  filter(trial == 1) %>%
  select(ID, regime, personality, PC1_pre, PC1_post) %>%
  distinct() %>%
  pivot_longer(cols      = c(PC1_pre, PC1_post),
               names_to  = "timepoint",
               values_to = "PC1") %>%
  mutate(timepoint = factor(recode(timepoint,
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

ggsave("fig_prepost_personality.png", fig_prepost,
       width = 20, height = 10, dpi = 300, bg = "white")

metrics_grid <- crickets_long %>%
  group_by(ID, regime, personality, timepoint) %>%
  summarise(
    `OFT:\nLatency to\nfirst move (s)`    = mean(FM_latency,       na.rm = TRUE),
    `OFT:\nTotal distance\n(cm)`           = mean(total_distance,   na.rm = TRUE),
    `OFT:\nExposed zone\ntime (s)`         = mean(time_exp,         na.rm = TRUE),
    `OFT:\nCentral zone\ntime (s)`         = mean(time_central,     na.rm = TRUE),
    `OFT:\nFreezing\nbouts (n)`            = mean(freezing_n,       na.rm = TRUE),
    `PCRT:\nLatency to\nresume (s)`        = mean(resume_latency,   na.rm = TRUE),
    `PCRT:\nTotal freeze\nduration (s)`    = mean(total_freeze_time,na.rm = TRUE),
    `PCRT:\nShelter-seeking\nlatency (s)`  = mean(shelter_latency,  na.rm = TRUE),
    .groups = "drop") %>%
  pivot_longer(
    cols      = `OFT:\nLatency to\nfirst move (s)`:`PCRT:\nShelter-seeking\nlatency (s)`,
    names_to  = "metric",
    values_to = "value") %>%
  mutate(
    timepoint = factor(timepoint, levels = c("pre", "post"),
                       labels = c("Pre", "Post")),
    regime    = factor(regime,
                       levels = c("sham", "autotomy"),
                       labels = c("Sham", "Autotomy")),
    assay = ifelse(grepl("OFT", metric), "OFT", "PCRT"),
    metric = factor(metric, levels = c(
      "OFT:\nLatency to\nfirst move (s)",
      "OFT:\nTotal distance\n(cm)",
      "OFT:\nExposed zone\ntime (s)",
      "OFT:\nCentral zone\ntime (s)",
      "OFT:\nFreezing\nbouts (n)",
      "PCRT:\nLatency to\nresume (s)",
      "PCRT:\nTotal freeze\nduration (s)",
      "PCRT:\nShelter-seeking\nlatency (s)")))

grid_summary <- metrics_grid %>%
  group_by(regime, personality, timepoint, metric, assay) %>%
  summarise(mean_val = mean(value, na.rm = TRUE),
    se_val = sd(value,   na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

(fig_grid <- ggplot(grid_summary,
                   aes(x        = timepoint,
                       y        = mean_val,
                       colour   = personality,
                       group    = personality)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_val - se_val,
                    ymax = mean_val + se_val),
                width = 0.15, linewidth = 0.65, alpha = 0.8) +
  geom_point(size = 2.8) +
  facet_grid(metric ~ regime,
             scales = "free_y",
             switch = "y") +
  scale_colour_manual(values = pal_personality,
                      name   = "Personality") +
  scale_x_discrete(expand = expansion(mult = 0.35)) +
  labs(
    x        = NULL,
    y        = NULL
  ) +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    strip.text.x     = element_text(face = "bold", size = 10),
    strip.text.y.left= element_text(angle = 0, hjust = 1, size = 8),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.placement  = "outside",
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 7),
    legend.position  = "bottom",
    panel.spacing.y  = unit(0.5, "lines"),
    panel.spacing.x  = unit(1,   "lines")))

ggsave("fig_metric_grid.png", fig_grid,
       width = 9, height = 14, dpi = 300, bg = "white")



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
    panel_label = recode(panel,
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

ggsave("fig_boxplot_grid.png",  fig_boxgrid,
       width = 20, height = 10, dpi = 300, bg = "white")

#More grids
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


ggsave("fig_boxplot_grid_personality.png", fig_boxgrid_pers,
       width = 20, height = 10, dpi = 300, bg = "white")
