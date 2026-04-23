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
library(survival)
library(survminer)
library(patchwork)


#Data & wrangling ----
crickets <- read.csv("cricket_autotomy_raw_dataset_v2.csv",
                     na = c("", "NA", "NaN"))

oft_pre_cols  <- grep("OFT_pre",  names(crickets), value = TRUE)
oft_post_cols <- grep("OFT_post", names(crickets), value = TRUE)
pcrt_pre_cols <- grep("PCRT_pre", names(crickets), value = TRUE)
pcrt_post_cols<- grep("PCRT_post",names(crickets), value = TRUE)

trial_from_col <- function(x) as.integer(str_extract(x, "(?<=_T)\\d"))

make_long_block <- function(df, cols, timepoint_label, assay_label) {
  df %>%
    select(individual_ID, all_of(cols)) %>%
    pivot_longer(cols = all_of(cols),
                 names_to  = "col_name",
                 values_to = "value") %>%
    mutate(
      trial    = trial_from_col(col_name),
      metric   = str_replace(col_name,
                             paste0(assay_label, "_(pre|post)_T\\d_"), ""),
      timepoint = timepoint_label,
      assay     = assay_label
    ) %>%
    select(individual_ID, timepoint, assay, trial, metric, value)
}

oft_pre_long  <- make_long_block(crickets, oft_pre_cols,  "pre",  "OFT")
oft_post_long <- make_long_block(crickets, oft_post_cols, "post", "OFT")
pcrt_pre_long <- make_long_block(crickets, pcrt_pre_cols, "pre",  "PCRT")
pcrt_post_long<- make_long_block(crickets, pcrt_post_cols,"post", "PCRT")

long_metrics <- bind_rows(oft_pre_long, oft_post_long,
                          pcrt_pre_long, pcrt_post_long) %>%
  pivot_wider(names_from = metric, values_from = value)

meta <- crickets %>%
  select(individual_ID, sex, age_days_post_moult, body_mass_g,
         autotomy_regime, personality_category,
         personality_PC1_pre, personality_PC1_post,
         autotomy_latency_s, autotomy_censored, autotomy_occurred,
         sham_pinch_duration_s)

crickets_long <- long_metrics %>%
  left_join(meta, by = "individual_ID") %>%
  rename(
    ID            = individual_ID,
    age           = age_days_post_moult,
    mass          = body_mass_g,
    regime        = autotomy_regime,
    personality   = personality_category,
    PC1_pre       = personality_PC1_pre,
    PC1_post      = personality_PC1_post,
    a_latency     = autotomy_latency_s,
    a_censored    = autotomy_censored,
    a_occured     = autotomy_occurred,
    pinch_dur     = sham_pinch_duration_s,
    FM_latency         = latency_first_move_s,
    total_distance     = total_distance_cm,
    time_exp           = time_exposed_zone_s,
    time_central       = time_central_zone_s,
    freezing_n         = freezing_bouts_n,
    resume_latency     = latency_resume_s,
    total_freeze_time  = total_freeze_duration_s,
    shelter_latency    = shelter_seeking_latency_s
  ) %>%
  mutate(
    ID          = factor(ID),
    sex         = factor(sex),
    regime      = factor(regime, levels = c("sham", "autotomy")),
    personality = factor(personality, levels = c("Shy","Intermediate","Bold")),
    timepoint   = factor(timepoint, levels = c("pre","post")),
    d_boldness  = PC1_post - PC1_pre
  )

crickets_analysis <- crickets %>%
  mutate(
    individual_ID      = factor(individual_ID),
    sex                = factor(sex),
    autotomy_regime    = factor(autotomy_regime, levels = c("sham","autotomy")),
    personality_category = factor(personality_category,
                                  levels = c("Shy","Intermediate","Bold")),
    autotomy_occurred  = as.logical(autotomy_occurred),
    autotomy_censored  = as.logical(autotomy_censored),
    delta_boldness     = personality_PC1_post - personality_PC1_pre,
    surv_status        = case_when(
      autotomy_regime == "autotomy" & autotomy_occurred == TRUE  ~ 1,
      autotomy_regime == "autotomy" & autotomy_occurred == FALSE ~ 0,
      TRUE ~ NA_integer_
    )
  )


pal_personality <- c(Shy = "yellow2", Intermediate = "orange", Bold = "red3")
pal_regime      <- c(sham = "lightblue3", autotomy = "red")
pal_timepoint   <- c(pre = "#888780", post = "#C04828")

theme_cricket <- function() {
  theme_classic() +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      axis.title       = element_text(size = 10),
      legend.position  = "bottom",
      strip.background = element_rect(fill = "grey95", colour = NA),
      strip.text       = element_text(face = "bold", size = 9)
    )
}


# ============================================================
# FIGURE 1 — Personality validation: behavioural profiles
# (pre-trial z-score bar chart across OFT + PCRT metrics)
# ============================================================

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
  theme(legend.position = c(0.85, 0.9)))

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
    theme(legend.position = c(0.85, 0.9)))


# ============================================================
# FIGURE 2 — Pre vs post behavioural profiles by treatment
# (same as Fig 1 but split by regime, alpha = timepoint)
# ============================================================

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
    metric = recode(metric,
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
  theme(axis.text.x = element_text(size = 7)))

ggsave("fig2_pre_post_profiles.png", p2, width = 13, height = 5.5, dpi = 300)
message("✓ Figure 2 saved")


# ============================================================
# FIGURE 3 — Autotomy latency by personality type
# (survival-style boxplot with censored points flagged)
# ============================================================
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


# ============================================================
# FIGURE 4 — Survival curves (Kaplan–Meier) by personality
# ============================================================

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


# ============================================================
# FIGURE 5 — Pre-trial boldness (PC1) vs autotomy latency
# (RQ1: does boldness predict autotomy threshold?)
# ============================================================

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


# ============================================================
# FIGURE 6 — Delta boldness (PC1 post − pre) by personality × regime
# (RQ2: does personality change after autotomy?)
# ============================================================

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


# ============================================================
# FIGURE 7 — Pre vs post PC1 scatter (individual trajectories)
# Highlights whether bold individuals are more/less stable
# ============================================================

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


# ============================================================
# FIGURE 8 — Body mass vs autotomy latency (confound check)
# ============================================================

p8 <- crickets_analysis %>%
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
  theme_cricket()

ggsave("fig8_mass_confound.png", p8, width = 7, height = 5, dpi = 300)
message("✓ Figure 8 saved")


# ============================================================
# FIGURE 9 — Sample composition summary
# (n per personality × regime cell, sex distribution)
# ============================================================

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

p9b <- crickets_analysis %>%
  count(personality_category, sex) %>%
  ggplot(aes(x = personality_category, y = n, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
  geom_text(aes(label = n),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c(M = "steelblue3", F = "salmon3"), name = "Sex") +
  labs(subtitle = "Sex distribution per personality",
       x = "Personality", y = "n") +
  theme_cricket() +
  theme(legend.position = "right")

p9 <- (p9a | p9b) +
  plot_annotation(title = "Figure 9 — Sample composition",
                  theme = theme(plot.title = element_text(face = "bold", size = 12)))

ggsave("fig9_sample_composition.png", p9, width = 11, height = 4.5, dpi = 300)
message("✓ Figure 9 saved")


# ============================================================
message("\n✅ All 9 figures saved to working directory.")
message("   Figures 1–9 cover: personality validation, pre/post profiles,")
message("   autotomy latency, survival curves, RQ1 regression, RQ2 delta")
message("   boldness, PC1 trajectories, confound check, and sample summary.")



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
    theme_cricket())
