setwd("/Users/zojamancekpali/Desktop/KU Leuven/ASP")
getwd()

library(tidyverse)
library(readxl)
dr <- read_excel("DR.xlsx")

head(dr)
str(dr)

boxplot(BP ~ Condition, data = dr)
plot(HR ~ Time, data = dr)

dr %>% arrange(match(Condition, c('Control', 'Cold', 'Pack'))) %>% 
       mutate(Condition = rename("Cold water" == "Cold"))

ggplot(dr, aes(x=Time, y=HR, group=Condition, color=Condition)) + 
  geom_point(size=0.9) +
  theme(legend.position = c(0.90, 0.87)) + 
  theme(legend.title=element_blank()) +
  theme_classic()

ggplot(dr, aes(x = Condition, y = HR, fill = Condition)) +
  geom_boxplot() +
  scale_fill_manual(values = c("royalblue", "springgreen4", "red")) +
  labs(y = "Heart rate (bpm)",
       x = "Treatment") +
  theme_classic() #fine

ggplot(dr, aes(x = Time, y = BP, color = Condition)) +
  geom_line() +
  geom_point()



library(tidyverse)
library(readxl)

# ── Load & tidy ──────────────────────────────────────────────────────────────
df <- read_xlsx("DR.xlsx") %>%
  mutate(
    Systolic  = as.numeric(str_extract(BP, "^[0-9]+")),
    Diastolic = as.numeric(str_extract(BP, "[0-9]+$")),
    Pulse_Pressure = Systolic - Diastolic,
    Condition = factor(Condition, levels = c("Control", "Cold", "Pack"),
                       labels = c("Control", "Cold water", "Ice pack")),
    Time = factor(Time, levels = c(0, 15, 30), labels = c("0", "15", "30"))
  )

# ── Colour palette ───────────────────────────────────────────────────────────
cond_colours <- c("Control" = "springgreen4", "Cold water" = "royalblue", "Ice pack" = "lightblue")

# ── Shared theme ─────────────────────────────────────────────────────────────
theme_dr <- function() {
  theme_classic(base_size = 12) +
    theme(
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey92", colour = NA),
      legend.position   = "Top",
      legend.title      = element_blank(),
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(colour = "grey40", size = 10),
      axis.title        = element_text(size = 11)
    )
}

# ── Summary helper (mean ± SE) ───────────────────────────────────────────────
summarise_var <- function(data, var) {
  data %>%
    group_by(Condition, Time) %>%
    summarise(
      mean = mean({{ var }}, na.rm = TRUE),
      se   = sd({{ var }}, na.rm = TRUE) / sqrt(sum(!is.na({{ var }}))),
      .groups = "drop"
    )
}

pd <- position_dodge(width = 0.15)

# ════════════════════════════════════════════════════════════════════════════
# 1.  HEART RATE over time
# ════════════════════════════════════════════════════════════════════════════
hr_sum <- summarise_var(df, HR)

(p_hr <- ggplot(hr_sum, aes(x = Time, y = mean, colour = Condition,
                           group = Condition)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.15, position = pd, linewidth = 0.7) +
  scale_colour_manual(values = cond_colours) +
  labs(
    x        = "Time point (seconds)",
    y        = "Heart rate (bpm)") +
  theme_classic() +
    theme(legend.position = "top"))

ggsave("plot_HR_over_time.png", p_hr, width = 7, height = 5, dpi = 150)
message("Saved: plot_HR_over_time.png")

#Plot 2:

sys_sum <- summarise_var(df %>% filter(Time != "15") %>% droplevels(), Systolic)
(p_sys <- ggplot(sys_sum, aes(x = Time, y = mean, colour = Condition,
                             group = Condition)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.15, position = pd, linewidth = 0.7) +
  scale_colour_manual(values = cond_colours) +
  labs(x = "Time point (seconds)", y = "Systolic BP (mmHg)") +
  theme_classic() +
  theme(legend.position = "top"))

ggsave("plot_SystolicBP_over_time.png", p_sys, width = 7, height = 5, dpi = 150)
message("Saved: plot_SystolicBP_over_time.png")

# ════════════════════════════════════════════════════════════════════════════
# 3.  DIASTOLIC BP over time
# ════════════════════════════════════════════════════════════════════════════
dia_sum <- summarise_var(df %>% filter(Time != "15") %>%  droplevels(), Diastolic)

(p_dia <- ggplot(dia_sum, aes(x = Time, y = mean, colour = Condition,
                             group = Condition)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.15, position = pd, linewidth = 0.7) +
  scale_colour_manual(values = cond_colours) +
  labs(
    x        = "Time point (seconds)",
    y        = "Diastolic BP (mmHg)") +
    theme_classic() +
    theme(legend.position = "top"))
ggsave("plot_DiastolicBP_over_time.png", p_dia, width = 7, height = 5, dpi = 150)
message("Saved: plot_DiastolicBP_over_time.png")

# ════════════════════════════════════════════════════════════════════════════
# 4.  PULSE PRESSURE over time
# ════════════════════════════════════════════════════════════════════════════
pp_sum <- summarise_var(df %>% filter(Time != "15") %>% droplevels(), Pulse_Pressure)

(p_pp <- ggplot(pp_sum, aes(x = Time, y = mean, colour = Condition,
                           group = Condition)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.15, position = pd, linewidth = 0.7) +
  scale_colour_manual(values = cond_colours) +
  labs(
    x        = "Time point (seconds)",
    y        = "Pulse pressure (mmHg)"
  ) +
  theme_classic() +
  theme(legend.position = "top"))

ggsave("plot_PulsePressure_over_time.png", p_pp, width = 7, height = 5, dpi = 150)
message("Saved: plot_PulsePressure_over_time.png")

# ════════════════════════════════════════════════════════════════════════════
# 5.  COMBINED panel: all four measures
# ════════════════════════════════════════════════════════════════════════════
library(patchwork)

(p_combined <- (p_hr) / (p_sys | p_dia) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = list(c("a", "b", "c")),
    tag_prefix = "",
    tag_suffix = ")",
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.tag  = element_text(face = "bold", size = 12))
  ) &
  theme(legend.position = "bottom"))
ggsave("plot_combined.png", p_combined, width = 7, height = 5, dpi = 150)
message("Saved: plot_combined.png")

message("\nAll plots saved successfully.")



#Boxplots
# ── 6. HR boxplot ────────────────────────────────────────────────────────────
# Subsets with 15 min dropped for BP measures
df_bp <- df %>% filter(Time != "15 min") %>% droplevels()

make_boxplot <- function(data, var, ylab) {
  ggplot(data, aes(x = Time, y = {{ var }}, fill = Condition)) +
    geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = 21,
                 outlier.size = 2, linewidth = 0.5, width = 0.6) +
    geom_jitter(aes(colour = Condition),
                position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
                size = 2, alpha = 0.6) +
    scale_fill_manual(values  = alpha(cond_colours, 0.35)) +
    scale_colour_manual(values = cond_colours) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.6))) +
    labs(x = "Time point (seconds)", y = ylab) +
    theme_dr()
}
# ── Subsets with 15 min dropped for BP measures ──────────────────────────────

# ── 6. HR boxplot ────────────────────────────────────────────────────────────
(bp_hr <- make_boxplot(df, HR, "Heart rate (bpm)"))

(bp_hr <- ggplot(df, aes(x = Condition, y = HR) +
    geom_boxplot(fill = Condition) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.15, position = pd, linewidth = 0.7) +
    scale_colour_manual(values = cond_colours) +
    labs(
      x        = "Condition",
      y        = "Heart rate (bpm)"
    ) +
    theme_classic() +
    theme(legend.position = "top")))


ggsave("boxplot_HR.png", bp_hr, width = 7, height = 5, dpi = 150)
message("Saved: boxplot_HR.png")

# ── 7. Systolic BP boxplot ───────────────────────────────────────────────────
bp_sys <- make_boxplot(df_bp, Systolic, "Systolic BP (mmHg)",
                       "Systolic blood pressure by condition and time",
                       "15 min excluded (no BP recorded)")

ggsave("boxplot_SystolicBP.png", bp_sys, width = 7, height = 5, dpi = 150)
message("Saved: boxplot_SystolicBP.png")

# ── 8. Diastolic BP boxplot ──────────────────────────────────────────────────
bp_dia <- make_boxplot(df_bp, Diastolic, "Diastolic BP (mmHg)",
                       "Diastolic blood pressure by condition and time",
                       "15 min excluded (no BP recorded)")

ggsave("boxplot_DiastolicBP.png", bp_dia, width = 7, height = 5, dpi = 150)
message("Saved: boxplot_DiastolicBP.png")

# ── 9. Pulse pressure boxplot ────────────────────────────────────────────────
bp_pp <- make_boxplot(df_bp, Pulse_Pressure, "Pulse pressure (mmHg)",
                      "Pulse pressure by condition and time",
                      "15 min excluded  |  Pulse pressure = systolic − diastolic")

ggsave("boxplot_PulsePressure.png", bp_pp, width = 7, height = 5, dpi = 150)
message("Saved: boxplot_PulsePressure.png")

# ── 10. Combined boxplot panel ───────────────────────────────────────────────
box_combined <- (bp_hr | bp_pp) / (bp_sys | bp_dia) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Cardiovascular responses by condition (boxplots)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  ) &
  theme(legend.position = "bottom")

ggsave("boxplot_combined.png", box_combined, width = 13, height = 10, dpi = 150)
message("Saved: boxplot_combined.png")

message("\nAll plots saved successfully.")