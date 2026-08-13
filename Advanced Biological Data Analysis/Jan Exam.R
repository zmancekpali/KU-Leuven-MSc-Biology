# ── PACKAGES ──────────────────────────────────────────────────────────────────

#!! [CRITIQUE] You loaded ~40 packages, the vast majority unused. This slows
#!! startup, causes namespace conflicts (you loaded 'scales' three times!), and
#!! makes it harder to see which functions come from which package. Load only
#!! what you actually call. The list below covers the entire exam.

library(readxl)    # read_xlsx()
library(vegan)     # metaMDS, vegdist, envfit, adonis2, scores
library(cluster)   # silhouette()
library(ggplot2)   # ggplot
library(lme4)      # glmer
library(MuMIn)     # AICc, model.sel
library(DHARMa)    # simulateResiduals, testDispersion
library(emmeans)   # emmeans, pairs, emtrends
library(car)       # Anova(type=3)
library(effects)   # allEffects — for quick model plots
library(ggeffects) #!! [CRITIQUE PART 2] You called ggpredict() on line 154 of
#!! your vdbw script but never loaded ggeffects. This would
#!! have thrown "could not find function ggpredict".


# =============================================================================
# PART 1 — Hans Jacquemyn
# Fish communities in ponds: NMDS + clustering + envfit
# =============================================================================

setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/EXAM")

comm <- read_xlsx("Exam data/Comm_data.xlsx")
env  <- read_xlsx("Exam data/Env_data.xlsx")

# Species abundance matrix (drop pond ID column)
com_m <- comm[, 2:34]    # 115 rows × 33 species
env_m <- env[, 2:25]     # 115 rows × 24 env variables


# ── NMDS ──────────────────────────────────────────────────────────────────────

nmds <- metaMDS(com_m, distance = "bray", k = 2, trymax = 100, autotransform = FALSE)   # set FALSE to be explicit; metaMDS
# would square-root + Wisconsin by default

nmds$stress
# < 0.10 → excellent fit
# < 0.20 → acceptable (your result was 0.19 — borderline acceptable, say so)
# > 0.30 → poor, consider more dimensions

stressplot(nmds)   # Shepard plot — non-metric fit quality


# ── Bray-Curtis dissimilarity ─────────────────────────────────────────────────

bray_dist <- vegdist(com_m, method = "bray")
# 0 = identical communities, 1 = completely different


# ── UPGMA hierarchical clustering ────────────────────────────────────────────

hc <- hclust(bray_dist, method = "average")   # UPGMA

plot(hc,
     labels = FALSE, hang = -1,
     main   = "UPGMA dendrogram (Bray–Curtis)",
     xlab   = "Ponds",
     ylab   = "Bray–Curtis dissimilarity")

# Fusion-level plot — visual aid for choosing k
plot(hc$height, nrow(com_m):2, type = "S",
     main = "Fusion levels — UPGMA",
     ylab = "k (number of clusters)",
     xlab = "h (node height)", col = "grey")
text(hc$height, nrow(com_m):2, nrow(com_m):2, col = "red", cex = 0.7)


# ── Silhouette analysis ───────────────────────────────────────────────────────

#!! [CRITIQUE — CRITICAL BUG] Lines 100–116 of your HJ script used two
#!! undefined objects:
#!!
#!!   p.dist        → never created. The correct object is 'bray_dist'
#!!                   (you defined it on line 89 with vegdist()).
#!!   p.dist.UPGMA  → never created. The correct object is 'hc'
#!!                   (you defined it on line 90 with hclust()).
#!!   plants        → never created. Should be 'com_m'.
#!!
#!! These three undefined variables would have caused immediate errors and given
#!! you zero output from the silhouette section. The fixes are below.

asw <- numeric(nrow(com_m))

for (k in 2:(nrow(com_m) - 1)) {
  sil    <- silhouette(cutree(hc, k = k), bray_dist)   # hc, not p.dist.UPGMA
  asw[k] <- summary(sil)$avg.width                     # bray_dist, not p.dist
}

k_best <- which.max(asw)
cat("Optimal number of clusters:", k_best, "\n") #22 clusters is best

plot(1:nrow(com_m), asw, type = "h",          # com_m, not plants
     main = "Silhouette — optimal k",
     xlab = "k (number of groups)",
     ylab = "Average silhouette width")
axis(1, k_best, paste("optimum", k_best, sep = "\n"),
     col = "red", col.axis = "red", font = 2)
points(k_best, max(asw), pch = 16, col = "red", cex = 1.5)

# Assign cluster membership
clusters <- cutree(hc, k = k_best)
table(clusters)   # how many ponds in each cluster?


# ── NMDS plot coloured by cluster ────────────────────────────────────────────

#!! [CRITIQUE] After the silhouette section you went back and re-plotted the NMDS
#!! without any cluster information — just plain points. The key deliverable for Q1
#!! is an NMDS plot where each point is coloured/shaped by its cluster. The version
#!! below is what you should have produced.

nmds_xy          <- as.data.frame(scores(nmds, display = "sites"))
nmds_xy$cluster  <- factor(clusters)

ggplot(nmds_xy, aes(NMDS1, NMDS2, colour = cluster, shape = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(type = "t", linetype = 2) +
  theme_classic() +
  labs(title   = paste0("NMDS (Bray–Curtis)  |  Stress = ",
                        round(nmds$stress, 3)),
       colour  = "Cluster",
       shape   = "Cluster")


# ── Which species characterise each cluster? ─────────────────────────────────

# Simple: mean abundance per species per cluster
cluster_means <- aggregate(com_m,
                           by  = list(cluster = clusters),
                           FUN = mean)
# Sort each cluster's species by mean abundance; top species = characteristic

# More rigorous: indicator species analysis (package 'indicspecies')
# library(indicspecies)
# indval <- multipatt(com_m, clusters, func = "IndVal.g", control = how(nperm = 999))
# summary(indval)


# ── Q2: Environmental drivers ─────────────────────────────────────────────────

# envfit — correlates env vectors to NMDS ordination space
envfit_res <- envfit(nmds, env_m, permutations = 999)
envfit_res   # prints r² and p-value for each variable

# PERMANOVA — formally tests whether community composition differs with environment
#!! [CRITIQUE — MISSING STEP] You never ran adonis2 / PERMANOVA. envfit shows
#!! *which* variables correlate with the ordination, but it does not formally test
#!! whether environmental conditions significantly explain community structure.
#!! adonis2 is the standard test for that, and you should have included it.

adonis2(com_m ~ ., data = env_m, method = "bray", permutations = 999)
# With 24 predictors this can be hard to interpret. A cleaner approach:
# run adonis2 with only the variables envfit identified as significant (p < 0.05).

# Plot — ONLY significant env vectors
#!! [CRITIQUE] You plotted all 24 vectors regardless of significance (no p.max
#!! argument). This clutters the plot. Filter to p < 0.05:

plot(nmds, type = "n", main = "NMDS + significant env variables (p < 0.05)")
points(nmds, display = "sites",
       col = clusters + 1, pch = 16, cex = 0.9)
plot(envfit_res, p.max = 0.05, col = "red", lwd = 1.5)
legend("topright", legend = paste("Cluster", 1:k_best),
       col = (1:k_best) + 1, pch = 16)


# =============================================================================
# PART 2 — Van den Berg / Wenseleers
# Adélie penguin moult timing — Poisson GLMM
# =============================================================================

peng <- read.csv("Exam data/Penguins.csv")

# ── Data preparation ──────────────────────────────────────────────────────────

peng <- peng |>
  dplyr::mutate(
    breeding_status = as.factor(breeding_status),
    bird_id         = as.factor(bird_id)
  ) |>
  dplyr::rename(
    status = breeding_status,
    ID     = bird_id,
    moult  = moult_start_days
  )

str(peng)
summary(peng)

# Set reference level for status
peng$status <- relevel(peng$status, ref = "Non-breeder")

#!! [CRITIQUE] You called set_sum_contrasts() (from afex) at line 79 of your
#!! script. This switches ALL factor contrasts to sum-to-zero (effects) coding,
#!! which changes what the Intercept means: instead of the reference group mean,
#!! it becomes the grand mean. This is a legitimate choice but it conflicts with
#!! your relevel() call and makes your written interpretation harder to follow.
#!! Unless you consciously need sum contrasts, leave this out and use the default
#!! treatment (dummy) coding — the reference level approach is more intuitive for
#!! exam write-ups.

# Quick exploratory plots
plot(moult ~ age,    data = peng, main = "Moult timing by age")
plot(moult ~ status, data = peng, main = "Moult timing by breeding status")
# → Successful breeders tend to start moulting LATER (higher moult_start_days)
#   This is consistent with the carry-over cost hypothesis.


# ── Build Poisson GLMMs ───────────────────────────────────────────────────────

# Response: moult_start_days — count from 0 → Poisson family
# Random effect: (1|ID) — same individual measured across years

model_null   <- glmer(moult ~ 1            + (1|ID), data = peng, family = poisson)
model_status <- glmer(moult ~ status       + (1|ID), data = peng, family = poisson)
model_age    <- glmer(moult ~ age          + (1|ID), data = peng, family = poisson)
model_add    <- glmer(moult ~ status + age + (1|ID), data = peng, family = poisson)
model_int    <- glmer(moult ~ status * age + (1|ID), data = peng, family = poisson)


# ── Model selection ───────────────────────────────────────────────────────────

MuMIn::AICc(model_null, model_status, model_age, model_add, model_int)

model.sel(model_null, model_status, model_age, model_add, model_int)
# Best model = lowest AICc AND ΔAIC > 2 from the next candidate
# Your result: model_int was best — correct choice ✓


# ── Overdispersion check ──────────────────────────────────────────────────────

sim_out <- DHARMa::simulateResiduals(fittedModel = model_int, plot = TRUE)
DHARMa::testDispersion(sim_out)
# p > 0.05 → no significant overdispersion → keep Poisson ✓
# p < 0.05 → overdispersed → switch to negative binomial or add obs-level RE:
#   peng$obs <- factor(seq_len(nrow(peng)))
#   model_int_od <- glmer(moult ~ status * age + (1|ID) + (1|obs), ...)


# ── Summary & inference ───────────────────────────────────────────────────────

summary(model_int)
# Fixed effects are on the LOG scale (Poisson log-link).
# Intercept = log(expected moult_start_days) for Non-breeders at age 0
# exp(coef) gives the multiplicative effect on the response.

Anova(model_int, type = 3)
# Type III Wald chi-square tests for each fixed effect, controlling for others.
# Reports whether status, age, and status:age interaction are significant.


# ── Collinearity check ────────────────────────────────────────────────────────

#!! [CRITIQUE — UNDEFINED VARIABLE] Line 128 of your vdbw script:
#!!   vif(model4)
#!! 'model4' was never defined anywhere in your script. You clearly meant
#!! model_int (or model_add). This would have thrown an error.
#!!
#!! Additionally, car::vif() does not work directly on glmer objects.
#!! Run it on a fixed-effects-only glm to check collinearity:

model_int_fe <- glm(moult ~ status * age, data = peng, family = poisson)
car::vif(model_int_fe)
# GVIF^(1/(2*Df)) < ~2.5 is acceptable for factors with multiple levels.
# Note: with an interaction term, main effects will show high VIF — this is
# expected and not a problem. You can ignore VIF when an interaction is in the model.


# ── Post-hoc comparisons ─────────────────────────────────────────────────────

#!! [CRITIQUE — MISSING CODE] The question explicitly said "Perform post-hoc
#!! comparisons." You mentioned them in your written answer but there is NO
#!! emmeans code in your vdbw script. You must show the actual pairwise output.

# Marginal means of status at the mean age
emm_status <- emmeans(model_int, ~ status,
                      at     = list(age = mean(peng$age)),
                      type   = "response")   # back-transforms to count scale
emm_status
pairs(emm_status, adjust = "tukey")

# Slopes of age for each status level (emtrends tests whether age effect differs)
emm_trends <- emtrends(model_int, ~ status, var = "age")
emm_trends
pairs(emm_trends, adjust = "tukey")
# This is the key post-hoc for an interaction: are the slopes different?


# ── Visualisation ─────────────────────────────────────────────────────────────

#!! [CRITIQUE] Two issues with your visualisation:
#!! 1. ggpredict() comes from the ggeffects package — which you never loaded.
#!!    This line would have silently failed or thrown an error.
#!! 2. Your ggplot showed only raw data points (geom_point), with no model
#!!    predictions overlaid. The question asked for "as good a visual
#!!    representation as possible" — that means showing predicted lines + CIs
#!!    from the best model, not just a scatter plot.

# Option A — ggeffects (clean ggplot output)
library(ggeffects)
pred <- ggpredict(model_int, terms = c("age [all]", "status"))

plot(pred) +
  theme_classic() +
  labs(
    title   = "Penguin moult timing — best model (status × age interaction)",
    x       = "Age (years)",
    y       = "Moult start (days after earliest)",
    colour  = "Breeding status",
    fill    = "Breeding status"
  )

# Option B — manual with raw data + predictions overlaid
pred_df <- as.data.frame(pred)

ggplot() +
  # raw data (jittered, semi-transparent)
  geom_jitter(data  = peng,
              aes(x = age, y = moult, colour = status),
              alpha = 0.3, width = 0.2, size = 1.5) +
  # model predictions + 95% CI ribbon
  geom_ribbon(data  = pred_df,
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2) +
  geom_line(data  = pred_df,
            aes(x = x, y = predicted, colour = group),
            linewidth = 1.2) +
  scale_colour_manual(values = c("steelblue", "firebrick", "darkgreen")) +
  scale_fill_manual(values   = c("steelblue", "firebrick", "darkgreen")) +
  theme_classic() +
  labs(
    title  = "Moult timing ~ status × age  |  GLMM Poisson + (1|ID)",
    x      = "Age (years)",
    y      = "Moult start (days after earliest)",
    colour = "Breeding status",
    fill   = "Breeding status"
  )

# Option C — effects package (quick, base R)
plot(allEffects(model_int), multiline = TRUE,
     confint = list(style = "auto"),
     type    = "response")


# ── Conclusion template ───────────────────────────────────────────────────────

# The interactive model (moult ~ status * age + (1|ID), Poisson) was the best
# fit (lowest AICc; ΔAIC > 2 vs. additive model).
#
# Overdispersion: not significant (DHARMa, p > 0.05) → Poisson family retained.
#
# Significant effects (Anova Type III):
#   - Breeding status:       χ² = ..., p < 0.05 → significant
#   - Age:                   χ² = ..., p > 0.05 → marginally non-significant
#   - Status × age (interaction): χ² = ..., p < 0.05 → significant
#
# The random effect of bird ID explained [X]% of residual variance (ICC).
#
# Post-hoc (emtrends): age slopes differed significantly between status groups:
#   - Failed breeders:     moult timing DECREASES with age (slope < 0, p < ...)
#   - Non-breeders:        little change with age (slope ≈ 0)
#   - Successful breeders: moult timing INCREASES with age (slope > 0, p < ...)
#
# Interpretation: Successful breeders moult progressively later as they age,
# consistent with increasing reproductive investment with experience.
# Failed breeders moult earlier with age, possibly reflecting faster recovery.
#
#!! [CRITIQUE — WRITTEN ANSWER] In your exam write-up you stated:
#!!   "successful breeders exhibit significantly LOWER time to moult than
#!!    non-breeders"
#!! But on line 94 of your own script you wrote:
#!!   "moult time longest in successful breeders"
#!! These contradict each other. Since moult_start_days = 0 is the EARLIEST
#!! moult, a HIGHER value means a LATER start. Your exploratory plot showed
#!! successful breeders have the HIGHEST values → they moult LATEST, not earliest.
#!! The carry-over hypothesis predicts this (energy cost of successful breeding
#!! delays moult) — so the biology was correct but the written sentence was
#!! inverted. Watch this on the resit: state clearly that successful breeders
#!! initiate moult significantly LATER (higher moult_start_days).
