#######################################################################
#             Terrestrial Ecology – Egenhoven Forest 2026             #
#                             Group 4                                 #
#######################################################################

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Terrestrial Ecology")
getwd()

dir.create("figures", showWarnings = FALSE) #save figures to separate folder

#Libraries
library(readxl)
library(vegan)
library(readxl)
library(ggplot2)
library(cluster)
library(ggrepel)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(corrplot)
library(patchwork)
library(ggtext)
install.packages("twinspan",
                 repos = "https://jarioksa.r-universe.dev",
                 type = "binary")
library(twinspan)

#colours ----
CLUST_COLS <- c("1"="#E63946","2"="#2A9D8F","3"="#457B9D","4"="#F4A261")

STAND_COLS <- setNames(c("#E63946","#F4A261","#2A9D8F","#457B9D",
    "#6A4C93","#E9C46A","#264653","#A8DADC","#B5838D","#6D6875"), 1:10)

corr_vars <- c("winter_water","summer_water","Ah","litter",
               "pH_H2O","pH_KCl","OM","NO3","NH4","moisture")

base_theme <- theme_classic(base_size = 12) +
  theme(legend.position = "right")

#Data (Ellenber) ----
ellenberg <- as.data.frame(read_excel(
  "Ellenberg Indicator values-2022-11-07.xlsx",
  sheet = "Tab-OriginalNamesValues"))

#Data (plots) ----
data <- read_excel("data.xlsx", sheet = "Combined")
head(data)

raw <- as.data.frame(read_excel("data.xlsx", sheet = "Combined")) #combined data
rownames(raw) <- trimws(as.character(raw[[1]])) #trim first row (labels)
raw <- raw[ , -1] #drop label column
colnames(raw) <- trimws(colnames(raw)) #clean column names

clean_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))
env_keywords <- c("dominant", "winter", "summer", "\\bah\\b", "litter",
                  "horizon", "water", "o-horiz", "o horiz")
is_env <- grepl(paste(env_keywords, collapse = "|"),
                tolower(rownames(raw)))

spe_raw <- raw[!is_env, ] #species rows
env_rows <- raw[ is_env, ] #environmental rows

spe_mat <- as.data.frame(lapply(spe_raw, clean_num))
rownames(spe_mat) <- rownames(spe_raw)
spe_mat[is.na(spe_mat)] <- 0

#merging duplicate species rows (Fagus sylvatica entered twice)
dup_spe <- rownames(spe_mat)[duplicated(tolower(trimws(rownames(spe_mat))))]
if (length(dup_spe) > 0) {
  cat("Duplicate species found and merged:", paste(dup_spe, collapse = ", "), "\n")
  spe_mat$species_key <- tolower(trimws(rownames(spe_mat)))
  spe_mat <- aggregate(. ~ species_key, data = spe_mat, FUN = sum)
  rownames(spe_mat) <- spe_mat$species_key
  spe_mat$species_key <- NULL
}

#cleaner column names
fix_colname <- function(nm) {
  nm <- sub("^X", "", nm)                
  nm <- trimws(nm)
  suppressWarnings({
    v <- as.numeric(nm)
    if (!is.na(v)) nm <- sprintf("%g.%d", floor(v), round((v %% 1) * 10))
  })
  nm
}
colnames(spe_mat) <- sapply(colnames(spe_mat), fix_colname)
colnames(env_rows) <- sapply(colnames(env_rows), fix_colname)

spe <- as.data.frame(t(spe_mat)) #plots as rows, species as columns

plot_ids <- rownames(spe)
cat("Plots:", nrow(spe), "| Species:", ncol(spe), "\n") #30 plots, 77 species
cat("Plot IDs:", paste(plot_ids, collapse = ", "), "\n")

get_env_row <- function(keyword) {
  rows <- rownames(env_rows)[grepl(keyword, tolower(rownames(env_rows)))]
  if (length(rows) == 0) return(rep(NA, nrow(spe)))
  env_cols <- sapply(colnames(env_rows), fix_colname)
  vals <- clean_num(as.character(env_rows[rows[1], ]))
  named <- setNames(as.numeric(vals), env_cols)
  as.numeric(named[plot_ids])
}

env <- data.frame(
  row.names    = plot_ids,
  winter_water = get_env_row("winter"),
  summer_water = get_env_row("summer"),
  Ah           = get_env_row("^ah$|ah horizon|ah-horiz|ah thickness"),
  litter       = get_env_row("litter|o-horiz|o horiz|o thickness"),
  stand        = as.integer(sub("\\..*", "", plot_ids)))

env$stand[env$stand == 3] <- 11
env$stand[env$stand == 4] <- 12 #group 3 was in stand 11, group 4 in stand 12

dom_row <- rownames(env_rows)[grepl("dominant", tolower(rownames(env_rows)))]
if (length(dom_row) > 0) {
  dom_vals  <- trimws(as.character(env_rows[dom_row[1], ]))
  dom_names <- sapply(colnames(env_rows), fix_colname)
  env$dominant_tree <- setNames(dom_vals, dom_names)[plot_ids]
}

#Data (lab) ----
lab <- as.data.frame(read_excel("lab.xlsx", sheet = "results"))
colnames(lab) <- trimws(colnames(lab))
lab$plot_id <- gsub("-", ".", trimws(as.character(lab$Plot)))
lab_lookup <- plot_ids
lab_lookup <- gsub("^3\\.", "11.", lab_lookup)
lab_lookup <- gsub("^4\\.", "12.", lab_lookup)

pull_lab <- function(col) {
  if (!col %in% colnames(lab)) return(rep(NA, length(plot_ids)))
  vals <- setNames(as.numeric(lab[[col]]), lab$plot_id)
  vals[lab_lookup]
}

env$moisture <- pull_lab("Moisture")
env$OM <- pull_lab("Organic Matter")
env$pH_H2O <- pull_lab("Ph H2O")
env$pH_KCl <- pull_lab("Ph KCl")
env$NO3 <- pull_lab("mg NO3/kg soil")
env$NH4 <- pull_lab("mg NH4-N/kg soil")

cat("Environmental variables loaded:\n")
print(head(env))
cat("\nStand numbers in data:\n")
print(sort(unique(env$stand))) 

#Diversity ----
div <- data.frame(row.names = plot_ids,
  stand = env$stand,
  S = specnumber(spe),
  H = diversity(spe, index = "shannon"),
  D1 = diversity(spe, index = "simpson"), #concentration index (D)
  D2 = diversity(spe, index = "invsimpson")) #inverse Simpson (1/D)  

div$Simpson <- 1 - div$D1   #diversity index delta1 (1-D)
div$J <- div$H / log(div$S) #Pielou's evenness (Shannon-based)
div$E <- div$D2 / div$S     #Simpson-based evenness

# Hellinger-transformed BC dissimilarity ----
spe_hell <- decostand(spe, method = "hellinger")
bc_dist  <- vegdist(spe_hell, method = "bray")

#NMDS ----
set.seed(42)
nmds <- metaMDS(spe_hell, distance = "bray", k = 2,
                trymax = 200, autotransform = FALSE, trace = FALSE)
cat(sprintf("Stress = %.4f\n", nmds$stress)) #stress = 0.1623 (acceptable)

plot_scores <- as.data.frame(scores(nmds, display = "sites"))
plot_scores$plot_id <- rownames(plot_scores)
plot_scores$stand <- env$stand
plot_scores$cluster <- NA  

spe_scores <- as.data.frame(scores(nmds, display = "species"))
spe_scores$species <- rownames(spe_scores)
spe_scores$freq <- colSums(spe > 0)  #occurrence frequency

png("figures/fig_shepard.png", width = 800, height = 600, res = 130)
stressplot(nmds, main = "Shepard Plot – NMDS Quality")
dev.off()
stressplot(nmds)

#Ellenberg values ----
ellenberg <- ellenberg[ , c("Taxon", "L", "T", "M", "R", "N", "S")]
ellenberg$Taxon <- trimws(tolower(as.character(ellenberg$Taxon)))

for (v in c("L","T","M","R","N","S")) {
  ellenberg[[v]] <- suppressWarnings(as.numeric(ellenberg[[v]]))
}

# Deduplicate by taking mean per species
ellenberg <- ellenberg %>%
  group_by(Taxon) %>%
  summarise(across(c(L,T,M,R,N,S), mean, na.rm = TRUE),
            .groups = "drop")

cat("Unique species in Ellenberg:", nrow(ellenberg), "\n")

spe_scores$species_key <- trimws(tolower(spe_scores$species))
ell_merged <- merge(spe_scores, ellenberg,
                    by.x = "species_key", by.y = "Taxon")

cat(sprintf("Matched %d of %d species to Ellenberg values\n",
            nrow(ell_merged), nrow(spe_scores))) #some not matched

spe_scores$species_key <- recode(spe_scores$species_key,
                                 "brachythecium rutabulum"  = "brachythecium rutabulum", #may not be in Ellenberg (moss)
                                 "carex acutiformes"        = "carex acutiformis", 
                                 "dryopteris sp."           = "dryopteris filix-mas", #assigned to most common sp.
                                 "euonymus europaeus "      = "euonymus europaeus", 
                                 "galeopsis tetrahit"       = "galeopsis tetrahit", #may not be in Ellenberg
                                 "gymnocarpium spp."        = "gymnocarpium dryopteris", #assigned to common sp.
                                 "hedera helix"             = "hedera helix aggr.", #aggregated in Ellenberg
                                 "poa spec"                 = "poa pratensis aggr.", #assigned to most common
                                 "populus canescens"        = "populus alba", #closest match
                                 "ribes rubrum"             = "ribes rubrum aggr.", #aggregated
                                 "rubus sp."                = "rubus fruticosus aggr.", #aggregated
                                 "ulmus spec."              = "ulmus glabra", #most common
                                 "veronica hederifolia"     = "veronica hederifolia aggr.", #aggregated
                                 "waldsteinia fragarioides" = "waldsteinia fragarioides") #may not be in Ellenberg

ell_merged <- merge(spe_scores, ellenberg,
                    by.x = "species_key", by.y = "Taxon") #remerge

cat(sprintf("Matched %d of %d species after name fixes\n",
            nrow(ell_merged), nrow(spe_scores))) #73/77 now

ell_vars <- c("L","T","M","R","N","S")
for (v in ell_vars) {
  vals <- as.numeric(ell_merged[[v]])
  mask <- !is.na(vals)
  if (sum(mask) < 5) next
  r1 <- cor.test(ell_merged$NMDS1[mask], vals[mask],
                 method = "spearman", exact = FALSE)
  r2 <- cor.test(ell_merged$NMDS2[mask], vals[mask],
                 method = "spearman", exact = FALSE)
  cat(sprintf("  Ellenberg %-2s ~ NMDS1: rho=%+.3f p=%.3f  | NMDS2: rho=%+.3f p=%.3f\n",
              v, r1$estimate, r1$p.value, r2$estimate, r2$p.value))
}

(p_ell_N <- ggplot(ell_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = N, size = freq), alpha = 0.8) +
    geom_text_repel(aes(label = species), size = 2.5,
                    max.overlaps = 30, colour = "#333333",
                    segment.colour = NA) +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg N", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    base_theme)
ggsave("figures/fig_ellenberg_N.png", p_ell_N,
       width = 11, height = 9, dpi = 180, bg = "white")

(p_ell_T <- ggplot(ell_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = T, size = freq), alpha = 0.8) +
    geom_text_repel(aes(label = species), size = 2.5,
                    max.overlaps = 30, colour = "#333333",
                    segment.colour = NA) +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg T", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    base_theme)
ggsave("figures/fig_ellenberg_T.png", p_ell_T,
       width = 11, height = 9, dpi = 180, bg = "white")

(p_ell_M <- ggplot(ell_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = M, size = freq), alpha = 0.8) +
    geom_text_repel(aes(label = species), size = 2.5,
                    max.overlaps = 30, colour = "#333333",
                    segment.colour = NA) +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg M", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    base_theme)
ggsave("figures/fig_ellenberg_M.png", p_ell_M,
       width = 11, height = 9, dpi = 180, bg = "white")

#envfit ----
env_num <- env[ , sapply(env, is.numeric)]
env_num <- env_num[ , !colnames(env_num) %in% "stand"]

ef <- envfit(nmds, env_num, permutations = 999, na.rm = TRUE)
print(ef) #winter water, summer water, Ah, litter, moisture, and NH4 significant (OM borderline)

ef_df <- as.data.frame(scores(ef, display = "vectors"))
ef_df$variable <- rownames(ef_df)
ef_df$r2 <- ef$vectors$r
ef_df$p <- ef$vectors$pvals
ef_df <- ef_df[order(-ef_df$r2), ]

print(ef_df[ef_df$p < 0.05, ])

#PERMANOVA ----
perm_stand <- adonis2(bc_dist ~ stand, data = env,
                      permutations = 999)
print(perm_stand) #by stand; significant

perm_tree <- adonis2(bc_dist ~ dominant_tree, data = env, 
                     permutations = 999) 
print(perm_tree) #by dominant tree species; significant

#Cluster analysis ----
k_clusters <- 4

hc_single   <- hclust(bc_dist, method = "single")
hc_complete <- hclust(bc_dist, method = "complete")
hc_upgma    <- hclust(bc_dist, method = "average")
hc_ward     <- hclust(bc_dist, method = "ward.D")

#higher Cophenetic correlation = dendrogram better representative of BC dissimilarity matrix
coph_single <- cophenetic(hc_single)
coph_complete <- cophenetic(hc_complete)
coph_upgma <- cophenetic(hc_upgma)
coph_ward <- cophenetic(hc_ward)

#correlation
r_single <- cor(bc_dist, coph_single)
r_complete <- cor(bc_dist, coph_complete)
r_upgma <- cor(bc_dist, coph_upgma)
r_ward <- cor(bc_dist, coph_ward)

#Gower distances (lower = better fit)
gow_single   <- sum((bc_dist - coph_single)^2)
gow_complete <- sum((bc_dist - coph_complete)^2)
gow_upgma    <- sum((bc_dist - coph_upgma)^2)
gow_ward     <- sum((bc_dist - coph_ward)^2)

best_method <- c("Single","Complete","UPGMA","Ward")[
  which.max(c(r_single, r_complete, r_upgma, r_ward))]
cat(sprintf("
  Best method by cophenetic correlation: %s", best_method)) #UPGMA = best

hc_best <- list(Single=hc_single, Complete=hc_complete,
                UPGMA=hc_upgma, Ward=hc_ward)[[best_method]] #UPGMA == average

clusters <- cutree(hc_best, k = k_clusters)
plot_scores$cluster <- factor(clusters)
env$cluster <- factor(clusters)

clusters_ward <- cutree(hc_ward, k = k_clusters) #report alongside (better for plotting; makes more clusters)

#twinspan
tw <- twinspan(spe)
plot(tw) #
#At the coarsest level: 2 community types (splits into two nodes)
#Left node (2) splits more = more variation within that branch
#Right node (3) more homogenous
png("figures/fig5b_twinspan.png", width = 1400, height = 900, res = 130)
plot(tw)
dev.off()

summary(tw)
#Node 2 (left) characterised by lack of Primula elatior and presence of Alliaria petiolata and Geranium robertianum → more nitrophilous, disturbed understory
#Node 3 (right) characterised by absence of Adoxa moschatellina → more mesic, less disturbed
#Plots within the same stand mostly cluster together: stand identity is a strong structuring factor; consistent with PERMANOVA result
#The indicator species driving the splits are mostly nitrophilous herbs (Alliaria, Urtica, Galium aparine) and shade-tolerant forest species (Anemone nemorosa, Carex sylvatica, Primula elatior), suggesting a gradient from disturbed/nutrient-rich to stable/shaded forest understory
#Plots 7.3, 9.1, and 6.3 appear as singletons (N=1), flagging them as potential outliers

twintable(tw)
#Left plots: wet, riparian community characterised by Ficaria verna, Primula elatior, Carex sylvatica, Filipendula ulmaria, Anemone nemorosa = indicators of moist, nutrient-rich alluvial forest
#Right plots: drier, more mesic community with Acer pseudoplatanus, Hedera helix, Veronica hederifolia, Geum urbanum = typical of more disturbed or nutrient-enriched forest understory
#Ficaria verna is the most consistent species across almost all plots = a generalist
#Anemone nemorosa, Paris quadrifolia, Carex sylvatica are left-associated = ancient woodland indicators
#Alliaria petiolata, Urtica dioica, Galium aparine are right-associated = nitrophilous disturbance indicators
#The singleton plots (7.3, 9.1, 6.3) stand out visually as isolated columns — confirming they are outliers

#Silhouette for Ward k=4 clusters
sil <- silhouette(clusters, bc_dist)
print(summary(sil))
#Average silhouette width: > 0.5 = reasonable, > 0.7 = strong: here; mean = 0.149 (bad)

png("figures/fig_silhouette.png", width = 800, height = 600, res = 130)
plot(sil, col = CLUST_COLS[1:4],
     border = NA)
dev.off()

cat("\n── Average silhouette width per k ──\n")
for (k in 2:6) {
  cl  <- cutree(hc_ward, k = k)
  s   <- silhouette(cl, bc_dist)
  cat(sprintf("  k = %d  avg silhouette = %.3f\n",
              k, mean(s[, 3])))
} #highest silhouette width for k = 2 (need to finish this)

#RDA ----
rda_vars <- c("winter_water", "summer_water", "Ah", "litter",
              "pH_H2O", "OM", "NO3", "NH4", "moisture")
rda_vars <- rda_vars[rda_vars %in% colnames(env)]
env_rda <- env[ , rda_vars, drop = FALSE]
complete_rows <- complete.cases(env_rda)

spe_rda_full <- spe_hell[complete_rows, , drop = FALSE]
env_rda_full <- env_rda[complete_rows, , drop = FALSE]
env_rda_scaled <- as.data.frame(scale(env_rda_full))

rda_full_model <- rda(spe_rda_full ~ ., data = env_rda_scaled)
rda_null <- rda(spe_rda_full ~ 1, data = env_rda_scaled)

set.seed(42)
rda_forward <- ordistep(rda_null,
                        scope = formula(rda_full_model),
                        direction = "forward",
                        permutations = 999,
                        trace = FALSE)

rda_final <- rda_forward
summary(rda_full_model)
(rda_r2 <- RsquareAdj(rda_full_model)) #R^2 = 0.5374; adjusted = 0.3292

rda_eig_all <- eigenvals(rda_full_model, model = "all")
rda_eig_con <- eigenvals(rda_full_model)
print(round(rda_eig_con / sum(rda_eig_all) * 100, 2)) #% variance explained per axis

rda_null <- rda(spe_rda_full ~ 1, data = env_rda_scaled)

set.seed(42)
rda_forward <- ordistep(rda_null,
                        scope = formula(rda_full_model),
                        direction = "forward",
                        permutations = 999,
                        trace = TRUE)

print(rda_forward) 
#Forward-selected variables: NH4, Ah, litter together explain 34.3% of total variance (R² = 0.343)
#RDA1 = 20.7% of total variance
#RDA2 = 7.6% of total variance
print(RsquareAdj(rda_forward)) #adjusted = 0.2675

rda_final <- rda_forward #with 4 environmental variables
anova.cca(rda_final, permutations = 999) #significant model overall
anova.cca(rda_final, by = "term", permutations = 999)
#NH4 explains most of the variance in community composition
#Ah and NO3 significant, winter water maginal (p = 0.058)
anova.cca(rda_final, by = "axis", permutations = 999)
#RDA 1 explains most (21.4%), RDA2 some (7.2%), RDA3 and RDA4 not significant (both p = 0.076 = borderline?)

soil_vars <- intersect(c("pH_H2O","OM","NO3","NH4","moisture"), rda_vars)
field_vars <- intersect(c("winter_water","summer_water","Ah","litter"), rda_vars)

if (length(soil_vars) > 0 & length(field_vars) > 0) {
  cat("\n── Variance partitioning: soil chemistry vs field measurements ──\n")
  vp <- varpart(spe_rda_full,
                env_rda_scaled[, soil_vars, drop = FALSE],
                env_rda_scaled[, field_vars, drop = FALSE])
  print(vp)
  plot(vp, digits = 2, bg = c("#2A9D8F", "#E9C46A"),
       Xnames = c("Soil chemistry", "Field measurements"),
       main = "Variance Partitioning – Egenhoven Forest") 
}
#Soil chemistry alone: 0.1096
#Field measurement alone: 0.089
#Overlap: 0.144
#Residuals = 0.671

ax1_pct <- round(rda_eig_con[1] / sum(rda_eig_all) * 100, 1)
ax2_pct <- round(rda_eig_con[2] / sum(rda_eig_all) * 100, 1)

rda_site_scores <- as.data.frame(scores(rda_final, display = "sites",   scaling = 2))
rda_spe_scores  <- as.data.frame(scores(rda_final, display = "species", scaling = 2))
rda_bp_scores   <- as.data.frame(scores(rda_final, display = "bp",      scaling = 2))

rda_site_scores$plot_id <- rownames(rda_site_scores)
rda_site_scores$stand   <- env$stand[complete_rows]
rda_spe_scores$species  <- rownames(rda_spe_scores)
rda_spe_scores$freq <- colSums(spe[complete_rows, rownames(rda_spe_scores)] > 0)
rda_bp_scores$variable  <- rownames(rda_bp_scores)

bp_scale <- 0.8 * min(
  max(abs(rda_site_scores[, 1:2])) / max(abs(rda_bp_scores[, 1:2])))
(rda_bp_plot <- rda_bp_scores %>%
  mutate(RDA1s = RDA1 * bp_scale,
         RDA2s = RDA2 * bp_scale))

#need to finish the RDA

#Plots ----
#1: SR and diversity for each stand
div_long <- div %>%
  pivot_longer(cols = c(S, H, Simpson, J, E),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("S","H","Simpson","J","E"),
                         labels = c("Species Richness","Shannon H'",
                                    "Simpson 1-D","Pielou's J",
                                    "Simpson Evenness E")))
(p1 <- ggplot(div_long, aes(x = factor(stand), y = value)) +
    stat_summary(fun = mean, geom = "bar", fill = "#2A9D8F",
                 colour = "white", linewidth = 0.4) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 width = 0.3, linewidth = 0.7) +
    facet_wrap(~metric, scales = "free_y", ncol = 3) +
    labs(x = "Stand", y = NULL) +
    base_theme +
    theme(strip.text = element_text(face = "bold"),
          legend.position = "none")) #dont like the arrangement

make_panel <- function(met, lab) {
  ggplot(div_long %>% filter(metric == lab),
         aes(x = factor(stand), y = value)) +
    stat_summary(fun = mean, geom = "bar", fill = "#2A9D8F",
                 colour = "white", linewidth = 0.4) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 width = 0.3, linewidth = 0.7) +
    labs(x = "Stand", y = NULL, title = lab) +
    base_theme +
    theme(plot.title = element_text(face = "bold", size = 11))
}

p_S <- make_panel("S", "Species Richness")
p_H <- make_panel("H", "Shannon H'")
p_Sim <- make_panel("Sim", "Simpson 1-D")
p_J <- make_panel("J", "Pielou's J")
p_E <- make_panel("E", "Simpson Evenness E")

(p1 <- (p_S | p_H | p_Sim) / (p_J | p_E) +
    plot_layout(heights = c(1, 1)))

ggsave("figures/fig1_diversity.png", p1, width = 14, height = 8,
       dpi = 180, bg = "white")


#Correlation heatmap 
div_metrics <- c("S", "H", "Simpson", "J", "E")
cor_mat <- matrix(NA, nrow = length(corr_vars),
                  ncol = length(div_metrics),
                  dimnames = list(corr_vars, div_metrics))
p_mat <- cor_mat

for (v in corr_vars) {
  for (m in div_metrics) {
    mask <- !is.na(env[[v]])
    if (sum(mask) < 5) next
    r <- cor.test(div[[m]][mask], env[[v]][mask], method = "spearman",
                  exact = FALSE)
    cor_mat[v, m] <- r$estimate
    p_mat[v, m]   <- r$p.value
  }
}

png("figures/fig2_corr_heatmap.png", width = 800, height = 700, res = 130)
corrplot(cor_mat, p.mat = p_mat, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.2, method = "color", type = "full",
         col = colorRampPalette(c("#E63946","white","#2A9D8F"))(200),
         tl.col = "black", tl.cex = 0.9,
         mar = c(0,0,2,0))
dev.off()

corrplot(cor_mat, p.mat = p_mat, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.2, method = "color", type = "full",
         col = colorRampPalette(c("#E63946","white","#2A9D8F"))(200),
         tl.col = "black", tl.cex = 0.9, mar = c(0,0,2,0))


#NMDS stand plot
var_labels <- c(
  winter_water = "WWT",
  summer_water = "SWT",
  Ah = "AhT",
  litter = "Litter",
  pH_H2O = "pH",
  pH_KCl = "pH (KCl)",
  OM = "OM",
  NO3 = "NO3",
  NH4 = "NH4",
  moisture = "SM")

ef_sig <- ef_df[ef_df$p < 0.05, ]
arrow_scale <- 0.7 * max(abs(plot_scores[ , 1:2]))
ef_arrows <- ef_sig %>%
  mutate(NMDS1 = NMDS1 * arrow_scale * sqrt(r2),
         NMDS2 = NMDS2 * arrow_scale * sqrt(r2)) %>% 
  mutate(label = var_labels[variable])

hulls <- plot_scores %>%
  group_by(stand) %>%
  slice(chull(NMDS1, NMDS2))

ef_arrows <- ef_arrows %>%
  mutate(
    label_x = case_when(
      variable == "winter_water" ~ NMDS1 * 2.0,
      variable == "NH4" ~ NMDS1 * 1.4,
      variable == "Ah" ~ NMDS1 * 1.6,
      variable == "moisture" ~ NMDS1 * 1.6,
      TRUE ~ NMDS1 * 1.6),
    label_y = case_when(
      variable == "winter_water" ~ NMDS2 * 0.3,
      variable == "NH4" ~ NMDS2 * 1.4,
      variable == "Ah" ~ NMDS2 * 1.6,
      variable == "moisture" ~ NMDS2 * 1.6,
      TRUE ~ NMDS2 * 1.6))

(p3a <- ggplot(plot_scores, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(fill = factor(stand)), shape = 21, size = 4,
               colour = "white", stroke = 0.8) +
    geom_text_repel(aes(label = plot_id, colour = factor(stand)),
                    size = 2.5, max.overlaps = 10, show.legend = FALSE) +
    geom_segment(data = ef_arrows,
                 aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                 colour = "#222222", linewidth = 0.8, inherit.aes = FALSE) +
    geom_text(data = ef_arrows,
              aes(x = NMDS1 * 1.15, y = NMDS2 * 1.15,
                  label = label),
              size = 3, colour = "#222222", fontface = "bold",
              nudge_y = case_when(
                ef_arrows$variable == "winter_water" ~ 0.04,
                ef_arrows$variable == "NH4" ~ -0,
                TRUE ~  0),
              inherit.aes = FALSE) +
    scale_fill_manual(values = STAND_COLS, name = "Stand") +
    scale_colour_manual(values = STAND_COLS, name = "Stand") +
    labs(x = "NMDS1", y = "NMDS2") +
    base_theme)
ggsave("figures/fig3a_nmds_plots.png", p3a, width = 9, height = 8,
       dpi = 180, bg = "white")

(p3b <- ggplot(plot_scores, aes(x = NMDS1, y = NMDS2)) +
   geom_point(aes(fill = factor(stand)), shape = 21, size = 4,
              colour = "white", stroke = 0.8) +
  geom_polygon(data = hulls,
              aes(fill = factor(stand), colour = factor(stand)),
              alpha = 0.15, linewidth = 0.5, linetype = "dashed") +
   geom_text_repel(aes(label = plot_id, colour = factor(stand)),
                   size = 2.5, max.overlaps = 10, show.legend = FALSE) +
   geom_segment(data = ef_arrows,
                aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                colour = "#222222", linewidth = 0.8, inherit.aes = FALSE) +
   geom_text(data = ef_arrows,
             aes(x = NMDS1 * 1.15, y = NMDS2 * 1.15,
                 label = label),
             size = 3, colour = "#222222", fontface = "bold",
             nudge_y = case_when(
               ef_arrows$variable == "winter_water" ~ 0.04,
               ef_arrows$variable == "NH4" ~ -0,
               TRUE ~  0),
             inherit.aes = FALSE) +
   scale_fill_manual(values = STAND_COLS, name = "Stand") +
   scale_colour_manual(values = STAND_COLS, name = "Stand") +
   labs(x = "NMDS1", y = "NMDS2") +
   base_theme)
ggsave("figures/fig3b_nmds_plots_polygon.png", p3b, width = 9, height = 8,
       dpi = 180, bg = "white")

#NMDS species plot (hard to read)
#species with frequency >= 4
spe_scores_label <- spe_scores %>%
  mutate(label = ifelse(freq >= 4, species, ""),
         size  = case_when(freq >= 8 ~ 3.5,
                           freq >= 5 ~ 3.0,
                           TRUE       ~ 2.5))
centroids <- plot_scores %>%
  group_by(stand) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

hulls <- plot_scores %>%
  group_by(stand) %>%
  slice(chull(NMDS1, NMDS2))

(p4 <- ggplot(spe_scores, aes(x = NMDS1, y = NMDS2)) +
    geom_polygon(data = hulls,
                 aes(x = NMDS1, y = NMDS2, fill = factor(stand),
                     colour = factor(stand)),
                 alpha = 0.1, linewidth = 0.5, linetype = "dashed",
                 inherit.aes = FALSE) +
    geom_point(aes(size = freq), colour = "#888888", alpha = 0.4) +
    geom_text_repel(data = spe_scores %>% filter(freq >= 4),
                    aes(label = species,
                        fontface = ifelse(freq >= 8, "bold", "plain")),
                    size = 2.6, colour = "#555555",
                    max.overlaps = 40,
                    segment.colour = "grey70", segment.size = 0.3) +
    geom_point(data = centroids,
               aes(x = NMDS1, y = NMDS2, fill = factor(stand)),
               shape = 23, size = 4, colour = "white", stroke = 0.8,
               inherit.aes = FALSE) +
    geom_text(data = centroids,
              aes(x = NMDS1, y = NMDS2,
                  label = stand,
                  colour = factor(stand)),
              size = 2.8, fontface = "bold",
              nudge_y = 0.06,
              inherit.aes = FALSE, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
    scale_fill_manual(values = STAND_COLS, name = "Stand") +
    scale_colour_manual(values = STAND_COLS, name = "Stand") +
    scale_size_continuous(range = c(1, 5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    base_theme)
ggsave("figures/fig4_nmds_species.png", p4, width = 13, height = 10,
       dpi = 180, bg = "white")

#4a - species only (species in >= 4 plots; bold for species in >=8 plots)
(p4a <- ggplot(spe_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = freq), colour = "#2A9D8F", alpha = 0.5) +
  geom_text_repel(data = spe_scores %>% filter(freq >= 4),
                  aes(label = species,
                      fontface = ifelse(freq >= 8, "bold", "plain")),
                  size = 2.6, colour = "#1a1a2e",
                  max.overlaps = 40,
                  segment.colour = "grey70", segment.size = 0.3) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(x = "NMDS1", y = "NMDS2") +
  base_theme)

#4b - stands only with hulls
(p4b <- ggplot(plot_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data = hulls,
               aes(fill = factor(stand), colour = factor(stand)),
               alpha = 0.15, linewidth = 0.5, linetype = "dashed") +
  geom_point(aes(fill = factor(stand)), shape = 21, size = 3,
             colour = "white", stroke = 0.8) +
  geom_text(data = centroids,
            aes(x = NMDS1, y = NMDS2,
                label = paste("S", stand, sep = ""),
                colour = factor(stand)),
            size = 3.5, fontface = "bold", inherit.aes = FALSE,
            show.legend = FALSE) +
  scale_fill_manual(values = STAND_COLS, name = "Stand") +
  scale_colour_manual(values = STAND_COLS, name = "Stand") +
  labs(x = "NMDS1", y = "NMDS2") +
  base_theme)


(p4_tri <- ggplot() +
    geom_polygon(data = hulls,
                 aes(x = NMDS1, y = NMDS2,
                     fill = factor(stand), colour = factor(stand)),
                 alpha = 0.15, linewidth = 0.5, linetype = "dashed") +
    geom_point(data = spe_scores,
               aes(x = NMDS1, y = NMDS2, size = freq),
               colour = "#333333", alpha = 0.4) +
    geom_text_repel(data = spe_scores %>% filter(freq >= 4),
                    aes(x = NMDS1, y = NMDS2, label = species,
                        fontface = ifelse(freq >= 8, "bold", "plain")),
                    size = 2.5, colour = "#333333",
                    max.overlaps = 40,
                    segment.colour = "grey70", segment.size = 0.3) +
    scale_fill_manual(values = STAND_COLS, name = "Stand") +
    scale_colour_manual(values = STAND_COLS, name = "Stand") +
    scale_size_continuous(range = c(1, 4), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    base_theme)


#RDA (stand)
(p_rda_stands <- ggplot(rda_site_scores, aes(x = RDA1, y = RDA2)) +
  geom_point(aes(fill = factor(stand)), shape = 21, size = 4,
             colour = "white", stroke = 0.8) +
  geom_text_repel(aes(label = plot_id, colour = factor(stand)),
                  size = 2.8, max.overlaps = 30, show.legend = FALSE) +
  geom_segment(data = rda_bp_plot,
               aes(x = 0, y = 0, xend = RDA1s, yend = RDA2s),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               colour = "#222222", linewidth = 0.9, inherit.aes = FALSE) +
  geom_text(data = rda_bp_plot,
            aes(x = RDA1s * 1.15, y = RDA2s * 1.15, label = variable),
            size = 3, colour = "#222222", fontface = "bold",
            inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
  scale_fill_manual(values = STAND_COLS, name = "Stand") +
  scale_colour_manual(values = STAND_COLS, name = "Stand") +
  labs(x = sprintf("RDA1 (%.1f%% of total variance)", ax1_pct),
       y = sprintf("RDA2 (%.1f%% of total variance)", ax2_pct)) +
  base_theme)
ggsave("figures/fig_rda_stands.png", p_rda_stands, width = 11, height = 9,
       dpi = 180, bg = "white")


#RDA (species)
rda_hulls <- rda_site_scores %>%
  group_by(stand) %>%
  slice(chull(RDA1, RDA2))

rda_centroids <- rda_site_scores %>%
  group_by(stand) %>%
  summarise(RDA1 = mean(RDA1), RDA2 = mean(RDA2))

(p_rda_species <- ggplot() +
  geom_polygon(data = rda_hulls,
               aes(x = RDA1, y = RDA2,
                   fill = factor(stand), colour = factor(stand)),
               alpha = 0.12, linewidth = 0.5, linetype = "dashed") +
  geom_text(data = rda_centroids,
            aes(x = RDA1, y = RDA2,
                label = paste0("S", stand),
                colour = factor(stand)),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  geom_point(data = rda_spe_scores,
             aes(x = RDA1, y = RDA2, size = freq),
             colour = "#333333", alpha = 0.4) +
  geom_text_repel(data = rda_spe_scores %>% filter(freq >= 4),
                  aes(x = RDA1, y = RDA2, label = species,
                      fontface = ifelse(freq >= 8, "bold", "plain")),
                  size = 2.5, colour = "#333333",
                  max.overlaps = 40,
                  segment.colour = "grey70", segment.size = 0.3) +
  geom_segment(data = rda_bp_plot,
               aes(x = 0, y = 0, xend = RDA1s, yend = RDA2s),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               colour = "#222222", linewidth = 0.9, inherit.aes = FALSE) +
  geom_text(data = rda_bp_plot,
            aes(x = RDA1s * 1.15, y = RDA2s * 1.15, label = variable),
            size = 3, colour = "#222222", fontface = "bold",
            inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
  scale_fill_manual(values = STAND_COLS, name = "Stand") +
  scale_colour_manual(values = STAND_COLS, name = "Stand") +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  labs(x = sprintf("RDA1 (%.1f%% of total variance)", ax1_pct),
       y = sprintf("RDA2 (%.1f%% of total variance)", ax2_pct)) +
  base_theme)
ggsave("figures/fig_rda_species.png", p_rda_species, width = 13, height = 10,
       dpi = 180, bg = "white")

#Environmental variables cluster
k_clusters <- 4
clusters <- cutree(hc_ward, k = k_clusters)
table(clusters)
env$cluster         <- factor(clusters)
plot_scores$cluster <- factor(clusters)

cluster_env <- cbind(env, S = div$S, H = div$H)

box_vars <- c("S","H","pH_H2O","OM","NO3","winter_water","Ah","litter")
box_vars <- box_vars[box_vars %in% colnames(cluster_env)]

box_labels <- c(
  S = "Species Richness",
  H = "Shannon H'",
  pH_H2O = "pH (H2O)",
  OM = "Organic Matter (%)",
  NO3 = "NO3 (mg/kg)",
  winter_water = "Winter Water Table (cm)",
  Ah = "Ah Horizon (cm)",
  litter = "Litter Layer (cm)")

box_long <- cluster_env %>%
  select(cluster, all_of(box_vars)) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = box_vars,
                           labels = box_labels[box_vars]))

(p6 <- ggplot(box_long, aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1.5,
               colour = "grey30") +
  facet_wrap(~variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = CLUST_COLS, name = "Cluster") +
  labs(x = "Cluster", y = NULL) +
  base_theme +
  theme(strip.text = element_text(face = "bold")))
ggsave("figures/fig6_cluster_env.png", p6, width = 14, height = 7,
       dpi = 180, bg = "white")

#NMDS by Ward cluster for environmental variables:
(p7 <- ggplot(plot_scores, aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(colour = cluster), level = 0.80,
               linetype = "dashed", linewidth = 0.6, alpha = 0.6) +
  geom_point(aes(fill = cluster), shape = 21, size = 4,
             colour = "white", stroke = 0.8) +
  geom_text_repel(aes(label = plot_id, colour = cluster),
                  size = 2.8, max.overlaps = 30, show.legend = FALSE) +
  geom_segment(data = ef_arrows,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               colour = "#222222", linewidth = 0.8, inherit.aes = FALSE) +
  geom_label_repel(data = ef_arrows,
                   aes(x = NMDS1 * 1.15, y = NMDS2 * 1.15,
                       label = paste0(variable, "\nR²=", round(r2,2),
                                      ", p=", round(p, 3))),
                   size = 2.5, colour = "#222222", fill = "white",
                   inherit.aes = FALSE, max.overlaps = 20) +
  scale_fill_manual(values = CLUST_COLS, name = "Cluster") +
  scale_colour_manual(values = CLUST_COLS, name = "Cluster") +
  labs(x = "NMDS1", y = "NMDS2") +
  base_theme)
ggsave("figures/fig7_nmds_cluster.png", p7, width = 11, height = 9,
       dpi = 180, bg = "white")


#envfit bar chart
ef_plot <- ef_df %>%
  mutate(sig   = ifelse(p < 0.05, "p < 0.05", "n.s."),
         label = sub("_", " ", variable))

(p8 <- ggplot(ef_plot, aes(x = reorder(label, r2), y = r2, fill = sig)) +
  geom_col(colour = "white") +
  coord_flip() +
  scale_fill_manual(values = c("p < 0.05" = "#2A9D8F", "n.s." = "#adb5bd"),
                    name = NULL) +
  labs(x = NULL, y = expression(R^2)) +
  base_theme +
  theme(legend.position = c(0.8, 0.2)))
ggsave("figures/fig8_envfit.png", p8, width = 8, height = 5,
       dpi = 180, bg = "white")


#Species richness/diversity vs env variables 
scatter_df1 <- data.frame(
  env_value = env$pH_KCl,
  diversity = div$S,
  stand = env$stand) %>% filter(!is.na(env_value))

scatter_df2 <- data.frame(
  env_value = env$NH4,
  diversity = div$Simpson,
  stand = env$stand) %>% filter(!is.na(env_value))

(p9a <- ggplot(scatter_df1, aes(x = env_value, y = diversity,
                               colour = factor(stand))) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "black",
              linewidth = 0.8, alpha = 0.15, inherit.aes = FALSE,
              aes(x = env_value, y = diversity)) +
  scale_colour_manual(values = STAND_COLS, name = "Stand") +
  labs(x = "pH (KCl)", y = "Species Richness (S)") +
  base_theme +
  theme(legend.position = "none"))

(p9b <- ggplot(scatter_df2, aes(x = env_value, y = diversity,
                               colour = factor(stand))) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "black",
              linewidth = 0.8, alpha = 0.15, inherit.aes = FALSE,
              aes(x = env_value, y = diversity)) +
  scale_colour_manual(values = STAND_COLS, name = "Stand") +
  labs(x = "NH4-N (mg/kg)", y = "Simpson 1-D") +
  base_theme)

(p9 <- p9a | p9b)
ggsave("figures/fig9_diversity_env.png", p9, width = 12, height = 5,
       dpi = 180, bg = "white")


#stacked bar plot (dominant trees):
tree_df <- env %>%
  mutate(plot_id = plot_ids) %>%
  count(stand, dominant_tree) %>%
  group_by(stand) %>%
  mutate(prop = n / sum(n))

(p_10 <- ggplot(tree_df, aes(x = factor(stand), y = prop,
                               fill = dominant_tree)) +
  geom_col(colour = "white", linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Stand", y = "Proportion of Plots",
       fill = "Dominant tree") +
  base_theme +
  theme(legend.text = element_text(size = 7)))
ggsave("figures/fig_10_trees.png", p_10, width = 10, height = 5,
       dpi = 180, bg = "white")
