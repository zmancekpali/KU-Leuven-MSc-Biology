#######################################################################
#           Terrestrial Ecology Final – Egenhoven Forest 2026         #
#                             Group 4                                 #
#######################################################################

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Terrestrial Ecology")
getwd()
dir.create("figuresFinal", showWarnings = FALSE)

#Libraries
library(ade4)
library(vegan)
library(readxl)
library(rstatix)
library(ggnewscale)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tibble)
library(tidyr)
library(RColorBrewer)
library(cluster)
library(corrplot)
library(ggdendro)
library(patchwork)

#Data
#Data (plots) ----
data <- as.data.frame(read_excel("data.xlsx", sheet = "Combined")) %>%
  column_to_rownames(var = names(.)[1]) %>%
  rename_with(trimws) %>%
  rename_with(~ ifelse(suppressWarnings(!is.na(as.numeric(.x))),
                       as.character(round(as.numeric(.x), 1)),
                       .x)) %>%
  rename_with(~ gsub("[^[:alnum:]._]", "_", .x)) 
head(data)
str(data)

species <- data[1:77,] #species only
species_num <- species %>%
  mutate(across(everything(), as.numeric)) %>% #as.numeric for everything
  t() %>% #plots = rows, species = columns
  as.data.frame() #reformat as data frame from matrix (t())

plots <- data[78:82,] #environmental variables only

environment <- plots %>% t() %>% #transpose
  as.data.frame() %>% #back to data frame
  rownames_to_column("plot") %>% #plot ID as separate column
  mutate(plot = gsub("^X", "", plot)) %>% #remove leading X
  rename(dominant_tree = `Dominant tree species`,
         winter_water = `Winter water`,
         summer_water  = `Summer water`,
         Ah = `Ah`,
         litter = `litter layer`) %>% #renaming to R-friendly
  mutate(winter_water = as.numeric(winter_water),
         summer_water = as.numeric(summer_water),
         Ah = as.numeric(Ah),
         litter = as.numeric(litter), #all as.numeric
         stand = as.integer(sub("\\..*", "", plot))) %>% #stand only numebrs for each stand
  column_to_rownames("plot") #plot = column names

lab <- read_excel("lab.xlsx", sheet = "results") %>%
  as.data.frame() %>%
  rename_with(trimws) %>%
  mutate(plot = gsub("-", ".", trimws(as.character(Plot))),
         plot = gsub("^3\\.", "11.", plot),
         plot = gsub("^4\\.", "12.", plot)) %>%
  select(plot, moisture = Moisture, OM = `Organic Matter`,
         pH_H2O = `Ph H2O`, pH_KCl = `Ph KCl`,
         NO3 = `mg NO3/kg soil`, NH4 = `mg NH4-N/kg soil`) #select and rename

environment <- environment %>% 
  select(dominant_tree, winter_water, summer_water, Ah, litter, stand) %>%
  rownames_to_column("plot") %>%
  left_join(lab, by = "plot") %>%
  column_to_rownames("plot") #env + lab together; joined by stand

ellenberg <- read_excel("Ellenberg Indicator values-2022-11-07.xlsx", sheet = "Tab-IVs-Tichy-et-al2023") %>%
  as.data.frame() %>%
  rename_with(trimws) %>% 
  rename(SeqID = `...1`,
         species = `...2`,
         L = LIGHT,
         ellenberg_T = TEMPERATURE,
         M = MOISTURE,
         R = REACTION,
         N = NUTRIENTS,
         ellenberg_S = SALINITY) %>%
  slice(-1) %>% 
  mutate(across(c(L, ellenberg_T, M, R, N, ellenberg_S), ~ suppressWarnings(as.numeric(.)))) %>%
  select(species, L, ellenberg_T, M, R, N, ellenberg_S) #changed to ellenberg_S/T because I use S for species richness later and T is TRUE in R
head(ellenberg)

#Diversity indices + species richness: ----
S <- specnumber(species_num)
H <- diversity(species_num, "shannon")
D1 <- diversity(species_num, "simpson")
D2 <- diversity(species_num, "inv")
Simpson <- 1-D1
J <- H/log(S)
E <- D2/S

diversity <- data.frame(plot = rownames(species_num), S = S, H = H,
                        J = J, E = E, D1 = Simpson) #D1 = Gini-Simpson index (higher 1-D = higher diversity)

merged <- cbind(diversity, environment[diversity$plot, ])
kruskal.test(S ~ stand, data = merged) #borderline (0.05164)
kruskal.test(H ~ stand, data = merged) #significant (0.02932)
dunn_test(merged, H ~ stand) #all n.s.
kruskal.test(D1 ~ stand, data = merged) #significant (0.01169)
dunn_test(merged, D1 ~ stand) #all n.s.
kruskal.test(J ~ stand, data = merged) #significant (0.02776)
dunn_test(merged, J ~ stand) #all n.s.
kruskal.test(E ~ stand, data = merged) #significant (0.01138)
dunn_test(merged, E ~ stand) #all n.s.

#Hill numbers ----
renyi <- renyi(species_num, scales = c(0,1,2), hill=FALSE)
plot(renyi)
hill <- renyi(species_num, scales = c(0,1,2), hill=TRUE)
plot(hill) #deprecated plotting code

#Hill numbers in ggplot
hill_df <- as.data.frame(hill)
hill_df$plot <- rownames(hill_df)
hill_df$stand <- sub("\\..*", "", hill_df$plot) #get individual stands and not quadrants

hill_long <- hill_df %>%
  pivot_longer(cols = c(`0`, `1`, `2`),
               names_to = "order",
               values_to = "diversity") %>%
  mutate(order = as.numeric(order),
         stand = factor(stand, levels = as.character(sort(unique(as.numeric(stand))))))

(p_hill_facet <- ggplot(hill_long, aes(x = order, y = diversity,
                                       group = plot, colour = stand)) +
    geom_line(alpha = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = c(0, 1, 2),
                       labels = c("0", "1", "2")) +
    labs(x = "Order q", y = "Effective number of species (Hill numbers)", colour = "Stand") +
    facet_wrap(~stand, ncol = 5) +
    theme_classic())
ggsave("figuresFinal/hill_numbers.png", p_hill_facet, width = 12, height = 7)

(Ne <- exp(diversity$H)) #effective species number
hill_df$Ne <- exp(diversity$H)
hill_df$stand <- factor(hill_df$stand, levels = c("1", "2", "5", "6", "7", "8", "9", "10", "11", "12"))

(p_hill_pp <- ggplot(hill_df, aes(x = stand, y = Ne, fill = stand)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 2) +
    labs(y = "Ne (q = 1)",
         x = "Stand") +
    theme_classic() +
    theme(legend.position = "none"))
ggsave("figuresFinal/hill_numbers_per_plot.png", p_hill_pp, width = 12, height = 7)

diversity %>%
  mutate(stand = sub("\\..*", "", plot),
         Ne = exp(H)) %>%
  group_by(stand) %>%
  summarise(mean_Ne = mean(Ne)) %>%
  arrange(desc(mean_Ne))

#Dissimilarities ----
D_Jaccard <- vegdist(species_num, method="jaccard", binary=TRUE)
D_Sorensen <- vegdist(species_num, method="bray", binary=TRUE)
D_Bray <- vegdist(species_num, method="bray")
D_hellinger <- decostand(species_num, method = "hellinger")

#Similarities ----
S_Jaccard <- 1 - D_Jaccard
S_Sorensen <- 1 - D_Sorensen
S_Bray <- 1 - D_Bray

#Correlations between diversity/SR and environmental variables----
#S:
cor.test(merged$S, merged$pH_H2O, method = "spearman") #n.s. 
cor.test(merged$S, merged$pH_KCl, method = "spearman") #negative; significant (0.04754)
cor.test(merged$S, merged$moisture, method = "spearman") #n.s.
cor.test(merged$S, merged$OM, method = "spearman") #n.s.
cor.test(merged$S, merged$NO3, method = "spearman") #negative; borderline (0.06744)
cor.test(merged$S, merged$NH4, method = "spearman") #n.s.
cor.test(merged$S, merged$winter_water, method = "spearman") #negative; significant (0.02011)
cor.test(merged$S, merged$summer_water, method = "spearman") #negative; significant (0.0387)
cor.test(merged$S, merged$Ah, method = "spearman") #n.s.
cor.test(merged$S, merged$litter, method = "spearman") #n.s.

#H':
cor.test(merged$H, merged$pH_H2O, method = "spearman") #n.s.
cor.test(merged$H, merged$pH_KCl, method = "spearman") #negative; borderline (0.06105)
cor.test(merged$H, merged$moisture, method = "spearman") #n.s.
cor.test(merged$H, merged$OM, method = "spearman") #n.s.
cor.test(merged$H, merged$NO3, method = "spearman") #n.s.
cor.test(merged$H, merged$NH4, method = "spearman") #n.s.
cor.test(merged$H, merged$winter_water, method = "spearman") #n.s.
cor.test(merged$H, merged$summer_water, method = "spearman") #n.s.
cor.test(merged$H, merged$Ah, method = "spearman") #n.s.
cor.test(merged$H, merged$litter, method = "spearman") #n.s.

#Simpson::
cor.test(merged$D1, merged$pH_H2O, method = "spearman") #n.s.
cor.test(merged$D1, merged$pH_KCl, method = "spearman") #positive; borderline (0.08272)
cor.test(merged$D1, merged$moisture, method = "spearman") #n.s.
cor.test(merged$D1, merged$OM, method = "spearman") #n.s.
cor.test(merged$D1, merged$NO3, method = "spearman") #n.s.
cor.test(merged$D1, merged$NH4, method = "spearman") #n.s.
cor.test(merged$D1, merged$winter_water, method = "spearman") #negative, significant (0.04151)
cor.test(merged$D1, merged$summer_water, method = "spearman") #n.s.
cor.test(merged$D1, merged$Ah, method = "spearman") #n.s.
cor.test(merged$D1, merged$litter, method = "spearman") #n.s.


#Or a faster way: 
env_cols <- c("pH_H2O", "pH_KCl", "moisture", "OM",
              "NO3", "NH4", "winter_water", "summer_water", "Ah", "litter")
merged[env_cols] <- lapply(merged[env_cols], as.numeric)
cors <- cor(merged[, c("S", "H", "D1")],
            merged[, env_cols],
            method = "spearman",
            use = "pairwise.complete.obs") #gives all Spearman correlations for each env variable and each diversity measure


pvals <- cor.mtest(merged[, c("S", "H", "D1", env_cols)], method = "spearman")
p_subset <- pvals$p[c("S", "H", "D1"), 
                    c("winter_water", "summer_water", "Ah", "litter",
                      "moisture", "OM", "pH_H2O", "pH_KCl", "NO3", "NH4")]

p_df <- as.data.frame(p_subset) %>%
  rownames_to_column("Index") %>%
  mutate(Index = recode(Index, "D1" = "1-D")) %>%
  pivot_longer(-Index, names_to = "Variable", values_to = "pval")

cor_df <- data.frame(
  Index = c("S", "H", "1-D"),
  rbind(cors[1,], cors[2,], cors[3,])) %>%
  pivot_longer(-Index, names_to = "Variable", values_to = "r") %>%
  left_join(p_df, by = c("Index", "Variable")) %>%
  mutate(fontface = case_when(
    pval <= 0.05 ~ "bold",
    pval <= 0.10 ~ "italic",
    TRUE ~ "plain"))

(corr_plot <- ggplot(cor_df, aes(x = Variable, y = Index, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(r, 2), fontface = fontface), size = 3.5) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "#D64045",
                         midpoint = 0, limits = c(-1, 1),
                         name = expression("Spearman "*rho)) +
    theme_classic() +
    scale_y_discrete(labels = c("H" = "H'", "S" = "SR", "1-D" = "1-D")) +
    scale_x_discrete(labels = c(
      "Ah" = "AhT",
      "litter" = "Litter",
      "moisture" = "SM",
      "NH4" = expression(NH[4]),
      "NO3" = expression(NO[3]),
      "OM" = "OM",
      "pH_H2O" = expression(pH~(H[2]*O)),
      "pH_KCl" = "pH (KCl)",
      "summer_water" = "SWT",
      "winter_water" = "WWT")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(x = NULL, y = NULL))
ggsave("figuresFinal/env_div_corr.png", corr_plot, width = 12, height = 7)

#NMDS (chord) ----
set.seed(123)
names(colSums(species_num)[colSums(species_num) == 0])
sum(species_num$`Gymnocarpium spp.`)
species_num <- species_num[, colnames(species_num) != "Gymnocarpium spp."]
names(colSums(species_num)[colSums(species_num) == 0]) #Removing species with cover = 0

nmdsChord <- metaMDS(decostand(species_num, "norm"), distance = "euclidean")
nmdsChord$stress #0.13699 with k = 2
stressplot(nmdsChord, main = "Shepard plot") #great

site_scores <- as.data.frame(scores(nmdsChord, display = "sites"))
site_scores$plot <- rownames(site_scores)
site_scores$stand <- sub("\\..*", "", site_scores$plot)

species_scores <- as.data.frame(scores(nmdsChord, display = "species"))
species_scores$species <- rownames(species_scores)

env_num <- merged[rownames(site_scores), env_cols]
env_num[] <- lapply(env_num, as.numeric)

nmds_envfit <- envfit(nmdsChord, env_num, permutations = 999, na.rm = TRUE)
print(nmds_envfit) #winter water, Ah, OM, moisture, and NH4 significant 

envfit_df <- as.data.frame(scores(nmds_envfit, display = "vectors"))
envfit_df$variable <- rownames(envfit_df)
envfit_df$p <- nmds_envfit$vectors$pvals
envfit_sig <- envfit_df[envfit_df$p <= 0.05, ]

#permanova
stand <- sub("\\..*", "", rownames(species_num)) #plot number (no subplot)
perm_chord <- adonis2(species_num ~ stand, method = "chord", permutations = 999)
#significant; 57.12% of variance in community composition explained by stand

#Clustering (UPGMA w chord) ----
chord_dist <- vegdist(decostand(species_num, "norm"), method = "euclidean")
clust_upgma <- hclust(chord_dist, method = "average")  # average = UPGMA
cor(chord_dist, cophenetic(clust_upgma)) #0.940
plot(clust_upgma, hang = -1, main = "UPGMA Chord dendrogram")
rect.hclust(clust_upgma, k = 2, border = "green")  
rect.hclust(clust_upgma, k = 3, border = "red") 
rect.hclust(clust_upgma, k = 4, border = "blue") #leaves one stand-alone

#Sillhouette
groups3 <- cutree(clust_upgma, k = 3)
sil3 <- silhouette(groups3, chord_dist)
plot(sil3, border = NA)
summary(sil3)

groups11 <- cutree(clust_upgma, k = 11)
sil11 <- silhouette(groups11, chord_dist)
plot(sil11, border = NA)

groups2 <- cutree(clust_upgma, k = 2)
sil2 <- silhouette(groups2, chord_dist)
png("figuresFinal/silhouette_k2.png", width = 800, height = 600, res = 130)
plot(sil2, border = NA)
dev.off()
summary(sil2)

sil_widths <- sapply(2:11, function(k) {
  g <- cutree(clust_upgma, k = k)
  mean(silhouette(g, chord_dist)[, "sil_width"])
})

png("figuresFinal/silhouette_k_optimisation.png", width = 800, height = 600, res = 130)
plot(2:11, sil_widths, type = "b", xlab = "k", 
     ylab = "Mean silhouette width", main = "Optimal k")
dev.off()

sil <- silhouette(groups2, chord_dist)
print(summary(sil)) #2 clusters is best
#Average silhouette width: > 0.5 = reasonable, > 0.7 = strong: here; mean = 0.23765 (bad)
png("figuresFinal/fig_silhouette.png", width = 800, height = 600, res = 130)
plot(sil, border = NA) #3 clusters is best
dev.off()


kt <- data.frame(k = 1:nrow(species_num), r = 0)

for (i in 2:(nrow(species_num) - 1)) {
  gr <- cutree(clust_upgma, i)
  distgr <- as.dist(outer(gr, gr, function(a, b) as.numeric(a != b)))
  mt <- cor(chord_dist, distgr, method = "pearson")
  kt[i, 2] <- mt
}
k.best <- which.max(kt$r)

plot(kt$k, kt$r, type = "h", 
     main = "Mantel-optimal number of clusters, UPGMA",
     xlab = "k (number of groups)", 
     ylab = "Pearson's correlation")
axis(1, k.best, paste("optimum", k.best, sep = "\n"), 
     col = "red", font = 2, col.axis = "red")
points(k.best, max(kt$r), pch = 16, col = "red", cex = 1.5) #Says 11 double check

plot(clust_upgma$height, nrow(species_num):2, type="S", main="Fusion levels - Chord - UPGMA",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(clust_upgma$height, nrow(species_num):2, nrow(species_num):2, col="red", cex = 0.8);


#2 CLUSTERS = NOW BEST
dend_data <- dendro_data(clust_upgma, type = "rectangle")
labels_df <- dend_data$labels
labels_df$cluster <- factor(groups2[as.character(labels_df$label)])

rect_df <- labels_df %>%
  group_by(cluster) %>%
  summarise(xmin = min(x) - 0.5,
            xmax = max(x) + 0.5) %>%
  mutate(ymin = -0.05,
         ymax = 1.4)

(p_dend <- ggplot() +
    geom_rect(data = rect_df,
              aes(xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax,
                  fill = cluster),
              alpha = 0.15, colour = NA) +
    geom_segment(data = dend_data$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = labels_df,
              aes(x = x, y = 0, label = label, colour = cluster),
              angle = 90, hjust = 1, size = 3) +
    scale_colour_brewer(palette = "Set1", name = "Cluster") +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.05))) +
    labs(x = NULL, y = "Chord distance") +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()))
ggsave("figuresFinal/dendrogram_chord_upgma_k=2.png", p_dend, width = 12, height = 7) #kinda ugly 

#Clustering KW----
merged$cluster <- factor(groups2[rownames(merged)])

kruskal.test(winter_water ~ cluster, data = merged) #significant (0.004919)
dunn_test(winter_water ~ cluster, data = merged) #significant (0.00492)
kruskal.test(summer_water ~ cluster, data = merged) #n.s.
kruskal.test(Ah ~ cluster, data = merged) #significant (0.004794)
dunn_test(Ah ~ cluster, data = merged) #significant (0.00479)
kruskal.test(litter ~ cluster, data = merged) #n.s.
kruskal.test(moisture ~ cluster, data = merged) #significant (0.005114)
dunn_test(moisture ~ cluster, data = merged) #significant (0.00511)
kruskal.test(pH_KCl ~ cluster, data = merged) #significant (0.04881)
dunn_test(pH_KCl ~ cluster, data = merged) #significant (0.0488)
kruskal.test(NO3 ~ cluster, data = merged) #n.s.
kruskal.test(NH4 ~ cluster, data = merged) #significant (0.01708)
dunn_test(NH4 ~ cluster, data = merged) #significant (0.0171)


merged %>%
  group_by(cluster) %>%
  summarise(
    median_WWT = median(winter_water, na.rm = TRUE),
    median_Ah = median(Ah, na.rm = TRUE),
    median_moisture = median(moisture, na.rm = TRUE),
    median_pH_KCl = median(pH_KCl, na.rm = TRUE),
    median_NH4 = median(NH4, na.rm = TRUE))


#NMDS plot + envfit + clusters ----
site_scores$cluster <- factor(groups2[rownames(site_scores)])
site_scores$stand <- sub("\\..*", "", rownames(site_scores))
site_scores$stand <- factor(site_scores$stand, 
                            levels = as.character(sort(unique(as.numeric(site_scores$stand)))))
hulls <- site_scores %>%
  group_by(cluster) %>%
  slice(chull(NMDS1, NMDS2))

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
  moisture = "SM") #renaming so it looks nicer on the plot with envfit
envfit_sig$label <- var_labels[envfit_sig$variable]

cluster_colours <- RColorBrewer::brewer.pal(2, "Set1")  #2 colours for 2 clusters
cluster_fills <- RColorBrewer::brewer.pal(2, "Pastel1")  #pastel of above

(p_nmds <- ggplot(data = site_scores, aes(x = NMDS1, y = NMDS2)) +
    geom_polygon(data = hulls,
                 aes(x = NMDS1, y = NMDS2, group = cluster,
                     fill = cluster, colour = cluster),
                 linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    scale_colour_manual(values = cluster_colours, name = "Cluster") +
    new_scale_colour() +
    geom_point(aes(colour = stand), size = 2) +
    geom_text_repel(aes(label = plot, colour = stand),
                    size = 2.5, show.legend = FALSE) +
    geom_segment(data = envfit_sig,
                 aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 colour = "black", linewidth = 0.4) +
    geom_text_repel(data = envfit_sig,
                    aes(x = NMDS1 * 1.1, y = NMDS2 * 1, label = label),
                    size = 3, colour = "black", fontface = "bold") +
    scale_colour_discrete(name = "Stand") +
    annotate("text", x = Inf, y = -Inf,
             label = paste("Stress =", round(nmdsChord$stress, 3)),
             hjust = 1.1, vjust = -0.5, size = 3, fontface = "bold") +
    theme_classic() +
    labs(x = "NMDS1", y = "NMDS2"))
ggsave("figuresFinal/nmds+cluster+envfit_k=2.png", p_nmds, width = 9, height = 7)

#Ellenberg: ----
colnames(species_num) <- trimws(colnames(species_num))
colnames(species_num) <- gsub("\u00a0", "", colnames(species_num)) #E. europaeus has some weird spacing issue
sum(colnames(species_num) %in% ellenberg$species) #63 matches between the two datasets

colnames(species_num)[!colnames(species_num) %in% ellenberg$species] #the ones that don't match between the two datasets (6)
ellenberg$species[grepl("^Poa", ellenberg$species)] #could be any of them
ellenberg$species[grepl("^Ulmus", ellenberg$species)] #renaming it to U. minor
ellenberg$species[grepl("^Rubus", ellenberg$species)] #could be any of them
ellenberg$species[grepl("^Dryopteris", ellenberg$species)] #could be any of them
ellenberg$species[grepl("^Quercus", ellenberg$species)] #there is no Quercus rubra in Ellenberg
ellenberg$species[grepl("^Populus", ellenberg$species)] #no Populus canescens in Ellenberg
ellenberg$species[grepl("^Ribes", ellenberg$species)] #aggregate found
ellenberg$species[grepl("^Veronica", ellenberg$species)] #same as above
ellenberg$species[grepl("^Waldsteinia", ellenberg$species)] #no W. fragarioides in Ellenberg
ellenberg$species[grepl("^Brachythecium", ellenberg$species)] #none at all in Ellenberg
ellenberg$species[grepl("^Hedera", ellenberg$species)] #aggregate found
ellenberg$species[grepl("^Galeopsis", ellenberg$species)] #aggregate found
colnames(species_num) <- dplyr::recode(colnames(species_num),
                                       "Carex acutiformes" = "Carex acutiformis",    
                                       "Euonymus europaeus " = "Euonymus europaeus", 
                                       "Hedera helix" = "Hedera helix aggr.",
                                       "Galeopsis tetrahit" = "Galeopsis tetrahit aggr.", 
                                       "Ribes rubrum" = "Ribes rubrum aggr.",
                                       "Veronica hederifolia" = "Veronica hederifolia aggr.")     
colnames(species_num)[colnames(species_num) == "Euonymus europaeus "] <- "Euonymus europaeus"
colnames(species_num)[!colnames(species_num) %in% ellenberg$species] #the ones that don't match between the two datasets (9)
#didnt rename Ulmus spec. and Dryopteris sp. because it creates a duplication error with other data with correct species names

#now matching the ellenberg values to the species
L_vals <- ellenberg$L[match(colnames(species_num), ellenberg$species)]
R_vals <- ellenberg$R[match(colnames(species_num), ellenberg$species)]
M_vals <- ellenberg$M[match(colnames(species_num), ellenberg$species)] 
T_vals <- ellenberg$ellenberg_T[match(colnames(species_num), ellenberg$species)]
N_vals <- ellenberg$N[match(colnames(species_num), ellenberg$species)]
S_vals <- ellenberg$ellenberg_S[match(colnames(species_num), ellenberg$species)]

#Converting for matrices:
ellenberg_vals <- data.frame(L = L_vals, ellenberg_T = T_vals,
                             M = M_vals, R = R_vals, N = N_vals, ellenberg_S = S_vals, row.names = colnames(species_num)) #matrix of the ellenberg variables

#species and ellenberg matrices
species_cover_matrix <- as.matrix(species_num)
ellenberg_matrix <- as.matrix(ellenberg_vals)

#Ellenberg values per sample plot: 
ellenberg_per_plot <- as.data.frame(
  species_cover_matrix %*% replace(ellenberg_matrix, is.na(ellenberg_matrix), 0) /
    (species_cover_matrix %*% !is.na(ellenberg_matrix)))
print(ellenberg_per_plot) #now we have each ellenberg value for each plot of the forest

#Correlations between NMDS1 sites and ellenberg variables: (with chord NMDS different)
nmds_scores <- as.data.frame(scores(nmdsChord, display = "sites"))
cor.test(nmds_scores$NMDS1, ellenberg_per_plot$L, method = "spearman") #n.s.
cor.test(nmds_scores$NMDS1, ellenberg_per_plot$ellenberg_T, method = "spearman") #negative, borderline (0.05831)
cor.test(nmds_scores$NMDS1, ellenberg_per_plot$M, method = "spearman") #n.s.
cor.test(nmds_scores$NMDS1, ellenberg_per_plot$R, method = "spearman") #negative, significant (6.414e-05)
cor.test(nmds_scores$NMDS1, ellenberg_per_plot$N, method = "spearman") #negative, significant ( 0.004371)
cor.test(nmds_scores$NMDS1, ellenberg_per_plot$ellenberg_S, method = "spearman") #n.s

#Correlations between NMDS2 sites and ellenberg values:
cor.test(nmds_scores$NMDS2, ellenberg_per_plot$L, method = "spearman") #positive; significant (0.007712)
cor.test(nmds_scores$NMDS2, ellenberg_per_plot$ellenberg_T, method = "spearman") #positive, significant (0.0002555)
cor.test(nmds_scores$NMDS2, ellenberg_per_plot$M, method = "spearman") #positive, significant (0.02189)
cor.test(nmds_scores$NMDS2, ellenberg_per_plot$R, method = "spearman") #n.s.
cor.test(nmds_scores$NMDS2, ellenberg_per_plot$N, method = "spearman") #n.s.
cor.test(nmds_scores$NMDS2, ellenberg_per_plot$ellenberg_S, method = "spearman") #negative; significant (7.214e-05)

#Correlations between ellenberg values and our measured values:
cor.test(ellenberg_per_plot$R, merged$pH_H2O, method = "spearman") #n.s.
cor.test(ellenberg_per_plot$R, merged$pH_KCl, method = "spearman") #positive, significant (0.0225)
#pH values somewhat correlated with R (reaction)
cor.test(ellenberg_per_plot$ellenberg_S, merged$pH_KCl, method = "spearman") #n.s.
cor.test(ellenberg_per_plot$M, merged$moisture, method = "spearman") #n.s.
cor.test(ellenberg_per_plot$N, merged$NO3, method = "spearman") #positive; significant (0.005027)
cor.test(ellenberg_per_plot$N, merged$NH4, method = "spearman") #positive, significant (0.02793)

#Ellenberg plots ----
ellenberg_merged <- merge(species_scores, ellenberg, by = "species")

#Polygons for the plots (with two clusters)
hull1 <- site_scores[site_scores$cluster == 1, ] %>% slice(chull(NMDS1, NMDS2))
hull2 <- site_scores[site_scores$cluster == 2, ] %>% slice(chull(NMDS1, NMDS2))
hulls <- rbind(hull1, hull2)
hull_colours <- cluster_colours[as.numeric(hulls$cluster)]

#Ellenberg N:
(p_ell_N_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = N), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 colour = hull_colours,
                 alpha = 0.4, linewidth = 0.8, linetype = "dashed") +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               size = 1, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold", max.overlaps = 10) +
    scale_colour_gradientn(
      colours = c("forestgreen", "white", "orchid3"),
      name = "Ellenberg N", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())

#Ellenberg T:
(p_ell_T_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = ellenberg_T), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 colour = hull_colours,
                 alpha = 0.4, linewidth = 0.8, linetype = "dashed") +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               size = 1, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold", max.overlaps = 10) +
    scale_colour_gradientn(
      colours = c("forestgreen", "white", "orchid3"),
      name = "Ellenberg T", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())

#Ellenberg M:
(p_ell_M_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = M), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 colour = hull_colours,
                 alpha = 0.4, linewidth = 0.8, linetype = "dashed") +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               size = 1, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold", max.overlaps = 10) +
    scale_colour_gradientn(
      colours = c("forestgreen", "white", "orchid3"),
      name = "Ellenberg F", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())

#Ellenberg R:
(p_ell_R_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = R), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 colour = hull_colours,
                 alpha = 0.4, linewidth = 0.8, linetype = "dashed") +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               size = 1, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold", max.overlaps = 10) +
    scale_colour_gradientn(
      colours = c("forestgreen", "white", "orchid3"),
      name = "Ellenberg R", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())

#Ellenberg S:
(p_ell_S_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = ellenberg_S), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 colour = hull_colours,
                 alpha = 0.4, linewidth = 0.8, linetype = "dashed") +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               size = 1, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold", max.overlaps = 10) +
    scale_colour_gradientn(
      colours = c("forestgreen", "white", "orchid3"),
      name = "Ellenberg S", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())

#Ellenberg L:
(p_ell_L_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = L), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 colour = hull_colours,
                 alpha = 0.4, linewidth = 0.8, linetype = "dashed") +
    scale_fill_manual(values = cluster_fills, name = "Cluster") +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               size = 1, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold", max.overlaps = 10) +
    scale_colour_gradientn(
      colours = c("forestgreen", "white", "orchid3"),
      name = "Ellenberg L", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())

#Grid of all ellenberg values:
(ellenberg_grid <- ((p_ell_N_species_plots + p_ell_T_species_plots + p_ell_M_species_plots) / (p_ell_R_species_plots + p_ell_S_species_plots + p_ell_L_species_plots)))
ggsave("figuresFinal/ellenberg_grid.png", ellenberg_grid, width = 15, height = 7, dpi = 180, bg = "white")

#Rank-abundance plot ----
rank_abund <- species_num %>%
  rownames_to_column("plot") %>%
  mutate(stand = sub("\\..*", "", plot)) %>%
  pivot_longer(-c(plot, stand), names_to = "species", values_to = "cover") %>%
  filter(cover > 0) %>%
  group_by(stand, species) %>%
  summarise(mean_cover = mean(cover), .groups = "drop") %>%
  group_by(stand) %>%
  arrange(desc(mean_cover)) %>%
  mutate(rank = row_number(),
         rel_cover = mean_cover / sum(mean_cover)) %>%
  ungroup() %>%
  mutate(stand = factor(stand, levels = as.character(sort(unique(as.numeric(stand))))))

top_species <- rank_abund %>%
  group_by(stand) %>%
  slice(1) %>%
  mutate(label = paste0(stand, ": ", species))

(p_rankabund <- ggplot(rank_abund, aes(x = rank, y = rel_cover, colour = stand)) +
    geom_line(alpha = 0.8) +
    geom_text_repel(data = top_species,
                    aes(x = 1, y = rel_cover,
                        label = paste0("(", stand, ") ", species)),
                    size = 2.5, show.legend = FALSE,
                    direction = "y",
                    hjust = 1,
                    nudge_x = 10,
                    max.overlaps = 20,
                    fontface = "italic",
                    segment.colour = "grey60",
                    segment.size = 0.3) +
    scale_y_log10() +
    scale_x_continuous(limits = c(0, 40)) +
    scale_colour_discrete(name = "Stand") +
    labs(x = "Species rank", y = "Relative cover (log scale)") +
    theme_classic())
ggsave("figuresFinal/rank_abundance.png", p_rankabund, width = 10, height = 7)
