#######################################################################
#             Terrestrial Ecology (2) – Egenhoven Forest 2026         #
#                             Group 4                                 #
#######################################################################

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Terrestrial Ecology")
getwd()
dir.create("figures2", showWarnings = FALSE)

#Libraries
library(vegan)
library(readxl)
library(ggplot2)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(dplyr)
library(tibble)
library(tidyr)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(corrplot)
library(patchwork)
library(ggtext)
library(twinspan)

#Data
#Data (plots) ----
data <- as.data.frame(read_excel("data.xlsx", sheet = "Combined")) %>% #import
  column_to_rownames(var = names(.)[1]) %>% #makes 'species' the row names
  rename_with(trimws) %>% #removes trailing spaces
  rename_with(~ ifelse(suppressWarnings(!is.na(as.numeric(.x))), 
                       as.character(round(as.numeric(.x), 1)),
                       .x)) %>% #removes floating decimals for better formatting
  rename_with(~ gsub("[^[:alnum:]._]", "_", .x)) #replaces non-letter/number/./_ characters with _
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
  mutate(plot = gsub("-", ".", trimws(as.character(Plot))), #remove characters, trim trailing spaces
         plot = gsub("^3\\.", "11.", plot), #plot 3 = plot 11
         plot = gsub("^4\\.", "12.", plot)) %>% #plot 4 = plot 12
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

diversity <- data.frame(plot = rownames(species_num), S = S, H = H, Simpson = Simpson,
  J = J, E = E)

boxplot(S~stand, data = merged)

#ANOVA for each diversity metric
merged <- cbind(diversity, environment[diversity$plot, ])
merged$stand <- sub("\\..*", "", rownames(merged))
s_lm <- lm(S ~ stand, data = merged)
plot(s_lm)
shapiro.test(resid(s_lm))
summary(aov(S ~ stand, data = merged)) #significant; species richness differs between the stands
summary(aov(H ~ stand, data = merged)) #significant; H' differs between the stands
summary(aov(Simpson ~ stand, data = merged)) #significant; Simpson's D differs between the stands
summary(aov(J ~ stand, data = merged)) #significant, evenness differs between the stands
summary(aov(E ~ stand, data = merged)) #significant, E differs between the stands

  #Post-hoc tests
  TukeyHSD(aov(S ~ stand, data = merged))
    #overall ANOVA significant but Tukey shows no specific pairs differ in species richness (low statistical power?)
  
  TukeyHSD(aov(H ~ stand, data = merged))
    #Stands 11 and 2 differ significantly: stand 11 has higher H'
    #Stands 11 and 9 differ significantly: stand 11 has higher H'
    #Stands 5 and 2 differ significantly: stand 5 has higher H'
    #Stands 5 and 9 differ significantly: stand 5 has higher H'
    table(merged$dominant_tree, merged$stand)
      #Stand 2: dominant tree = Q. robur
      #Stand 11: dominant trees = C. betulus, F. sylvatice, and P. abies
      #Stand 9: dominant tree: A. pseudoplatanus
      #Stand 5: dominant trees: A. pseudoplatanus and F. sylvatica
  
  TukeyHSD(aov(Simpson ~ stand, data = merged))
    #Stands 12 + 1 AND 2 + 1 marginally significant (both higher Simpson than 1)
    #Stands 9 and 1: stand 9 has higher Simpson's diversity 
    #Stands 12 and 11: stand 12 has higher Simpson's diversity
    #Stands 2 and 11: stand 2 has higher Simpson's diversity
    #Stands 6 and 11: stand 6 has higher Simpson's diversity
    #Stands 9 and 11: stand 9 has higher Simpson's diversity
    #Stands 5 and 12: stand 12 has higher Simpson's diversity
    #Stands 5 and 2: stand 2 has higher Simpson's diversity
    #Stands 5 and 6: stand 6 has higher Simpson's diversity
    #Stands 5 and 9: stand 9 has higher Simpson's diversity
    
  TukeyHSD(aov(J ~ stand, data = merged))
    #Stands 2 + 1 (1 more even) AND 6 + 11 (11 more even) marginally different
    #Stands 11 and 12: stand 11 is more even
    #Stands 2 and 11: stand 11 is more even
    #Stands 9 and 11: stand 11 is more even
    #Stands 5 and 2: stand 5 is more even
  
  TukeyHSD(aov(E ~ stand, data = merged))
    #Stands 12 and 1: stand 1 is more even
    #Stands 2 and 1: stand 1 is more even
    #Stands 6 and 1: stand 1 is more even
    #Stands 7 and 1: marginally different (1 more even)
    #Stands 9 and 1: stand 1 is more even
    #Stands 11 and 10: stand 11 is more even
    #Stands 12 and 11: stand 11 is more even
    #Stands 2 and 11: stand 11 is more even
    #Stands 6 and 11: stand 11 is more even
    #Stands 7 and 11: stand 11 is more even
    #Stands 8 and 11: stand 11 is more even
    #Stands 9 and 11: stand 11 is more even
    #Stands 5 and 12: stand 5 is more even
    #Stands 5 and 2:: stand 5 is more even
    #Stands 6 and 5: stand 5 is more even
    #Stands 9 and 5: stand 5 is more even

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
      labs(x = "Order q", y = "Effective number of species (exp H')", colour = "Stand") +
      facet_wrap(~stand, ncol = 5) +
      theme_classic())
  ggsave("figures2/hill_numbers.png", p_hill_facet, width = 12, height = 7)

  (Ne <- exp(diversity$H)) #effective species number
  hill_df$Ne <- exp(diversity$H)
  hill_df$stand <- factor(hill_df$stand, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

  (p_hill_pp <- ggplot(hill_df, aes(x = stand, y = Ne, fill = stand)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 2) +
    labs(y = "Effective number of species (exp H')",
         x = "Stand") +
    theme_classic() +
    theme(legend.position = "none"))
  ggsave("figures2/hill_numbers_per_plot.png", p_hill_pp, width = 12, height = 7)
  
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
merged <- cbind(diversity, environment[diversity$plot, ])

#S:
cor.test(merged$S, merged$pH_H2O, method = "spearman") #n.s. 
cor.test(merged$S, merged$pH_KCl, method = "spearman") #negative; significant
cor.test(merged$S, merged$moisture, method = "spearman") #n.s.
cor.test(merged$S, merged$OM, method = "spearman") #n.s.
cor.test(merged$S, merged$NO3, method = "spearman") #negative; borderline
cor.test(merged$S, merged$NH4, method = "spearman") #n.s.
cor.test(merged$S, merged$winter_water, method = "spearman") #negative; significant
cor.test(merged$S, merged$summer_water, method = "spearman") #negative; significant
cor.test(merged$S, merged$Ah, method = "spearman") #n.s.
cor.test(merged$S, merged$litter, method = "spearman") #n.s.

#H':
cor.test(merged$H, merged$pH_H2O, method = "spearman") #n.s.
cor.test(merged$H, merged$pH_KCl, method = "spearman") #negative; borderline
cor.test(merged$H, merged$moisture, method = "spearman") #n.s.
cor.test(merged$H, merged$OM, method = "spearman") #n.s.
cor.test(merged$H, merged$NO3, method = "spearman") #n.s.
cor.test(merged$H, merged$NH4, method = "spearman") #n.s.
cor.test(merged$H, merged$winter_water, method = "spearman") #n.s.
cor.test(merged$H, merged$summer_water, method = "spearman") #n.s.
cor.test(merged$H, merged$Ah, method = "spearman") #n.s.
cor.test(merged$H, merged$litter, method = "spearman") #n.s.

#Simpson::
cor.test(merged$Simpson, merged$pH_H2O, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$pH_KCl, method = "spearman") #positive; borderline
cor.test(merged$Simpson, merged$moisture, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$OM, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$NO3, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$NH4, method = "spearman") #negative; significant
cor.test(merged$Simpson, merged$winter_water, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$summer_water, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$Ah, method = "spearman") #n.s.
cor.test(merged$Simpson, merged$litter, method = "spearman") #n.s.

#Or a faster way: 
env_cols <- c("pH_H2O", "pH_KCl", "moisture", "OM",
              "NO3", "NH4", "winter_water", "summer_water", "Ah", "litter")
merged[env_cols] <- lapply(merged[env_cols], as.numeric)
cors <- cor(merged[, c("S", "H", "Simpson")],
    merged[, env_cols],
    method = "spearman",
    use = "pairwise.complete.obs") #gives all Spearman correlations for each env variable and each diversity measure



cor_df <- data.frame(
  Index = c("S", "H", "Simpson"),
  rbind(cors[1,], cors[2,], cors[3,])) %>%   #your three correlation vectors
  pivot_longer(-Index, names_to = "Variable", values_to = "r")

(corr_plot <- ggplot(cor_df, aes(x = Variable, y = Index, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3.5) +
  scale_fill_gradient2(low = "#D64045", mid = "white", high = "steelblue",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Spearman r") +
  theme_classic() +
  scale_y_discrete(labels = c("H" = "H'", "S" = "SR", "Simpson" = "Simpson's D")) +
  scale_x_discrete(labels = c(
      "Ah" = "Ah",
      "litter" = "Litter",
      "moisture" = "Moisture",
      "NH4" = expression(NH[4]),
      "NO3" = expression(NO[3]),
      "OM" = "OM",
      "pH_H2O" = expression(pH~(H[2]*O)),
      "pH_KCl"= "pH (KCl)",
      "summer_water" = "SWT",
      "winter_water" = "WWT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = NULL, y = NULL))
ggsave("figures2/env_div_corr.png", p_hill_pp, width = 12, height = 7)

#NMDS ----
names(colSums(species_num)[colSums(species_num) == 0])
sum(species_num$`Gymnocarpium spp.`)
species_num <- species_num[, colnames(species_num) != "Gymnocarpium spp."]
names(colSums(species_num)[colSums(species_num) == 0])

#Bray NMDS
set.seed(123)
nmdsBray <- metaMDS(species_num, distance="bray", k=3)
plot(nmdsBray, type='t')
nmdsBray$stress #0.138 (with k = 3) and 0.210 with k = 2

#Chord NMDS
nmdsChord <- metaMDS(decostand(species_num,"norm"),distance="euclidean")
nmds <- nmdsChord
nmdsChord$stress #0.137 (basically identical)
stressplot(nmds, main="Shepard plot") #good

site_scores <- as.data.frame(scores(nmdsChord, display = "sites"))
species_scores <- as.data.frame(scores(nmdsChord, display = "species"))
species_scores$species <- rownames(species_scores)
nmdsChord$species <- wascores(nmdsChord$points, species_num)

site_scores$plot <- rownames(site_scores)
site_scores$stand <- sub("\\..*", "", site_scores$plot)
species_scores$species <- rownames(species_scores)

plot(nmdsChord, display = "sites", type = "t")
plot(nmdsChord, display = "species", type = "t")
plot(nmdsChord, type = "t") #biplot

#envfit
env_cols <- c("winter_water", "summer_water", "Ah", "litter",
              "moisture", "OM", "pH_H2O", "pH_KCl", "NO3", "NH4")
env_num <- merged[, env_cols]
env_num[] <- lapply(env_num, as.numeric)#only looking at the environmental columns

nmds_envfit <- envfit(nmdsChord, env_num, permutations = 999, na.rm = TRUE)
#wnter water, Ah, litter, moisture, and NH4 significant, summer water borderline
print(nmds_envfit)

envfit_df <- as.data.frame(scores(nmds_envfit, display = "vectors"))

plot(nmdsChord, type='t')
plot(nmds_envfit, p.max = 0.05)

#permanova
stand <- sub("\\..*", "", rownames(species_num)) #plot number (no subplot)
perm_bray <- adonis2(species_num ~ stand, method = "bray", permutations = 999)
#significant; 61.85% of variance in community composition explained by plot

perm_chord <- adonis2(species_num ~ stand, method = "chord", permutations = 999)
#significant; 57.12% of variance in community composition explained by plot

#Clustering ----
bray <- vegdist(species_num, method="bray")

single <- hclust(bray, "single")
complete <- hclust(bray, "complete")
UPGMA <- hclust(bray, "average")
Ward <- hclust(bray, "ward.D")

par(mfrow=c(2,2))

plot(single, hang = -1)
plot(complete, hang = -1)
plot(UPGMA, hang = -1)
plot(Ward, hang = -1)

#Cophenetic correlations (higher = better)
single_coph <- cophenetic(single)
cor_single <- cor(bray, single_coph) #0.8374173

complete_coph <- cophenetic(complete)
cor_complete <- cor(bray, complete_coph) #0.6708201

UPGMA_coph <- cophenetic(UPGMA)
cor_UPGMA <- cor(bray, UPGMA_coph) #0.8573125 (BEST)

ward_coph <- cophenetic(Ward)
cor_ward <- cor(bray, ward_coph) #0.5640871

#Gower's distance (lower = better)
(gow_single <- sum((bray - single_coph)^2)) #14.80681
(gow_complete <- sum((bray - complete_coph)^2)) #20.26683
(gow_UPGMA <- sum((bray - UPGMA_coph)^2)) #3.223047 (BEST)
(gow_ward <- sum((bray - ward_coph)^2)) #858.014

#Sillhouette
clusters <- cutree(UPGMA, k = 2)
sil <- silhouette(clusters, bray)
print(summary(sil))
#Average silhouette width: > 0.5 = reasonable, > 0.7 = strong: here; mean = 0.23765 (bad)
png("figures2/fig_silhouette.png", width = 800, height = 600, res = 130)
plot(sil, border = NA)
dev.off()

par(mfrow=c(1,1))

#OR a faster way to find the best cluster ----
(best_method <- c("Single","Complete","UPGMA","Ward")[
  which.max(c(cor_single, cor_complete, cor_UPGMA, cor_ward))]) #UPGMA is best

plot(UPGMA, hang = -1)
rect.hclust(UPGMA, 2) #2 clusters makes most sense

#Chord clustering: ----
grpdist <- function(X)
{require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- daisy(gr, "gower")
  distgr}
kt <- data.frame(k=1:nrow(species_num), r=0)
for (i in 2:(nrow(species_num)-1)){
  gr <- cutree(UPGMA, i)
  distgr <- grpdist(gr)
  mt <- cor(bray, distgr, method="pearson")
  kt[i,2] <- mt}
k.best <- which.max(kt$r)
plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, UPGMA",
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
#8 clusters best (way too many)

plot(UPGMA$height, nrow(species_num):2, type="S", main="Fusion levels - Chord - UPGMA",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(UPGMA$height, nrow(species_num):2, nrow(species_num):2, col="red", cex = 0.8);
#See jumps from 8 - 9, 7 - 8, and 
#Gotta pick lower than 11 cause too much either 8 or 5
# when I rerun kt, 11 clusters has a lower r than 10, and highest is technically 8 but thats too many still

#it keeps climbing to 11 but that's way too much, settle down for 5 or so because the climb isn't that big either way:
#k         r
#1   1 0.0000000
#2   2 0.6015669
#3   3 0.6131204
#4   4 0.6364870
#5   5 0.6400609
#6   6 0.7216084
#7   7 0.7471950
#8   8 0.7488695 (highest)
#9   9 0.7432208
#10 10 0.7243540
#11 11 0.6859614


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
#didnt rename Ulmust spec. and Dryopteris sp. because it creates a duplication error with other data with correct species names

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

  #removing NAs:
  ellenberg_vals <- ellenberg_vals %>%
    mutate(across(everything(), ~ replace_na(., 0))) 

  #species and ellenberg matrices
  species_cover_matrix <- as.matrix(species_num)
  ellenberg_matrix <- as.matrix(ellenberg_vals)
  
  #Ellenberg values per sample plot: 
  ellenberg_per_plot <- as.data.frame(species_cover_matrix %*% ellenberg_matrix / rowSums(species_cover_matrix))
  print(ellenberg_per_plot) #now we have each ellenberg value for each plot of the forest

  #Correlations between NMDS1 sites and ellenberg variables: (with chord NMDS different)
  nmds_scores <- as.data.frame(scores(nmdsBray, display = "sites"))
  cor.test(nmds_scores$NMDS1, ellenberg_per_plot$L, method = "spearman") #n.s.
  cor.test(nmds_scores$NMDS1, ellenberg_per_plot$ellenberg_T, method = "spearman") #n.s
  cor.test(nmds_scores$NMDS1, ellenberg_per_plot$M, method = "spearman") #n.s.
  cor.test(nmds_scores$NMDS1, ellenberg_per_plot$R, method = "spearman") #negative, significant
  cor.test(nmds_scores$NMDS1, ellenberg_per_plot$N, method = "spearman") #n.s.
  cor.test(nmds_scores$NMDS1, ellenberg_per_plot$ellenberg_S, method = "spearman") #n.s
  #NMDS1(Bray) captures reaction out of the ellenberg values variation
  
  #Correlations between NMDS2 sites and ellenberg values:
  cor.test(nmds_scores$NMDS2, ellenberg_per_plot$L, method = "spearman") #positive; significant
  cor.test(nmds_scores$NMDS2, ellenberg_per_plot$ellenberg_T, method = "spearman") #positive, significant
  cor.test(nmds_scores$NMDS2, ellenberg_per_plot$M, method = "spearman") #negative, borderline (p = 0.077)
  cor.test(nmds_scores$NMDS2, ellenberg_per_plot$R, method = "spearman") #n.s.
  cor.test(nmds_scores$NMDS2, ellenberg_per_plot$N, method = "spearman") #positive, significant
  cor.test(nmds_scores$NMDS2, ellenberg_per_plot$ellenberg_S, method = "spearman") #positive; significant

  #Correlations between ellenberg values and our measured values:
  cor.test(ellenberg_per_plot$R, merged$pH_H2O, method = "spearman") #n.s.
  cor.test(ellenberg_per_plot$R, merged$pH_KCl, method = "spearman") #positive, significant
    #pH values correlated with R (reaction)
  cor.test(ellenberg_per_plot$ellenberg_S, merged$pH_KCl, method = "spearman") #n.s.
  cor.test(ellenberg_per_plot$M, merged$moisture, method = "spearman") #n.s.
  cor.test(ellenberg_per_plot$N, merged$NO3, method = "spearman") #positive; significant 
  cor.test(ellenberg_per_plot$N, merged$NH4, method = "spearman") #positive, significant

  
#NMDS + Cluster analysis plots----
  #cluster analysis
  UPGMA <- hclust(bray, "average") #same as above
  plot(UPGMA, hang = -1, main = "UPGMA cluster dendrogram")
  rect.hclust(UPGMA, k = 2, border = "green") #two clusters make most sense
  rect.hclust(UPGMA, k = 3, border = "blue") #here one plot (11.2) is stand-alone as a cluster
  rect.hclust(UPGMA, k = 4, border = "red") #here 11.2 and 9.1 are stand-alone custers
  
  groups <- cutree(UPGMA, k = 3)
  plot(nmds, display = "sites", type = "n")
  text(nmds, display = "sites", labels = rownames(species_num), col = groups)
  ordihull(nmds, groups, col = c("black", "red", "green"), lwd = 2) #two clusters; even when k = 3, with 11.2 alone
  #distinct groups visible: black and red are the biggest; with green and blue stand alone
  
  #Nicer plot for three original clusters
  site_scores$cluster <- as.factor(groups[site_scores$plot])
  hull1 <- site_scores[site_scores$cluster == 1, ] %>% slice(chull(NMDS1, NMDS2))
  hull2 <- site_scores[site_scores$cluster == 2, ] %>% slice(chull(NMDS1, NMDS2))
  hull3 <- site_scores[site_scores$cluster == 3, ] %>% slice(chull(NMDS1, NMDS2))
  hulls <- rbind(hull1, hull2, hull3)
  
  (p_clust <- ggplot(site_scores, aes(NMDS1, NMDS2, color = cluster, label = plot)) +
      geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                   alpha = 0.1, colour = NA) +
      geom_point(size = 3) +
      geom_text_repel(size = 2.5) +
      theme_classic()) #two distinct clusters with 11.2 alone
  ggsave("figures2/cluster_nmds_threeclusters.png", p_clust, width = 7, height = 6)
  
  
  groups4 <- cutree(UPGMA, k = 4)
  plot(nmds, display = "sites", type = "n")
  text(nmds, display = "sites", labels = rownames(species_num), col = groups4)
  ordihull(nmds, groups, col = c("black", "red"), lwd = 2) #two clear groups 
  
  #Nicer plot for four clusters (original)
  site_scores$cluster <- as.factor(groups4[site_scores$plot])
  hull1 <- site_scores[site_scores$cluster == 1, ] %>% slice(chull(NMDS1, NMDS2))
  hull2 <- site_scores[site_scores$cluster == 2, ] %>% slice(chull(NMDS1, NMDS2))
  hull3 <- site_scores[site_scores$cluster == 3, ] %>% slice(chull(NMDS1, NMDS2))
  hull4 <- site_scores[site_scores$cluster == 4, ] %>% slice(chull(NMDS1, NMDS2))
  hulls <- rbind(hull1, hull2, hull3, hull4)
  (p_clust4 <- ggplot(site_scores, aes(NMDS1, NMDS2, color = cluster, label = plot)) +
      geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                   alpha = 0.1, colour = NA) +
      geom_point(size = 3) +
      geom_text_repel(size = 2.5) +
      theme_classic()) #two distinct clusters; with 2 stand-alone stands
  ggsave("figures2/cluster_nmds_4clusters.png", p_clust4, width = 7, height = 6)
  
  #BEST ONE (imo):
  groups2 <- cutree(UPGMA, k = 2)
  plot(nmds, display = "sites", type = "n")
  text(nmds, display = "sites", labels = rownames(species_num), col = groups2)
  ordihull(nmds, groups, col = c("black", "red"), lwd = 2) #two clear groups 
  
  #Nicer plot for two clusters
  site_scores$cluster <- as.factor(groups2[site_scores$plot])
  hull1 <- site_scores[site_scores$cluster == 1, ] %>% slice(chull(NMDS1, NMDS2))
  hull2 <- site_scores[site_scores$cluster == 2, ] %>% slice(chull(NMDS1, NMDS2))
  hulls <- rbind(hull1, hull2)
  (p_clust2 <- ggplot(site_scores, aes(NMDS1, NMDS2, color = cluster, label = plot)) +
      geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                   alpha = 0.1, colour = NA) +
      geom_point(size = 3) +
      geom_text_repel(size = 2.5) +
      theme_classic()) #two distinct clusters
  ggsave("figures2/cluster_nmds.png", p_clust2, width = 7, height = 6)
  
  
  #cluster analysis with chord (looks bad) ----
  chord <- vegdist(species_num, method = "chord")
  chord_clust <- hclust(chord, "ward.D2")
  groups <- cutree(chord_clust, k = 8)
  
  site_scores$cluster <- as.factor(groups[site_scores$plot])
  
  hulls <- site_scores %>%
    group_by(cluster) %>%
    slice(chull(NMDS1, NMDS2))
  
  (p_clust_chord <- ggplot(site_scores, aes(NMDS1, NMDS2, color = cluster, label = plot)) +
      geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                   alpha = 0.1, colour = NA) +
      geom_point(size = 3) +
      geom_text_repel(size = 2.5) +
      theme_classic()) #terrible
  ggsave("figures2/cluster_nmds_8clusters_chord.png", p_clust_chord, width = 7, height = 6)
  
  
  
  #Comparing each of the two clusters with the lab variables: ---- (probably delete)
  merged$cluster <- as.factor(groups2[rownames(merged)])
  boxplot(stand ~ cluster, data = merged, main = "stand by cluster") #similar
  boxplot(pH_H2O ~ cluster, data = merged, main = "pH(H2O) by cluster") #Similar pH for both; cluster 1 more variability
  boxplot(pH_KCl ~ cluster, data = merged, main = "pH(KCL) by cluster") #Similar pH for both; cluster 1 more variability and outliers
  boxplot(moisture ~ cluster, data = merged, main = "Moisture by cluster") #group two lower soil moisture
  boxplot(OM ~ cluster, data = merged, main = "Organic matter by cluster") #similar with two outliers
  boxplot(NO3 ~ cluster, data = merged, main = "NO3 by cluster") #cluster 2 very high variability; means similar
  boxplot(NH4 ~ cluster, data = merged, main = "NH4 by cluster") #very different
  boxplot(litter ~ cluster, data = merged, main = "Litter by cluster") #cluster two = high variability
  boxplot(winter_water ~ cluster, data = merged, main = "Winter water table by cluster") #very different
  boxplot(summer_water ~ cluster, data = merged, main = "Summer water table by cluster") #cluster 2 = just a line
  boxplot(Ah ~ cluster, data = merged, main = "Ah by cluster") #different
  boxplot(OM ~ cluster, data = merged, main = "OM by cluster") #similar (two outliers)
  
  summary(aov(stand ~ cluster, data = merged)) #n.s.
  summary(aov(pH_H2O ~ cluster, data = merged)) #n.s.
  summary(aov(pH_KCl ~ cluster, data = merged)) #n.s.
  summary(aov(moisture ~ cluster, data = merged)) #significant; soil moisture differs between the two clusters
  summary(aov(OM ~ cluster, data = merged)) #n.s.
  summary(aov(NO3 ~ cluster, data = merged)) #significant; NO3 differs between the two clusters
  summary(aov(NH4 ~ cluster, data = merged)) #significant; NH4 differs between the clusters
  summary(aov(litter ~ cluster, data = merged)) #n.s.
  summary(aov(winter_water ~ cluster, data = merged)) #significant; winter water table differs between the clusters
  summary(aov(summer_water ~ cluster, data = merged)) #n.s.
  summary(aov(Ah ~ cluster, data = merged)) #significant; Ah differs between the clusters
  summary(aov(OM ~ cluster, data = merged)) #n.s.
  
  
  #comparing each cluster with the diversity variables
  boxplot(S ~ cluster, data = merged, main = "S by cluster") #Similar 
  boxplot(H ~ cluster, data = merged, main = "H' by cluster") #similar, one outlier
  boxplot(Simpson ~ cluster, data = merged, main = "Simpson by cluster") #similar 
  boxplot(J ~ cluster, data = merged, main = "J by cluster") #similar ish
  boxplot(E ~ cluster, data = merged, main = "E by cluster") #somewhat different; 3 outliers
  table(merged$dominant_tree, merged$cluster) #some differences in the dominant trees for each cluster
  
  summary(aov(S ~ cluster, data = merged)) #n.s.
  summary(aov(H ~ cluster, data = merged)) #n.s.
  summary(aov(Simpson ~ cluster, data = merged)) #n.s.
  summary(aov(J ~ cluster, data = merged)) #n.s.
  summary(aov(E ~ cluster, data = merged)) #n.s.
  #no significant differences in species richness, diversity, or evenness across the clusters

#Diversity and SR plots: ----
diversity_long <- diversity %>%
  mutate(stand = sub("\\..*", "", plot), #separating them by plot (not quadrant) and ordering them in ascending order
         stand = factor(stand, levels = sort(unique(as.numeric(sub("\\..*", "", plot)))))) %>%  
  pivot_longer(cols = c(S, H, Simpson, J, E),
         names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("S","H","Simpson","J","E"),
                         labels = c("Species Richness","Shannon H'",
                                    "Simpson 1-D","Pielou's J",
                                    "Simpson Evenness E")))

  (p1 <- ggplot(diversity_long, aes(x = stand, y = value, fill = stand)) +
    stat_summary(fun = mean, geom = "bar", linewidth = 0.4) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 width = 0.3, linewidth = 0.7) +
    facet_wrap(~metric, scales = "free_y", ncol = 3) +
    labs(x = "Stand", y = NULL) +
    theme_classic() +
    theme(strip.text = element_text(face = "bold"),
          legend.position = "none")) 

ggsave("figures2/fig1_diversity_indices.png", p1, width = 14, height = 8,
       dpi = 180, bg = "white")
  
#NMDS plots (without clusters) ----
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

env_scores <- as.data.frame(scores(nmds_envfit, display = "vectors"))
env_scores$variable <- rownames(env_scores)
env_scores$pval <- nmds_envfit$vectors$pvals
env_scores$r2 <- nmds_envfit$vectors$r
env_sig <- env_scores[env_scores$pval <= 0.05, ] #only keeping environmental variables with p < 0.05 from envfit
env_sig$label <- var_labels[env_sig$variable] #adding above labels

groups2 <- cutree(UPGMA, k = 2)
plot(nmdsBray, display = "sites", type = "n")
text(nmdsBray, display = "sites", labels = rownames(species_num), col = groups2)
ordihull(nmdsBray, groups, col = c("black", "red"), lwd = 2) #???

site_scores$cluster <- as.factor(groups2[site_scores$plot])
hull1 <- site_scores[site_scores$cluster == 1, ] %>% slice(chull(NMDS1, NMDS2))
hull2 <- site_scores[site_scores$cluster == 2, ] %>% slice(chull(NMDS1, NMDS2))
hulls <- rbind(hull1, hull2)

(p_envfit <- ggplot() +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2, colour = stand), size = 3) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 alpha = 0.1, colour = NA) +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot), size = 2.5) +
    geom_segment(data = env_sig, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.3, "cm")), colour = "black") +
    geom_text_repel(data = env_sig, aes(x = NMDS1, y = NMDS2, label = label),
                    size = 3, colour = "black", fontface = "bold") +
    labs(x = "NMDS1", y = "NMDS2", colour = "Stand") +
    theme_classic()) #two clusters; 5 env. drivers
ggsave("figures2/nmds+envfit+clusters.png", p_envfit, width = 9, height = 8,
       dpi = 180, bg = "white")

#NMDS species plot ----
names(colSums(species_num)[colSums(species_num) == 0])
sum(species_num$`Gymnocarpium spp.`)
species_num <- species_num[, colnames(species_num) != "Gymnocarpium spp."]
names(colSums(species_num)[colSums(species_num) == 0])

#Bray NMDS
set.seed(123)
nmdsBray <- metaMDS(species_num, distance="bray", k=3)
plot(nmdsBray, type='t')
nmdsBray$stress #0.138 (with k = 3) and 0.210 with k = 2

site_scores <- as.data.frame(scores(nmdsBray, display = "sites"))
species_scores <- as.data.frame(scores(nmdsBray, display = "species"))
species_scores$species <- rownames(species_scores)
nmdsBray$species <- wascores(nmdsBray$points, species_num)

site_scores$plot <- rownames(site_scores)
site_scores$stand <- sub("\\..*", "", site_scores$plot)
species_scores$species <- rownames(species_scores)

plot(nmdsBray, display = "sites", type = "t")
plot(nmdsBray, display = "species", type = "t")
plot(nmdsBray, type = "t") #biplot

spe_scores <- as.data.frame(nmdsBray$species)
colnames(spe_scores)[1:2] <- c("NMDS1", "NMDS2")
spe_scores$species <- rownames(spe_scores)

spe_scores$freq <- colSums(species_num > 0)[spe_scores$species] #how many plots does a species occur in?

site_scores$cluster <- as.factor(groups2[site_scores$plot]) #SAME CLUSTERS AS ABOVE (k=2)
hull1 <- site_scores[site_scores$cluster == 1, ] %>% slice(chull(NMDS1, NMDS2))
hull2 <- site_scores[site_scores$cluster == 2, ] %>% slice(chull(NMDS1, NMDS2))
hulls <- rbind(hull1, hull2)

(p_sp_envfit <- ggplot() +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster, fill = cluster),
                 alpha = 0.1, colour = NA) +
    geom_point(data = spe_scores, aes(x = NMDS1, y = NMDS2, size = freq),
               colour = "#888888", alpha = 0.5) +
    geom_text_repel(data = spe_scores[spe_scores$freq >= 4, ],
                    aes(x = NMDS1, y = NMDS2, label = species),
                    size = 2.5, colour = "#555555", max.overlaps = 40,
                    fontface = "italic") +
    geom_segment(data = env_sig, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.3, "cm")), colour = "black") +
    geom_text_repel(data = env_sig, aes(x = NMDS1, y = NMDS2, label = label),
                    size = 3, colour = "black", fontface = "italic") +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
    scale_size_continuous(range = c(1, 5), name = "Plot occurrence \nfrequency") +
    labs(x = "NMDS1", y = "NMDS2", fill = "Cluster") +
    theme_classic()) #hard to read
ggsave("figures2/nmds_species + envfit + clusters.png", p_sp_envfit, width = 13, height = 10,
       dpi = 180, bg = "white")


#Ellenberg plots ----
ellenberg_merged <- merge(spe_scores, ellenberg, by = "species")

#Polygons for the plots (with two clusters)
hull1 <- site_scores[site_scores$cluster == 1, ] %>%
  slice(chull(NMDS1, NMDS2))
hull2 <- site_scores[site_scores$cluster == 2, ] %>%
  slice(chull(NMDS1, NMDS2))
hulls <- rbind(hull1, hull2)

#Ellenberg N:
(p_ell_N_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
   geom_point(aes(colour = N), alpha = 0.8) +
   geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster),
                fill = NA, colour = c("red", "blue")[hulls$cluster],
                linewidth = 0.8, linetype = "dashed") +
   #geom_text_repel(aes(label = species), size = 2.5,
                   #max.overlaps = 30, colour = "#333333",
                   #segment.colour = NA) +
   geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               shape = 17, size = 3, colour = "black") +
   geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold") +
   scale_colour_gradientn(
     colours = c("#457B9D", "white", "#E63946"),
     name = "Ellenberg N", na.value = "grey80") +
   scale_size_continuous(range = c(1,5), guide = "none") +
   labs(x = "NMDS1", y = "NMDS2") +
   theme_classic())
ggsave("figures2/fig_ellenberg_N_plots_species.png", p_ell_N_species_plots,
         width = 11, height = 9, dpi = 180, bg = "white")

#Ellenberg T:
(p_ell_T_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = ellenberg_T), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster),
                 fill = NA, colour = c("red", "blue")[hulls$cluster],
                 linewidth = 0.8, linetype = "dashed") +
    #geom_text_repel(aes(label = species), size = 2.5,
                    #max.overlaps = 30, colour = "#333333",
                    #segment.colour = NA) +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               shape = 17, size = 3, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold") +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg T", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
   theme_classic())
ggsave("figures2/fig_ellenberg_T_plots_species.png", p_ell_T_species_plots,
       width = 11, height = 9, dpi = 180, bg = "white")

#Ellenberg M:
(p_ell_M_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = M), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster),
                 fill = NA, colour = c("red", "blue")[hulls$cluster],
                 linewidth = 0.8, linetype = "dashed") +
    #geom_text_repel(aes(label = species), size = 2.5,
                    #max.overlaps = 30, colour = "#333333",
                    #segment.colour = NA) +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               shape = 17, size = 3, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold") +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg M", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())
ggsave("figures2/fig_ellenberg_M_plots_species.png", p_ell_M_species_plots,
       width = 11, height = 9, dpi = 180, bg = "white")

#Ellenberg R:
(p_ell_R_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = R), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster),
                 fill = NA, colour = c("red", "blue")[hulls$cluster],
                 linewidth = 0.8, linetype = "dashed") +
    #geom_text_repel(aes(label = species), size = 2.5,
                    #max.overlaps = 30, colour = "#333333",
                    #segment.colour = NA) +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               shape = 17, size = 3, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold") +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg R", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())
ggsave("figures2/fig_ellenberg_R_plots_species.png", p_ell_R_species_plots,
       width = 11, height = 9, dpi = 180, bg = "white")

#Ellenberg S:
(p_ell_S_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = ellenberg_S), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster),
                 fill = NA, colour = c("red", "blue")[hulls$cluster],
                 linewidth = 0.8, linetype = "dashed") +
    #geom_text_repel(aes(label = species), size = 2.5,
                    #max.overlaps = 30, colour = "#333333",
                    #segment.colour = NA) +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               shape = 17, size = 3, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold") +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg S", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())
ggsave("figures2/fig_ellenberg_S_plots_species.png", p_ell_S_species_plots,
       width = 11, height = 9, dpi = 180, bg = "white")

#Ellenberg L:
(p_ell_L_species_plots <- ggplot(ellenberg_merged, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = L), alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, group = cluster),
                 fill = NA, colour = c("red", "blue")[hulls$cluster],
                 linewidth = 0.8, linetype = "dashed") +
    #geom_text_repel(aes(label = species), size = 2.5,
                    #max.overlaps = 30, colour = "#333333",
                    #segment.colour = NA) +
    geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2),
               shape = 17, size = 3, colour = "black") +
    geom_text_repel(data = site_scores, aes(x = NMDS1, y = NMDS2, label = plot),
                    size = 2.5, colour = "black", fontface = "bold") +
    scale_colour_gradientn(
      colours = c("#457B9D", "white", "#E63946"),
      name = "Ellenberg L", na.value = "grey80") +
    scale_size_continuous(range = c(1,5), guide = "none") +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_classic())
ggsave("figures2/fig_ellenberg_L_plots_species.png", p_ell_L_species_plots,
       width = 11, height = 9, dpi = 180, bg = "white")

#Grid of all ellenberg values:
(ellenberg_grid <- ((p_ell_N_species_plots + p_ell_T_species_plots + p_ell_M_species_plots) / (p_ell_R_species_plots + p_ell_S_species_plots + p_ell_L_species_plots)))
ggsave("figures2/ellenberg_grid.png", ellenberg_grid, width = 15, height = 7, dpi = 180, bg = "white")
