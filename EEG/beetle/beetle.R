setwd("~/Desktop/KU Leuven/EEG")
getwd()

library(ggplot2)

beetle <- read.table("beetle/WGS_table_Pogonus_final.tsv", header=TRUE, sep="\t")

head(beetle)
str(beetle)
unique(beetle$comp)  
outliers_BE2 <- subset(beetle, comp=="Belgium" & Fst > 0.2)$genome_pos
outliers_FR <- subset(beetle, comp=="France" & Fst > 0.2)$genome_pos
outliers_ES <- subset(beetle, comp=="Spain" & Fst > 0.2)$genome_pos
unique(beetle$comp)

#Q1: Overall degree of genetic divergence between ecotypes ----
sympatric <- subset(beetle, comp %in% c("Belgium", "France", "Spain"))

#Histogram of Fst per population
ggplot(data=sympatric) +
  geom_histogram(aes(x = Fst), bins = 50, fill = "pink3", color = "white") +
  facet_grid(comp~.) +
  labs(x = "Fst", y = "Count") +
  theme_classic2()
tapply(sympatric$Fst, sympatric$comp, summary)

#Q2: Is divergence driven by ecotype or geography? ----
beetle$type <- ifelse(beetle$comp %in% c("Belgium", "France", "Spain"),
                      "Sympatric (ecotype)", "Allopatric (geography)")

#Boxplot comparing Fst distributions
ggplot(data = beetle) +
  geom_boxplot(aes(x = comp, y = Fst, fill = type)) +
  scale_fill_manual(values=c("Sympatric (ecotype)"="tomato",
                             "Allopatric (geography)"="steelblue")) +
  labs(x = "Comparison", y = "Fst", fill = "Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Overlaid histograms
ggplot(data = beetle) +
  geom_histogram(aes(x = Fst, fill = type), bins = 50, alpha = 0.6,
                 position = "identity", color = NA) +
  scale_fill_manual(values = c("Sympatric (ecotype)" = "tomato",
                             "Allopatric (geography)" = "steelblue")) +
  labs(x = "Fst", y = "Count", fill = "Type") +
  theme_bw()

#Q3: Identify loci under adaptive differentiation ----
sympatric <- subset(beetle, comp %in% c("Belgium", "France", "Spain"))

#Manhattan plot
ggplot(data = sympatric) +
  geom_point(aes(x = genome_pos, y = Fst, color = chromosome), size = 0.5, alpha = 0.7) +
  facet_grid(comp~.) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "red") +  
  labs(x = "Genome position (bp)", y = "Fst") +
  theme_bw() +
  theme(legend.position = "none")

#High-Fst outlier loci (potential adaptive loci)
outliers <- subset(sympatric, Fst > 0.2)
cat("Number of outlier loci (Fst > 0.2):", nrow(outliers), "\n")

table(outliers$chromosome, outliers$comp) #which chromosomes are the outliers on?

#Q4: Which ecotype has the derived adaptation? ----
outliers_BE <- subset(beetle, comp =="Belgium" & Fst > 0.2)
outliers_FR <- subset(beetle, comp == "France" & Fst > 0.2)
outliers_SP <- subset(beetle, comp == "Spain" & Fst > 0.2)
#Pop1 = short-winged (tidal), Pop2 = long-winged (seasonal)

tajima_long_BE <- data.frame(
  TajimaD = c(outliers_BE$TajDPop1, outliers_BE$TajDPop2),
  Ecotype  = rep(c("Pop1", "Pop2"),
                 each = nrow(outliers_BE)))

ggplot(data = tajima_long_BE) +
  geom_boxplot(aes(x = Ecotype, y = TajimaD, fill = Ecotype)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Pop1" = "coral",
                             "Pop2" = "skyblue")) +
  labs(x = "Ecotype", y = "Tajima's D") +
  theme_bw() +
  annotate("text", x = 0.5, y = 2.5, label = "Belgium", fontface = "bold") +
  theme(legend.position="none")

tajima_long_FR <- data.frame(
  TajimaD = c(outliers_FR$TajDPop1, outliers_FR$TajDPop2),
  Ecotype  = rep(c("Pop1", "Pop2"),
                 each = nrow(outliers_FR)))

ggplot(data = tajima_long_FR) +
  geom_boxplot(aes(x = Ecotype, y = TajimaD, fill = Ecotype)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Pop1" = "coral",
                             "Pop2" = "skyblue")) +
  labs(x = "Ecotype", y = "Tajima's D") +
  theme_bw() +
  annotate("text", x = 0.5, y = 2.9, label = "France", fontface = "bold") +
  theme(legend.position ="none")

tajima_long_SP <- data.frame(
  TajimaD = c(outliers_SP$TajDPop1, outliers_SP$TajDPop2),
  Ecotype  = rep(c("Pop1", "Pop2"),
                 each = nrow(outliers_SP)))

ggplot(data = tajima_long_SP) +
  geom_boxplot(aes(x = Ecotype, y = TajimaD, fill = Ecotype)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Pop1" = "coral",
                             "Pop2" = "skyblue")) +
  labs(x = "Ecotype", y = "Tajima's D") +
  theme_bw() +
  annotate("text", x = 0.5, y = 2.5, label = "Spain", fontface = "bold") +
  theme(legend.position = "none")


#Q5: Single or multiple genomic regions? ----
outlier_chroms <- names(sort(table(outliers$chromosome), decreasing = TRUE))[1:5]

top_chrom <- outlier_chroms[1]
chrom_data <- subset(sympatric, chromosome == top_chrom)

ggplot(data = chrom_data) +
  geom_point(aes(x = genome_pos, y = Fst, color = comp), size = 1, alpha = 0.8) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "red") +
  labs(x = "Position on chromosome (bp)", y = "Fst", color = "Population") +
  theme_bw()

#Are the SAME loci outliers in all three populations?
outliers_BE2 <- subset(beetle, comp == "Belgium" & Fst > 0.2)$genome_pos
outliers_FR <- subset(beetle, comp == "France" & Fst > 0.2)$genome_pos
outliers_ES <- subset(beetle, comp == "Spain" & Fst > 0.2)$genome_pos

shared_BE_FR <- intersect(outliers_BE2, outliers_FR)
shared_BE_SP <- intersect(outliers_BE2, outliers_ES)
shared_FR_SP <- intersect(outliers_FR, outliers_ES)
shared_all <- intersect(shared_BE_FR, outliers_ES)

sympatric_BE <- subset(beetle, comp == "Belgium")
sympatric_BE$outlier_status <- ifelse(
  sympatric_BE$genome_pos %in% shared_all, "Shared outlier (all 3 pops)",
  ifelse(sympatric_BE$genome_pos %in% outliers_BE2, "Belgium outlier only", "Background"))

ggplot(data = sympatric_BE) +
  geom_point(aes(x = genome_pos, y = Fst, color = outlier_status), size = 0.7, alpha = 0.8) +
  scale_color_manual(values = c("Shared outlier (all 3 pops)" = "red",
                              "Belgium outlier only" = "orange",
                              "Background" = "grey60")) +
  labs(x = "Genome position (bp)", y = "Fst", color = "") +
  theme_bw()

shared_all <- intersect(intersect(outliers_BE2, outliers_FR), outliers_ES)
shared_BE_FR <- setdiff(intersect(outliers_BE2, outliers_FR), outliers_ES)
shared_BE_ES <- setdiff(intersect(outliers_BE2, outliers_ES), outliers_FR)
shared_FR_ES <- setdiff(intersect(outliers_FR, outliers_ES), outliers_BE2)
only_BE <- setdiff(outliers_BE2, union(outliers_FR, outliers_ES))
only_FR <- setdiff(outliers_FR, union(outliers_BE2, outliers_ES))
only_ES <- setdiff(outliers_ES, union(outliers_BE2, outliers_FR))

sympatric_BE <- subset(beetle, comp == "Belgium")
sympatric_BE$outlier_status <- "Background"
sympatric_BE$outlier_status[sympatric_BE$genome_pos %in% shared_all] <- "Shared all 3"
sympatric_BE$outlier_status[sympatric_BE$genome_pos %in% shared_BE_FR] <- "Belgium + France only"
sympatric_BE$outlier_status[sympatric_BE$genome_pos %in% shared_BE_ES] <- "Belgium + Spain only"
sympatric_BE$outlier_status[sympatric_BE$genome_pos %in% only_BE] <- "Belgium only"

ggplot(data = sympatric_BE) +
  geom_point(aes(x = genome_pos, y = Fst, color = outlier_status), size = 0.7, alpha = 0.8) +
  scale_color_manual(values=c(
    "Background" = "grey70",
    "Shared all 3" = "red",
    "Belgium + France only"= "blue",
    "Belgium + Spain only" = "purple",
    "Belgium only" = "orange")) +
  labs(title = "Outlier sharing across populations (Belgium)",
       x = "Genome position (bp)", y = "Fst", color = "") +
  theme_bw()

#Q6: De novo mutations vs standing genetic variation? ----
#at outlier loci: if Pi is high in BOTH ecotypes, that suggests SGV
outliers_all <- subset(beetle, comp %in% c("Belgium","France","Spain") & Fst > 0.2)

ggplot(data = outliers_all) +
  geom_point(aes(x = piPop1, y = piPop2, color = comp), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(#title="Nucleotide diversity at outlier loci\nHigh Pi in both = standing genetic variation",
       x = "Pi short-winged (Pop1)", y = "Pi long-winged (Pop2)", color = "Population") +
  theme_bw()

#Dxy vs Fst: high Dxy + high Fst = SGV (old)
#low Dxy + high Fst = recent sweep (de novo)
ggplot(data = sympatric) +
  geom_point(aes(x = Dxy, y = Fst, color = comp), size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "red") +
  labs(#title="Dxy vs Fst\nHigh Dxy + High Fst suggests old divergence / standing variation",
       x = "Dxy (absolute divergence)", y = "Fst", color = "Population") +
  theme_bw()
