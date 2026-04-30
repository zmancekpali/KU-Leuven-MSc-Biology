#WD
setwd("~/Desktop/KU Leuven/EEG/RDA_exercise") #linked with github repository so I did not use the here() package
getwd()

#Libraries
library(vegan)
library(tidyverse)
library(corrplot)
library(robust)
library(qvalue)

#Functions
source("RDA_Functions.R")

#Data
variables <- read.table("Data/Variables.txt", header = TRUE, sep = ",") #comma separator needed
allele_freq <- read.table("Data/Allele-Frequencies.txt", header = TRUE, sep = ",") #runs for a really long time

dim(variables)
dim(allele_freq)

rownames(variables) <- variables$Population
rownames(allele_freq) <- variables$Population

Y <- allele_freq #matrix of allele frequencies

env_vars <- variables[, c("AHM.6190","bFFP.6190","CMD.6190","DD_0.6190",
                          "DD_18.6190","DD18.6190","DD5.6190","eFFP.6190",
                          "EMT.6190","Eref.6190","FFP.6190","MAP.6190",
                          "MAR.6190","MAT.6190","MCMT.6190","MSP.6190",
                          "MWMT.6190","NFFD.6190","PAS.6190","PPT_sm.6190",
                          "PPT_wt.6190","RH.6190","SHM.6190","Tave_sm.6190",
                          "Tave_wt.6190","TD.6190")]
env_scaled <- as.data.frame(scale(env_vars))

geo_vars <- variables[, c("Latitude", "Longitude")]
geo_scaled  <- as.data.frame(scale(geo_vars))

pop_struct <- variables[, c("PC1", "PC2", "PC3")]
pop_scaled  <- as.data.frame(scale(pop_struct))

#Practical ----
set.seed(42)
Y_sub <- Y[, sample(ncol(Y), 2000)] #2000 loci only

#RDA with all 26 environmental variables
rda_full <- rda(Y ~ ., data = env_scaled, scale = TRUE)
vif.cca(rda_full) #many VIF > 10

  #Removing high VIF values from highest to lowest until all < 10
  max(vif.cca(rda_full)) #TD.6190 highest (VIF = 17,384.82)
  
  env_scaled2 <- env_scaled[, !names(env_scaled) %in% c("TD.6190")]
  rda2 <- rda(Y ~ ., data = env_scaled2, scale = TRUE)
  vif.cca(rda2)
  max(vif.cca(rda2)) #DD_18.6190 highest (VIF = 12,089.91)
  
  env_scaled3 <- env_scaled2[, !names(env_scaled2) %in% c("DD_18.6190")]
  rda3 <- rda(Y ~ ., data = env_scaled3, scale = TRUE)
  vif.cca(rda3)
  max(vif.cca(rda3)) #FFP.6190 highest (VIF = 5,044.49)
  
  env_scaled4 <- env_scaled3[, !names(env_scaled3) %in% c("FFP.6190")]
  rda4 <- rda(Y ~ ., data = env_scaled4, scale = TRUE)
  vif.cca(rda4)
  max(vif.cca(rda4)) #MAT.6190 highest (VIF = 2,030.297)
  
  env_scaled5 <- env_scaled4[, !names(env_scaled4) %in% c("MAT.6190")]
  rda5 <- rda(Y ~ ., data = env_scaled5, scale = TRUE)
  vif.cca(rda5)
  max(vif.cca(rda5)) #Tave_wt.6190 highest (VIF = 1,272.636)
  
  env_scaled6 <- env_scaled5[, !names(env_scaled5) %in% c("Tave_wt.6190")]
  rda6 <- rda(Y ~ ., data = env_scaled6, scale = TRUE)
  vif.cca(rda6)
  max(vif.cca(rda6)) #Tave_sm.6190 highest (VIF = 380.3013)
  
  env_scaled7 <- env_scaled6[, !names(env_scaled6) %in% c("Tave_sm.6190")]
  rda7 <- rda(Y ~ ., data = env_scaled7, scale = TRUE)
  vif.cca(rda7)
  max(vif.cca(rda7)) #DD5.6190 highest (VIF = 302.1682)
  
  env_scaled8 <- env_scaled7[, !names(env_scaled7) %in% c("DD5.6190")]
  rda8 <- rda(Y ~ ., data = env_scaled8, scale = TRUE)
  vif.cca(rda8)
  max(vif.cca(rda8)) #MAP.6190 highest (VIF = 130.8639)
  
  env_scaled9 <- env_scaled8[, !names(env_scaled8) %in% c("MAP.6190")]
  rda9 <- rda(Y ~ ., data = env_scaled9, scale = TRUE)
  vif.cca(rda9)
  max(vif.cca(rda9)) #NFFD.6190 highest (VIF = 101.551)
  
  env_scaled10 <- env_scaled9[, !names(env_scaled9) %in% c("NFFD.6190")]
  rda10 <- rda(Y ~ ., data = env_scaled10, scale = TRUE)
  vif.cca(rda10)
  max(vif.cca(rda10)) #MCMT.6190 highest (VIF = 85.80532)
  
  env_scaled11 <- env_scaled10[, !names(env_scaled10) %in% c("MCMT.6190")]
  rda11 <- rda(Y ~ ., data = env_scaled11, scale = TRUE)
  vif.cca(rda11)
  max(vif.cca(rda11)) #MSP.6190 highest (VIF = 57.57806)
  
  env_scaled12 <- env_scaled11[, !names(env_scaled11) %in% c("MSP.6190")]
  rda12 <- rda(Y ~ ., data = env_scaled12, scale = TRUE)
  vif.cca(rda12)
  max(vif.cca(rda12)) #Eref.6190 highest (VIF = 42.2904)
  
  env_scaled13 <- env_scaled12[, !names(env_scaled12) %in% c("Eref.6190")]
  rda13 <- rda(Y ~ ., data = env_scaled13, scale = TRUE)
  vif.cca(rda13)
  max(vif.cca(rda13)) #CMD.6190 highest (VIF = 37.81217)
  
  env_scaled14 <- env_scaled13[, !names(env_scaled13) %in% c("CMD.6190")]
  rda14 <- rda(Y ~ ., data = env_scaled14, scale = TRUE)
  vif.cca(rda14)
  max(vif.cca(rda14)) #eFFP.6190 highest (VIF = 27.97325)
  
  env_scaled15 <- env_scaled14[, !names(env_scaled14) %in% c("eFFP.6190")]
  rda15 <- rda(Y ~ ., data = env_scaled15, scale = TRUE)
  vif.cca(rda15)
  max(vif.cca(rda15)) #EMT.6190 highest (VIF = 27.52538)
  
  env_scaled16 <- env_scaled15[, !names(env_scaled15) %in% c("EMT.6190")]
  rda16 <- rda(Y ~ ., data = env_scaled16, scale = TRUE)
  vif.cca(rda16)
  max(vif.cca(rda16)) #bFFP.6190 highest (VIF = 11.386263)
  
  env_scaled17 <- env_scaled16[, !names(env_scaled16) %in% c("bFFP.6190")]
  rda17 <- rda(Y ~ ., data = env_scaled17, scale = TRUE)
  vif.cca(rda17)
  max(vif.cca(rda17)) #SHM.6190 highest (VIF = 10.62087)
  
  env_scaled18 <- env_scaled17[, !names(env_scaled17) %in% c("SHM.6190")]
  rda18 <- rda(Y ~ ., data = env_scaled18, scale = TRUE)
  vif.cca(rda18)
  max(vif.cca(rda18)) #max VIF < 10
  
  #9 total environmental variables selected: 
  env_final_full <- env_scaled[, c("AHM.6190", "DD_0.6190", "DD18.6190", "MAR.6190",
                                   "MWMT.6190", "PAS.6190", "PPT_sm.6190", 
                                   "PPT_wt.6190", "RH.6190")]
  
  vp_full <- varpart(Y, env_final_full, geo_scaled, pop_scaled)
  print(vp_full)
  plot(vp_full,
       Xnames = c("Environment", "Geography", "Pop. Structure"),
       bg = c("tomato", "steelblue", "gold"),
       digits = 3)
  #geography (b) alone explains the least, pop. structure (c) alone explains the most
  #large shared fraction (g = 0.04388) implies overlap, but most variance 
    #(residuals = 0.89878; ~90%) remains unexplained
  
rda_gea_full <- rda(Y ~ . + Condition(as.matrix(cbind(geo_scaled, pop_scaled))),
                    data = env_final_full, scale = TRUE)
anova_axes <- anova.cca(rda_gea_full, by = "axis", permutations = 999)
print(anova_axes) #this maxes out my vector memory limit; using the first 3 axes

    (outlier_res <- rdadapt(rda_gea_full, K = 3))
    (n_outliers <- sum(outlier_res$q.values < 0.05)) #567 outliers
    (adaptive_idx <- which(outlier_res$q.values < 0.05)) #the exact loci outliers
  
    locus_scores <- scores(rda_gea_full, choices = 1:2, display = "species", scaling = 3)
    site_scores <- scores(rda_gea_full, choices = 1:2, display = "sites",   scaling = 3)
    env_scores <- scores(rda_gea_full, choices = 1:2, display = "bp",      scaling = 3)
    
    col_loci <- rep("grey70", nrow(locus_scores))
    col_loci[adaptive_idx] <- "red"
    
    plot(locus_scores, pch = 20, col = col_loci, cex = 0.4,
         xlab = "RDA1", ylab = "RDA2")
    points(site_scores, pch = 21, bg = "steelblue", col = "white", cex = 1)
    arrows(0, 0, env_scores[,1]*0.9, env_scores[,2]*0.9,
           length = 0.1, col = "darkblue", lwd = 2)
    text(env_scores*0.95, rownames(env_scores), col = "red", cex = 0.8)

    plot(-log10(outlier_res$p.values),
         col = ifelse(outlier_res$q.values < 0.05, "red", "grey60"),
         pch = 20, cex = 0.4,
         xlab = "Locus", ylab = "-log10(p-value)")
    abline(h = -log10(0.05 / length(outlier_res$p.values)), lty = 2)
    
#Assignment ----
vars_to_remove <- c("DD_18.6190","TD.6190","FFP.6190","MAT.6190",
                    "Tave_wt.6190","DD5.6190","Tave_sm.6190","MSP.6190",
                    "NFFD.6190","DD_0.6190","PPT_wt.6190","CMD.6190",
                    "EMT.6190","eFFP.6190","Eref.6190","SHM.6190","bFFP.6190")

env_final <- env_scaled[, !names(env_scaled) %in% vars_to_remove]
rda_check <- rda(Y ~ ., data = env_final, scale = TRUE)
vif.cca(rda_check) #all VIFs still < 10 (independent of Y so no difference to the practical work)

vp_full <- varpart(Y, env_final, geo_scaled, pop_scaled)
print(vp_full)
plot(vp_full,
     Xnames = c("Environment", "Geography", "Pop. Structure"),
     bg = c("tomato", "steelblue", "gold"),
     digits = 3)
  #geography (b) alone explains the least, pop.structure (c) alone the most
  #overlap (g = 0.4372) still present, but still 90% ov variance unexplained

rda_gea_full <- rda(Y ~ . + Condition(as.matrix(cbind(geo_scaled, pop_scaled))),
                    data = env_final, scale = TRUE)
outlier_res <- rdadapt(rda_gea_full, K = 3)
(n_outliers <- sum(outlier_res$q.values < 0.05)) #605 outliers now
(adaptive_idx <- which(outlier_res$q.values < 0.05))

locus_scores <- scores(rda_gea_full, choices = 1:2, display = "species", scaling = 3)
site_scores  <- scores(rda_gea_full, choices = 1:2, display = "sites",   scaling = 3)
env_scores   <- scores(rda_gea_full, choices = 1:2, display = "bp",      scaling = 3)
col_loci <- rep("grey70", nrow(locus_scores))
col_loci[adaptive_idx] <- "red"

plot(locus_scores, pch = 20, col = col_loci, cex = 0.4,
     xlab = "RDA1", ylab = "RDA2")
points(site_scores, pch = 21, bg = "steelblue", col = "white", cex = 1)
arrows(0, 0, env_scores[,1]*0.9, env_scores[,2]*0.9,
       length = 0.1, col = "darkblue", lwd = 2)
text(env_scores*0.95, rownames(env_scores), col = "darkblue", cex = 0.8)

plot(-log10(outlier_res$p.values),
     col = ifelse(outlier_res$q.values < 0.05, "red", "grey60"),
     pch = 20, cex = 0.4,
     xlab = "Locus", ylab = "-log10(p-value)")
abline(h = -log10(0.05 / length(outlier_res$p.values)), lty = 2)

pdf("pairwise_correlations.pdf", width = 14, height = 14)
corrplot(cor(env_scaled), method = "color", type = "lower",
         tl.cex = 0.7, addCoef.col = "black", number.cex = 0.4, diag = FALSE)
dev.off()

#VIF check again for full dataset:
rda_full <- rda(Y ~ ., data = env_scaled, scale = TRUE)
vif.cca(rda_full) #many VIF > 10
max(vif.cca(rda_full)) #TD.6190 highest (VIF = 17,384.82)

env_scaled2 <- env_scaled[, !names(env_scaled) %in% c("TD.6190")]
rda2 <- rda(Y ~ ., data = env_scaled2, scale = TRUE)
vif.cca(rda2)
max(vif.cca(rda2)) #DD_18.6190 highest (VIF = 12,089.91)

env_scaled3 <- env_scaled2[, !names(env_scaled2) %in% c("DD_18.6190")]
rda3 <- rda(Y ~ ., data = env_scaled3, scale = TRUE)
vif.cca(rda3)
max(vif.cca(rda3)) #FFP.6190 highest (VIF = 5,044.49)

env_scaled4 <- env_scaled3[, !names(env_scaled3) %in% c("FFP.6190")]
rda4 <- rda(Y ~ ., data = env_scaled4, scale = TRUE)
vif.cca(rda4)
max(vif.cca(rda4)) #MAT.6190 highest (VIF = 2,030.297)

env_scaled5 <- env_scaled4[, !names(env_scaled4) %in% c("MAT.6190")]
rda5 <- rda(Y ~ ., data = env_scaled5, scale = TRUE)
vif.cca(rda5)
max(vif.cca(rda5)) #Tave_wt.6190 highest (VIF = 1,272.636)

env_scaled6 <- env_scaled5[, !names(env_scaled5) %in% c("Tave_wt.6190")]
rda6 <- rda(Y ~ ., data = env_scaled6, scale = TRUE)
vif.cca(rda6)
max(vif.cca(rda6)) #Tave_sm.6190 highest (VIF = 380.3013)

env_scaled7 <- env_scaled6[, !names(env_scaled6) %in% c("Tave_sm.6190")]
rda7 <- rda(Y ~ ., data = env_scaled7, scale = TRUE)
vif.cca(rda7)
max(vif.cca(rda7)) #DD5.6190 highest (VIF = 302.1682)

env_scaled8 <- env_scaled7[, !names(env_scaled7) %in% c("DD5.6190")]
rda8 <- rda(Y ~ ., data = env_scaled8, scale = TRUE)
vif.cca(rda8)
max(vif.cca(rda8)) #MAP.6190 highest (VIF = 130.8639)

env_scaled9 <- env_scaled8[, !names(env_scaled8) %in% c("MAP.6190")]
rda9 <- rda(Y ~ ., data = env_scaled9, scale = TRUE)
vif.cca(rda9)
max(vif.cca(rda9)) #NFFD.6190 highest (VIF = 101.551)

env_scaled10 <- env_scaled9[, !names(env_scaled9) %in% c("NFFD.6190")]
rda10 <- rda(Y ~ ., data = env_scaled10, scale = TRUE)
vif.cca(rda10)
max(vif.cca(rda10)) #MCMT.6190 highest (VIF = 85.80532)

env_scaled11 <- env_scaled10[, !names(env_scaled10) %in% c("MCMT.6190")]
rda11 <- rda(Y ~ ., data = env_scaled11, scale = TRUE)
vif.cca(rda11)
max(vif.cca(rda11)) #MSP.6190 highest (VIF = 57.57806)

env_scaled12 <- env_scaled11[, !names(env_scaled11) %in% c("MSP.6190")]
rda12 <- rda(Y ~ ., data = env_scaled12, scale = TRUE)
vif.cca(rda12)
max(vif.cca(rda12)) #Eref.6190 highest (VIF = 42.2904)

env_scaled13 <- env_scaled12[, !names(env_scaled12) %in% c("Eref.6190")]
rda13 <- rda(Y ~ ., data = env_scaled13, scale = TRUE)
vif.cca(rda13)
max(vif.cca(rda13)) #CMD.6190 highest (VIF = 37.81217)

env_scaled14 <- env_scaled13[, !names(env_scaled13) %in% c("CMD.6190")]
rda14 <- rda(Y ~ ., data = env_scaled14, scale = TRUE)
vif.cca(rda14)
max(vif.cca(rda14)) #eFFP.6190 highest (VIF = 27.97325)

env_scaled15 <- env_scaled14[, !names(env_scaled14) %in% c("eFFP.6190")]
rda15 <- rda(Y ~ ., data = env_scaled15, scale = TRUE)
vif.cca(rda15)
max(vif.cca(rda15)) #EMT.6190 highest (VIF = 27.52538)

env_scaled16 <- env_scaled15[, !names(env_scaled15) %in% c("EMT.6190")]
rda16 <- rda(Y ~ ., data = env_scaled16, scale = TRUE)
vif.cca(rda16)
max(vif.cca(rda16)) #bFFP.6190 highest (VIF = 11.386263)

env_scaled17 <- env_scaled16[, !names(env_scaled16) %in% c("bFFP.6190")]
rda17 <- rda(Y ~ ., data = env_scaled17, scale = TRUE)
vif.cca(rda17)
max(vif.cca(rda17)) #SHM.6190 highest (VIF = 10.62087)

env_scaled18 <- env_scaled17[, !names(env_scaled17) %in% c("SHM.6190")]
rda18 <- rda(Y ~ ., data = env_scaled18, scale = TRUE)
vif.cca(rda18)
max(vif.cca(rda18)) #max VIF < 10

