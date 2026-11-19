#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/EXAM/Resit")
getwd()

#Libraries
library(readxl) #used
library(cluster) #used
library(tidyverse) #used
library(ggbiplot)
library(plotly)
library(vegan) #used

#Data
primula <- read_xlsx("Primula.xlsx")
head(primula) 
str(primula)
primula$Morph <- as.factor(primula$Morph)
primula$Ecotype <- as.factor(primula$Ecotype)
primula$Population <- as.factor(primula$Population)

primula_num <- primula %>% dplyr::select(Stigma_height, Anther_length, Tube_length, 
                                         Tube_width, Petal_length, Petal_width, 
                                         Corolla_diam, Herkogamy)
str(primula_num) #only numeric data now (all of the floral characteristics)

#PCA ----
primula_num <- na.omit(primula_num)
pca_primula <- prcomp(primula_num, center = TRUE, scale. = TRUE)
pca_primula #s.d and eigenvectors --> Show which variables contribute most to each component (eigenvectors of which elements are loadings)

eigenvalues = pca_primula$sdev^2 #amount of variance explained per PC
eigenvalues #amount of variance of the data along the principal axes; one eigenvalue per PC
sum(eigenvalues)  #total amount of variation of the variables; sum eigenvectors = total variability
summary(pca_primula)  # Shows how much variance is explained by the different principal components (in proportions)
screeplot(pca_primula, main = "Scree plot", type="lines") ##PC1 and PC2 important, rest not that important

par(mfrow = c(1,3))
plot(pca_primula$x[,c(1,2)],col ="black", cex=1) #pca_primula$x contains the scores --> position on graph per object (F=U*Yc)
plot(pca_primula$x[,c(1,3)],col ="black", cex=1)
plot(pca_primula$x[,c(2,3)],col ="black", cex=1)
par(mfrow = c(1,1))

levels(primula$Ecotype) #Forest = 0; Grassland = 1
plot(pca_primula$x[,c(1,2)], pch=21,bg=c("forestgreen","blue")[primula$Ecotype],cex=1)
legend("topleft",c("Forest", "Grasslands"), fill=c("forestgreen","blue"))
text(pca_primula$x[,c(1,2)], labels=row.names(primula), pos=3) #add ID's of objects; pos=3 meansadding the text above the coordinates

(p <- autoplot(pca_primula, data = primula, colour = 'Ecotype', label = FALSE, ellipse = TRUE, 
               loadings = TRUE, loadings.colour = 'blue',
               loadings.label = TRUE, loadings.label.size = 3))
ggplotly(p)

primula_manova <- manova(cbind(Stigma_height, Anther_length, Tube_length, Tube_width,
                               Petal_length, Petal_width, Corolla_diam, Herkogamy) ~ as.factor(Ecotype), data = primula)
summary(primula_manova, tol = 0) #ecotypes do significiantly differ in their floral traits

summary(aov(primula_manova))
adonis2(primula_num ~ Ecotype, data = primula, permutations = 999, method = "bray") #



#PCA with morph
pca_primula <- prcomp(primula_num, center = TRUE, scale. = TRUE)

(p2 <- autoplot(pca_primula, data = primula, colour = 'Morph', label = FALSE, loadings = TRUE , loadings.colour = 'blue',
                loadings.label = TRUE, loadings.label.size = 3))

primula_manova2 <- manova(cbind(Stigma_height, Anther_length, Tube_length, Tube_width,
                                Petal_length, Petal_width, Corolla_diam, Herkogamy) ~ as.factor(Morph), data = primula)
summary(primula_manova2, tol = 0) #Morphs do significiantly differ in their floral traits
summary(aov(primula_manova2))
adonis2(primula_num ~ Morph, data = primula, permutations = 999, method = "bray") #

