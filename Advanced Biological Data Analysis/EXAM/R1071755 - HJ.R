##%#########################################################################%##
#                                                                             #
#                     Exam - Zoja Manček Páli (r1071755)                      #
#                                 Jaquemyn                                    #
#                          Date: 26.1.2026, 13:00                             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/EXAM")
getwd()

#packages
library(readxl)
library(afex)
library(factoextra)
library(car)
library(tidyr)
library(dplyr)
library(effects) 
library(emmeans)
library(ggplot2)
library(ggpubr)
library(lattice)
library(lmtest)
library(multcomp)
library(MuMIn)
library(openxlsx)
library(rockchalk)
library(tidyverse)
library(rJava)
library(glmulti)
library(rockchalk)
library(scales)
library(nlme)
library(nnet)
library(reshape2)
library(robustbase)
library(MASS)
library(Matrix)
library(nlstools)
library(nlsMicrobio) # contains some bacterial growth data and fit functions
library(investr) # for plotFit function
library(MultiKink)
library(splines)
library(scales) #see through points on scatter plot
library(survival)
library(ggfortify)
library(KMsurv)
library(flexsurv)
library(Rmisc)
library(ggthemes)
library(ggsignif)
library(gridExtra)
library(ggprism)
library(scales)
library(svglite)
library(gtools) # for the stars.pval function
library(digest)
library(devtools)
library(ggbiplot)
library(vegan)
library(gclus)
library(ade4)

#data
comm <- read_xlsx("Exam data/Comm_data.xlsx")
env <- read_xlsx("Exam data/Env_data.xlsx")

head(comm) #species abundances
head(env) #env variables

env_m <- env[,(2:25)]
#Abundances are not continuous variables, so I will use an NMDS
str(comm)
comm <- mutate_if(comm, is.character, as.factor)
com_m = comm[,c(2:34)] #only selecting the numerical data to work with

#NMDS
nmds <- metaMDS(com_m, distance = "bray") #use original dataset as input, metaMDS calculates distance matrix
nmds #stress = 0.19; not ideal
plot(nmds, type="text", main=paste("NMDS/Bray -Stress =", round(nmds$stress,3)))

stressplot(nmds, main=paste("Shepard plot - Euclidean", round(nmds$stress,3)))
gof=goodness(nmds)
plot(nmds, type="t", main="goodness-of-fit")
points(nmds, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit

bray_dist <- vegdist(com_m, method = "bray") #0 = completely similar, 1 = completely different communities
hc <- hclust(bray_dist, method = "average") #UPGMA clustering, unless there is a reason not to use it, use this


plot(hc,
     labels = FALSE,
     hang = -1,
     main = "Hierarchical clustering of ponds (Bray–Curtis)",
     xlab = "Ponds",
     ylab = "Bray–Curtis dissimilarity")

p.dist.UPGMA <- hclust(p.dist, method="average");
plot(p.dist.UPGMA, main="UPGMA")

plot(p.dist.UPGMA$height, nrow(com_m):2, type="S", main="Fusion levels - UPGMA",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(p.dist.UPGMA$height, nrow(com_m):2, nrow(com_m):2, col="red", cex = 0.8)  

asw <- numeric(nrow(com_m));
for (k in 2:(nrow(com_m)-1)) {
  sil <- silhouette(cutree(p.dist.UPGMA, k=k), p.dist)
  asw[k] <- summary(sil)$avg.width
}
k.best.single <- which.max(asw);
k.best.single
plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, single",
     xlab="k (number of groups)", ylab="Average silhouette width")
axis(1, k.best.single, paste("optimum",k.best.single,sep="\n"), col="red", font=2, col.axis="red")
points(k.best.single, max(asw), pch=16, col="red", cex=1.5)  

#two clusters!
nmds 
plot(nmds, type="n", main=paste("NMDS/Chord - Stress =", round(nmds$stress,3)))
text(nmds,display=c("species")) 


library(vegan)
nmds <- metaMDS(comm = com_m, distance = "bray", trace = FALSE, autotransform = FALSE)
plot(nmds)    
nmds_xy <- data.frame(nmds$points)  
nmds_xy$pond <- comm$Pond_ID
ggplot(nmds_xy, aes(MDS1, MDS2)) +
  geom_point() +
  theme_classic()


nmds$stress  #acceptable
envfit_res <- envfit(nmds, env_m)
plot(nmds)
plot(envfit_res)
