rm(list=ls())
######################################################
############# NMDS & cluster #########################
# Code for exercise 1 and 2 - Practicals 

#set proper working directory

############# Exercise 1: NMDS #######################
library(openxlsx)
mycor.all<-read.xlsx("Epipactis_fungi.xlsx", rowNames = T) #rowNames=True allows for the labels of the specimens to be used
library(dplyr)
str(mycor.all)
mycor.all<-mutate_if(mycor.all, is.character, as.factor)
mycor = mycor.all[,c(2:84)] #only select the numerical data to work with
library(vegan)

#creating the Dissimilarity matrix
#for non-binary data, you could use the following methods
mycor.euc <- vegdist(mycor, method = "euclidean")
mycor.man <- vegdist(mycor, method = "manhattan")
mycor.bc <- vegdist(mycor, method = "bray")
mycor.chord <- vegdist(decostand(mycor, "norm"), method="euclidean")
?vegdist
#for binary data (which we have)
library(ade4)
mycor.jac<-dist.binary(mycor, method=1)
?dist.binary
# Dissimilarity matrix visualization, pink=similar, blue is dissimilar
##build coldiss function: see code on Toledo to create the function
library(gclus)
coldiss(mycor.bc, byrank=FALSE, diag=TRUE) 
coldiss(mycor.jac, byrank=FALSE, diag=TRUE) 

par(mfrow=c(1,1))
#################### 
### NMDS - Euclidean (Don't use the Euclidean distance matrix because it cannot handle zeros very well)
mycor.nmds.euc <- metaMDS(mycor, distance = "euclidean") #use original dataset as input, metaMDS calculates distance matrix
mycor.nmds.euc #stress = 0.12
plot(mycor.nmds.euc, type="text", main=paste("NMDS/Euclidean -Stress =", round(mycor.nmds.euc$stress,3)))

# Stress: 0.05 = excellent; 0.1 = great; 0.2 = ok; 0.3 = poor representation

# Orchid species are colored
levels(mycor.all$Species)
plot(mycor.nmds.euc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Euclidean - Stress =", round(mycor.nmds.euc$stress,3  )))
legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"), text.font=8, text.width =0.5, bty="n" )

## $points: plotting only the plant objects; if you want to plot the columns (OTU's in this case) you can put $species
text(mycor.nmds.euc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
## display the mycor. OTU's as text
##if you want to plot the text of the plant objects (the rows) you put display="sites"

#shepards plot compares the distances in the ordination plot with the original distances
stressplot(mycor.nmds.euc, main=paste("Shepard plot - Euclidean", round(mycor.nmds.euc$stress,3)))
gof=goodness(mycor.nmds.euc)
plot(mycor.nmds.euc, type="t", main="goodness-of-fit")
points(mycor.nmds.euc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit

###################### 
### NMDS - Bray-Curtis (You could use the Bray-Curtis distance matrix since it can also handle binary data very well)
mycor.nmds.bc <- metaMDS(mycor, distance = "bray")
mycor.nmds.bc #stress=0.1264
plot(mycor.nmds.bc, type="t", main=paste("NMDS/Bray-Curtis - Stress =", round(mycor.nmds.bc$stress,3)))


# Stress: 0.05 = excellent; 0.1 = great; 0.2 = ok; 0.3 = poor representation

# Orchid species are colored
plot(mycor.nmds.bc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Bray-Curtis - Stress =", round(mycor.nmds.bc$stress,3  )))
legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"),text.font=8, text.width =0.5, bty="n")
##$points: plotting only the plant objects

text(mycor.nmds.bc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
## display the mycor. OTU's as text

# Stress and goodness of fit
stressplot(mycor.nmds.bc, main=paste("Shepard plot - Bray-Curtis", round(mycor.nmds.bc$stress,3)))
gof=goodness(mycor.nmds.bc)
plot(mycor.nmds.bc, type="t", main="goodness-of-fit")
points(mycor.nmds.bc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit

################## 
### NMDS - Jaccard (The Jaccard distance matrix is also very suitable for binary data)
mycor.nmds.jacc <- metaMDS(mycor, distance = "jaccard")
plot(mycor.nmds.jacc, type="t", main=paste("NMDS/Jaccard - Stress =", round(mycor.nmds.jacc$stress,3)))

# Orchid species are colored

plot(mycor.nmds.jacc$points,pch=21,col="black",cex=2,bg=c("green","blue","red")[mycor.all$Species],main=paste("NMDS/Jaccard - Stress =", round(mycor.nmds.jacc$stress,3  )))
text(mycor.nmds.jacc, display = "species",cex=0.7) # Adding the mycorrhizal OTU's on the map
legend("topleft",c("E_helleborine","E_neerlandica", "E_palustris"), fill=c("green","blue","red"),text.font=8, text.width =0.2, bty="n")


# Stress and goodness of fit
stressplot(mycor.nmds.jacc, main=paste("Shepard plot - Jaccard", round(mycor.nmds.jacc$stress,3)))
gof=goodness(mycor.nmds.jacc) 
plot(mycor.nmds.jacc, type="t", main="goodness-of-fit")
points(mycor.nmds.jacc, display="sites", cex=2*gof/mean(gof)) # Large values (large points in this case) represent poor fit

####in this case, with this dataset, completely the same results as with the BRAY_CURTIS distance matrix
################## Other distance matrices are also well suited for binary data. Not only the ones mentioned above.e.g. Sorenssen



############# Exercise 2: Clustering #######################
plants<-read.xlsx("meadow_plant_species.xlsx")
str(plants)

### Distance matrix 
#p.dist = vegdist(plants,"euclidean") # Euclidean - Not good for this dataset due to the zero's
#p.dist = vegdist(plants,"bray") # Bray-Curtis distance 
p.dist = vegdist(decostand(plants,"norm"),method="euclidean") # Chord distance - Good option 

# Chord distance is especially useful for community data


### Different clustering methods
par(mfrow=c(2,2))
# Single linkage agglomerative clustering
p.dist.single <- hclust(p.dist, method="single")
plot(p.dist.single, main="Single linkage")

# Complete linkage agglomerative clustering
p.dist.complete <- hclust(p.dist, method="complete");
plot(p.dist.complete, main="Complete linkage")

# Unweighted average linkage agglomerative clustering (UPGMA)
p.dist.UPGMA <- hclust(p.dist, method="average");
plot(p.dist.UPGMA, main="UPGMA")


# Ward's minimum variance method
p.dist.ward <- hclust(p.dist, method="ward.D");
plot(p.dist.ward,main="Ward")
#p.dist.ward$height <-sqrt(p.dist.ward$height);
#plot(p.dist.ward)


### Assessing which clustering method works best
# Single linkage agglomerative clustering
p.dist.single.coph <- cophenetic(p.dist.single)
plot(p.dist, p.dist.single.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
     main = c("Single linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.single.coph),3))))
abline(0,1)
lines(lowess(p.dist,p.dist.single.coph), col="red")
gow.dist.single <- sum((p.dist-p.dist.single.coph)^2); 
gow.dist.single # Small values indicate a high fit


# Complete linkage agglomerative clustering
p.dist.complete.coph <- cophenetic(p.dist.complete);
plot(p.dist, p.dist.complete.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
     main = c("complete linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.complete.coph),3))))
abline(0,1)
lines(lowess(p.dist,p.dist.complete.coph), col="red")
gow.dist.complete <- sum((p.dist-p.dist.complete.coph)^2); 
gow.dist.complete # Small values indicate a high fit

# Unweighted average linkage agglomerative clustering (UPGMA)
p.dist.UPGMA.coph <- cophenetic(p.dist.UPGMA);
plot(p.dist, p.dist.UPGMA.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
     main = c("UPGMA linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.UPGMA.coph),3))))
abline(0,1)
lines(lowess(p.dist,p.dist.UPGMA.coph), col="red")
gow.dist.UPGMA <- sum((p.dist-p.dist.UPGMA.coph)^2); 
gow.dist.UPGMA # Small values indicate a high fit

# Ward's minimum variance method
p.dist.ward.coph <- cophenetic(p.dist.ward);
plot(p.dist, p.dist.ward.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp=1, xlim=c(0,sqrt(2)), ylim=c(0,sqrt(2)), 
     main = c("ward linkage", paste("Cophenetic correlation ", round(cor(p.dist, p.dist.ward.coph),3))))
abline(0,1)
lines(lowess(p.dist,p.dist.ward.coph), col="red")
gow.dist.ward <- sum((p.dist-p.dist.ward.coph)^2); 
gow.dist.ward # Small values indicate a high fit

##UPGMA is best
##for illustrative purposes, for the subsequent analyses we give the codes for all different clustering methods. But actually the next steps need
# to be only done for the best clustering method (UPGMA in this case)
par(mfrow=c(1,1))

### The optimal amount of clusters - fusion plots
# Single linkage
plot(p.dist.single$height, nrow(plants):2, type="S", main="Fusion levels - Single",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(p.dist.single$height, nrow(plants):2,nrow(plants):2, col="red", cex = 0.8)  

# Complete linkage
plot(p.dist.complete$height, nrow(plants):2, type="S", main="Fusion levels - Complete",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(p.dist.complete$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  

# UPGMA
plot(p.dist.UPGMA$height, nrow(plants):2, type="S", main="Fusion levels - UPGMA",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(p.dist.UPGMA$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  

# Ward's D
plot(p.dist.ward$height, nrow(plants):2, type="S", main="Fusion levels - ward",
     ylab="k (number of clusters)", xlab="h (node height)", col="grey")
text(p.dist.ward$height, nrow(plants):2, nrow(plants):2, col="red", cex = 0.8)  

# It is quite difficult and arbitrary to choose the optimal number of clusters, but based on 
# the fusion plots (and the information given with the exercise) I'd choose 5 clusters
par(mfrow=c(1,2))
par(mfrow=c(1,1))
### The optimal amount of clusters - silhouette width approach
library(cluster)

# Single linkage
asw <- numeric(nrow(plants));
for (k in 2:(nrow(plants)-1)) {
  sil <- silhouette(cutree(p.dist.single, k=k), p.dist)
  asw[k] <- summary(sil)$avg.width
}
k.best.single <- which.max(asw);
k.best.single
plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, single",
     xlab="k (number of groups)", ylab="Average silhouette width")
axis(1, k.best.single, paste("optimum",k.best.single,sep="\n"), col="red", font=2, col.axis="red")
points(k.best.single, max(asw), pch=16, col="red", cex=1.5)  

# Complete linkage
asw <- numeric(nrow(plants));
for (k in 2:(nrow(plants)-1)) {
  sil <- silhouette(cutree(p.dist.complete, k=k), p.dist)
  asw[k] <- summary(sil)$avg.width
}
k.best.complete <- which.max(asw);
k.best.complete
plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, complete",
     xlab="k (number of groups)", ylab="Average silhouette width")
axis(1, k.best.complete, paste("optimum",k.best.complete,sep="\n"), col="red", font=2, col.axis="red")
points(k.best.complete, max(asw), pch=16, col="red", cex=1.5)  

# UPGMA
asw <- numeric(nrow(plants));
for (k in 2:(nrow(plants)-1)) {
  sil <- silhouette(cutree(p.dist.UPGMA, k=k), p.dist)
  asw[k] <- summary(sil)$avg.width
}
k.best.upgma <- which.max(asw);
k.best.upgma
plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, UPGMA",
     xlab="k (number of groups)", ylab="Average silhouette width")
axis(1, k.best.upgma, paste("optimum",k.best.upgma,sep="\n"), col="red", font=2, col.axis="red")
points(k.best.upgma, max(asw), pch=16, col="red", cex=1.5)  


# Ward
asw <- numeric(nrow(plants));
for (k in 2:(nrow(plants)-1)) {
  sil <- silhouette(cutree(p.dist.ward, k=k), p.dist)
  asw[k] <- summary(sil)$avg.width
}  
k.best.ward <- which.max(asw);
k.best.ward
plot(1:nrow(plants), asw, type="h", main="Silhouette-optimal number of clusters, Ward",
     xlab="k (number of groups)", ylab="Average silhouette width")
axis(1, k.best.ward, paste("optimum",k.best.ward,sep="\n"), col="red", font=2, col.axis="red")
points(k.best.ward, max(asw), pch=16, col="red", cex=1.5)  

# The optimal amount of clusters - Mantel approach

##first define the function to make the binary matrix:
grpdist <- function(X)
{require(cluster)
 gr <- as.data.frame(as.factor(X))
 distgr <- daisy(gr, "gower")
 distgr}
kt <- data.frame(k=1:nrow(plants), r=0)


# Single linkage
for (i in 2:(nrow(plants)-1)){
  gr <- cutree(p.dist.single, i)
  distgr <- grpdist(gr)
  mt <- cor(p.dist, distgr, method="pearson")
  kt[i,2] <- mt}
k.best <- which.max(kt$r)
plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, single",
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
points(k.best, max(kt$r), pch=16, col="red", cex=1.5)

# Complete linkage
for (i in 2:(nrow(plants)-1)){
  gr <- cutree(p.dist.complete, i)
  distgr <- grpdist(gr)
  mt <- cor(p.dist, distgr, method="pearson")
  kt[i,2] <- mt}
k.best <- which.max(kt$r)
plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, complete",
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
points(k.best, max(kt$r), pch=16, col="red", cex=1.5)

# UPGMA
for (i in 2:(nrow(plants)-1)){
  gr <- cutree(p.dist.UPGMA, i)
  distgr <- grpdist(gr)
  mt <- cor(p.dist, distgr, method="pearson")
  kt[i,2] <- mt}
k.best <- which.max(kt$r)
plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, UPGMA",
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
points(k.best, max(kt$r), pch=16, col="red", cex=1.5)

# Ward
for (i in 2:(nrow(plants)-1)){
  gr <- cutree(p.dist.ward, i)
  distgr <- grpdist(gr)
  mt <- cor(p.dist, distgr, method="pearson")
  kt[i,2] <- mt}
k.best <- which.max(kt$r)
plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters, Ward",
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", font=2, col.axis="red")
points(k.best, max(kt$r), pch=16, col="red", cex=1.5)

par(mfrow=c(1,1))


### Silhouette plots # Use the highest number of optimal clusters found in the last exercise
cutg1 <- cutree(p.dist.single, k=7);
sil1 <- silhouette(cutg1, p.dist);
silo1 <- sortSilhouette(sil1);
rownames(silo1) <- row.names(plants)[attr(silo1,"iOrd")];
plot(silo1, main="Silhouette plot - Single", cex.names=0.8,col=silo1+1, nmax.lab=100)

cutg2 <- cutree(p.dist.complete, k=7);
sil2 <- silhouette(cutg2, p.dist);
silo2 <- sortSilhouette(sil2);
rownames(silo2) <- row.names(plants)[attr(silo2,"iOrd")];
plot(silo2, main="Silhouette plot - Complete", cex.names=0.8,col=silo2+1, nmax.lab=100)

cutg3 <- cutree(p.dist.UPGMA, k=7); #you can try different numbers of clusters (e.g the max k=7, or the intermediate k=5)
sil3 <- silhouette(cutg3, p.dist);
silo3 <- sortSilhouette(sil3); ##orders the rows of sil by clusters and decreasing silhouette width
rownames(silo3) <- row.names(plants)[attr(silo3,"iOrd")];#also order the plant numbers accordingly
plot(silo3, main="Silhouette plot - UPGMA", cex.names=0.8,col=silo3+1,  nmax.lab=100)


cutg4 <- cutree(p.dist.ward, k=7);
sil4 <- silhouette(cutg4, p.dist);
silo4 <- sortSilhouette(sil4);
rownames(silo4) <- row.names(plants)[attr(silo4,"iOrd")];
plot(silo4, main="Silhouette plot - Ward", cex.names=0.8,col=silo4+1, nmax.lab=100)


### Plotting the different clusters on the dendrogram
# Single
plants.final.single <- reorder(p.dist.single, p.dist);
plot(plants.final.single, hang = -1, xlab="5 groups", sub="", ylab="Height",
     main="Chord - Single (reordered)", labels=cutree(plants.final.single,k=25));
rect.hclust(plants.final.single,k=5)  

# Complete
plants.final.complete <- reorder(p.dist.complete, p.dist);
plot(plants.final.complete, hang = -1, xlab="5 groups", sub="", ylab="Height",
     main="Chord - Complete (reordered)", labels=cutree(plants.final.complete,k=25));
rect.hclust(plants.final.complete,k=5)  

# UPGMA
plants.final.upgma <- reorder(p.dist.UPGMA, p.dist);#reorder: making sure nearby object pairs are adjacent
plot(plants.final.upgma, hang = -1, xlab="5 groups", sub="", ylab="Height",
     main="Chord - UPGMA (reordered)", labels=cutree(plants.final.upgma,k=25));
##you could also use labels = row.names(plants) insead of labels=cutree(plants.final.upgma,k=25)
rect.hclust(plants.final.upgma,k=5) #you can test here the difference between 5 and 7 clusters 
#or with hcoplot
source("hcoplot.R")
hcoplot(p.dist.UPGMA, p.dist, k=5)


# Ward
plants.final.ward <- reorder(p.dist.ward, p.dist);
plot(plants.final.ward, hang = -1, xlab="5 groups", sub="", ylab="Height",
     main="Chord - Ward (reordered)", labels=cutree(plants.final.ward,k=25));
rect.hclust(plants.final.ward,k=5)  


### Combine the NMDS with the cluster results
plants.nmds <- metaMDS(decostand(plants,"norm"),distance="euclidean") # Compare to the same distance matrix - currently Chord
#metaMDS does not have an in-built"Chord" distance matrix to use. You need to use distance=euclidean on a standardized (decostand) dataset), 
#similar as for constructing the distance matrix itself (see Ex1)
plants.nmds 
plot(plants.nmds, type="n", main=paste("NMDS/Chord - Stress =", round(plants.nmds$stress,3)))
text(plants.nmds,display=c("sites")) #(fyi: display=c("species") plots the columns)
#or
plot(plants.nmds, type="text", main=paste("NMDS/Chord - Stress =", round(plants.nmds$stress,3)))

# give colours based on the the clusters of the cluster analysis
plants.groups <- cutree(p.dist.UPGMA, k=5)
str(plants.groups)
plants.groups<-as.factor(plants.groups)
grp.lev <- levels((plants.groups)) #for the legend
p <- ordiplot(plants.nmds, type="n", main="NMDS/Chord - clusters UPGMA")
abline(h=0, lty=2)
abline(v=0, lty=2)
points(plants.nmds$points, pch=21, cex=1.5, bg=plants.groups)
legend("bottomleft",legend=grp.lev, pch=16, col=grp.lev)


text(plants.nmds$points, row.names(plants), pos=4, cex =0.7)
text(plants.nmds,display=c("species")) #not very aesthetically


# Add the dendrogram 
ordicluster(p, p.dist.UPGMA, col="dark grey")





