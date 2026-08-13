
####################
# PCA and biplot
####################

library(digest)
library(devtools)
library(ggbiplot)
library(openxlsx)


rm(list=ls()) #to clean up your environment when necessary

#### Exercise 1 - diploid-hexaploid contact zone of Aster amellus
aster=read.xlsx("Aster.xlsx")

View(aster)
str(aster)
aster$Ploidy=as.factor(aster$Ploidy)
which(is.na(aster))
asternew<-na.omit(aster) #a PCA on a dataset with missing data gives errors
which(is.na(asternew))
aster.data=asternew[,2:ncol(asternew)]            # Selection of the columns that contain data, leaving out the "Ploidy" level of the individuals
View(aster.data)
#
pairs(aster.data, cex=0.8, pch=21, col=c("blue","green","red")[asternew$Ploidy])
?pairs
cor(aster.data, use="complete.obs", method="pearson") #complete.obs is to include only the individuals where all data is present (omitting those with missing values)
#cor function gives correlation coefficients
# Variables that are correlated are: 
#ligule.length and Nr.stem.leaves
#ligule.length and Stem.length
#Nr.stem.leaves and Stem.length
#Nr.stem.leaves and Ligule.width

#yes, the PCA will be usefull to reduce the dimensions as quite some variables are correlated (however rather low correlation coefficients)

# You should use a correlation matrix, since not all data are in the same unit (mm, cm, count data) -> scale.=TRUE; center always has to be TRUE
pca.aster = prcomp(aster.data, center=T, scale.=T)


#
pca.aster #s.d and eigenvectors --> Show which variables contribute most to each component (eigenvectors of which elements are loadings)
#eigenvectors needed for interpretation of the data (see later)

eigenvalues = pca.aster$sdev^2 #amount of variance explained per PC
eigenvalues #amount of variance of the data along the principal axes; one eigenvalue per PC
sum(eigenvalues)  #total amount of variation of the variables; sum eigenvectors = total variability
summary(pca.aster)  # Shows how much variance is explained by the different principal components (in proportions)
screeplot(pca.aster, main = "Scree plot", type="lines") ##PC1 and PC2 important, rest not that important

#
plot(pca.aster$x[,c(1,2)],col ="black", cex=1) #pca.aster$x contains the scores --> position on graph per object (F=U*Yc)
plot(pca.aster$x[,c(1,3)],col ="black", cex=1)
plot(pca.aster$x[,c(2,3)],col ="black", cex=1)

# Different color for each group: blue=diploid, green=hexaploid, red=tetraploid
# use the Ploidy column of the asternew dataset, because pca.aster uses the dataset without the NA point 
levels(asternew$Ploidy) #check the order of the factor to interpret colours
plot(pca.aster$x[,c(1,2)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
legend("topleft",c("diploid", "hexaploid", "tetraploid"), fill=c("blue","green","red"))
text(pca.aster$x[,c(1,2)], labels=row.names(asternew), pos=3) #add ID's of objects; pos=3 meansadding the text above the coordinates
?text

plot(pca.aster$x[,c(1,3)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
plot(pca.aster$x[,c(2,3)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
plot(pca.aster$x[,c(1,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
plot(pca.aster$x[,c(2,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)
plot(pca.aster$x[,c(3,4)], pch=21,bg=c("blue","green","red")[asternew$Ploidy],cex=1)

# Diploids and hexaploids are clearly separated on the first PC axis, tetraploids (the current hybrid of the two cytotypes)
# are not morphologically very distinct from the other two groups.
# In the literature the diploids and hexaploids are considered to be two different subspecies.
# Since intermediate hybrids (tetraploids) are easily formed, all three groups still belong to the same species.

####compare graph with the loadings (eigenvectors) to check which original variables contribute most to the variability on the axes:
pca.aster

#these variables can also visualised on biplots:
#
par(mfrow = c(1, 2))
biplot(pca.aster, scale=0)           # Distance biplot - distances among objects are ~ Euclidian (cfr PCAplot above)
biplot(pca.aster, pc.biplot=TRUE)    # Correlation biplot - angles between descriptors reflect their correlation

par(mfrow = c(1, 1))

# a nicer way to make correlation biplots with using ggbiplot
library(ggbiplot)

# use the Ploidy column of the asternew dataset, because pca.aster uses the dataset without the NA point 
g <- ggbiplot(pca.aster, obs.scale = 1, var.scale = 1, groups = asternew$Ploidy, ellipse = TRUE, circle = FALSE); 
g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
g;
?ggbiplot


# Hexaploids are generally bigger; higher stem length, larger ligules and a higher number of stem leaves


#### FYI: nicer biplot (see code in theory slides)
# ggfortify::autoplot() is a wrapper that produces ggplot2 graphics; autoplot returns ggplot objects so you can add ggplot layers
library(ggfortify);

biplot2 <- autoplot(pca.aster, data=asternew, colour='Ploidy', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position.inside= c(0.85, 0.15)) +
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(legend.text = element_text(colour="black", size = 12)) +
  scale_fill_manual(values=c("blue","green","red")) +
  scale_color_manual(values=c("blue","green","red")) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
  geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);

biplot2;


##plotting other PC's can be done by specifying the axes with x= and y= (default are PC1 and PC2):
biplot2b <- autoplot(pca.aster, x=3, y=4, data=asternew, colour='Ploidy', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position.inside= c(0.85, 0.15)) +
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(legend.text = element_text(colour="black", size = 12)) +
  scale_fill_manual(values=c("blue","green","red")) +
  scale_color_manual(values=c("blue","green","red")) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
  geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);

biplot2b;


##once you loaded ggfortify, making an 'ordinary ggbiplot will give errors.
#unload ggfortify in order to ggbiplot to work again

##for your interest: visualisation with fviz_pca from package factoextra 

library(factoextra)

fviz_pca(pca.aster)#with biplot
fviz_pca_ind(pca.aster)#without biplot
fviz_pca_ind(pca.aster, habillage=asternew$Ploidy, invisible="quali", geom= c ("point", "text"), 
             pointsize=2, labelsize=4)#different colours for different groups
#the argument invisible=quali is to remove the mean point for each habillage that is generated
fviz_pca_ind(pca.aster, axes = c(1,2), habillage=asternew$Ploidy, invisible="quali", label="var",
             addEllipses=T, ellipse.type="convex",title="PCA aster", 
             palette=c("blue","green","red"))
fviz_pca(pca.aster, axes = c(1,2), habillage=asternew$Ploidy, invisible="quali", label="var",
             addEllipses=T, ellipse.type="convex",title="PCA aster", 
             palette=c("blue","green","red"))
# habillage: convex hulls around groups
#axes: define which PC axes to visualise
fviz_pca_ind(pca.aster, axes = c(1,3), habillage=asternew$Ploidy, invisible="quali", label="var",
             addEllipses=T, ellipse.type="convex",title="PCA aster", 
             palette=c("blue","green","red"))


###################################
### Exercise 2 - Queen pheromones in social wasps

# Social wasp datasets - cuticular hydrocarbon data was log-ratio transformed to account for differences in concentrations among samples
dolicho<-read.xlsx("Dolichovespula saxonica log-ratio.xlsx", rowNames = T)
View(dolicho)
library(dplyr)
str(dolicho)
dolicho<-mutate_if(dolicho, is.character, as.factor)
dolicho.data = dolicho[,6:ncol(dolicho)]
which(is.na(dolicho.data)) #no missing data so ok

# PCA Dolichovespula saxonica
# Use a covariance matrix because all data is in the same unit (log ratio transformed cuticular hydrocarbon data) -> scale.=FALSE
pca.dolicho = prcomp(dolicho.data, center=T, scale.=F)  

pca.dolicho #it is not the case that some components stand out in contributing to the variance explained; it is more the total combination of components 
# that explain variability (and explain differences between groups)

eigenvalues=pca.dolicho$sdev^2 #amount of variance of the data along the principal axes; one eigenvalue per PC
sum(eigenvalues)#total amount of variation of the variables; sum eigenvectors = total variability
eigenvalues
summary(pca.dolicho) #proportion variance explained per PC
screeplot(pca.dolicho, main= "Scree plot",type="lines") #first 4 PC axes are meaningfull

levels(dolicho$Caste)

# PCA plots of Dolichovespula saxonica - queen=blue, reproductive worker=green, sterile worker=red
plot(pca.dolicho$x[,c(1,2)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
legend("bottomleft", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
text(pca.dolicho$x, labels=row.names(dolicho), pos=3) 
plot(pca.dolicho$x[,c(1,3)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
legend("topright", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
plot(pca.dolicho$x[,c(2,3)], pch=21,bg=c("blue","green","red")[dolicho$Caste],cex=1.5)
legend("topright", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
# PC1 shows that queens smell different than sterile workers
# when workers become reproductive they start smelling like the queen
# The Queen Pheromone is a fertility signal in D. saxonica

par(mfrow = c(1, 2))
# biplot
biplot(pca.dolicho,cex=0.9, scale=0) # Distance biplot - distances among objects are ~ Euclidian (cfr PCAplot above)
biplot(pca.dolicho, pc.biplot=TRUE) # Correlation biplot - angles between descriptors reflect their correlation
par(mfrow = c(1, 1))

#  biplot using ggbiplot
#  if the package ggfortify is loaded, you might have to unload it first for ggbiplot to work
g <- ggbiplot(pca.dolicho, obs.scale = 1, var.scale = 1, groups = dolicho$Caste, ellipse = TRUE, circle = FALSE);
g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
g
?ggbiplot


#### FYI: nicer biplots (see code in theory slides)
library(ggfortify);

biplot2 <- autoplot(pca.dolicho, data=dolicho, colour='Caste', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position.inside= c(0.85, 0.15)) +
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(legend.text = element_text(colour="black", size = 12)) +
  scale_fill_manual(values=c("blue","green","red")) +
  scale_color_manual(values=c("blue","green","red")) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
  geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);

biplot2;


#biplot using factoextra
fviz_pca(pca.dolicho, axes = c(1,2), habillage=dolicho$Caste, invisible="quali", label="var",
         addEllipses=T, ellipse.type="convex",title="PCA dolicho", 
         palette=c("blue","green","red"))



#################################################################################################################
## Vespula vulgaris##############################################################################################
#################################################################################################################

vespula<-read.xlsx("Vespula vulgaris log-ratio.xlsx", rowNames = T)
vespula<-mutate_if(vespula, is.character, as.factor)
vespula.data = vespula[,3:ncol(vespula)]
which(is.na(vespula.data)) 
View(vespula)
# PCA Vespula vulgaris
# Use a covariance matrix because all data is in the same unit (log ratio transformed cuticular hydrocarbon data) -> scale.=FALSE
pca.vespula = prcomp(vespula.data,center=T,scale.=F)  

eigenvalues=pca.vespula$sdev^2
sum(eigenvalues)
eigenvalues
summary(pca.vespula)
screeplot(pca.vespula, main= "Scree plot",type="lines")

pca.vespula



# PCA plots of Vespula vulgaris - queen=blue, reproductive worker=green, sterile worker=red
levels(vespula$Caste) #check the order of the different groups to link the colours
plot(pca.vespula$x[,c(1,2)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
legend("topleft", c("queen", "rep worker", "ster worker"), fill=c("blue","green","red"))
text(pca.vespula$x[,c(1,2)], labels= row.names(vespula), pos=2) 
plot(pca.vespula$x[,c(1,3)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
text(pca.vespula$x[,c(1,3)], labels=row.names(vespula), pos=2) 
plot(pca.vespula$x[,c(2,3)], pch=21,bg=c("blue","green","red")[vespula$Caste],col ="black", cex=1.5)
text(pca.vespula$x[,c(2,3)], labels=row.names(vespula), pos=2) 
# PC1 shows that queens smell different than all workers
#SW4 is probably an outlier that needs to be checked
# when workers become reproductive they do not smell like the queen.
# The Queen Pheromone is not a fertility signal in V. vulgaris

#  biplot using ggbiplot
g <- ggbiplot(pca.vespula, obs.scale = 1, var.scale = 1, groups = vespula$Caste, ellipse = TRUE, circle = FALSE);
g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
g


#  biplot using autoplot
biplot3 <- autoplot(pca.vespula, data=vespula, colour='Caste', frame=TRUE, frame.type="convex",  loadings=TRUE, loadings.colour = 'darkred', loadings.label = TRUE, loadings.label.size=4, loadings.label.colour = "darkred", loadings.label.vjust = 1.3, size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position.inside= c(0.85, 0.15)) +
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(legend.text = element_text(colour="black", size = 12)) +
  scale_fill_manual(values=c("blue","green","red")) +
  scale_color_manual(values=c("blue","green","red")) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  geom_vline(xintercept=0, linetype="dashed", linewidth=0.5) +
  geom_hline(yintercept=0, linetype="dashed", linewidth=0.5);

biplot3;


###################################
## Exercise 3 - Fish in ponds
###################################

fish<-read.xlsx("Data_Ponds.xlsx")
str(fish)
fish$VIS=as.factor(fish$VIS)
fish$Poel.Id.=as.factor(fish$Poel.Id.)
View(fish)

#
fish.data=fish[,c(2:17,19:ncol(fish))] # Remove the variables fish and Poel(=pond) from the dataset 
#(poel = ID and is not data to enter in the PCA)
#vis (fish) is our categorizing variable to examine differences for (~ predictor variable)

# Use a correlation matrix because not all variables have the same unit
pca.fish=prcomp(fish.data,center=T,scale.=T)
eigenvalues=pca.fish$sdev^2
sum(eigenvalues)
summary(pca.fish)

pca.fish

#
screeplot(pca.fish, main= "Scree plot",type="lines")
#the first  three PC's seem informative, containing most of the variability


#yellow = no fish
levels(fish$VIS)
plot(pca.fish$x[,c(1,2)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
plot(pca.fish$x[,c(1,3)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)
plot(pca.fish$x[,c(2,3)], pch=21,bg=c("yellow","blue")[fish$VIS],col ="black", cex=1)

#
biplot(pca.fish,scale=0)
biplot(pca.fish,pc.biplot=TRUE)

#
g <- ggbiplot(pca.fish, obs.scale = 1, var.scale = 1, groups = fish$VIS, ellipse = TRUE, circle = FALSE);
g <- g + theme(legend.direction = 'horizontal', legend.position = "top");
g;

#
fviz_pca(pca.fish, axes = c(1,2), habillage=fish$VIS, invisible="quali", label="var",
         addEllipses=T, ellipse.type="convex",title="PCA Fish", 
         palette=c("yellow", "blue"))
fviz_pca(pca.fish, axes = c(3,4), habillage=fish$VIS, invisible="quali", label="var",
         addEllipses=T, ellipse.type="convex",title="PCA Fish", 
         palette=c("yellow", "blue"))

#EXTRA: Test whether the principal components differ significantly between the two groups.

hist(pca.fish$x[,1])        # Check whether your variables (the positions of the points on the PC's: columns of matrix F) are normally distributed (to perform following analyses)
shapiro.test(pca.fish$x[,1])
hist(pca.fish$x[,2])
shapiro.test(pca.fish$x[,2])

library(afex)
library(car)
set_sum_contrasts()
fish.data.pc=cbind(fish,pca.fish$x[,1:4]) # Adding the first four PCs to the original dataset
mod1=lm(PC1~VIS,data=fish.data.pc)
summary(mod1)#The  presence/absence of fish has a significant effect on the position on PC1
Anova(mod1, type="III")
plot(fish.data.pc$VIS,fish.data.pc$PC1, xlab="VIS", ylab="PC1")

#The relationship between pc1 and the presence/absence of fish is significant, however, because we really don't see clear 
# structuring on the PCA, we doubt the biological relevance of this 'statistical difference'

mod2=lm(PC2~VIS,data=fish.data.pc)
summary(mod2)  
Anova(mod2, type="III") # Not significant
plot(fish.data.pc$VIS,fish.data.pc$PC2,  xlab="VIS", ylab="PC2")


