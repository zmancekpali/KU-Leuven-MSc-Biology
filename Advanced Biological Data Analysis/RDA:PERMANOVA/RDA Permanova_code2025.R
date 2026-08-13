###################################
#########      RDA     ############ 
###################################

#set appropriate working directory 
 #e.g. setwd("C:/Users/u0081311/OneDrive - KU Leuven/ABDA 2025/MultiVariate/Practical11_RDA")

library(vegan)
library(openxlsx)

bryo<-read.xlsx("Bryophyte_community.xlsx",rowNames = T)    # Bryophyte communities
env<-read.xlsx("Environment_Vegetation.xlsx",rowNames = T)  # Environmental variables (including: chemical compounds, slope and two NMDS axes that represent the surrounding vegetation)

# NMDS scores here are a proxy of the communities of higher plants in the environment
# At the bottom of this script the  NMDS analysis of the vascular plant data to obtain this data is shown

##### Exercise 1.1
# RDA on untransformed bryophyte data
bryo.rda <- rda(bryo~.,env) # the dot refers to including all variables of the dataset env; 
#bryo = response matrix; env = explanatory matrix
summary(bryo.rda)
RsquareAdj(bryo.rda)$r.squared                # R^2 = proportion of constrained variance
RsquareAdj(bryo.rda)$adj.r.squared            # adjusted R^2

# RDA on Hellinger transformed bryophyte data
bryo.hel <- decostand(bryo,"hellinger")
bryo.hel.rda <- rda(bryo.hel~.,env) 
summary(bryo.hel.rda)
RsquareAdj(bryo.hel.rda)$r.squared                # R^2
RsquareAdj(bryo.hel.rda)$adj.r.squared            # adjusted R^2
# adjusted R²is % of variance explained, adjusted is to compensate for the fact that more variables lead to higher R²

# Use the hellinger transformed dataset for further analyses (it has the highest Adj. R^2)

##### Exercise 1.2
anova.cca(bryo.hel.rda, step=1000)                
anova.cca(bryo.hel.rda, by="axis", step=1000)  
#model significant --> bryophyte explained by env


##### Exercise 1.3
# Distance triplot: distances between sites, between species or between sites and species approximate their euclidean distances.
par(mar=c(5,5,5,5)) #set plot margins
plot(bryo.hel.rda, scaling=1,  main="Triplot RDA bryophytes scaling = 1",type="none") # empty plot  
bryo.sc1 <- scores(bryo.hel.rda, scaling=1) #retrieve the scores for sites and species of bryophytes
points(bryo.sc1$sites,col="black",pch=21,cex=0.7) #plot the site scores in points
text(bryo.sc1$species, row.names(bryo.sc1$species), col="red",cex=0.8)#plot the bryo species in text (response variables as labels)
##we could also add lines (or arrows) to these labels:
arrows(0,0,bryo.sc1$species[,1], bryo.sc1$species[,2], length =0.05, lty=1, col="red")
#length=0.05 is length of the arrow triangle (tips)
text(bryo.hel.rda, display="bp",col="blue", scaling=1)#display biplot --> gives the environmental characteristics in arrows (arrows for continuous explanatiory variables))


#display: bp= biplot, cn= centroids, wa or sites= sites, sp= species

#####R code from theory
plot(bryo.hel.rda, scaling=1,  main="Triplot RDA bryophytes scaling = 1") #everything at once
##we can also add lines for the response variables
bryo.sc1 <- scores(bryo.hel.rda, choices= 1:2, display="sp", scaling=1)
arrows(0,0,bryo.sc1[,1], bryo.sc1[,2], length =0, lty=1, col="red")


# Correlation triplot: the angles in the biplot between response (abundance data) and explanatory (environment) variables, and 
# between response variables themselves or explanatory variables themselves, reflect their correlations.
par(mar=c(5,5,5,5))
plot(bryo.hel.rda, scaling=2,  main="Triplot RDA bryophytes scaling = 2",type="none")   
bryo.sc1 <- scores(bryo.hel.rda, scaling=2)
points(bryo.sc1$sites,col="black",pch=21,cex=0.7)
text(bryo.sc1$species, row.names(bryo.sc1$species),col="red",cex=0.8)
arrows(0,0,bryo.sc1$species[,1], bryo.sc1$species[,2], length =0.05, lty=1, col="red")
text(bryo.hel.rda,display="bp",col="blue")

##### Exercise 1.4
vif.cca(bryo.hel.rda)
# Correlations among the environmental variables are <10

##### Exercise 1.5
# Criterion is P < 0.05
step.forward <- ordistep(rda(bryo.hel ~ 1, data = env), scope=formula(bryo.hel.rda), direction="forward", pstep=1000)
# Final model:  rda(bryo.hel ~ NMDS1 + NMDS2 + NH3 + Si + Corg + Ca + pH ,data=env)
# The final model can differ (!!)
?ordistep
step.forward$call

# Criterion is to stop when adj.R2 of full model is exceeded
step.forward.R2 <- ordiR2step(rda(bryo.hel ~ 1, data = env), scope=formula(bryo.hel.rda), direction="forward", pstep=1000)
step.forward.R2$call
# Final model:  rda(bryo ~ NMDS1 + NMDS2 + Si + NH3 + Corg + Ca ,data=env)
# In this case the final model without ph is better (the R² of the reduced model cannot exceed the R² of the full model)

# Let's continue with the model with ph in ex 1.6: NMDS1 + NMDS2 + Si + NH3 + Corg + Ca + pH

##### Exercise 1.6
bryo.hel.rda2 = rda(bryo.hel ~ NMDS1 + NMDS2 + Si + NH3 + Corg + Ca + pH ,data=env)
RsquareAdj(bryo.hel.rda2)$adj.r.squared 


##### Exercise 1.7
#distance triplot
par(mar=c(5,5,5,5))
plot(bryo.hel.rda2, scaling=1,  main="Triplot RDA bryophytes (forward selection) - scaling = 1",type="none")
bryo.sc.new <- scores(bryo.hel.rda2, scaling = 1)
points(bryo.sc.new$sites,col="black",pch=21,cex=0.7)
text(bryo.sc.new$species, row.names(bryo.sc.new$species),col="red",cex=0.8)
arrows(0,0,bryo.sc.new$species[,1], bryo.sc.new$species[,2], length =0.05, lty=1, col="red")
text(bryo.hel.rda2, display="bp",col="blue")

#label sites: use text instead of points: 
text(bryo.sc.new$sites,col="black")
#or 
text(bryo.hel.rda2,scaling=1, display="wa",col="black", cex=0.8)

#correlation triplot
par(mar=c(5,5,5,5))
plot(bryo.hel.rda2, scaling=2,  main="Triplot RDA bryophytes (forward selection) - scaling = 2",type="none")
bryo.sc.new <- scores(bryo.hel.rda2, scaling = 2)
points(bryo.sc.new$sites,col="black",pch=21,cex=0.7)
text(bryo.sc.new$species, row.names(bryo.sc.new$species),col="red",cex=0.8)
arrows(0,0,bryo.sc.new$species[,1], bryo.sc.new$species[,2], length =0.05, lty=1, col="red")
text(bryo.hel.rda2, display="bp",col="blue")

##### Exercise 1.8 - partial RDA
env2 <- env[,c(1:15)] # Environmental variables
veg <- env[,c(16:17)] # NMDS axes representing the vegetation data

bryo.pRDA <- rda(bryo.hel, veg, env2)
bryo.pRDA
# Conditioned: Variance explained by the covariable 'env2' and removed
# Constrained: Variance explained purely by 'veg'
# Unconstrained: Residual variance
RsquareAdj(bryo.pRDA)$adj.r.squared 

anova.cca(bryo.pRDA, step = 1000) # Vascular plant communities (i.e. NMDS axes) significantly affect the bryophyte community even though the constrained variance is low

bryo.pRDA2 <- rda(bryo.hel, env2, veg)
bryo.pRDA2
RsquareAdj(bryo.pRDA2)$adj.r.squared 
# Conditioned: Variance explained by 'veg' and removed
# Constrained: Variance explained purely by 'env2'
# Unconstrained: Residual variance

anova.cca(bryo.pRDA2, step = 1000) # The environmental variables also significantly affect the bryophyte community

#### Exercise 1.9
bryo.varpart <- varpart(bryo.hel, veg, env2)
par(mar=c(1,1,1,1))
plot(bryo.varpart, bg=c("blue","green"),Xnames=c("Veg","Env2"),cex=1.5)

bryo.varpart

#visualise outcomes-->use these values in function below (draw.pairwise.venn)
#the values in the venndiagram obtained from 'varpart' are the R²adjusted values
#the outcomes (conditioned, constrained and unconstrained) obtained from the partial RDA are the
#unadjusted R² values

# Or you could use:
library(VennDiagram)
par(mar=c(1,1,1,1))
plot.new()
draw.pairwise.venn(0.26,0.28, 0.21, c("Vegetation","Environment"), col=c("blue","green"), 
                   fill=c("light blue","lightgreen"),cex=1.3, cat.cex = 1.5)

## the numbers you need to fill in are the [a+b], [b+c] and [b] results respectively from the varpart

#############################################################################################
# FYI:  NMDS on the vascular plant community: an illustration of how the columns of the NMDS axes have been obtained

plant = read.xlsx("Vascular_plant_community.xlsx",rowNames = T)
head(plant)

nmds = metaMDS(decostand(plant,"nor"),distance="euclidean") # Chord distance
nmds
scores(nmds) ##these are the values in the dataset 'Environment_vegetation'
plot(nmds$points,pch=21,col="black",cex=1,main=paste("NMDS Stress =", round(nmds$stress,3  )))
text(nmds, display = "species",cex=0.7) # Adding plant species
#########################################################################################



############################################################################
# Exercise 2: MANOVA
##########################################################################


library(vegan)

#Exercise 2.1
manova_data <- read.xlsx("manova_data.xlsx")
str(manova_data)
manova_data$IC<-as.factor(manova_data$IC)
manova_data$year<-as.factor(manova_data$year)
levels(manova_data$IC)


# Exercise 2.2
MSP.manova <- manova(cbind(MSP1, MSP2, MSP3)~IC, data=manova_data) 
summary(MSP.manova, test="Wilks")


# Exercixe 2.3
summary(MSP.manova, test="Wilks")
summary(MSP.manova, test="Pillai")
summary(MSP.manova, test="Hotelling-Lawley")
summary(MSP.manova, test="Roy")

# Exercise 2.4
summary(aov(cbind(MSP1, MSP2, MSP3)~IC, data=manova_data))
# MSP-2 is most affected by inbreeding, but MSP-1 also affected


#gives same results:
library(afex)
library(car)
set_sum_contrasts()
fit1<-lm(MSP1~IC, data=manova_data)
fit2<-lm(MSP2~IC, data=manova_data)
fit3<-lm(MSP3~IC, data=manova_data)

Anova(fit1, type="III")
Anova(fit2, type="III")
Anova(fit3, type="III")

# Exercise 2.5 --> two-way MANOVA
IC.two.manova <- manova(cbind(MSP1,MSP2,MSP3)~IC*year, data=manova_data)
summary(IC.two.manova,test="Wilks")
summary(IC.two.manova, test="Pillai")

# MSP production did not differ significantly between years. However, we found a significant interaction of 'level of inbreeding' and 'year' on MSP production. 
# This indicates that MSP production varied slightly between years (but not significantly) and this variation was stronger in some of the IC groups. 


# Exercise 2.6
flight.manova <- manova(cbind(drythor,dryabdo,dryhead)~IC*year, data=manova_data)
summary(flight.manova,test="Wilks")
summary(flight.manova,test="Pillai")
# Inbreeding significantly affects flight performance and flight performance also differs between years, and the effect of inbreeding is different over the different years
summary(aov(cbind(drythor, dryabdo, dryhead)~IC*year, data=manova_data))
# Inbreeding mainly affects size of the thorax and the size of the head
# Furthermore, the size of the head differed between years and we found a significant interaction between inbreeding and year on the size of the thorax.

#in a model containing many response variables, the model could be simplified by removing the response variables that are not affected by inbreeding, year or an interaction between inbreeding and year


####################################
#Exercise 3: PERMANOVA
#####################################


# Exercise 3.1
OTU = read.xlsx("OTU.xlsx")
id = read.xlsx("id_variables.xlsx")

# Exercise 3.2
m1 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="bray")
m1 #only gives an oveall p-value for the model
m1 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="bray", by="terms")
m1
# There is a significant effect of location and a marginally significant (0.05<P<0.1) interaction effect 
?adonis2
# Exercise 3.3
m2 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="euclidean", by="terms")
m2
m3 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="jaccard", by="terms")
m3 
m4 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="manhattan", by="terms")
m4 
m5 <- adonis2(OTU ~ species*location, data=id, permutations=1000, method="canberra", by="terms")
m5

# Exercise 3.4 try additive models as interactions term is not or only marginally significant
m6 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="bray", by="terms")
m6
m7 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="euclidean", by="terms")
m7
m8 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="jaccard", by="terms")
m8
m9 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="manhattan", by="terms")
m9
m10 <- adonis2(OTU ~ species+location, data=id, permutations=1000, method="canberra", by="terms")
m10

# Exercise 3.5
m11 <- adonis2(OTU ~ location, data=id, permutations=1000, method="bray" , by="terms")
m11

# Exercise 3.6
# Bacteria are rather generalists. They are not really associated with specific bug species, but they are location specific.

