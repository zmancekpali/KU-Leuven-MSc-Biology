setwd("/Users/zoja/Desktop/KU Leuven/EEG")
getwd()
#### Script for basic data analysis
#### of Notothenia coriiceps ddRAD genotypes
## 20/03/2026
## E. Decru 


#install if needed (see also general package installation on Toledo)

install.packages("mmod")
install.packages("plotly")
install.packages("qvalue")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("qvalue")

#### load packages
library(vcfR) # to read vcf file
library(adegenet) # to manipulate data
library(poppr) # to visualize and filter missing data
library(hierfstat) # to calculate fst
library(pegas) # to calculate other f statistics
library(mmod) # to calculate gst
library(MASS) # to plot mds
library(factoextra) #to plot PCA
library(pcadapt) # to screen for outliers
library(plotly) # to use manhattan plots interactively
library(ggplot2) # to plot manhattan plots
library(qvalue) # to adjust p values


#set your working directory to the folder you made
# use : setwd("PATH")

#### STEP 1: load data and apply basic filters
#####
## read vcf 
ncor <- read.vcfR("notothenia_genomics/populations.snps.vcf") 

## convert to genind format
ncor <- vcfR2genind(ncor)
ncor #130 individuals, 2,728 loci, 5,456 alleles

#check information in genind object:
ncor@pop
popNames(ncor)
locNames(ncor)
indNames(ncor)
#we have no populations in our object (information lost due to transformation to genind)


## add population labels
# first read in the popmap file from last week,
# which contains the individuals and population labels
popmap <-  read.delim(("notothenia_genomics/popmap.tsv"), header = F)
head(popmap)
popmap$V2
# we need to exclude two individuals that were
# excluded during the Stacks bioinformatics pipeline
# these individuals are NTA_126 and STA_024
# we can exclude them like so:
popmap <- popmap[-c(106, 112), ]
# now we add the population labels to our genind object
pop(ncor) <- popmap$V2
ncor@pop
popNames(ncor)
locNames(ncor)
indNames(ncor)
# we need to double check that individuals and population
# labels are matching
cbind(indNames(ncor), ncor@pop)
ncor # 
#you can further check out your data e.g. with:
ncor@all.names


## calculate some basic summary statistics
ncor_smry <- summary(ncor)
ncor_smry$n.by.pop #number of indivifuals per pop
ncor_smry$loc.n.all #number of alleles per locus (2)
ncor_smry$pop.n.all #number of SNPs per pop
ncor_smry$NA.perc #Percentage of missing data
ncor_smry$Hobs #Observed heterozygosity per locus
ncor_smry$Hexp #Expected heterozygosity per locus

## investigate the number of alleles in more detail
barplot(ncor_smry$loc.n.all, ylab = "Number of alleles",
        main = "Number of alleles per locus")

## investigate the heterozygosity in more detail
plot(ncor_smry$Hexp, ncor_smry$Hobs,
     main = "Observed vs expected heterozygosity")
abline(0, 1, col = "red")
#Red line: observed = expected heterozygosity
#Here, less heterozygosity than expected (maybe inbreeding, assortative mating, ...)


## investigate missing data in more detail
info_table(ncor, type = "missing", plot = T, plotlab = F)
#NTA = a lot of missing data
#NKGI = litte missing data

## filter on missing data
ncor # 
ncor <- missingno(ncor, type = "loci", cutoff = 0.30) #retain only data with at most 30% missing data
#1859 loci with missing data removed
ncor <- missingno(ncor, type = "geno", cutoff = 0.30)
#25 genotypes removed
ncor # how many individuals and loci are remaining after filtering?
#105 individuals, 869 loci, 1738 alleles

## check missing data again
info_table(ncor, type = "missing", plot = T, plotlab = F) #Much better, but NTA as a whole removed
ncor_smry <- summary(ncor)
ncor_smry$NA.perc #6.649131% missing data now (better)

## filter also on minor allele frequency
ncor <- informloci(ncor, cutoff = 0, MAF = 0.05, quiet = F)
#Only retaining the loci with at least 0.05 variation

## assess the filtered data set again
ncor #105 individuals, 833 loci, 1666 alleles
ncor_smry <- summary(ncor)
ncor_smry$NA.perc
plot(ncor_smry$Hexp, ncor_smry$Hobs,
     main = "Observed vs expected heterozygosity")
abline(0, 1, col = "red")
#much more expected -> more aligned with HW

## add metadata for the filtered data set
metadata <- read.csv("notothenia_genomics/metadata.csv", header = T, sep = ",", dec = ".")
# check whether metadata and genind are in the same order
cbind(indNames(ncor), as.character(metadata$ind))
# need to subset and re-order
inc <- which(as.character(metadata$ind) %in% indNames(ncor))
metadata <- metadata[inc, ]
metadata <- metadata[order(match(metadata[[1]], indNames(ncor))), ]
cbind(indNames(ncor), as.character(metadata$ind))
# add to the genind object
strata(ncor) <- as.data.frame(metadata)
ncor@strata
setPop(ncor) <- ~pop
pop(ncor)

## clean up
rm(inc, popmap, ncor_smry)
#####


#### STEP 2: calculate FST and GST
#####
## overall F statistics
(ncor_fst_overall <- wc(ncor)) #FST = 0.007759513; #FIS = 0.1615496 (relatively low inbreeding coefficient)
#FST - very low genetic differentiation between the subopulations (gene flow relatively free between them)

## per locus
plot.default(sort(ncor_fst_overall$per.loc[, 1]), main = 'Fst')
plot.default(sort(ncor_fst_overall$per.loc[, 2]), main = 'Fis')

## calculate pairwise FST after Weir & Cockerham
(ncor_fst_wc_pw <- genet.dist(ncor, method = "WC84")) # this may run for some time
ncor_fst_wc_pw 
max(ncor_fst_wc_pw) #maximum genetic differentiation: NKGI & STA

## calculate pairwise GST after Hedrick
(ncor_gst_pw <- pairwise_Gst_Hedrick(ncor)) # ignore the warnings
ncor_gst_pw  
max(ncor_gst_pw) #maximum genetic differentiation: DI & STA (and followed closly by NKGI and STA)

## plot as heatmap
heatmap(as.matrix(ncor_fst_wc_pw), Rowv = NA, Colv = NA, symm = T) #STA and NKGI appear most different
heatmap(as.matrix(ncor_gst_pw), Rowv = NA, Colv = NA, symm = T) #STA and DI appear most different; but also STA and NKGI

## plot as MDS
ncor_fst_wc_pw_d <- dist(ncor_fst_wc_pw)
ncor_fst_mds <- isoMDS(ncor_fst_wc_pw_d)
ncor_fst_mds$stress #0.001735748 = low stress
ncor_fst_mds
plot(ncor_fst_mds$points[, 1], ncor_fst_mds$points[, 2],
     col = funky(length(levels(ncor$pop))), pch = 16, cex = 1.2,
     xlim = c(-0.04, 0.08), ylim = c(-0.03, 0.03))
text(ncor_fst_mds$points[, 1], ncor_fst_mds$points[, 2], pos = c(4, 3, 1, 2, 4, 4, 1),
     labels = attr(ncor_fst_wc_pw, "Labels"), cex = 1.2) #STA is clearly very different from the others

## same for GST
ncor_gst_pw_d <- dist(ncor_gst_pw)
ncor_gst_mds <- isoMDS(ncor_gst_pw_d)
ncor_gst_mds$stress #0.006532055 = also low stress
ncor_gst_mds
plot(ncor_gst_mds$points[, 1], ncor_gst_mds$points[, 2],
     col = funky(length(levels(ncor$pop))), pch = 16, cex = 1.2,
     xlim = c(-0.03, 0.09), ylim = c(-0.03, 0.03))
text(ncor_gst_mds$points[, 1], ncor_gst_mds$points[, 2], pos = c(4, 3, 1, 2, 4, 4, 1),
       labels = attr(ncor_gst_pw, "Labels"), cex = 1.2) #STA is again very different

## amova
ncor_dist  <- dist(ncor)
ncor_stra  <- strata(ncor)
set.seed(20210401)
(ncor_amova <- pegas::amova(ncor_dist ~ group/pop, data = ncor_stra, nperm = 1000))
ncor_amova

## clean up
rm(ncor_amova, ncor_fst_mds, ncor_fst_overall,
   ncor_gst_mds, ncor_stra, ncor_dist, ncor_fst_wc_pw, ncor_fst_wc_pw_d,
   ncor_gst_pw, ncor_gst_pw_d)
#####


#### STEP 3: perform a PCA and DAPC
#####
## create a data matrix and replace missing values
ncor_matrix <- tab(ncor, NA.method = "mean")

## run a pca
ncor_pcacomp <- prcomp(ncor_matrix, center=T, scale.=F)
summary(ncor_pcacomp)
# 8.810 % explained by PC1-4

#plot the axes 1 and 2:

s.class(ncor_pcacomp$x[,c(1,2)], xax= 1, yax=2, fac=ncor$pop,
        label=levels(ncor$pop), col=funky(length(levels(ncor$pop)))) #STA slightly removed

s.class(ncor_pcacomp$x[,c(3,4)], xax= 1, yax=2, fac=ncor$pop,
        label=levels(ncor$pop), col=funky(length(levels(ncor$pop)))) #all overlapped

## run a dapc
ncor_dapc <- dapc(ncor, n.da = 4, n.pca = 80)

## plot axes 1 and 2
scatter(ncor_dapc, col=funky(length(levels(ncor$pop)))) #STA very different

## plot axes 3 and 4
scatter(ncor_dapc, xax=3, yax=4, col=funky(length(levels(ncor$pop)))) #here STA right in the middle

## predict re-assignment
pred1 <- predict.dapc(ncor_dapc)
barplot(t(100 * round(pred1$posterior, 2)), col = funky(length(levels(ncor$pop))), ylab = "% assignment")

## run a cross-validation (command may run for some time)
ncor_xval <- xvalDapc(ncor_matrix, ncor$pop, n.pca.max = 80, training.set = 0.9,
                      result = "groupMean", scale = F, n.rep = 10, 
                      n.pca = c(seq(5, by = 5, to = 80)), xval.plot = T)
ncor_xval #used first 5 PCs (variance = 0.195)

## re-run dapc with optimized results
ncor_dapc <- dapc(ncor, n.da = 5, n.pca = 70)
scatter(ncor_dapc, col=funky(length(levels(ncor$pop)))) #sta different
scatter(ncor_dapc, xax=3, yax=4, col=funky(length(levels(ncor$pop)))) #all overlain
pred1 <- predict.dapc(ncor_dapc)
barplot(t(100 * round(pred1$posterior, 2)), col = funky(length(levels(ncor$pop))), ylab = "% assignment")

## clean up
rm(ncor_dapc, ncor_matrix, ncor_pcacomp, ncor_xval, pred1)
#####

#### STEP 4: scan for outlier loci
#####
## prepare data
ncor_df <- genind2df(ncor, usepop = F, oneColPerAll = F)
dim(ncor_df)
ncor_df[1:10,1:10]
ncor_df[ncor_df == '00'] <- '0'
ncor_df[ncor_df == '01'] <- '1'
ncor_df[ncor_df == '10'] <- '1'
ncor_df[ncor_df == '11'] <- '2'
ncor_df[1:10, 1:10]
ncor_pcadapt_matrix <- read.pcadapt(ncor_df, type = 'lfmm')
ncor_pcadapt_matrix[1:10, 1:10]

## run pcadapt
set.seed(20210401)
ncor_pcadapt <- pcadapt(ncor_pcadapt_matrix, K = 20, min.maf = 0.05) # ignore warning message

## check screeplot for optimal number of PCs
plot(ncor_pcadapt, option = 'screeplot')

## check score plot
plot(ncor_pcadapt, option = "scores",
     pop = pop(ncor)) # ignore warning message

## on PC 3 & 4
plot(ncor_pcadapt, option = "scores", i = 3, j = 4,
     pop = pop(ncor)) # ignore warning message

## re-run pcadapt with optimal K
set.seed(20210401)
ncor_pcadapt <- pcadapt(ncor_pcadapt_matrix, K = 3, min.maf = 0.05)

## examine
summary(ncor_pcadapt)

## replace NAs in pvalues with 1
ncor_pcadapt$pvalues[is.na(ncor_pcadapt$pvalues)] <- 1
ncor_pcadapt$pvalues
length(ncor_pcadapt$pvalues) #833

## plot loci
manhattan_plot(ncor_pcadapt, chr.info = NULL, snp.info = locNames(ncor), plt.pkg = 'plotly')
plot(ncor_pcadapt, option = "manhattan")

## same with ggplot
manhattan_plot(ncor_pcadapt, chr.info = NULL, plt.pkg = 'ggplot') +
  theme_bw() +
  xlab("Locus index") +
  ggtitle("") +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## investigate statistics in more detail
qq_plot(ncor_pcadapt)
plot(ncor_pcadapt, option = "qqplot")
hist(ncor_pcadapt$pvalues, xlab = "p-values", main = NULL, breaks = 50,
     col = "orange")

## correct p values for multiple testing
qval <- qvalue(ncor_pcadapt$pvalues)$qvalues
qval
length(qval) #833


## identify outliers at a 0.05 threshold
ncor_outliers <- which(qval < 0.05)
length(ncor_outliers) #17 (with k = 3) -> 17 significant values
(ncor_outliers <- locNames(ncor)[ncor_outliers])
ncor_outliers

ncor_outliers <- which(qval < 0.05)
snp_pc <- get.pc(ncor_pcadapt, ncor_outliers) #gives each SNP and the PC its most associated with 
(snp.pc.grouped <- group_by(snp_pc, PC)) #groups PCs
(snp.grouped.summary <- dplyr::count(snp.pc.grouped)) #tidyverse conflicts with ()masks

## clean up
rm(ncor_df, ncor_pcadapt, ncor_outliers, ncor_pcadapt_matrix, qval)
#####
