################################################
#####       Practical -- RDA analysis      #####
################################################
#####         Necessary R Functions        #####
################################################

#### Function to conduct a RDA based genome scan
rdadapt <- function(rda,K)
{
  zscores<-rda$CCA$v[,1:as.numeric(K)]
  resscale <- apply(zscores, 2, scale)
  resmaha <- covRob(resscale, distance = TRUE, na.action= na.omit, estim="pairwiseGK")$dist
  lambda <- median(resmaha)/qchisq(0.5,df=K)
  reschi2test <- pchisq(resmaha/lambda,K,lower.tail=FALSE)
  qval <- qvalue(reschi2test)
  q.values_rdadapt<-qval$qvalues
  return(data.frame(p.values=reschi2test, q.values=q.values_rdadapt))
}

#### Read in genotype files (012 format from vcftools):
read.geno <- function(file) 
{
  geno <- fread(file, data.table = F, header = F)[,-1]
  rownames(geno) <- as.character(read.table(paste(file, 'indv', sep='.'))[,1]) #indv names as rownames
  genopos <- read.table(paste(file, 'pos', sep='.'))
  colnames(geno) <- as.character(paste(genopos[,1], genopos[,2], sep=';')) #Chr.pos as colnames
  return(as.data.frame(geno))
}


#### Convert genotype file (read.geno output) to penlight format:
geno2gl <- function(geno) {
  list <- as.list(as.data.frame(t(geno)))
  loci <- colnames(geno)
  positions <- as.character(lapply(str_split(loci,';'), function(x) x[2]))
  chromosomes <- as.character(lapply(str_split(loci,';'), function(x) x[1]))
  gl <- new('genlight', as.list(as.data.frame(t(geno))))
  gl@chromosome <- as.factor(chromosomes)
  gl@position <- as.factor(positions)
  gl@loc.names <- loci
  return(gl)
}

