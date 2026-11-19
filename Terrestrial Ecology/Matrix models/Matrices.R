setwd("~/")
setwd("~/Desktop/KU Leuven/Terrestrial Ecology/Matrix models")
getwd()

#packages
library(readxl)
library(popbio)
library(popdemo)

#data
data_raw <- read_excel("assignment.xls")
colnames(data_raw)[1:3] <- c("individual", "t0", "t1")
#remove header rows and new recruits
data <- data[data$t0 != "-" & !is.na(data$t0), ]

#count totals per starting stage
(stage_totals <- table(data$t0))
#At t = 0: 50 flowering individuals, 48 juveniles, 184 non-flowering juveniles, 41 seedlings
  n <- c(S = 41, J = 48, N = 184, F = 50) #initial values at t = 0 = initial pop vector

transitions <- table(data$t0, data$t1)
stage_totals <- rowSums(transitions)
transition_probs <- transitions / stage_totals #values for the life cycle graph

#fecundity calculation:
new_seedlings <- sum(data_raw$t0 == "-") #35
flowering_total <- stage_totals["F"] #50
(fecundity <- new_seedlings / flowering_total) #fecundity = 0.7


#Population matrix:
A <- matrix(c(0, 0, 0, 0.700, #0.7 = fecundity
  0.8049, 0.0625, 0, 0, #0.8049 = S -> J; 0.0625 = J -> J
  0, 0.7708, 0.6902, 0.5600, #0.7708 = J -> N, 0.6029 = N -> N, 0.5600 = N -> F
  0, 0, 0.0761, 0.2200), byrow = TRUE, nrow = 4) #0.0761 = N -> F, 0.2200 = F -> F
rownames(A) <- colnames(A) <- c("S", "J", "N", "F")

#Reproductive value:
reproductive.value(A)

#R0:
net.reproductive.rate(A)

#growth rate:
lambda(A) #0.8403844
(r <- log(lambda(A))) #r = -0.1738959

#stable stage distribution:
stable.stage(A)
#         S          J          N          F 
#0.07679060 0.07945751 0.75156100 0.09219089 

#Keyfitz delta: 
KeyfitzD(A, n) #0.1819016

#Cohen's distance:
CohenD(A,n)

#Projections:
p <- pop.projection(A, n, 60) #60 time steps
stage.vector.plot(p$stage.vector[,1:15], col = c('cyan', 'darkorchid1', 'darkblue', "red"), las = 1, prop = FALSE)

#Fundamental matrix:
fundamental.matrix(A)
