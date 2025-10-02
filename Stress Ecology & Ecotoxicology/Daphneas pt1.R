#WD
setwd("~/") #erases previously set WDs
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Stress Ecology & Ecotoxicology")
getwd()

#Packages
library(drc)
library(ggplot2)
library(MASS)
library(openxlsx)

#Data
daph <- read.xlsx("daphneas_pt1.xlsx")

#Data check
head(daph)
str(daph)

#Plots
boxplot(immobility ~ conc, data = daph) #see some pattern here

#Find EC50 at 48 hours
(EC48h <- drm(immobility ~ conc, data = daph, fct =
               LL.4(names=c("Slope","Lower Limit","Upper Limit", "EC50"))))
#This is a 4-parameter log-logistic regression
#Slope = steepness of the curve
  #Slope = -3.88, p < 0.05 -> this means that the curve increases steeply as 
                              #log(concentration) increases. The absolute value 
                              #tells you how sharp the transition is around the EC50.

#Lower Limit = response at the lowest dose (baseline)
  #Lower limit: 1.921, p > 0.05 -> At very low concentrations (close to 0), the 
                                   #model predicts about ~2% immobilisation, but 
                                   #because it’s not significant it is effectively 
                                   #“flat at 0%”. This matches your control vials 
                                   #(all 0% immobilisation).

#Upper Limit = response at the highest dose (maximal effect)
  #Upper limit -> 99.91, p < 0.05 -> At high concentrations, essentially 100% 
                                     #immobilisation. That’s consistent with your 
                                     #data at 1.6 mg/L ZnO.

#EC50 = the concentration at which = 50% immobilisation
  #EC50 -> 0.364, p < 0.05 -> The model estimates that half the daphnids are immobile 
                              #at 0.364 mg/L ZnO after 48 h.

summary(EC48h)

ED(EC48h, c(50),interval='delta') #this gives the 95% confidence interval
#so the interval ranges between 0.320 mg/L and 0.408 mg/L
#we can me 95% confident that the EC50 at 48 hours falls between 0.320 and 0.408 mg/L


plot(EC48h, broken=TRUE, xlab="Concentration", ylab= "Immobility")
#circles show the mean observed response per concentration
#the line shows the log-logistic regression model fitted to those observations
#classic S-shape

#ggplot graph -> looks nicer
daph_1 <- data.frame(conc = exp(seq(log(min(daph$conc[daph$conc>0])),
                                     log(max(daph$conc)), length=100)))
daph_1$fit <- predict(EC48h, daph_1)

#EC50 and CI from model earlier
EC50 <- 0.364
CI_low <- 0.3200
CI_high <- 0.408

conc_breaks <- c(0.1, 0.2, 0.4, 0.8, 1.6)

(daph_plot <- ggplot() +
    # observed points
    geom_point(data = daph,
               aes(x = conc, y = immobility, colour = "Observed data"),
               size = 2) +
    # fitted line
    geom_line(data = daph_1,
              aes(x = conc, y = fit, colour = "Dose-response model fit"),
              linewidth = 1) +
    # vertical line at EC50
    geom_vline(xintercept = EC50, linetype = "dashed", colour = "red") +
    # EC50 point
    geom_point(aes(x = EC50, y = 50), colour = "red", size = 3) +
    # EC50 label with CI
    annotate("text", x = EC50, y = 45,
             label = paste0("EC50 = ", EC50, 
                            " mg/L\n(95% CI: ", 
                            CI_low, "–", CI_high, ")"),
             hjust = -0.1, vjust = 0, colour = "red") +
    scale_x_log10(breaks = conc_breaks,
      labels = conc_breaks) +    
    scale_colour_manual(values = c("Observed data" = "cornflowerblue", "Dose-response model fit" = "blue2")) +
    labs(x = expression("ZnO concentration (mg·L"^-1*")"),
    y = expression("Immobility of "*italic("Daphnea magna")*" (%)"),
    title = "Dose–response curve for nano-ZnO exposure (48 h)",
    color = NULL) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(), 
    legend.position = "bottom"))

ggsave("dose_response_plot.jpg", daph_plot, units = "cm", 
       width = 30, height = 20)
