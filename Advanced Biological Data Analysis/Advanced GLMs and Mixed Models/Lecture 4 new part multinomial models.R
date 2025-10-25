library(nnet)
library(effects)
library(ggplot2)
library(car)
library(export)
library(MuMIn)
library(reshape2)

setwd("C:/Users/pvdbe/GDrive/Teaching/Advanced Biological Data Analysis/Aanpassingen 2024/Lecture 3 - generalized linear models")

#Load the dataset
df <- read.csv("red_fox.csv")

# Histograms for each Prey Selection category, split by Vegetation Cover
ggplot(df, aes(x = Prox_Human_Activity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_grid(Diet ~ Veg_Cover) +
  labs(title = "Histograms of Proximity to Human Activity by Prey Selection and Vegetation Cover",
       x = "Proximity to Human Activity (km)",
       y = "Count") +
  theme_minimal()



# Fit the multinomial logistic regression model
# We need to ensure the outcome variable is a factor
df$Diet <- as.factor(df$Diet)

# Fit the model
fit <- multinom(Diet ~ Prox_Human_Activity + Veg_Cover, data = df)

# Summary of the model
summary(fit)
Anova(fit, type="III")

plot(allEffects(fit), multiline=TRUE, confint=list(style="auto"))


graph2ppt(file="effectplot_additive")

# Fit the model
fit2 <- multinom(Diet ~ Prox_Human_Activity * Veg_Cover, data = df)

# Summary of the model
summary(fit2)
Anova(fit2, type="III")

plot(allEffects(fit2), multiline=TRUE, confint=list(style="auto"))

AICc(fit, fit2)

#extract model effects for predictor 'Prox_Human_Activity'
#only extract the first four columns that have the actual probabilities
proximity_effect <- as.data.frame(Effect("Prox_Human_Activity", fit))[1:4]
#use 'melt' to turn the table from wide to long format
proximity_effect <- melt(proximity_effect, id.vars = "Prox_Human_Activity", variable.name = "Diet", value.name = "Probability")

#make a stacked effects plot
ggplot(proximity_effect, aes(x = Prox_Human_Activity, y = Probability, fill = Diet)) +
  geom_area() +
  labs(x = "Proximity to Human Activity (km)",
       y = "Probability") +
  theme_minimal()

#extract model effects for predictor 'Veg_Cover'
#only extract the first four columns that have the actual probabilities
veg_cover_effect <- as.data.frame(Effect("Veg_Cover", fit))[,1:4]
#use 'melt' to turn the table from wide to long format
veg_cover_effect <- melt(veg_cover_effect, id.vars = "Veg_Cover", variable.name = "Diet", value.name = "Probability")

#make a stacked effects plot
ggplot(veg_cover_effect, aes(x = Veg_Cover, y = Probability, fill = Diet)) +
  geom_bar(stat = "identity", position = 'stack') +
  labs(x = "Vegetation Cover",
       y = "Probability") +
  theme_minimal()


library(MASS)

df <- read.csv("butterflies.csv")
head(df)

df$Dev_Stage <- factor(df$Dev_Stage, levels = c("Egg", "Larva", "Pupa", "Adult"), ordered = TRUE)

# Fit the ordinal logistic regression model
fit3 <- polr(Dev_Stage ~ Temp + Host_Species + Week, data = df, Hess = TRUE)
Anova(fit3, type="III")

Temp_effect <- as.data.frame(Effect("Temp", fit3, xlevels=list(Temp=seq(min(df$Temp), max(df$Temp), length.out=100))))[,1:5]
#use 'melt' to turn the table from wide to long format
Temp_effect <- melt(Temp_effect, id.vars = "Temp", variable.name = "Dev_Stage", value.name = "Probability")

#make a stacked effects plot
ggplot(Temp_effect, aes(x = Temp, y = Probability, fill = Dev_Stage)) +
  geom_area() +
  labs(x = "Average Temperature",
       y = "Probability") +
  theme_minimal()

Host_effect <- as.data.frame(Effect("Host_Species", fit3))[,1:5]
#use 'melt' to turn the table from wide to long format
Host_effect <- melt(Host_effect, id.vars = "Host_Species", variable.name = "Dev_Stage", value.name = "Probability")

#make a stacked effects plot
ggplot(Host_effect, aes(x = Host_Species, y = Probability, fill = Dev_Stage)) +
  geom_bar(stat = "identity", position = 'stack') +
  labs(x = "Host_Species",
       y = "Probability") +
  theme_minimal()

Time_effect <- as.data.frame(Effect("Week", fit3, xlevels=list(Week=seq(min(df$Week), max(df$Week), length.out=100))))[,1:5]
#use 'melt' to turn the table from wide to long format
Time_effect <- melt(Time_effect, id.vars = "Week", variable.name = "Dev_Stage", value.name = "Probability")

#make a stacked effects plot
ggplot(Time_effect, aes(x = Week, y = Probability, fill = Dev_Stage)) +
  geom_area() +
  labs(x = "Time",
       y = "Probability") +
  theme_minimal()




