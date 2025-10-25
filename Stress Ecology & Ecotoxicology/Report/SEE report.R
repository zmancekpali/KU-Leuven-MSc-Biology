setwd('~/Desktop/KU Leuven/Stress Ecology & Ecotoxicology/Report')
getwd()


#data
chronic <- read.xlsx("dataset_chronic_2025.xlsx")
acute <- read.xlsx("Acutetox_2025.xlsx")

head(chronic)
str(chronic)

acute <- acute %>% mutate(
  temp_f = factor(temperature, levels = c(20,24), labels = c("20°C","24°C")),
  zinc_f = factor(zinc, levels = c(0,1), labels = c("No Zn","Zn")),
  total = alive + dead + immo,
  prop_alive = alive / total)

chronic <- chronic %>% rename(temp = temperature, 
                              h560 = "Absorbance(560nm)Haem",
                              h576 = "Absorbance(576nm)Haem",
                              h600 = "Absorbance(600nm)Haem",
                              h_nn = "Haemo.non.normalized",
                              p593 = "Absorbance(593nm)Prot") %>%  mutate(
                                temp_f = factor(temp, levels = c(20,24), labels = c("20°C","24°C")),
                                zinc_f = factor(zinc, levels = c(0,1), labels = c("No Zn","Zn")))

head(acute)
str(acute)

#preliminary plots
boxplot(immo ~ zinc, data = acute)

boxplot(ctmax ~ temp, data = chronic)
plot(zinc ~ h560, data = chronic)
plot(zinc ~ h576, data = chronic)
plot(zinc ~ h600, data = chronic)
plot(zinc ~ p593, data = chronic)
plot(zinc ~ hemo, data = chronic)
plot(zinc ~ protein, data = chronic)


#EC50
