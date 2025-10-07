setwd("~/") #erases previously set WDs
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Stress Ecology & Ecotoxicology")
getwd()

#Packages
library(tidyverse)

#Data
hb <- read.xlsx("hb.xlsx")

head(hb)
str(hb)

hb <- hb %>%
  # 1) Use YOUR column names
  rename(
    ID   = id,
    A560 = abs_560_h,
    A576 = abs_576_h,
    A600 = abs_600_h,
    A593 = abs_593_p
  ) %>%
  # 2) Clean + coerce to numeric (removes NBSP, tabs, etc.)
  mutate(
    across(
      c(A560, A576, A600, A593),
      ~ as.numeric(str_replace_all(as.character(.), "[^0-9eE+\\-.]", ""))
    )
  ) %>%
  # 3) Compute the metrics
  mutate(
    H576_peak = A576 - (A560 + A600) / 2,
    protein_mg_per_mL = (A593 - 0.302) / 0.132,
    relative_Hb_content = H576_peak / protein_mg_per_mL
  )


