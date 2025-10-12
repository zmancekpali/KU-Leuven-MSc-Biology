# VERY IMPORTANT for people who aren?t using a KULeuven Computer,
# but a personal one (this applies to both Windows as Mac users):
#
#     Make sure you?re using the MOST RECENT VERSION versions of both R as RStudio!
#     This to avoid errors when installing packages (last part of the script)!
#
# Be on the safe side and just:
#
#    Download & install a fresh copy of R: https://cran.freestatistics.org/
#    & download & install the newest RStudio: https://www.rstudio.com/products/rstudio/download/


### Installing Essential Packages needed in the practicals of this course ##
###         (needs to be done ONLY ONCE each 'new' computer!)             ##
### Mac user should first install XQuartz, from https://www.xquartz.org/  ##

### also download the most recent version of Rtools from https://cran.freestatistics.org/
setwd(setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis"))
install.packages("afex")
install.packages("broom")
install.packages("carData")
install.packages("devtools")
install.packages("digest")
install.packages("doBy")
install.packages("dplyr")
install.packages("effects")
install.packages("emmeans")
install.packages("flextable")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("ggmap")
install.packages("tmaptools")
install.packages("httpuv") 
install.packages("lmtest")
install.packages("lsmeans")
install.packages("lsr")
install.packages("multcomp")
install.packages("MuMIn")
install.packages("nlme")
install.packages("lme4")
install.packages("officer")
install.packages("openxlsx")
install.packages("psych")
install.packages("pwr")
install.packages("rgl")
install.packages("rockchalk")
install.packages("rvg")
install.packages("scales")
install.packages("stargazer")
install.packages("tibble")
install.packages("tidyr")
install.packages("tidyselect")
install.packages("tikzDevice")
install.packages("vegan")
install.packages("xml2")
install.packages("xtable")
install.packages("lattice")
install.packages("robustbase")
install.packages("glmulti")

#as the newest version of the "car" package gives some problems that are not yet solves, download a former version:
remotes::install_version("car", version = "3.1-2", repos = "https://cloud.r-project.org")




#specific packages for each session######
#3 model selection
install.packages("rJava")
install.packages("MASS") 
install.packages("glmulti")
#glmulti en rjava!


#6 nonlinear relationships
install.packages("nlstools")
install.packages("nlsMicrobio") # contains some bacterial growth data and fit functions
install.packages("investr") # for plotFit function
install.packages("MultiKink")#for dataset on the exercise on splines

#7
install.packages("survival")
install.packages("ggfortify")
install.packages("KMsurv")
install.packages("flexsurv")
install.packages("Rmisc")

#Mulativariate
install.packages("factoextra")
install.packages("ggbiplot")


install.packages("ade4")

install.packages("vegan")
install.packages("gclus")
install.packages("cluster")

install.packages("VennDiagram")


#data visualisation

install.packages("export")
install.packages("ggprism")
install.packages("svglite")
install.packages("gtools")

#extra
install.packages("glue")
install.packages("pacman")
install.packages("lubridate")

library(export)        # When you have a plot displayed in RStudio you can run this line:
# graph2ppt(file="Rplot.pptx", width=7, height=5, append=TRUE)
# A Powerpoint file named Rplot.pptx will be created in your workdir.
# This Powerpoint contains your plot formatted as a vector file.
# Right-Click it > Group > Ungroup to start edditing it.
# Every time you run this line (after a new plot was made in RStudio),
# a new slide will be added to this file because ', append=' is set TRUE (not FALSE).
#
# Because R always keeps on adding new layers above the allready excisting plot,
# it is advisable to completely wipe your plot window before creating the plot you
# want to export to ppt. Otherwise your vector file may contain previous plots as
# underlaying layers. This can be done using the script:
# while (dev.cur()>1) dev.off()
# 
# Instead of exporting graphs to Powerpoint, you can also choose to export
# these as .svg using graph2svg(file="Rplot.svg", width=7, height=5)
# With Inkscape (freeware) you can easily make beautiful graphs from these .svg?s
# 
# Also ANOVA-tables can be directly exported to excel using:
# fit=aov(yield ~ block + N * P + K, npk)
# x=summary(fit)
# table2excel(x=x,file="table_aov.xlsx", sheetName = "Anova_table", add.rownames = TRUE, digits=4,digitspvals=3, trim==TRUE, font="Times New Roman",pointsize=16,append=TRUE)
# with 'trim==TRUE', p-values lower than 0.001 will be shown as '<0.001'.


# /$$$$$$$$ /$$   /$$  /$$$$$$  /$$$$$$$$ /$$ /$$$$$$        /$$$$$$ /$$$$$$$$ /$$
#/__  $$__/| $$  | $$ /$$__  $$|__  $$__/| $//$$__  $$      |_  $$_/|__  $$__/| $$
#   | $$   | $$  | $$| $$  \ $$   | $$   |_/| $$  \__/        | $$     | $$   | $$
#   | $$   | $$$$$$$$| $$$$$$$$   | $$      |  $$$$$$         | $$     | $$   | $$
#   | $$   | $$__  $$| $$__  $$   | $$       \____  $$        | $$     | $$   |__/
#   | $$   | $$  | $$| $$  | $$   | $$       /$$  \ $$        | $$     | $$       
#   | $$   | $$  | $$| $$  | $$   | $$      |  $$$$$$/       /$$$$$$   | $$    /$$
#   |__/   |__/  |__/|__/  |__/   |__/       \______/       |______/   |__/   |__/