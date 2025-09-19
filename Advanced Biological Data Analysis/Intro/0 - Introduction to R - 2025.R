### A QUICK INTRODUCTION TO R ###

#### Start with refolding this code to get a better overview of this introduction session:
#### Refold all (Windows): Alt + O  OR Refold all (Mac): Cmd + option + O

#### This script works with folding sections. Click on the small black arrow left
#### from a title OR the double blue arrow right from it to unfold.
#### Note that not every title contains text / code.

## 1. > IMPORTANT REMARKS BEFORE WE START (read everything carefully!!!) ----
## 1.1. - Install the latest version of R and R studio !!! ====

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


## 1.2. - Advantages and disadvantages of R ====

# + Freely available 
# + Contains basic and advanced statistical analysis routines
# + The code is easy to modify
# - In comparison to other software, the menu system is less developed

## 1.3. - Where to find R help online? ====

# Important websites, books, information:

# Look at: www.r-project.org.
# From there, follow the links under "Documentation"
 
# Look at: www.statmethods.net,
# which is a very helpful web page with R documentation and examples

# Look at: www.stackoverflow.com
# where people ask questions and (usually) get answers to their problems


##1.4 - Preparing your laptop settings ====
#Please go through the folder on Toledo: "Introduction to R - instructions: Computer settings & Installing and opening files in R"
# Make sure your computer uses '.' as decimal separator and ',' as list separator

# IMPORTANT: First, make sure Microsoft Excel uses '.' instead of ',' as a decimal separator.
#            and that it uses ',' as a list separator. (cf. the intro ppt!)
# IF NOT:
# -> WINDOWS: Control Panel > Region > Additional settings:
#             1) Set "Decimal symbol" to '.'
#             2) Leave "grouping" empty
#             3) Set "List separator" to ','
#
# -> MAC: 'System preferences' -> 'Language and Region' -> 'Advanced':
#         1) Set "Number separators (Decimal)" to '.'
#         2) Leave "Number separators (Grouping) empty 

 


## 2. > PACKAGES AND HELP FILES (read everything carefully!!!) ----
## 2.2. - Installing and running packages ====

# One important feature of R is using packages. Packages expand base R to 
# include additional functions, and often they are made for a specific purpose.
# For instance, the package "vegan" was created for vegetation ecologists,
# and it contains many functions to analyze diversity and similarities
# in vegetation plots.

# In both R and RStudio you can use the command install.packages()
# For example, if we would like to install the package called "vegan",
# place the cursor on the next line or select the entire text with your cursor
# and type Ctrl-R, Ctrl-Enter, or click Run.

install.packages("vegan")

# Another way of doing this in RStudio is to use the "Packages"-tab in 
# the window on the lower right. Click the tab, click "Install".
# The window that now pops up lets you first choose whether to install
# from the CRAN repository, in which most packages can be found, or from
# a Package Archive File (.zip or .tar.gz), which only occasionally happens
# when you want to install a package that is not (yet) available on CRAN.
# In the next field type "vegan" and let it install 
# to the default packages folder.

# When installed you can not yet use the functionality of the package.
# First, use the command.

library(vegan)
 
# Now, it should be ready to use the functions that package provides.

#PLEASE INSTALL ALL PACKAGES YOU WILL NEED THROUGHOUT THIS COURSE! YOU FIND ALL OF THEM IN THE SCRIPT WE PROVIDED:
#"General package installation 2023 _ Advanced Data Analysis"


## 2.3. - Help files ====

# Another thing that is important for using both base R and any package 
# is knowing how to reach the help files for the different functions.
# For example, the package "data.table" contains a function called
# "sort", which sorts data into an ascending or descending order.
# But we don't know how it works yet, so you want to get some help.
# There are two ways to do this in R, either by typing: 

help(sort)

?sort #same result

# which will show the help file of the sort function included in base R.
# when you already know the name of the function (the one named "sort" in this case), 
# and you want to know what it can do and how you have to supply the arguments.



## 3. > IMPORTING DATA IN R ----

# With biological data it is very common to enter data it in MS Excel or similar software, 
# so it is essential to know how to read/load your data from such a spreadsheet program into R.
# Below you'll find a way to read data into R.

## 3.1. - Make a working directory,  place your data in it & tell R where this folder is ====

# first download the content (= script + dataset) for this practical from Toledo to a newly created folder!
# This newly created folder is your working directory.

# If you use a KU Leuven PC, you can place this folder on the C drive within the workdir folder (C:/workdir/YOURNEWFOLDER).  
# This workdir directory is private for each student and should appear on any computer that you log in on the 
# KU Leuven domain. It can also be used for other courses. 
# If you use your own laptop, download the content available on Toledo in a newly created folder somewhere else
# on your computer.



## 3.2. - Connect your working directory to R (tell R where your WorkDir is located) ====

# First, we are going to set the working directory, which is the default directory
# where R is going to read/load files or write/save files. The function is setwd("") 
# this means: set working directory.

?setwd # Have a look at the help file, if you want to know more about the setwd function

getwd()
setwd("/Users/zojamancekpali/Desktop/KU Leuven/Advanced Biological Data Analysis/Intro")
getwd()

# If you use a KU Leuven PC, it can look like this:

setwd("C:/Users/u0139012/OneDrive - KU Leuven/Practica KUL/1 - Semester I/Biological Data Analysis (B-KUL-G0S04A)/1 - Practicals 2020/0 - introduction to R") 

# If you use a personal PC, it can look like this:

setwd("C:/Users/u0139012/OneDrive/Documents/Biological Data Analysis/Practical 0") 

# Note that R uses '/' where Windows and Mac use '\' !! So you have to change them!!!

# Alternative way of setting your workdir in RStudio:
# Click session > Set Working Directory > Choose directory (or To source location)
# This sets your Workdir to the directory your R-script (*.R-file) is

## 3.3. - Load data to a dataframe in R ====

# Once R now where your working directory is, you can tell R to import your dataset from this folder.

#excel files can be directly imported with the option read.xlsx from the package openxlsx
library(openxlsx)
TREESdata <- read.xlsx("Trees.xlsx", sheet=1)
TREESdata # show the dataframe named 'TREESdata' (Further in 4 we will see the difference between vectors, matrices and dataframes)
head(TREESdata) # only shows the headers + first six lines (easier to check if your datasets loaded well)

#if you have .csv files,Use the function read.table() or read.csv to load your .csv to a dataframe in R. Here, you need to specify what your column separator is (sep="," tells the function that our columns
# are separated by a , instead of a ; or a tab) 

TREESdata <- read.table("Trees-2.csv", header=TRUE, sep=",")
TREESdata <- read.csv("Trees-2.csv", header=TRUE, sep=",") #check whether you obtain the same dataset if you read in the .csv file

# Which can be simplified too (make sure your computer uses '.' as a decimal separator and
# ',' as a column separator -> see introduction Powerpoint)

TREESdata<- read.csv("Trees-2.csv")
# This function does the same because the main settings of read.csv() are:
# 'header=TRUE' & sep=","; therefore you don?t need to write this)

## 3.4. - Check your data & the object mode(s): numerical, integer, factor or character ====

# BEFORE STARTING ANALYSIS, ALWAYS CHECK IF YOUR DATA IS ENTERED CORRECTLY IN R !!!!!!!!!!!!

# When an object is loaded in R, it is loaded in your so-called workspace. In RStudio, 
# the loaded objects of your workspace are listed in the "Environment"-tab, in the 
# window in the top right corner. 

# Click on the object to view it, or you can simply type the name of the object,
# to view it in the console.

TREESdata #The dataset we loaded 

# Make sure 'TREESdata' is loaded properly (e.g. columns not sticking together) before continuing,
# otherwise go back to 3.3

# Here are some other ways to quickly check if your data has been entered properly,
# e.g. to check if there's no mistakes in the column delimitation, or if continuous
# variables are not read as characters, etc.

# This function gives you the first 6 rows:

head(TREESdata)

# This function gives you some simple summary statistics:

summary(TREESdata)

# The following function gives you the number of rows (observations) and columns (variables) of
# the data object:

dim(TREESdata)

# The following function gives you the object mode (Numeric, Integer, Factor, Character) for each column of
# a dataframe together with some info on the different elements/slots it contains.

# This can be a very useful function throughout R, since you have to deal with so many input and output objects. 

str(TREESdata) #Girth = numerical, Height = integer & volume = numerical

# !! Remark: it is important to always check the type of object with str() before you start any analysis! !:
#
# Columns in dataframes (see 4.3) can contain four different data types: numerical (=numbers with decimals), integer (= whole numbers), factor
# (= factor with different levels) and character (=text)
#
# for example, if you have a categorical variable called Temperature with two levels: 20C and 24C which are entered as
# 20 and 24 in your dataset, then RStudio (and R) will set Temperature automatically as Integer since 20 and 24 are full
# numbers (not having decimals). However, in your analysis you want to use them as a Factor (=categorical variables) in for
# example an ANOVA. You can change the data type the temperature column within your dataset using:
#
# dataset$Temperature <- as.factor(dataset$Temperature) #(other functions possible: as.numeric, as.integer and as.character)
#
# The $ sign means 'within'; so "dataset$Temperature" means "the column 'Temperature' WITHIN the dataset named 'dataset'".
#
# Next: check again with str() if the data type of your columns is now correct
#
# The latest version of RStudio (and R) will automatically set columns which contain text (e.g. a column with the factor
# 'sex' containing the levels 'male and 'female') to the data type 'character'. You can in this case change the data type
# to 'factor' by using:

# dataset$sex <- as.factor(dataset$sex)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# An easier way to immediately change all 'character' columns to 'factor' columns is by using:

# dataset <- mutate_if(dataset, is.character, as.factor) 
# this functions is from the package dplyr:  library(dplyr)
        
# We will use this function multiple times in the practicals
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Other functions are:
#
# as.factor()
# as.character()
# as.numeric()
# as.integer()





## 4. > CREATING AND MODYFING DATA IN OBJECTS ----

# Creating data in R might take a bit of time to get used to and it is not something that
# you will do very often. Nonetheless, the syntax can be useful, and will help you to know
# the differences between vectors, matrices and dataframes in R.

# In this example will add 'vector', 'matrix' or 'dataframe' to the object name to make clear
# what type of object you?re working with (in the practicals we?ll not do this anymore !!).

## 4.1. - Vectors ====

# A vector has a mode (numeric, character, factor or integer) which tells R what type of data is in the vector.
# you can check the mode:  str(name_of_you_vector)


##        * Fill with own data ####

# We'll use the hypothetical example of an experiment in which beetles are caught in 
# pitfall traps over several days.

# Enter the number of the 1st day of your experiment:

vector.beetles.day1 <- c(4,1,2,1,0,3,3,12,7,3,1,2,5) # If you want to make a character vector:
                                                     # vectorname <- c("one","two","three")

vector.beetles.day1  # will list these observations.

str(vector.beetles.day1) # To check the object mode of your vector. In this case
                         # the vector is numerical

# We type c() around the observations so that these are combined into 
# a vector (hence the "c" of combine or concatenate).
# The vector "beetles.day1" is now loaded in your workspace.

##        * Fill with sequence (e.g. 1 - 13) ####
# Let's assume now that we also want to create another vector that
# contains the numbers of the pitfall traps: '1' to '13'.
# A useful function for creating this data is seq().

vector.pitfall.trap.names <- seq(1, 13, 1)

vector.pitfall.trap.names

# which creates a sequence of data, starting at 1, ending at 13, in steps of 1.

##        * Fill with text + sequence (e.g. Trap 1 - Trap 13) ####
# Or, even better, if we want to give them the name 'Trap 1' to 'Trap 13' we can use the following:

vector.pitfall.trap.names <- paste("trap", seq(1, 13, 1), sep=".")
# sep="." places a point in between the word "trap" and its number
# (It is important that you NEVER use spaces in names used in R)
# Note that text is always placed between " ".

vector.pitfall.trap.names # see the vector

##        * Copy & edit certain data points ####

# Now, we'll continue to the second day of your experiment. You might find that the numbers
# of beetles are extremely similar to day 1. All are the same, except for Trap 5 and Trap 12.
# One way to make the vector for the 2nd day is:

vector.beetles.day2 <- vector.beetles.day1  # make a new vector for day 2, start with a copy from day 1
                              # and edit it in the following steps.
                              # (have a look at your workspace if it appears)

vector.beetles.day2

vector.beetles.day2[5] <- 2          # modify beetles.day2 on the 5th data point to get the value of 2

vector.beetles.day2

vector.beetles.day2[12] <- 3         # modify beetles.day2 on the 12th data point to now get the value of 3

vector.beetles.day2

# You could also have combined these two, by writing

vector.beetles.day2[c(5,12)] <- c(2,3)

##        * Calculate from another vector ####

# Now, for the third day you (hypothetically) found almost three times (we will do: 'times three minus one') 
# as many beetles in each trap as on the second day, except for the 6th and 8th trap, where you only found 
# one-third the number of beetles. Of course, you could enter the data directly, which is probably what 
# but here we want to illustrate how you can use a previously created vector to create this new data
# you would normally do, for the third day.

vector.beetles.day3 <- 3*vector.beetles.day2-1

vector.beetles.day3 

vector.beetles.day3[c(6,8)] <- vector.beetles.day2[c(6,8)]/3

vector.beetles.day3 


## 4.2. - Matrices ====

# All columns in a matrix must have the same mode (numeric, character, factor or integer) and all
# the columns should have the same length.
# You can check the mode of your matrix:  str(name_of_you_matrix)

# In this example we will build a matrix containing numerical data.

##        * Combine vectors into a matrix ####

# Finally we have the data for all 3 days, but now they are still in separate
# vectors. We might want to merge these into an object for further analysis. Combining
# vectors into columns of one matrix goes as follows (this is not a dataframe! - see further):

matrix.beetles.all <- cbind(vector.beetles.day1, vector.beetles.day2, vector.beetles.day3)

matrix.beetles.all

str(matrix.beetles.all) # Our matrix contains numerical data (headers are characters)

##        * Edit data points in a matrix ####


matrix.beetles.all[5,1] <- 8 # Write '8' to the 5th row in the 1st column
matrix.beetles.all[8,2] <- 3 # Write '3' to the 8th row in the 2nd column

## 4.3. - dataframes ====

# A dataframe is more general than a matrix, in that different columns can have different
# modes (numeric, character, factor or integer). So if you want to combine for example columns
# with categorical variables (e.g. sex: male/female) with columns having numeric data (e.g. temperature in ?C),
# than you need to use a dataframe.
# You can check the mode(s) of your dataframe columns:  str(name_of_you_dataframe) OR WITH summary(name_of_you_dataframe)

# In this example we will first start with a simple dataframe only containing numerical columns.

##        * Convert matrix to dataframe ####

# Okay, now let's try to prepare a dataframe with all your beetle data so that it can 
# actually be used for analysis. Let's first put the names of the traps together with the
# data in a single dataframe.

dataframe.beetles.data <- as.data.frame(matrix.beetles.all)

head(dataframe.beetles.data) #have a look at the first lines of your dataframe

str(dataframe.beetles.data) # Each of the three dataframe columns contains numerical data


##        * Add/overwrite column names ####

# Never ever use a 'space' in data. Use a '_' or a '.' instead !!!!!!!!!!!

colnames(dataframe.beetles.data) <- paste(c("day_1", "day_2", "day_3")) #adds 'day_1' to column 1, 'day_2' to column 2,...

head(dataframe.beetles.data) # Changing column names ok?


# You can also automatically generate labels 'day_1' - 'day_3':

colnames(dataframe.beetles.data) <- paste("day_", 1:3, sep = "")

head(dataframe.beetles.data) # Changing column names ok?


# alternatively you could have changed each column separate:

names(dataframe.beetles.data)[1] <- "day_1" #changes the first column name to 'day 1'
names(dataframe.beetles.data)[2] <- "day_2"
names(dataframe.beetles.data)[3] <- "day_3"

head(dataframe.beetles.data) # Same result

##        * Add/overwrite row names ####

# In 4.1 we made a vector with pitfall names:

vector.pitfall.trap.names

#We can now past these names to the row names of our dataframe

row.names(dataframe.beetles.data) <- vector.pitfall.trap.names 

dataframe.beetles.data # Changing row names ok?


##        * Stacking ####

# Okay, that's starting to look pretty good, but  now the different days are side-by-side
# in so-called wide format. However, this is usually not the best way to enter data for
# later analysis. Actually, for many tests you will need the data to be in long format or
# stacked format. You can use the function stack() for this.

dataframe.beetles.data.stacked <- stack(dataframe.beetles.data)

dataframe.beetles.data <- dataframe.beetles.data.stacked # Than overwrite the original beetles.data with the stacked version

dataframe.beetles.data                # Stacking Ok?

str(dataframe.beetles.data) # The first column contains numerical data, the second factorial data

# This is all fine, but now we lost the information that for instance the 1st data
# point, the 14th data point, and the 27th data point are all paired, because they are from the
# same pitfall trap, so let's add this information as well. To do this, you can use the 
# function rep() to repeat the names of the traps 3 times.

dataframe.beetles.data$Trap <- as.factor(rep(vector.pitfall.trap.names, 3)) # we made the vector 'vector.pitfall.trap'
                                                                            # in step 4.1 of this script

dataframe.beetles.data

# Now give the columns the following names: "Nr.of.beetles", "day" and "trap" 

names(dataframe.beetles.data)[1] <- "Nr.of.beetles"
names(dataframe.beetles.data)[2] <- "Day"
names(dataframe.beetles.data)[3] <- "Trap"

dataframe.beetles.data # Column names ok?

##        * Add a new column with (log)transformed data ####

# Now ->add a column<- named 'Log.Nr.of.beetles' with log(1+x) transformed Nr. of beetles

dataframe.beetles.data$Log1p.Nr.of.beetles <- log(1+dataframe.beetles.data$Nr.of.beetles)

dataframe.beetles.data # Log transformed column added?

##        * Overwrite an existing column ####

# Multiply the number of beetles by two and ->overwrite the original column<- in the dataset

dataframe.beetles.data$Nr.of.beetles <- 2*(dataframe.beetles.data$Nr.of.beetles)

dataframe.beetles.data # Column overwritten?

##        * Object modes: numerical, integer, factor and character containing columns  ####

# Here we repeat what we saw in 3.6 !!

# This is starting to look like a proper dataset. However, if we now use the funtion str()

str(dataframe.beetles.data)

# we see that the variable Day is read as if it is a categorical factor (cf. 3.6), even though this is
# supposed to be a numerical or integer variable, because day 2 is later than day 1 and you can
# count the days. The variable Trap, on the other hand, should indeed be a categorical factor, 
# since trap 1 is not more or less than trap 2. They are simply different traps. Hence, as a 
# final step, let's make the variable Day into a numerical one, so that it can be used properly
# for later analysis.

dataframe.beetles.data$Day <- rep(1:3, each=13)    # repeat day 1 to day 3 each 13 times

dataframe.beetles.data

str(dataframe.beetles.data)

# If you would enter the data in excel, you would probably just enter in the variable Trap numbers from 1 to 13.
dataframe.beetles.data$Trap <- rep(1:13)
str(dataframe.beetles.data)

# Now R will read it as an Integer object, however, like mentioned above this is a categorical variable (Factor)
# You can change the type of object by typing:
dataframe.beetles.data$Trap <- as.factor(dataframe.beetles.data$Trap) #as.factor can also be replaced by
                                                                      # as.numeric or by as.integer
str(dataframe.beetles.data)


##        * Re-order levels of a factor (determines order in graphs and plots) ####

# If you have multiple levels in a categorical variable (13 levels in the factor 'Trap' which we coded as
# a factor in the previous lines),
# then you can choose to order them differently by typing:

levels(dataframe.beetles.data$Trap) # Check the current order of the levels within the factor 'trap'

dataframe.beetles.data$Trap <- factor(dataframe.beetles.data$Trap,levels=c("13", "12", "11", "10","9","8","7","6","5","4","3","2","1")) # Re-order

levels(dataframe.beetles.data$Trap) # Check the modified order of the levels within the factor 'trap'

# This could be useful when having multiple treatments, where you want to place your control first in for example plots
# e.g.: data$treatment <- factor(data$treatment, levels=c("control", "low dose", "high dose"))

## 5. > ACCESSING AND SUBSETTING DATA IN OBJECTS ----

# The different objects you've created so far have different formats.
# Have a look at these again and notice the differences:

vector.beetles.day1      # is a vector
 
matrix.beetles.all       # is a matrix
 
dataframe.beetles.data      # is a dataframe

# These three basic object types all have different ways of accessing the data in them.

## 5.1. - Vectors ####

##        * Read a data point / data points ####
# vector.beetles.day1 is an example of a vector
# Vectors have only a single dimension and, as you've seen before, 
# a single data point can be accessed by typing for example:

vector.beetles.day1[2]         # prints the 2nd vector element, which has the value of 1

# multiple points can be accessed:

vector.beetles.day1[c(1,2,3)]  # prints vector elements 1, 2, and 3, which have the values 4, 1, and 2
 
# If you were to use
 
vector.beetles.day1[3,6] # You get the error: "Error in vector.beetles.day1[3, 6] : incorrect number of dimensions"


## 5.2. - Matrices ####
##        * Read a data point / column / row ####
# The object matrix.beetles.all, on the other hand, is an example of a matrix.
# A matrix is a purely numerical object (in cannot contain factors!)
# A matrix has two or more dimensions and so you need to use commas
# to specify the correct dimension. Try:

matrix.beetles.all[2,]     # which shows you the second row, i.e. number of beetles in the 2nd pitfall
                           # trap for all three days.

matrix.beetles.all[2,2]    # which shows you only the data of the 2nd pitfall trap on the 2nd day.

# Let's get on with some other functions that let you access your data.

##        * Logic functions ==, > or <: TRUE or FALSE ####
# Using functions such as ==, > or < will return a logical TRUE or FALSE.
# For example:

matrix.beetles.all == 1

# or 

matrix.beetles.all > 5

# These return TRUE for all elements of beetles.all that equal the value 1, 
# or are higher than 5, respectively, and FALSE otherwise.

# Sometimes you want the positions where a TRUE occurs:

which(matrix.beetles.all == 1)

# or

which(matrix.beetles.all > 5)

# If you look carefully at those positions, this also shows you that R starts counting
# at the top of the first column and goes down before going to the next column.



## 5.3. - dataframes ====


# A dataframe is more general than a matrix, in that different columns can have different
# modes (numeric, character, factor or integer). So if you want to combine for example columns
# with categorical variables (e.g. sex: male/female) with columns having numeric data (e.g. temperature in ?C),
# than you need to use a dataframe:

dataframe.beetles.data # The dataframe we made in 4.3
                       # Data imported in R with read.xlsx, read.table() or read.csv() is also directly stored
                       # as a dataframe (e.g. TREESdata)!!

##        * Add a new column containing a categorical factor with multiple levels ####

# Beetles were not only trapped on two days, but also in three populations: the first 16 rows in Leuven,
# the 12 following rows in Heverlee and the 11 last rows in Lubbeek. You can add this information in 
# a new column with header 'Population':

dataframe.beetles.data$Population <- c(rep("Leuven",16), rep("Heverlee",12), rep("Lubbeek",11))

dataframe.beetles.data  #Check if a column with Population is added.

# NOTE THAT NOT EVERY LOCATION WAS SAMPLED ON EVERY DAY - THIS IS NO PROBLEM FOR THIS EXAMPLE - WE WILL GIVE
# A NOTE ON THIS IN 7 (under 'check if your dataset is balanced')

##        * List the column names of your dataframe ####


# The different variables (=column names) that are in our dataframe:
names(dataframe.beetles.data)

##        * Get data from a certain column: $-symbol ####


# Within dataframes you can refer to a certain column using the $-symbol:
dataframe.beetles.data$Nr.of.beetles

# But this would not be possible with a matrix:

matrix.beetles.all # quick recap about how our matrix looks like

matrix.beetles.all$vector.beetles.day1 # Error in beetles.all$beetles.day1 : $ operator is invalid for atomic vectors

# On the other hand, you can transform a matrix into a dataframe (like we saw in 4.3.):

dataframe.beetles.all <- as.data.frame(matrix.beetles.all)

dataframe.beetles.all

# And now you can easily access its named variables:

dataframe.beetles.all$vector.beetles.day1



# Similarly, the command names() only works for dataframes and not for matrices.
# Hence you can now easily change the variable names of your data:

names(dataframe.beetles.all) <- c("day_1", "day_2", "day_3")

dataframe.beetles.all

##        * Make a subset (e.g. only data from Heverlee) ####

# Another useful feature of a dataframe is that you can quite easily make a subset
# of your data. For instance, when you only want the trees in the "Heverlee" population:

# Lets go back to our original dataframe 'dataframe.beetles.data' (the one in which we added the population names):

dataframe.beetles.data

dataframe.beetles.data.Heverlee <- subset(dataframe.beetles.data, Population=="Heverlee")

dataframe.beetles.data.Heverlee

# If you would only want to keep the lines where Day=1, you can type:
dataframe.beetleDay.onlyday1 <- subset(dataframe.beetles.data, Day=="1")

dataframe.beetleDay.onlyday1

# If you would only want to keep the lines where Day=1 AND those of Day=2 you can type:
dataframe.beetleDay.onlyday1and2 <- subset(dataframe.beetles.data, Day=="1" | Day=="2") # ('|' means 'OR')

dataframe.beetleDay.onlyday1and2

##        * Compare the mean of two groups (e.g. how many % more/less beetle caught on average in Heverlee compared to Leuven) ####

# We will check if the mean number of beetles caught in total (over all days) is more/less in Heverlee then in Leuven.
# You can only report this number if the difference is significant (see following practicals)

dataframe.beetles.data

Heverlee <- subset(dataframe.beetles.data, Population=="Heverlee")
Leuven <- subset(dataframe.beetles.data, Population=="Leuven")

((mean(Heverlee$Nr.of.beetles)/mean(Leuven$Nr.of.beetles))-1)*100# -0.56 => mean # of beetles caught in Heverlee is 0.56% lower than in Leuven
                                                                  # You can only report this percentage when there is a SIGNIFICANT difference
                                                                  # (you need to statistically test this!)

# OR:

((mean(Leuven$Nr.of.beetles)/mean(Heverlee$Nr.of.beetles))-1)*100 # 0.56 => mean # of beetles caught in Leuven is 0.56% higher than in Heverlee
                                                                  # You can only report this percentage when there is a SIGNIFICANT difference
                                                                  # (you need to statistically test this!)

##        * Exclude certain observations (=exclude rows) ####

# If you want to perform a statistical test on a subset of your dataset (eg. on all traps except traps 5 & 8),
# than you can extract the needed lines to a new table by:
dataframe.beetles.data.subset <- subset(dataframe.beetles.data, Trap!="5" & Trap!="8") # Removes all rows with Trap=8 and Trap=5 ('!=' means 'NOT')

dataframe.beetles.data.subset

# This will give you a subset where trap 5 and 8 that run full of water on one of the days are removed from analysis

## 5.5. - Combine objects (as vector / horizontally / vertically) ====

##        * Combine data as a vector ####

# Let's take our 'dataframe.beetles.data' dataframe

dataframe.beetles.data

# If we would for example like to combine our column 'Nr.of.beetles' with the column 'Log1p.Nr.of.beetles' to a single vector
# then we could use c():

combinedvector <- c(dataframe.beetles.data$Nr.of.beetles,dataframe.beetles.data$Log1p.Nr.of.beetles)       # combine objects into a vector

combinedvector

##        * Combine data horizontally (combine as columns) ####

# Let's take our 'dataframe.beetles.data' dataframe

dataframe.beetles.data

# If we would as an example like to horizontally combine this dataframe with a copy of it, we could use
# cbind() (= column bind):


dataframe.horizontally.combined <- cbind(dataframe.beetles.data,dataframe.beetles.data) # combine objects as columns

dataframe.horizontally.combined


##        * Combine data vertically (combine as rows) ####

# Let?s take our 'dataframe.beetles.data' dataframe

dataframe.beetles.data

# If we would as an example like to vertically combine this dataframe with a copy of it, we could use
# rbind() (= row bind):


dataframe.vertically.combined <- rbind(dataframe.beetles.data,dataframe.beetles.data) # combine objects as columns

dataframe.vertically.combined


## 5.5. - A fourth object type: lists ====

# Another type of basic object that you will need to understand is a list.
# We can, for instance, make a list multiple objects such as:

data.list <- list(dataframe.beetles.data.subset, dataframe.beetles.data, Heverlee)

data.list

# Now, you can also refer to the dataframe "Heverlee" by typing:

data.list[[3]]
 
# since, it is the third object in the list. This can be useful when you have 
# a lot of datasets or datafiles that need to handled, but it won't be used very often
# in this course. This example is just for you to understand how it works.


## 5.6. - Other types of objects ====

# There are many more types of derived objects though. Every function in R creates
# an object that can be saved in the memory by giving it a name and using <- or =. 
# Once it's saved you can use it to recall output from the function.

# Take for example a Pearson correlation object:

correlation <- cor.test(vector.beetles.day1, vector.beetles.day2, method="pearson")

# If you now want to find the correlation estimate and p-value, 
# you can simply call the object:

correlation 

# and find that the correlation coefficient estimate is ... and the p-value is...

# However, if you want to use only these two statistics in further
# analysis, it's useful to be able to only get these two values.
# The way to do this is by typing:

correlation$estimate
correlation$p.value

# One way to find out about which arguments to use after the $ is by 
# calling the help documentation (cf. 1.3.)

help(cor.test)

# and look in the section called "Value". 
# Here you find which type of object you have created and which values 
# can be extracted from it. Another way to do this is by the aforementioned 
# function str().

str(correlation)

# Now, let's get the p-values for all three correlations:

p.value1 <- cor.test(vector.beetles.day1, vector.beetles.day2, method="pearson")$p.value
p.value1
p.value2 <- cor.test(vector.beetles.day1, vector.beetles.day3, method="pearson")$p.value
p.value2
p.value3 <- cor.test(vector.beetles.day2, vector.beetles.day3, method="pearson")$p.value
p.value3



## 6. > WORKING WITH NA?s (Where? / Remove / overwrite) ----

# The functions is.na() and na.omit() can also be handy.
# They test for occurrence of missing values: NA, meaning "Not Available".


# Let?s go back to the matrix 'matrix.beetles.all' (the handling of NA?s with is.na() and na.omit() also
# works with vectors and dataframes; but in this example we?ll use it on a matrix):

# For example, let's assume that on the 1st day the 5th pitfall trap was completely full
# of water and on the 2nd day this happened to Trap 8. Therefore, the data can't be used
# and we'll have to replace the values with NA, like so:

matrix.beetles.all

matrix.beetles.all[5,1] <- NA # the 5th row in the 1st column - (This works because we have a matrix
                              # you can?t change data in dataframe with this method!)

matrix.beetles.all[8,2] <- NA # the 8th row in the 2nd column

matrix.beetles.all # Check how your matrix looks like

# Now check for all instances where we find NA:

is.na(matrix.beetles.all)
which(is.na(matrix.beetles.all))

# This function is mostly handy when, for example, you have a lot of data and
# it's hard to figure out what their position is. Moreover, some functions in R
# can't handle missing data, so that you would have to remove all traps that have
# some missing data.

matrix.beetles.all  # Check how your matrix looks like

matrix.beetles.all.without.rows.containing.NA <- na.omit(matrix.beetles.all)

matrix.beetles.all.without.rows.containing.NA # below this table you see that rows 5 & 8 where deleted,
# because at least one column in these rows contained an NA

# Fine, but now for some special biological or statistical reason (normally you don?t do this!!!)
# you assume that those pitfall
# traps with missing data did not contain any beetles in the first place and you would 
# therefore replace these values with zeroes. You can now replace all missing values by
# zeroes as follows:

matrix.beetles.all[is.na(matrix.beetles.all)] <- 0

matrix.beetles.all




## 7. > SUMMARY STATISTICS ----


## 7.1. - R as a pocket calculator ====

# You can also use R as a regular pocket calculator, such as:
 
3*12
 
# But of course we can do calculations also on entire vectors:

vector.beetles.day2 - vector.beetles.day1 # calculates the elementwise differences between them



## 7.2. - length, sum, median, mean, max, min ====
# The following functions do what you would expect already from reading their names.

# Let's have a look again at our dataframe 'dataframe.beetles.all'

dataframe.beetles.all

head(dataframe.beetles.all) # or at only the headers + first lines

dataframe.beetles.all$day_1

length(dataframe.beetles.all$day_1)

sum(dataframe.beetles.all$day_1)  

median(dataframe.beetles.all$day_1)  

mean(dataframe.beetles.all$day_1)  

max(dataframe.beetles.all$day_1)  

min(dataframe.beetles.all$day_1)  


# If your data vector contains NA's (missing values), you can often add an option 
# to the function in order to remove missing values, see help(mean) for this.


## 7.3. - Check if a dataset is balanced (important for ANOVA, ANCOVA,...) ====
# We?ll go back to our beetles dataframe with the different days of sampling and the different locations:
dataframe.beetles.data

# We can check if the Nr of observations is balanced over the populations (= equal amount of observations for population) by:

table(dataframe.beetles.data$Population) #our data is absolutely not balanced!

# We can check if our dataset is balanced over days & populations (= equal amount of observations for each day for each location) by:

table(dataframe.beetles.data$Day, dataframe.beetles.data$Population) #our data is absolutely not balanced!

# You can even add more levels for more complex datasets:  table(DATA$Xcat1, DATA$Xcat2, DATA$Xcat...))

## 8. > PLOTTING DATA ----
## 8.1. - Scatter plots ====

# Simple scatter-plotting in R is easy, but more complicated graphs take
# quite a long time to figure out and really is a study in itself.

# We will use the dataframe 'TREESdata' which we loaded in 3.3 !!

TREESdata
head(TREESdata)

# Hence we'll start with something very simple:

plot(TREESdata$Girth, TREESdata$Volume)
# or
plot(TREESdata$Girth, TREESdata$Height)
# or
plot(TREESdata$Volume, TREESdata$Height)

# You will find that it does not look like there is a relationship 
# between girth and height, but girth and volume seem to correlate.

# But now let's make the graph look a little bit nicer. You can adjust many parameters,
# so here we'll immediately tinker with a whole lot of them to give you an idea.
# (Select all of the following and then Ctrl-R or click Run.)

plot(TREESdata$Girth, TREESdata$Volume,
     ylim = c(0,80),  # range of y-axis
     xlim = c(5,25),  # range of x-axis
     ylab = "Volume of the tree", # label of the y-axis
     xlab = "Girth of the tree",  # label of the x-axis
     col = adjustcolor("dark green", alpha.f=0.3),  # alpha.f=0.3 is to make points semi-transparent 
     pch = 16,  # symbol used 
     cex = 1.2  # point size
)

## 8.2. - Histograms ====

# Another useful plot to see the distribution of your data is a histogram.

hist(TREESdata$Girth)
hist(TREESdata$Volume)
hist(TREESdata$Height)

## 8.3. - Boxplots ====

# A third commonly used plotting method is a boxplot.
# This way you can visualize whether there are differences between
# your sampled groups.

boxplot(dataframe.beetles.data$Nr.of.beetles ~ dataframe.beetles.data$Population, ylab="Girth")



## 9. FUNCTIONS, FOR-LOOPS AND CONTINGENCY TABLES ----
## 9.1. - Functions: make your own ====

# Suppose you want the standard deviation of a variable.
# Let's try this:

std(dataframe.beetles.all$day_3) # Error: could not find function "std"

# Hence, this is not a function that is available, but we would like to use it,
# so lets' make a function that calculates a standard deviation ourselves.
# We will call it std. 

# As you can find in any statistics text book, the function will have to calculate
# the square root of the variance. In R, this must become a function, that uses 
# other calculations or already available functions, such as sqrt() and var().

std <- function(x) sqrt(var(x))

std(dataframe.beetles.all$day_3) # This works!


## 9.2. - For-loops ====

# A somewhat related topic are "for-loops".
# If (for some strange and unknown reasons) you'd  want to sum the height of each 
# tree with the height of the next tree in your data, you could use such a loop.
 
height.sum <- vector() 
for (i in 1:nrow(TREESdata)){
  height.sum[i] = TREESdata$Height[i] + TREESdata$Height[i+1]
}

height.sum

TREESdata <- cbind(TREESdata, height.sum)

TREESdata

 
## 10. INSTALL ALL PACKAGES NEEDED DURING THE FOLLOWING practicals ----

##  If you haven't done so already: install all packages needed for the further practicals (takes ca. 15 minutes).
## Please make sure everything is installed before the next practical; Preferably install them now se we can help in case of problems.
##  Open: "General package installation 2023 _ Advanced Data Analysis" (see Toledo) and run this
##  script line by line.







