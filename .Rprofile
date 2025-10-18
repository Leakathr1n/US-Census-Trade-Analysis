# Title: US Census Trade Analysis
# Author: Lea Röller
# Date: 28. September 2025
# Last date modified: mentioned next to added code snippets


## Folders  ----
#Creating the folders that we need
if (!dir.exists("00 Additional files")) dir.create("00 Additional files")
if (!dir.exists("01 RDS")) dir.create("01 RDS")
if (!dir.exists("02 Scripts")) dir.create("02 Scripts")
if (!dir.exists("03 Outputs")) dir.create("03 Outputs")
if (!dir.exists("Archive")) dir.create("Archive")



## Libraries ----
# Installing libraries that are not installed yet 


# Loading libraries
library(jsonlite) # handling data in JSON format
library(glue) # constructing paths, variable names etc.
library(writexl) # Exporting excel sheets
library(readxl) # Reading excel sheets
library(ggplot2) #graphics
library(dplyr) # cleaning and working with data frames
library(tidyr) # re-shaping data

detach("package:dplyr", unload = TRUE) # putting the dyplr library before stats so that we can filter properly
library(dplyr, warn.conflicts = FALSE)
# search()




## Graphic Options ----
options(scipen = 999) # disable scientific notation for graphics later


## Goodbye ----
# Ensure that everything has run
print ("Hello - let's go!")
