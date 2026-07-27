#------------------------------------------------------------------------------#
# This file This file is going to create an analysis for Lab 1 for the first class
# PROJECT NAME : Lecture_1_Data_Analysis
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.4.1
# AUTHOR : Zachary_Johnigan
# DATE CREATED : Tuesday_July_21_2026
# NOTES : xxxx
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Set up working space and import packages
#------------------------------------------------------------------------------#

# Clean the Environment
rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages
package.list <- c("tidyverse")

# ???
for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}













# Set Working Directory
getwd()

my_directory <- "/Users/zacharyjohnigan/Library/CloudStorage/OneDrive-TheUniversityofChicago/UChicago Student/Quarter 8 - Summer 2026/2 Data For Policy Analys:Mgmt/Week 1/DPAM_Lecture_1"

my_data <- paste0(my_directory, "/Data/")
# "/Users/zacharyjohnigan/Library/CloudStorage/OneDrive-TheUniversityofChicago/UChicago Student/Quarter 8 - Summer 2026/2 Data For Policy Analys:Mgmt/Week 1/DPAM_Lecture_1/Data/"
my_data

output_dir <- paste0(my_directory, "Lecture_1")












# Load
load("Data/", 37941-0005-Data.rda)
?load
View(da37941.0005)
colnames(da37941.0005)
class(da37941.0005$CB9_AGECAT_SERVE_0)

library(dplyr)
glimpse(da37941.0005)

