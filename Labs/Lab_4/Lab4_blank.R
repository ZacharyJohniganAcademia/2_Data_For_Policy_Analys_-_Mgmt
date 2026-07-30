#------------------------------------------------------------------------------#
# This file xxxxx
# PROJECT NAME : Lab4_Zachary
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.2
# AUTHOR : Zachary Johnigan
# DATE CREATED : Thursday, July 30, 2026
# NOTES : xxxx
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Set up working space and import packages
#------------------------------------------------------------------------------#

rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages

package.list <- c("tidyverse")

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}