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

# This cleans up everything in the "Environment" pane.
rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages
package.list <- c("tidyverse")

# read the `package.list` for the list of packages we will be needing
# Check what packages are installed installed arleady 
# If any of the packages in the c() list are not currently installed, install them.
for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}