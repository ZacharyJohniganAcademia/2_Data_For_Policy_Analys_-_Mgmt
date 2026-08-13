#------------------------------------------------------------------------------#
# This file prepares NSECE 2019 data for Lab 7 of the Data for Policy Analysis 
# class.
# PROJECT NAME : Data for Policy Analysis
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.3
# AUTHOR : Zachary Johnigan
# DATE CREATED : 08-11-2026
# NOTES : 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 1. Set up working space and import packages
#------------------------------------------------------------------------------#

rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages

package.list <- c("tidyverse", "ggplot2", "knitr", "rmarkdown", "scales", 
                  "stargazer")

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}

# Set up working directory 

## check your directory
getwd()

## set up working directory 

my_directory   <- "/home/zacharyjohnigan/UChicago_Student/2_Data_For_Policy_Analys_-_Mgmt/"
my_data        <- paste0(my_directory, "Data/")
output_dir     <- paste0(my_directory, "Labs/Lab_7/")

#------------------------------------------------------------------------------#
# 2. Load dataset
#------------------------------------------------------------------------------#

# Import NSECE 2019 workforce questionnnaire

load(paste0(my_data, "37941-0005-Data.rda"))

## To import .CSV files use this code
## data <- read.csv("[full directory here]")

#------------------------------------------------------------------------------#
# 3. Basic exploratory data inspection
#------------------------------------------------------------------------------#

# Understand variable class WF9_PROFDEV_WRKSHP,
# WF9_PROFDEV_COACH, WF9_PROFDEV_MEETING, WF9_PROFDEV_COURSE, WF9_WORK_WAGE, 
# WF9_WORK_FT, WF9_CHAR_GENDER, WF9_CAREER_EXPERIENCE, WF9_CHAR_EDUC, 
# WF9_CHAR_RACE

class(da37941.0005$WF9_WORK_FT) #factor
summary(da37941.0005$WF9_WORK_FT) #nominal + discrete

class(da37941.0005$WF9_CHAR_GENDER) #factor
summary(da37941.0005$WF9_CHAR_GENDER) #nominal + discrete

class(da37941.0005$WF9_CAREER_EXPERIENCE) #factor
summary(da37941.0005$WF9_CAREER_EXPERIENCE) #ordinal + discrete (since the bigger group is less than 5 years of experience, I will keep that as my reference group). 
  
class(da37941.0005$WF9_CHAR_RACE) #factor
summary(da37941.0005$WF9_CHAR_RACE) #nominal (I will choose white as my reference group)

class(da37941.0005$WF9_WORK_WAGE) #numeric
summary(da37941.0005$WF9_PROFDEV_WRKSHP) #ratio + continuous 

#------------------------------------------------------------------------------#
# 4. Basic cleaning and manipulation
#------------------------------------------------------------------------------#

# Rename dataset

nsece_2019_wf <- da37941.0005

# Subset dataset and rename variables

nsece_2019_wf_subset <- 
  nsece_2019_wf %>%
  select(WF9_WORK_FT, 
         WF9_CHAR_GENDER, 
         WF9_CAREER_EXPERIENCE,  
         WF9_CHAR_RACE, 
         WF9_WORK_WAGE) %>%
  rename(full_time = WF9_WORK_FT, 
         gender = WF9_CHAR_GENDER, 
         yrs_experience = WF9_CAREER_EXPERIENCE, 
         race = WF9_CHAR_RACE,
         hr_wage = WF9_WORK_WAGE) 

# Since I have to create dummy variables to add them as predictors in the model 
# I won't recode any variables 

# Create new dummy variables (Problem Set 2 help)

nsece_2019_wf_dummy <- 
  nsece_2019_wf_subset %>%
  mutate(full_time_d = ifelse(full_time == "(1) Full-time worker", 1, 0), # use mutate() to create new dummy variables
         
         female_d = ifelse(gender == "(2) Female", 1, 0),
         
         exp_5to10_d = ifelse(yrs_experience == "(2) >5 to <=10 years", 1, 0),
         exp_11to15_d = ifelse(yrs_experience == "(3) >10 to <=15 years", 1, 0),
         exp_16to20_d = ifelse(yrs_experience == "(4) >15 to <=20 years", 1, 0),
         exp_21to25_d = ifelse(yrs_experience == "(5) >20 to <=25 years", 1, 0),
         exp_26plus_d = ifelse(yrs_experience == "(6) >25 years", 1, 0),
         
         black_d = ifelse(race == "(2) Black or African American Only", 1, 0),
         asian_d = ifelse(race == "(3) Asian Only", 1, 0),
         other_d = ifelse(race == "(8) Other", 1, 0)) 

## Let's examine the new variables created [do the same for the rest of the variables]
class(nsece_2019_wf_dummy$female_d) #numeric
table(nsece_2019_wf_dummy$female_d) #the variable was created as expected 
summary(nsece_2019_wf_dummy$female_d) 

nsece_2019_wf_log_wage <- 
  nsece_2019_wf_dummy %>%
  filter(hr_wage > 0) %>%
  mutate(log_hr_wage = log(hr_wage))

## Let's examine the new variables created [do the same for the rest of the variables]
class(nsece_2019_wf_log_wage$log_hr_wage) #numeric
summary(nsece_2019_wf_log_wage$log_hr_wage) # No zeros and numbers make much mor sense 

#------------------------------------------------------------------------------#
# Run .Rmd file and generate report
#------------------------------------------------------------------------------#

render(input = paste0(output_dir, 
                      "Lab/Run_Lab7.Rmd"),
       output_file = "Lab7_Report.html")
