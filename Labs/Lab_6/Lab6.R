#------------------------------------------------------------------------------#
# This file prepares NSECE 2019 data for Lab 6 of the Data for Policy Analysis 
# class.
# PROJECT NAME : Data for Policy Analysis
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.3
# AUTHOR : Aida Pacheco-Applegate
# DATE CREATED : 07-27-2026
# NOTES : 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 1. Set up working space and import packages
#------------------------------------------------------------------------------#

rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages

package.list <- c("tidyverse", "ggplot2")

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}

# Set up working directory 

## check your directory
getwd()

## set up working directory 

my_directory   <- "/Users/aidapacheco-applegate/Desktop/PhD/Summer 2026/Data for Policy Analysis/"
my_data        <- paste0(my_directory, "Data/")
output_dir     <- paste0(my_directory, "Classes/Class 6/")

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
  
class(da37941.0005$WF9_CHAR_EDUC) #factor
summary(da37941.0005$WF9_CHAR_EDUC) #ordinal + discrete (I will choose some college credit but no degree as my reference group)

class(da37941.0005$WF9_CHAR_RACE) #factor
summary(da37941.0005$WF9_CHAR_RACE) #nominal (I will choose white as my reference group)

class(da37941.0005$WF9_PROFDEV_WRKSHP) #factor
summary(da37941.0005$WF9_PROFDEV_WRKSHP) #nominal + discrete

class(da37941.0005$WF9_PROFDEV_COACH) #factor
summary(da37941.0005$WF9_PROFDEV_WRKSHP) #nominal + discrete

class(da37941.0005$WF9_PROFDEV_MEETING) #factor
summary(da37941.0005$WF9_PROFDEV_WRKSHP) #nominal + discrete

class(da37941.0005$WF9_PROFDEV_COURSE) #factor
summary(da37941.0005$WF9_PROFDEV_WRKSHP) #nominal + discrete

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
  select(WF9_WORK_FT, WF9_CHAR_GENDER, WF9_CAREER_EXPERIENCE, WF9_CHAR_EDUC, 
         WF9_CHAR_RACE, WF9_PROFDEV_WRKSHP, WF9_PROFDEV_COACH, WF9_PROFDEV_MEETING, 
         WF9_PROFDEV_COURSE, WF9_WORK_WAGE) %>%
  rename(full_time = WF9_WORK_FT, 
         gender = WF9_CHAR_GENDER, 
         yrs_experience = WF9_CAREER_EXPERIENCE, 
         educ_level = WF9_CHAR_EDUC, 
         race = WF9_CHAR_RACE,
         part_workshop = WF9_PROFDEV_WRKSHP,
         part_coaching = WF9_PROFDEV_COACH, 
         part_meeting = WF9_PROFDEV_MEETING, 
         part_course = WF9_PROFDEV_COURSE, 
         hr_wage = WF9_WORK_WAGE) 

# Since I have to create dummy variables to add them as predictors in the model 
# I won't recode any variables except for the ones corresponding to professional 
# development

# Recode professional development variables

nsece_2019_wf_recode <- 
  nsece_2019_wf_subset %>%
  mutate(part_workshop_char = as.character(part_workshop), # use mutate() to create new character variables while keeping the original variables unchanged
         part_workshop_char = case_when(
           part_workshop == "(1) Yes" ~ "Yes", # recode the variable responses to simpler labels
           part_workshop == "(2) No" ~ "No"),
           
         part_coaching_char = as.character(part_coaching),
         part_coaching_char = case_when(
           part_coaching == "(1) Yes" ~ "Yes",
           part_coaching == "(2) No" ~ "No"), 
         
         part_meeting_char = as.character(part_meeting),
         part_meeting_char = case_when(
           part_meeting == "(1) Yes" ~ "Yes",
           part_meeting == "(2) No" ~ "No"), 
         
         part_course_char = as.character(part_course),
         part_course_char = case_when(
           part_course == "(1) Yes" ~ "Yes",
           part_course == "(2) No" ~ "No"))
         
# Create new dummy variables

nsece_2019_wf_dummy <- 
  nsece_2019_wf_recode %>%
  mutate(full_time_d = ifelse(full_time == "(1) Full-time worker", 1, 0), # use mutate() to create new dummy variables
         
         female_d = ifelse(gender == "(2) Female", 1, 0),
         
         exp_5to10_d = ifelse(yrs_experience == "(2) >5 to <=10 years", 1, 0),
         exp_11to15_d = ifelse(yrs_experience == "(3) >10 to <=15 years", 1, 0),
         exp_16to20_d = ifelse(yrs_experience == "(4) >15 to <=20 years", 1, 0),
         exp_21to25_d = ifelse(yrs_experience == "(5) >20 to <=25 years", 1, 0),
         exp_26plus_d = ifelse(yrs_experience == "(6) >25 years", 1, 0),
         
         less_hs_d = ifelse(educ_level == "(1) Less than High School", 1, 0),
         ged_d = ifelse(educ_level == "(3) GED or high school equivalency", 1, 0),
         hs_d = ifelse(educ_level == "(4) High school graduate", 1, 0),
         aa_d = ifelse(educ_level == "(6) Associate degree (AA, AS)", 1, 0),
         ba_d = ifelse(educ_level == "(7) Bachelor's degree (BA, BS, AB)", 1, 0), 
         grad_d = ifelse(educ_level == "(8) Graduate or professional degree", 1, 0), 
         
         black_d = ifelse(race == "(2) Black or African American Only", 1, 0),
         asian_d = ifelse(race == "(3) Asian Only", 1, 0),
         other_d = ifelse(race == "(8) Other", 1, 0), 
         
         pd = ifelse(part_workshop_char == "Yes" | part_coaching_char == "Yes" | part_meeting_char == "Yes" | part_course_char == "Yes", 1, 0)) 
# If the respondent answered "Yes" to at least one of the four activities (workshop, coaching, meeting, or course), code pd as 1.
# Otherwise, code pd as 0.

## Let's examine the new variables created [do the same for the rest of the variables]
class(nsece_2019_wf_dummy$pd) #numeric
table(nsece_2019_wf_dummy$pd) #the variable was created as expected 
summary(nsece_2019_wf_dummy$pd) 

#------------------------------------------------------------------------------#
# 5. Run linear regression model
#------------------------------------------------------------------------------#

# Model 2: Hourly wage is our outcome variable, pd is the predictor of interest, and 
# the rest of the variables are controls

lm_wage_pd_model2 <- lm(hr_wage ~ pd + full_time_d + female_d + exp_5to10_d + 
                          exp_11to15_d + exp_16to20_d + exp_21to25_d + exp_26plus_d + 
                          less_hs_d + ged_d + hs_d + aa_d+ ba_d + grad_d + black_d + 
                          asian_d + other_d,
                     data = nsece_2019_wf_dummy)

summary(lm_wage_pd_model2)

## 1. Create a residual plot

### 1.1 Create a dataset containing residuals and predicted values

residual_data <- 
  data.frame(
  fitted = lm_wage_pd_model2$fitted.values,
  residuals = lm_wage_pd_model2$residuals)

### 1.2 Create the residual plot

residual_plot <- 
  ggplot(residual_data, aes(x = fitted, y = residuals)) +
  geom_point() + # add points showing the residual value for each observation
  geom_hline(yintercept = 0, linetype = "dashed") + # add a horizontal dashed line at zero to show where predictions perfectly match observations
  labs(
    title = "Residual Plot", # add title to the graph
    x = "Fitted Values", # add title to the x-axis 
    y = "Residuals") # add title to the y-axis 

residual_plot

# The residual plot shows that the residuals are generally centered around zero 
# with no strong systematic pattern, suggesting that the linear model is appropriate. 
# However, the greater spread of residuals at higher fitted values and the presence 
# of several large positive residuals indicate possible heteroscedasticity and 
# outliers, meaning the model predicts high wages less accurately than lower wages.

## 2. Create histogram of the residuals

hist_residual <- 
  ggplot(residual_data, aes(x = residuals)) +
  geom_histogram() +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Count")

hist_residual

# The histogram shows that most residuals are centered around zero, indicating 
# that the model predicts hourly wages reasonably well for most workers. However, 
# the distribution is right-skewed because of several large positive residuals, 
# suggesting the presence of outliers and a slight departure from the normality 
# assumption.

## 3. Interpretation of coefficients

# The intercept of $14.24 represents the predicted hourly wage for the reference 
# group: part-time, White male workers with less than 5 years of experience, some 
# college education, and no professional development participation.

# After controlling for employment status, gender, experience, education, and race, 
# professional development participation was not significantly associated with 
# hourly wages, suggesting that workers who participated in professional development 
# did not earn significantly more than those who did not. We do not have enough evidence
# to reject the null hypothesis of non-significant relationship between these two 
# variables. 

# Employment status and gender were both statistically significant. Compared with 
# part-time workers, full-time workers earned, on average, $1.11 less per hour, 
# after accounting for the other variables in the model. Similarly, female workers 
# earned, on average, $3.25 less per hour than male workers, after accounting 
# for the other variables in the model.

# Experience was positively associated with hourly wages. Compared with workers 
# who had less than 5 years of experience, those with 5–10 years of experience 
# earned, on averae, $1.35 more per hour, while workers with 11–15 years, 16–20 years, 
# 21–25 years, and 26 or more years of experience earned approximately $2.58, $3.23, 
# $5.17, and $4.45 more per hour, on average respectively. These findings 
# suggest that wages generally increased with years of experience, with the 
# largest wage premium observed among workers with 21–25 years of experience.

# Educational attainment was also a significant predictor of hourly wages. 
# Relative to workers with some college education, there were no statistically 
# significant differences in wages for workers with less than a high school diploma, 
# a GED, or a high school diploma. However, workers with an associate's degree 
# earned, on average, $2.17 more per hour, those with a bachelor's degree earned, 
# on average, $5.61 more per hour, and those with a graduate degree earned, on 
# average, $10.69 more per hour, indicating a strong positive relationship 
# between higher levels of education and hourly wages.

# Finally, some categories of race were associated with hourly wages. Compared 
# with White workers, Black workers earned, on average, $1.06 less per hour, 
# while there was no statistically significant difference between White and Asian 
# workers. Workers identifying as another race earned approximately $1.48 more 
# per hour than White workers. 

## 4. Overall model fit 

# The multiple linear regression model was statistically significant, 
# F(17, 3798) = 47.81, p < .001, indicating that the predictors jointly explain 
# variation in hourly wages. The model explained approximately 17.6% (r-squared) of 
# the variation in hourly wages, suggesting that while the included variables 
# are important predictors of wages, a substantial amount of variation remains 
# unexplained by the model.

## 5. Conclusion

# Overall, the results indicate that experience and educational attainment were 
# the strongest positive predictors of hourly wages, whereas professional development 
# participation was not significantly associated with wages after adjusting for 
# the other characteristics included in the model.

#------------------------------------------------------------------------------#
# 6. BONUS: REMOVING OUTLIERS & LOG TRANSFORMATION
#------------------------------------------------------------------------------#

# Remove outliers from hourly wage 

hist_hrwage <- 
  ggplot(nsece_2019_wf_dummy, aes(x = hr_wage)) +
  geom_histogram() +
  labs(
    title = "Histogram of Hourly Wage",
    x = "Hourly Wage",
    y = "Count")

hist_hrwage # After looking at the distribution, I will truncate the wage data for values above $40 and remove $0

# Create new data with wage values below $40

nsece_2019_wf_no_outliers <- 
  nsece_2019_wf_dummy %>%
  filter(hr_wage <= 40 & hr_wage > 0)

summary(nsece_2019_wf_no_outliers$hr_wage)

# Re-run model with new data

lm_wage_pd_model3 <- lm(hr_wage ~ pd + full_time_d + female_d + exp_5to10_d + 
                          exp_11to15_d + exp_16to20_d + exp_21to25_d + exp_26plus_d + 
                          less_hs_d + ged_d + hs_d + aa_d+ ba_d + grad_d + black_d + 
                          asian_d + other_d,
                        data = nsece_2019_wf_no_outliers)

summary(lm_wage_pd_model3)

# Re-run residual plot

residual_data <- 
  data.frame(
    fitted = lm_wage_pd_model3$fitted.values,
    residuals = lm_wage_pd_model3$residuals)

### 1.2 Create the residual plot

residual_plot2 <- 
  ggplot(residual_data, aes(x = fitted, y = residuals)) +
  geom_point() + # add points showing the residual value for each observation
  geom_hline(yintercept = 0, linetype = "dashed") + # add a horizontal dashed line at zero to show where predictions perfectly match observations
  labs(
    title = "Residual Plot", # add title to the graph
    x = "Fitted Values", # add title to the x-axis 
    y = "Residuals") # add title to the y-axis 

residual_plot2 # this doesn't seem to solve the issue. We do not have homoscedasticity. 

# Re-run histogram

hist_residual <- 
  ggplot(residual_data, aes(x = residuals)) +
  geom_histogram() +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Count")

hist_residual # this looks more like a normal distribution so normality assumption seems to hold

# Create log(wage) variable. 
# A log transformation reduces the impact of very high wage values and can 
# improve the fit of a linear regression model by making the distribution of 
# wages more symmetric. In a log-wage model, coefficients are interpreted as 
# approximate percentage changes in hourly wages rather than changes in dollars.

## When we create the variable we have to make sure we have non-zero values 

nsece_2019_wf_log_wage <- 
  nsece_2019_wf_dummy %>%
  filter(hr_wage > 0) %>%
  mutate(log_hr_wage = log(hr_wage))

summary(nsece_2019_wf_log_wage$log_hr_wage)

hist_hrwage <- 
  ggplot(nsece_2019_wf_log_wage, aes(x = log_hr_wage)) +
  geom_histogram() +
  labs(
    title = "Histogram of Hourly Wage",
    x = "Hourly Wage",
    y = "Count")

hist_hrwage #this variable looks better than the regular hourly_wage variable

# Re-run model with new data

lm_wage_pd_model4 <- lm(log_hr_wage ~ pd + full_time_d + female_d + exp_5to10_d + 
                          exp_11to15_d + exp_16to20_d + exp_21to25_d + exp_26plus_d + 
                          less_hs_d + ged_d + hs_d + aa_d+ ba_d + grad_d + black_d + 
                          asian_d + other_d,
                        data = nsece_2019_wf_log_wage)

summary(lm_wage_pd_model4) # this looks like a better model (look R-squared)

# Re-run residuals plot 

residual_data <- 
  data.frame(
    fitted = lm_wage_pd_model4$fitted.values,
    residuals = lm_wage_pd_model4$residuals)

residual_plot3 <- 
  ggplot(residual_data, aes(x = fitted, y = residuals)) +
  geom_point() + # add points showing the residual value for each observation
  geom_hline(yintercept = 0, linetype = "dashed") + # add a horizontal dashed line at zero to show where predictions perfectly match observations
  labs(
    title = "Residual Plot", # add title to the graph
    x = "Fitted Values", # add title to the x-axis 
    y = "Residuals") # add title to the y-axis 

residual_plot3 # the residuals now look randomly distributed around 0.

# Re-run histogram

hist_residual <- 
  ggplot(residual_data, aes(x = residuals)) +
  geom_histogram() +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Count")

hist_residual # this looks like a normal distribution.

# Interpretation of female coefficient as an example: 
# Female workers earned approximately 13.3% lower hourly wages than 
# comparable male workers, holding all other variables constant.