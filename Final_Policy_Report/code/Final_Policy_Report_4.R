# Final Policy Report
# Data for Policy Analysis and Management
# Zachary Johnigan
# Summer 2026

# ============================================================
# 1. Set Up Working Environment
# ============================================================

rm(list = ls())
options("error")
options(lifecycle_disable_verbose_retirement = TRUE)

package.list <- c(
  "tidyverse",
  "ggplot2",
  "knitr",
  "rmarkdown",
  "scales",
  "stargazer"
)

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only = TRUE)
}

my_directory <- "/home/zacharyjohnigan/UChicago_Student/2_Data_For_Policy_Analys_-_Mgmt/"
my_data <- paste0(my_directory, "Final_Policy_Report/data/")
code_dir <- paste0(my_directory, "Final_Policy_Report/code/")
output_dir <- paste0(my_directory, "Final_Policy_Report/report/")


# ============================================================
# 2. Load Data
# ============================================================

mass_shootings <- read_csv(paste0(my_data, "mass_shootings.csv"))
names(mass_shootings)
unique(mass_shootings$location...2)
unique(mass_shootings$location...8)
unique(mass_shootings$type)
# ============================================================
# 3. Prepare Data for Analysis
# ============================================================

# ============================================================
# 3. Prepare Data for Analysis
# ============================================================

mass_shootings_project <-
  mass_shootings %>%
  select(
    fatalities,
    location_type = location...8
  )


# ============================================================
# 4. Examine Variables
# ============================================================

glimpse(mass_shootings_project)
summary(mass_shootings_project)
table(mass_shootings_project$location_type)
summary(mass_shootings_project$fatalities)

# ============================================================
# 5. Data Cleaning and Derived Variables
# ============================================================

mass_shootings_project <-
  mass_shootings_project %>%
  mutate(
    location_type = str_trim(location_type),
    location_type = str_to_title(location_type)
  ) %>%
  drop_na(
    fatalities,
    location_type
  )


# ============================================================
# 6. Summarize Individual Variables
# ============================================================

location_table <-
  mass_shootings_project %>%
  count(location_type) %>%
  mutate(percent = (n / sum(n)) * 100)

location_table

fatalities_summary <-
  mass_shootings_project %>%
  summarise(
    n = n(),
    mean = mean(fatalities, na.rm = TRUE),
    median = median(fatalities, na.rm = TRUE),
    min = min(fatalities, na.rm = TRUE),
    max = max(fatalities, na.rm = TRUE)
  )

fatalities_summary


# ============================================================
# 7. Explore Associations
# ============================================================

fatalities_by_location <-
  mass_shootings_project %>%
  group_by(location_type) %>%
  summarise(
    n = n(),
    mean_fatalities = mean(fatalities, na.rm = TRUE),
    median_fatalities = median(fatalities, na.rm = TRUE)
  )

fatalities_by_location

ggplot(
  mass_shootings_project,
  aes(x = location_type)
) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Mass Shootings by Location Type",
    x = "Location Type",
    y = "Number of Incidents"
  )

ggplot(
  mass_shootings_project,
  aes(x = location_type, y = fatalities)
) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Fatalities by Location Type",
    x = "Location Type",
    y = "Fatalities"
  )


# ============================================================
# 8. Linear Regression
# ============================================================

model <- lm(
  fatalities ~ location_type,
  data = mass_shootings_project
)

summary(model)


# ============================================================
# 9. Regression Diagnostics
# ============================================================

plot(
  fitted(model),
  residuals(model),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residual Plot"
)

abline(h = 0)

hist(
  residuals(model),
  main = "Distribution of Model Residuals",
  xlab = "Residuals"
)


# ============================================================
# 10. Render HTML Report
# ============================================================

rmarkdown::render(
  paste0(output_dir, "Final_Policy_Report.Rmd")
)
