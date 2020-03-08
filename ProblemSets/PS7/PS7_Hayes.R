library(tidyverse)
library(stargazer)
library(mice)

wages <- read.csv("~/R/DScourseS20/ModelingOptimization/wages.csv")

# Drop observation if hgc or tenure is missing

wages <- wages %>%
  filter(is.na(hgc)== FALSE) %>%
  filter(is.na(tenure) == FALSE)

stargazer(wages)

# Mutate wages to create tenure_sq variable
wages <- wages %>%
  mutate(tenure_sq = tenure^2)

# Create a data frame of the observation with missing logwage
wages_missing <- wages %>%
  filter(is.na(wages$logwage))

# Find the rate of missing logwages
length(wages_missing$logwage)/length(wages$logwage)


# Create a data frame for all observations where logwage is available
wages_no_missing_logwage <- wages %>%
  filter(is.na(wages$logwage) == FALSE)
# First model using only complete data
model_one <- lm(data = wages_no_missing_logwage, logwage ~ hgc + college + tenure + tenure_sq + age + married)
summary(model_one)

# Find the mean of logwage using only complete data
logwage_mean <- sum(wages_no_missing_logwage$logwage)/length(wages_no_missing_logwage$logwage)

# For all NA values of logwage, make logwage = mean(logwage)
wages_mean_imputed <- wages %>%
  mutate(logwage = replace_na(logwage, logwage_mean))
wages_mean_imputed

model_two <- lm(data = wages_mean_imputed, logwage ~ hgc + college + tenure + tenure_sq + age + married)
# This computes the logwage of missing values...  
for (i in seq_along(wages_missing$logwage)) {
  wages_missing$logwage <- predict(model_one, wages_missing)
}

# Use model one to fill in NA values of logwage in the original data frame
for (i in seq_along(wages$logwage)) {
  wages_lm_imputed <- wages %>%
    mutate(logwage = replace_na(logwage, predict(model_one, wages)[i]))
}
wages_lm_imputed
summary(wages_lm_imputed)

model_three <- lm(data = wages_lm_imputed, logwage ~ hgc + college + tenure + tenure_sq + age + married)
summary(model_three)
# Using mice...

wages_imputed <- mice(wages, seed = 123)
summary(wages_imputed)

fitted_logwage <- with(wages_imputed, lm(logwage ~ hgc + college + tenure + tenure_sq + age + married))
summary(pool(fitted_logwage))
