# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)

df <- read.csv("C:/Users/maxch/Desktop/Jobs/EOG/downtime_dataset.csv")

#####################################################################################
# DATA PREPROCESSING
# Convert datetime to DateTime type and extract features
df$datetime <- as.POSIXct(df$datetime)
df$hour <- as.numeric(format(df$datetime, "%H"))
df$day_of_week <- as.factor(format(df$datetime, "%A"))

# Convert necessary columns to factors
df$impact.level <- as.factor(df$impact.level)
df$platform <- as.factor(df$platform)
df$company.accountability <- as.factor(df$company.accountability)

# Ensure levels are set correctly
levels(df$company.accountability) <- c("No", "Yes")

# Check for missing values
print(sapply(df, function(x) sum(is.na(x))))

# Handle missing values by imputing or removing them
# For simplicity, here we use median/mode imputation for numeric/factor variables
impute_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
impute_mode <- function(x) replace(x, is.na(x), as.numeric(names(sort(table(x), decreasing = TRUE))[1]))

df <- df %>%
  mutate(across(where(is.numeric), impute_median)) %>%
  mutate(across(where(is.factor), impute_mode))

# Verify that there are no missing values after imputation
print(sapply(df, function(x) sum(is.na(x))))

# Feature Engineering: Interaction terms and transformations
df <- df %>%
  mutate(impact.users = as.numeric(impact.level) * users.impacted,
         log.downtime = log1p(sum.of.downtime.hours))

# Select only numeric columns for correlation matrix
numeric_cols <- df %>% select(where(is.numeric))

# Calculate the correlation matrix
M <- cor(numeric_cols)

# Print the correlation matrix
print(M)

# Visualize the correlation matrix
library(corrplot)
corrplot(M, method = "circle")
