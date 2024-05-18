# R Script produced by Max Chalitsios

#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("caret")
#install.packages("lubridate")
#install.packages("gridExtra")
#install.packages("ggcorrplot")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(caret)
library(gridExtra)
library(ggcorrplot)

dataset <- read.csv("C:/Users/maxch/Desktop/Jobs/EOG/downtime_report_modified.csv")
View(dataset)

##################################################################
# DATA PREPROCESSING
dataset <- dataset %>%
  mutate(
    `impact.level` = as.factor(`impact.level`),
    platform = as.factor(platform),
    `company.accountability` = as.factor(`company.accountability`)
  )

# Convert binary columns for apps to factors
dataset$`App.A` <- as.factor(dataset$`App.A`)
dataset$`App.B` <- as.factor(dataset$`App.B`)
dataset$`App.C` <- as.factor(dataset$`App.C`)
dataset$`App.D` <- as.factor(dataset$`App.D`)
dataset$`App.E` <- as.factor(dataset$`App.E`)

##################################################################
# FEATURE ENGINEERING:

# Convert 'date.time' to datetime format
dataset$date.time <- mdy_hm(dataset$date.time)

# Extract day of the week
dataset$day.of.week <- wday(dataset$date.time, label = TRUE)

# Extract hour of the day
dataset$hour.of.day <- hour(dataset$date.time)

# Create a binary feature for weekend
dataset$is.weekend <- ifelse(dataset$day.of.week %in% c("Sat", "Sun"), 1, 0)

dataset$season <- case_when(
  month(dataset$date.time) %in% c(12, 1, 2) ~ "Winter",
  month(dataset$date.time) %in% c(3, 4, 5) ~ "Spring",
  month(dataset$date.time) %in% c(6, 7, 8) ~ "Summer",
  month(dataset$date.time) %in% c(9, 10, 11) ~ "Fall"
)

# Null value check
sapply(dataset, function(x) sum(is.na(x)))
str(dataset)
View(dataset)
summary(dataset)
head(dataset)

##################################################################
# EXPLORATORY DATA ANALYSIS (EDA)
# Visual frequency of each app being affected
app_data <- dataset %>% select(starts_with("App"))
app_melted <- app_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "App",
    values_to = "Affected"
  )

ggplot(app_melted, aes(x = App, fill = as.factor(Affected))) +
  geom_bar(position = "stack") +
  labs(x = "App", y = "Count", fill = "Affected Status") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), labels = c("Not Affected", "Affected")) +
  theme_minimal()

# Distribution of sum of downtime hours
#NOTE: This is a log normal distribution
ggplot(dataset, aes(x = sum.of.downtime.hours)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Sum of Downtime Hours",
       x = "Sum of Downtime Hours",
       y = "Frequency") +
  theme_minimal()
#Time Series Analysis
ggplot(dataset, aes(x = date.time, y = sum.of.downtime.hours)) +
  geom_line() +
  labs(title = "Time Series of Downtime Hours", x = "Date", y = "Sum of Downtime Hours") +
  theme_minimal()
# Impact Level Distribution
ggplot(dataset, aes(x = impact.level, fill = impact.level)) +
  geom_bar() +
  labs(title = "Distribution of Impact Levels", x = "Impact Level", y = "Count") +
  theme_minimal()
#Platform-wise Downtime Analysis
ggplot(dataset, aes(x = platform, y = sum.of.downtime.hours, fill = platform)) +
  geom_bar(stat = "identity") +
  labs(title = "Downtime Hours by Platform", x = "Platform", y = "Total Downtime Hours") +
  theme_minimal()
#User Impact and Downtime Hours Relationship
ggplot(dataset, aes(x = user.impact, y = sum.of.downtime.hours)) +
  geom_point(aes(color = impact.level)) +
  geom_smooth(method = "lm") +
  labs(title = "Relationship Between User Impact and Downtime Hours", x = "User Impact", y = "Downtime Hours") +
  theme_minimal()
#Day of Week Analysis
ggplot(dataset, aes(x = day.of.week, y = sum.of.downtime.hours, fill = day.of.week)) +
  geom_bar(stat = "identity") +
  labs(title = "Downtime Hours by Day of the Week", x = "Day of the Week", y = "Sum of Downtime Hours") +
  theme_minimal()
#Seasonality of Downtime
ggplot(dataset, aes(x = season, y = sum.of.downtime.hours, fill = season)) +
  geom_bar(stat = "identity") +
  labs(title = "Downtime Hours by Season", x = "Season", y = "Total Downtime Hours") +
  theme_minimal()
#Binary Variables (App Affected)
app_data <- dataset %>%
  select(starts_with("App")) %>%
  mutate(across(everything(), as.integer)) %>%
  pivot_longer(cols = everything(), names_to = "App", values_to = "Affected") %>%
  group_by(App) %>%
  summarise(Affected = sum(Affected))
ggplot(app_data, aes(x = App, y = Affected, fill = App)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Apps Being Affected", x = "App", y = "Frequency") +
  theme_minimal()


##################################################################
#DEEPER INSIGHTS
# 1. Correlation Matrix(TODO: modify data to create high correlation between sum of downtime hours & user impact)
numeric_vars <- dataset %>%
  select(sum.of.downtime.hours, user.impact)

correlation_matrix <- cor(numeric_vars)
ggcorrplot(correlation_matrix, method = "circle", type = "lower",
           lab = TRUE, title = "Correlation Matrix")


# 2. Text Analysis on Summaries
# 3. Anomaly Detection

colnames(dataset)
