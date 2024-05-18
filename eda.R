
library(tidyverse)
library(lubridate)
library(reshape2)
library(corrplot)
library(scales)



df <- read.csv("C:/Users/maxch/Desktop/Jobs/EOG/downtime_dataset.csv")
#View(df)

# Define mappings for categorical variables
impact_levels <- c("low", "medium", "high", "critical")
platforms <- c("Desktop", "Mobile", "API")
company_accountability <- c("no", "yes")

# Convert datetime column to datetime type and map numeric values to factors
df <- df %>%
  mutate(datetime = ymd_hms(datetime), # Use ymd_hms for "YYYY-MM-DD HH:MM:SS" format
         impact.level = factor(impact.level, levels = 0:3, labels = impact_levels),
         platform = factor(platform, levels = 0:2, labels = platforms),
         company.accountability = factor(company.accountability, levels = 0:1, labels = company_accountability))


# df <- df %>%
#   mutate(datetime = ymd_hms(datetime), # Use ymd_hms for "YYYY-MM-DD HH:MM:SS" format
#          impact.level = as.factor(impact.level),
#          platform = as.factor(platform),
#          company.accountability = as.factor(company.accountability))

# Check the structure of the dataset
str(df)
View(df)
head(df)

################################################################
# Bar Plot: Platform vs. Users Impacted
#DOESNT REALLY MATTER WHAT PLATFORM IT IS CONCLUSION
ggplot(df, aes(x = platform, y = users.impacted, fill = platform)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Users Impacted by Platform", x = "Platform", y = "Average Users Impacted") +
  theme_minimal() +
  scale_fill_viridis_d()

################################################################
# Impact Level vs. Downtime Hours
# Density Plot
ggplot(df, aes(x = sum.of.downtime.hours, fill = as.factor(new_impact_level))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot: Impact Level vs. Downtime Hours", x = "Sum of Downtime Hours", y = "Density", fill = "Impact Levels") +
  theme_minimal() +
  scale_fill_viridis_d(labels = impact_levels) +
  xlim(0, 12)  #zoom

# Violin Plot
ggplot(df, aes(x = as.factor(new_impact_level), y = sum.of.downtime.hours, fill = as.factor(new_impact_level))) +
  geom_violin() +
  labs(title = "Violin Plot: Impact Level vs. Downtime Hours", x = "Impact Level", y = "Sum of Downtime Hours", fill = "Impact Level") +
  theme_minimal() +
  scale_fill_viridis_d(labels = impact_levels) +
  scale_x_discrete(labels = impact_levels)
  #ylim(0, 10)  #zoom

################################################################
# Extract day of week and hour from datetime
df <- df %>%
  mutate(day_of_week = wday(datetime, label = TRUE),
         hour = hour(datetime))

# Heatmap: Downtime by Day of Week and Hour
#High impact on users at 9PM on Mondays and Sundays could indicate firewall team operations or system updates.
#To confirm this we can examine system and firewall logs regarding incident reports around the high impact times.

#High impact on users from Midnight to 8AM on Wednesdays could indicate maintenance being done at midnight such as an update and system overload from
#pushing code to production at 8AM
ggplot(df, aes(x = hour, y = day_of_week, fill = users.impacted)) +
  geom_tile() +
  labs(title = "Heatmap of Downtime Events by Day of Week and Hour", x = "Hour of Day", y = "Day of Week") +
  scale_fill_viridis_c() +
  theme_minimal()


################################################################
# Violin Plot: Impact Level vs. Users Impacted
ggplot(df, aes(x = impact.level, y = users.impacted, fill = impact.level)) +
  geom_violin() +
  labs(title = "Impact Level vs. Users Impacted", x = "Impact Level", y = "Users Impacted") +
  theme_minimal() +
  scale_fill_viridis_d()
################################################################
# # Histogram: Impact Level vs. Users Impacted
# ggplot(df, aes(x = impact.level, fill = users.impacted)) +
#   geom_histogram(stat = "count", position = "stack") +
#   scale_fill_viridis_c() +
#   labs(title = "Impact Level vs. Users Impacted", x = "Impact Level", y = "Users Impacted") +
#   theme_minimal()
################################################################
# Aggregate data by month for the line graph
df_monthly <- df %>%
  mutate(month = floor_date(datetime, "month")) %>%
  group_by(month) %>%
  summarise(users.impacted = sum(users.impacted)) %>%
  ungroup() %>%
  mutate(month = as.Date(month))  # Ensure month is a Date object

# Line Graph: Datetime (by month) vs. Users Impacted
ggplot(df_monthly, aes(x = month, y = users.impacted)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(title = "Users Impacted per Month", x = "Month", y = "Users Impacted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################
# Histogram with Kernel Density Plot
#Sum of Downtime Hours by Frequency
ggplot(df, aes(x = sum.of.downtime.hours)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "orange", size = 1) +
  labs(title = "Sum of Downtime Hours Frequency Plot",
       x = "Sum of Downtime Hours",
       y = "Frequency") +
  theme_minimal()
################################################################
# Heatmap: Sum of Downtime Hours by Day of Week and Hour
heatmap_data <- df %>%
  group_by(day_of_week, hour) %>%
  summarise(total_downtime_hours = sum(sum.of.downtime.hours)) %>%
  ungroup()

ggplot(heatmap_data, aes(x = hour, y = day_of_week, fill = total_downtime_hours)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Sum of Downtime Hours by Day of Week and Hour", x = "Hour of Day", y = "Day of Week") +
  theme_minimal()
################################################################

