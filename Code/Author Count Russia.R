# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

df <- read.csv("russia_after_48hrs - Correct.csv")

# Assuming df is your dataframe
# Convert Date column to proper datetime format
df <- df %>%
  mutate(Date = mdy_hm(Date))  # ymd_hms handles the 'yyyy-mm-dd HH:MM:SS' format
df <- df %>%
  filter(!is.na(Date))

# Round to nearest 2-hour intervals
df <- df %>%
  mutate(RoundedDate = floor_date(Date, unit = "1 hour"))

# Group by RoundedDate and count distinct authors
author_counts <- df %>%
  group_by(RoundedDate) %>%
  summarise(UniqueAuthors = n_distinct(Author))

# Plot the time series
ggplot(author_counts, aes(x = RoundedDate, y = UniqueAuthors)) +
  geom_line(color = "red", size = 1.2) +  # Adjust line color and weight
  labs(title = "Count of Distinct Authors 02/08 6pm - 02/10",
       x = "Date (Rounded to 2 Hours)",
       y = "Distinct Author Count") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%m/%d %H:%M") +  # Tick marks every 2 hours
  theme_classic() +
  scale_y_continuous(limits = c(300, 5000))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))