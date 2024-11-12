library(tidyverse)
library(readxl)
library(lubridate)

data1 <- read_xlsx('Feb 6-7 New.xlsx')
data2 <- read_xlsx('Feb 7-8.xlsx') 
data3 <- read_xlsx('Feb 8-8 New.xlsx')
data4 <- read_xlsx('Feb 8-9 New.xlsx')
data5 <- read_xlsx('Feb 9-9 New.xlsx')
data6 <- read_xlsx('Feb 9_10 New.xlsx')

data_part1 <- data4[34319:39017,]
data_part2 <- data4[1:34318,]

before <- rbind(data1,data2, data3,data_part1)
after <- rbind(data_part2,data5,data6)

before <- before %>%
  filter(`Engagement.Type`== NA)
after <- after %>%
  filter(`Engagement.Type`== NA)


before$Date <- as.POSIXct(before$Date, tz = "", '%Y-%m-%d %H:%M:%OS')
after$Date <- as.POSIXct(after$Date, tz = "", '%Y-%m-%d %H:%M:%OS')


exclude_words <- c("Instagram", "Sina Weibo","Subreddit","Facebook","Tiktok","Reddit","Root","Linkedin","Page Type","Language","Title")
# Create a regex pattern to match columns containing the words
pattern <- paste0(exclude_words, collapse = "|")

before <- before %>%
  select(-matches(pattern))
after <- after %>%
  select(-matches(pattern))

complete <- rbind(before,after)

write.csv(before, 'russia_before_new_2.csv')
write.csv(after, 'russia_after_new_2.csv')
write_excel_csv(complete,'russia_complete_new_2.csv')


# Step 2: Create a new column for the hour
df <- after %>%
  mutate(hour = format(Date, "%Y-%m-%d %H:00:00"))

# Step 3: Aggregate data by hour
hourly_counts <- df %>%
  group_by(hour) %>%
  summarise(count = n())

# Aggregate data by hour
hourly_counts <- df %>%
  group_by(hour) %>%
  summarise(count = n())

# Convert the hour column back to POSIXct
hourly_counts$hour <- as.POSIXct(hourly_counts$hour)


# Plot with ggplot2 as a bar chart
ggplot(hourly_counts, aes(x = hour, y = count)) +
  ylim(0,5000) +
  geom_bar(stat = "identity", fill = 'red', color = 'black') +
  scale_x_datetime(date_labels = "%b %d %H%M", date_breaks = "2 hour") +
  labs(title = "Count of Tweets by 2 Hours 02/08 6pm - 02/10",
       x = "Date Hour",
       y = "Count of tweets") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


