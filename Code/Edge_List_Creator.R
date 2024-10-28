library(dplyr)

Tweets_network_NT <- read.csv("russia_after_new.csv")
 Tweets_network_NT <- Tweets_network_NT %>%
   filter(Engagement.Type == "QUOTE"|Engagement.Type == "RETWEET")





# Generate the range of dates
start_date <- as.Date("2024-02-08")
end_date <- as.Date("2024-02-10")
dates <- seq(start_date, end_date, by = "day")

# Loop through each date
for (i in 1:length(dates)) {
  current_end_date <- dates[i]
  
  # Filter the data
  filtered_data <- Tweets_network_NT %>%
    filter(Date >= start_date & Date <= current_end_date) %>%
    filter(Author != "") %>%
    filter(Thread.Author != "")
  
  # Summarise and group the data
  edge_list <- summarise(group_by(filtered_data, Author, Thread.Author), count = n())
  edge_list <- edge_list[order(edge_list$count, decreasing = TRUE), ]
  edge_list <- edge_list[!duplicated(edge_list[c("Author", "Thread.Author")]),]
  
  # Add Type column
  Type <- rep("Directed", nrow(edge_list))
  edge_list$Type <- Type
  
  # Rename columns
  colnames(edge_list) <- c("Source", "Target", "Weight", "Type")
  edge_list <- edge_list[,c("Target", "Source", "Weight", "Type")]
  
  # Create file name based on date
  file_name <- paste0("After_Quote_Retweets_Edges/", format(current_end_date, "%b_%d"), "_edgelist_quote&retweet_after.csv")
  
  # Write CSV file
  write.csv(edge_list, file_name, row.names = FALSE)
}