#===============================================================================
#  File:    05-adding-random-US-and-merging-subissues.R
#  Purpose: Construct the final topic–attention time series used in the APSR paper
#
#  This script performs two major tasks required to replicate the analysis:
#
#  (A) Merge topic "sub-issues" detected by the LDA model into broader
#      macro-issues (e.g., two Student Debt topics → one combined topic).
#
#  (B) Add an additional time series measuring issue attention among
#      *random users geolocated inside the United States*, using their LDA
#      posterior topic distributions.
#
#  These steps produce the final dataset of daily topic attention for:
#      - Democrats in Congress
#      - Republicans in Congress
#      - Mass public (Dem, Rep, and overall)
#      - Media
#      - Random Twitter users
#      - Random *US-based* Twitter users (added here)
#
#  Input:
#      dataverse_files/data/main-time-series-PRE.csv
#          → Daily topic attention for all groups *except* US-random users
#
#      dataverse_files/topics/lda-output/lda-USrs-results.Rdata
#          → Posterior topic probabilities for US-based random users
#
#  Output:
#      data/main-time-series.csv  (main dataset used for all analysis & figures)
#
#  Original paper:
#      Barbera et al. (2019), American Political Science Review
#===============================================================================


#===============================================================================
# 1. Load packages
#===============================================================================
library(dplyr)
library(rio)


#===============================================================================
# 2. Load input data
#===============================================================================

# Main time series for all groups except US-random users
db <- import("dataverse_files/data/main-time-series-PRE.csv")

# LDA posterior topic distributions for US‐based random sample
print(load("dataverse_files/topics/lda-output/lda-USrs-results.Rdata"))


#===============================================================================
# 3. Merge topic "sub-issues" into broader issue categories
#===============================================================================

# Mapping of sub-topics → new merged topics (101–104)
subissues_to_merge <- list(
  c(27, 56),            # Student Debt → topic 101
  c(11, 74),            # Hobby Lobby case → topic 102
  c(38, 59),            # Budget debate → topic 103
  c(17, 26, 35, 42, 49) # Gov shutdown → topic 104
)

# Groups whose attention series we will modify
groups <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# Sort and convert topic number to "topicXX" string format
new_db <- db %>%
  arrange(topic, date) %>%
  mutate(topic = paste0("topic", topic))

# Loop over each group of subtopics and merge them
for (i in 1:length(subissues_to_merge)) {

  subissues <- subissues_to_merge[[i]]

  # Create 730 empty rows (one per day) for the new merged topic
  new_empty_rows <- as.data.frame(matrix(
    nrow = 730, ncol = ncol(new_db),
    data = rep(NA, 730 * ncol(new_db))
  ))

  colnames(new_empty_rows) <- colnames(new_db)
  new_empty_rows$topic <- paste0("topic", 100 + i)

  # Add blank rows to dataset
  new_db <- rbind(new_db, new_empty_rows)

  # Compute merged attention series for each group
  for (group in groups) {

    group_db <- new_db[, c("date", "topic", group)]
    colnames(group_db)[3] <- "gr"

    new_issue_group_series <- group_db %>%
      filter(topic %in% paste0("topic", subissues)) %>%
      spread(topic, gr) %>%
      dplyr::select(-date) %>%
      mutate(merged_issue = rowSums(.)) %>%
      dplyr::select(merged_issue)

    new_db[new_db$topic == paste0("topic", 100 + i), group] <- new_issue_group_series
  }
}

# Return topic labels to numeric
new_db$topic <- gsub("topic", "", new_db$topic)


#===============================================================================
# 4. Add issue–attention series for US-based random Twitter users
#===============================================================================

model_data <- new_db

# Extract posterior distributions (documents × topics)
top_dist <- results$topics

# Create combined topics for US-random users (same merged definitions)
issue101 <- top_dist[,27] + top_dist[,56]
issue102 <- top_dist[,11] + top_dist[,74]
issue103 <- top_dist[,38] + top_dist[,59]
issue104 <- top_dist[,17] + top_dist[,26] + top_dist[,35] + top_dist[,42] + top_dist[,49]

# Append new topics
top_dist <- cbind(top_dist, issue101, issue102, issue103, issue104)

# Reshape to match the structure of model_data
top_dist_reshaped <- NULL
for (i in 1:ncol(top_dist)) {
  top_dist_reshaped <- rbind(
    top_dist_reshaped,
    data.frame(topic = as.character(i), random_us = top_dist[,i])
  )
}

# Append US-random series to the main dataset
random_us <- top_dist_reshaped[,"random_us"]
new_model_data <- cbind(model_data, random_us)
new_model_data <- as.data.frame(new_model_data)

# Sanity check: random vs random_us should be almost identical
cor.test(new_model_data$random, new_model_data$random_us)


#===============================================================================
# 5. Filter out non-political topics using authors’ classification
#===============================================================================

pol_issues <- c(
  3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
  32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51,
  53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
  89, 93, 96, 97, 99, 100,
  101, 102, 103, 104          # newly merged issues
)

final_data <- new_model_data %>%
  filter(topic %in% pol_issues)


#===============================================================================
# 6. Output: final dataset used for all tables and figures
#===============================================================================

write.csv(final_data, "data_replication/main-time-series.csv", row.names = FALSE)
