#===============================================================================
#  File:    02-figure1_replication.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? ..."
#  Purpose: Replicate Figure 1 using our replicated main-time-series dataset.
#           Figure shows average attention to each topic over the 113th Congress
#           for each group (MCs, public, media).
#  Data In: 
#           - ./data_replication/main-time-series.csv              (replicated)
#           - dataverse_files/data/pa2our_topics_crosswalk_merged_subissues.csv
#             (topic → label crosswalk from authors)
#  Data Out: 
#           - ./images_replication/figure1.png
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# OPTIONAL (for your local machine only; not needed in “clean” replication)
# setwd("/Users/zhaoshuti/Desktop/TAT/replication")

# DATA
#===============================================================================
# Time series with attention distribution for all groups across time
db <- read.csv("data_replication/main-time-series.csv")

# Crosswalk table containing human-readable labels for our merged issues 
pa2our <- read.csv("dataverse_files/data/pa2our_topics_crosswalk_merged_subissues.csv")

# DATA WRANGLING
#===============================================================================
# - filter out non-political issues
pol_issues <- c(
  3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
  32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51, 
  53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
  89, 93, 96, 97, 99, 100,
  # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42
  # new merged issues:
  101, 102, 103, 104
)

db <- db %>%
  filter(topic %in% pol_issues) %>%
  mutate(random = random_us) %>%   # rename US random sample to "random"
  dplyr::select(-random_us)

# - add human-readable topic labels
db$topic <- as.character(db$topic)

pa2our <- pa2our %>%
  rename(topic = our_, label = our_topic_label) %>%
  dplyr::select(topic, label)

pa2our$topic <- as.character(pa2our$topic)

db <- left_join(db, pa2our, by = "topic")

# MAIN
#===============================================================================
# - compute mean attention by topic and group
db_long <- db %>%
  dplyr::select(label, rep, dem, pubdem, pubrep, public, random, media) %>%
  gather(group, att, -label)

out_db <- db_long %>%
  group_by(group, label) %>%
  summarize(av = mean(att, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

# - readable group labels
out_db$group <- recode(
  factor(out_db$group),
  `dem`    = "Democrats\nin Congress",
  `rep`    = "Republicans\nin Congress",
  `pubdem` = "Democratic\nSupporters",
  `pubrep` = "Republican\nSupporters",
  `public` = "Attentive\nPublic",
  `random` = "General\nPublic",
  `media`  = "Media"
)

# - order groups: first political, then public/media
out_db$group <- factor(
  out_db$group,
  levels = c(
    "Democrats\nin Congress",
    "Republicans\nin Congress",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic",
    "Media"
  )
)

# - order topics within each facet
out_db <- out_db %>%
  arrange(group, av)

out_db$label <- factor(out_db$label, levels = unique(out_db$label))

# OUTPUT -- FIGURE
#===============================================================================
dir.create("images_replication", showWarnings = FALSE)

p <- ggplot(out_db, aes(x = label, y = av)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~ group, nrow = 1) +
  scale_y_continuous(
    breaks  = seq(0, 1, 0.01),
    labels  = paste0(seq(0, 100, 1), "%")
  ) +
  ylab("\nAverage daily attention (LDA topic posterior probability) to each political issue (113th Congress)") +
  xlab("") +
  coord_flip() +
  theme(
    panel.background   = element_blank(),
    strip.text         = element_text(size = 20),
    axis.text.y        = element_text(size = 18),
    axis.text.x        = element_text(size = 14),
    axis.title.x       = element_text(size = 18),
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.major.y = element_line(color = "gray70", linetype = "dotted")
  )

# Save to file (no plot in Plots pane; goes straight to PNG)
png("images_replication/figure1.png", width = 1800, height = 1400)
print(p)
dev.off()
