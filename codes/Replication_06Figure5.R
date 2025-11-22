#===============================================================================
#  File:    06-figure5_replication.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? ..."
#  Purpose: Replicate Figure 5, showing the correlation between issue-level
#           responsiveness (IRFs) and issue salience (average attention).
#  Data In: 
#           - ./main-time-series.csv
#             (topic attention time series for all groups)
#           - ./var_irfs_topic_*.Rdata
#             (issue-level cumulative IRFs from VAR models)
#           - ./pa2our_topics_crosswalk_merged_subissues.csv
#             (topic → human-readable label crosswalk from authors)
#  Data Out: 
#           - ./figure5.png
#             (replication of Figure 5)
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ggplot2)
library(ggrepel)

# DATA
#===============================================================================
# Time series with attention distribution for all groups across time
db <- read.csv("main-time-series.csv")

# List of issues coded as political (keep only these topics)
pol_issues <- c(
  3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
  32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 50, 51, 
  53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
  89, 93, 96, 97, 99, 100,
  # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42, 49
  # new merged issues:
  101, 102, 103, 104
)

# Load crosswalk with human-readable labels for topic codes
pa2our <- read.csv("pa2our_topics_crosswalk_merged_subissues.csv")


# IRF DATA: LOAD ISSUE-LEVEL IRFs AND BUILD MASTER DATASET
#===============================================================================
# Variables of interest in VAR/IRF models
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# Initialize empty data frame to store IRF info for all issues
irf_data <- NULL

total   <- length(pol_issues)
counter <- 0

for (top in pol_issues) {
  # update counter and report progress
  counter <- counter + 1
  print(paste0("[", counter, "/", total, "]"))
  
  file_name <- paste0("var_irfs_topic_", top, "_logit-UPD.Rdata")
  load(file_name)  # object loaded: 'var_irfs_cum'
  
  # iterate through all combinations of covariate and response groups
  covs  <- names(var_irfs_cum$irf)
  resps <- covs
  
  for (covariate in covs) {
    for (response in resps) {
      # number of forecast horizons available
      cum_days_n <- nrow(var_irfs_cum$irf[[covariate]])
      
      # template rows for this issue × covariate × response
      new_rows <- data.frame(
        issue_num = top,
        cov       = rep(covariate, cum_days_n),
        out       = response,
        day       = 1:cum_days_n,
        pe        = NA,
        lwr       = NA,
        upr       = NA
      )
      
      # fill in point estimate and 95% CI bounds, then invert logit transform
      for (estimate in c("irf", "Lower", "Upper")) {
        cov_irf_est <- as.data.frame(
          var_irfs_cum[[estimate]][[covariate]]
        )[[response]]
        
        # invert logit and re-center (subtract 0.5) to express change in prob.
        if (estimate == "irf") {
          new_rows$pe  <- inv.logit(cov_irf_est) - 0.5 
        } else if (estimate == "Lower") {
          new_rows$lwr <- inv.logit(cov_irf_est) - 0.5
        } else {
          new_rows$upr <- inv.logit(cov_irf_est) - 0.5
        }
      }
      
      # append rows for this covariate-response pair to master IRF dataset
      irf_data <- rbind(irf_data, new_rows)
    }
  }
}

# Keep only IRFs among MCs, party supporters, and public groups
irf_plot <- irf_data %>%
  filter(
    cov %in% c("dem", "rep", "pubdem", "pubrep", "public", "random"),
    out %in% c("dem", "rep", "pubdem", "pubrep", "public", "random")
  )

# Remove within-type effects (public→public and political→political)
agenda_type <- data.frame(
  var  = c("dem", "rep", "pubdem", "pubrep", "public", "random"),
  type = c("pol", "pol", "pub", "pub", "pub", "pub")
)

cov_agenda_type <- agenda_type %>%
  rename(cov = var, cov_agenda_type = type)
out_agenda_type <- agenda_type %>%
  rename(out = var, out_agenda_type = type)

cov_agenda_type$cov <- as.character(cov_agenda_type$cov)
out_agenda_type$out <- as.character(out_agenda_type$out)
irf_plot$cov        <- as.character(irf_plot$cov)
irf_plot$out        <- as.character(irf_plot$out)

irf_plot <- left_join(irf_plot, cov_agenda_type)
irf_plot <- left_join(irf_plot, out_agenda_type)

irf_plot <- irf_plot %>%
  filter(cov_agenda_type != out_agenda_type)

# Merge human-readable topic labels
pa2our_tomerge <- pa2our %>%
  dplyr::select("our_", "our_topic_label") %>%
  rename(issue_num = our_, label = our_topic_label)

pa2our_tomerge$issue_num <- as.character(pa2our_tomerge$issue_num)
irf_plot$issue_num       <- as.character(irf_plot$issue_num)

irf_plot <- left_join(irf_plot, pa2our_tomerge)

# Readable labels for outcome and covariate groups (IRF side)
irf_plot$out <- recode(
  irf_plot$out,
  `dem`    = "Democratic MCs",
  `rep`    = "Republican MCs",
  `pubdem` = "Democratic\nSupporters",
  `pubrep` = "Republican\nSupporters",
  `public` = "Attentive\nPublic",
  `random` = "General\nPublic"
)

irf_plot$cov <- recode(
  irf_plot$cov,
  `dem`    = "Democrats\nin Congress",
  `rep`    = "Republicans\nin Congress",
  `pubdem` = "Democratic\nSupporters",
  `pubrep` = "Republican\nSupporters",
  `public` = "Attentive\nPublic",
  `random` = "General\nPublic"
)

# Set factor order for outcome and covariate groups
irf_plot$out <- factor(
  irf_plot$out,
  levels = c(
    "Democratic MCs",
    "Republican MCs",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic"
  )
)

irf_plot$cov <- factor(
  irf_plot$cov,
  levels = c(
    "Democrats\nin Congress",
    "Republicans\nin Congress",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic"
  )
)

# Keep 15-day cumulative IRFs; retain all (thresholding is done later via CIs)
irf_toplot <- irf_plot %>%
  filter(day == 15) %>%
  arrange(out, cov, pe) %>%
  mutate(label = factor(label, levels = unique(label)))


# AVERAGE TOPIC ATTENTION (SALIENCE)
#===============================================================================
# Compute average attention to each topic by group (for salience measure)
db <- db %>%
  filter(topic %in% pol_issues) %>%
  mutate(random = random_us) %>%   # rename US random sample to "random"
  dplyr::select(-random_us)

# Add human-readable topic labels
db$topic <- as.character(db$topic)

pa2our <- pa2our %>%
  rename(topic = our_, label = our_topic_label) %>%
  dplyr::select(topic, label)

pa2our$topic <- as.character(pa2our$topic)

db <- left_join(db, pa2our)

# Average attention by topic and group
db_long <- db %>%
  dplyr::select(label, rep, dem, pubdem, pubrep, public, random, media) %>%
  tidyr::gather(group, att, -label)

out_db <- db_long %>%
  group_by(group, label) %>%
  summarize(av = mean(att, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

# Readable labels for groups (salience side)
out_db$group <- recode(
  factor(out_db$group),
  `dem`    = "Democratic MCs",
  `rep`    = "Republican MCs",
  `pubdem` = "Democratic\nSupporters",
  `pubrep` = "Republican\nSupporters",
  `public` = "Attentive\nPublic",
  `random` = "General\nPublic",
  `media`  = "Media"
)

# Order groups: first political, then public/media
out_db$group <- factor(
  out_db$group,
  levels = c(
    "Democratic MCs",
    "Republican MCs",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic",
    "Media"
  )
)

# Sort labels by attention within group (used to fix factor order of topics)
out_db <- out_db %>%
  arrange(group, av)

out_db$label <- factor(out_db$label, levels = unique(out_db$label))

# Rename to match IRF data (so we can merge on covariate group + topic label)
out_db <- out_db %>%
  rename(cov = group)

# Ensure character for merging
out_db$cov    <- as.character(out_db$cov)
out_db$label  <- as.character(out_db$label)
irf_toplot$cov   <- as.character(irf_toplot$cov)
irf_toplot$label <- as.character(irf_toplot$label)


# MERGE IRFs WITH AVERAGE ATTENTION AND PREPARE PLOTTING DATA
#===============================================================================
main <- left_join(irf_toplot, out_db) %>%
  # select a subset of issue labels to annotate in the plot
  mutate(
    label_av = NA,
    label_av = ifelse(
      (cov == "Democratic\nSupporters" & out == "Democratic MCs" &
         label %in% c("Gun Violence")) |
        (cov == "Republican\nSupporters" & out == "Republican MCs" &
           label %in% c("IRS Scandal")),
      as.character(label), label_av
    ),
    # shorten IRS label for plotting
    label_av = ifelse(grepl("IRS", label_av), "IRS", as.character(label_av))
  ) %>%
  # rescale IRF estimates and attention to percentage points
  mutate(
    pe  = (pe  * 100) / 10,
    lwr = (lwr * 100) / 10,
    upr = (upr * 100) / 10,
    av  = av * 100
  )

# Reverse factor order of covariate groups for plotting
main$cov <- factor(
  main$cov,
  levels = rev(c(
    "Democratic MCs",
    "Republican MCs",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic",
    "Media"
  ))
)


# PLOT -- FIGURE 5
#===============================================================================
png("figure5.png", width = 1200, height = 600)

ggplot(
  main %>%
    # only keep IRFs where the outcome is MCs (Democratic or Republican)
    filter(out %in% c("Democratic MCs", "Republican MCs")),
  aes(
    x    = av,
    y    = pe,
    ymin = lwr,
    ymax = upr,
    col  = cov,
    fill = cov
  )
) +
  # point estimates with 95% CI
  geom_pointrange(alpha = 0.3) +
  # linear trend line with 95% CI band
  geom_smooth(method = "lm", se = TRUE, level = 0.95, alpha = 0.4) +
  geom_hline(yintercept = 0) +
  ylab("15-day Impulse Response Functions \n(in percentage points)\n") +
  xlab("\nAverage daily attention during the 113th Congress") +
  scale_color_manual(
    "",
    values = c("gray30", "orange2", "red2", "blue2")
  ) +
  scale_fill_manual(
    "",
    values = c("gray30", "orange2", "red2", "blue2")
  ) +
  scale_x_continuous(
    breaks = seq(0, 5, 1),
    labels = paste0(seq(0, 5, 1), "%")
  ) +
  facet_grid(out ~ cov) +
  # add text labels for selected issues (above the main cloud of points)
  geom_text_repel(
    aes(label = label_av),
    col  = "black",
    size = 5,
    ylim = c(2.3, 3)
  ) +
  theme(
    legend.position   = "none",
    panel.background  = element_blank(),
    panel.grid.major  = element_line(colour = "gray90"),
    strip.text        = element_text(size = 14),
    strip.background  = element_rect(color = "black"),
    axis.text         = element_text(size = 12),
    axis.title        = element_text(size = 12),
    legend.text       = element_text(size = 12),
    panel.spacing     = unit(2, "lines")
  )

dev.off()
