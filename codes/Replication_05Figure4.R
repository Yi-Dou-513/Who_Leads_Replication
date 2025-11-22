#===============================================================================
#  File:    05-figure4_replication.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? ..."
#  Purpose: Replicate Figure 4, showing issue-level IRFs between groups
#           (MCs and public groups) for all political issues.
#  Data In: 
#           - ./main-time-series.csv
#             (topic attention time series for all groups)
#           - ./pa2our_topics_crosswalk_merged_subissues.csv
#             (topic → human-readable label crosswalk from authors)
#           - ./var_irfs_topic_*.Rdata
#             (issue-level IRFs, if PART A already run)
#  Data Out: 
#           - ./var_irfs_topic_*.Rdata
#             (estimated VAR-based cumulative IRFs by issue)
#           - ./figure4.png
#             (replication of Figure 4)
#===============================================================================


# PACKAGES
#===============================================================================
library(dplyr)
library(vars)
library(tidyr)
library(ggplot2)
library(boot)


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


# INSTRUCTIONS
#===============================================================================
# This script has two main parts:
#
#   PART A: Estimate issue-level VAR models (7 lags) and save cumulative
#           impulse-response functions (IRFs) for each political issue.
#
#   PART B: Load the saved IRFs and construct Figure 4 by:
#             - transforming IRFs back from the logit scale,
#             - keeping only cross-group effects (no within-group),
#             - merging human-readable topic labels,
#             - plotting 15-day effects for selected group pairs.
#
# If you only want to reproduce Figure 4 and already have the *.Rdata files,
# you can skip PART A and go directly to PART B.


#===============================================================================
#=============== PART A: ESTIMATING ISSUE-LEVEL VARs AND IRFs ==================
#===============================================================================

# DATA WRANGLING
#===============================================================================
# - keep only political issues
db <- db %>%
  filter(topic %in% pol_issues)

# - create list of variables of interest; for the updated US random sample,
#   drop old 'random' variable and rename 'random_us' to 'random'
db <- db %>%
  dplyr::select(-random) %>%
  rename(random = random_us)

variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - keep only date, topic, and variables of interest (no lag variables yet)
db <- db[, c("date", "topic", variables)]

# - logit-transform all series (avoid zeros, then apply logit)
for (v in variables) {
  # pull series for group v
  x <- db[, v]
  # some groups have trailing NAs by issue; treat them as 0
  x[which(is.na(x))] <- 0
  # add 1 percentage point to avoid 0s before logit transform
  x <- x + 0.01
  # apply logit transformation
  logit_x <- log(x / (1 - x))
  # uncomment next line to verify no NAs remain
  # print(length(which(is.na(logit_x))))
  db[, v] <- logit_x
}

# - loop over issues and estimate VAR + cumulative IRFs issue by issue
for (top in unique(db$topic)) {
  # subset data for a single issue
  maindb <- db %>%
    filter(topic == top)
  
  # build model.matrix with all endogenous variables (no intercept)
  mformula   <- formula(paste0("~", paste0(variables, collapse = " + ")))
  model_data <- model.matrix(mformula, maindb[, variables])
  model_data <- model_data[, 2:ncol(model_data)]   # drop intercept column
  
  # only endogenous variables enter the VAR (no topic dummies)
  X_endogenous <- model_data
  
  # estimate VAR with 7 lags of each endogenous variable
  var_model <- VAR(y = X_endogenous, p = 7)
  
  # compute cumulative IRFs up to 60 days ahead
  var_irfs_cum <- irf(var_model, n.ahead = 60, cumulative = TRUE)
  
  # OUTPUT (PART A)
  #=============================================================================
  # save cumulative IRFs for this issue (used later in PART B)
  save(
    var_irfs_cum, 
    file = paste0("var_irfs_topic_", top, "_logit-UPD.Rdata")
  )
}


#===============================================================================
#======================== PART B: GENERATING FIGURE 4 ==========================
#===============================================================================

# DATA (PART B)
#===============================================================================
# - load dataset with human-readable topic labels
pa2our <- read.csv("pa2our_topics_crosswalk_merged_subissues.csv")

# - list of variables of interest (same as in PART A)
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - initialize empty data frame to store IRF info for all issues
irf_data <- NULL

# - loop over political issues and read in corresponding IRFs
total   <- length(pol_issues)
counter <- 0
for (top in pol_issues) {
  # update counter and print progress
  counter <- counter + 1
  print(paste0("[", counter, "/", total, "]"))
  
  file_name <- paste0("var_irfs_topic_", top, "_logit-UPD.Rdata")
  load(file_name)  # object loaded: 'var_irfs_cum'
  
  # iterate over all combinations of covariate and response groups
  covs  <- names(var_irfs_cum$irf)
  resps <- covs
  
  for (covariate in covs) {
    for (response in resps) {
      # number of forecast horizons available
      cum_days_n <- nrow(var_irfs_cum$irf[[covariate]])
      
      # template rows for this covariate-response pair and issue
      new_rows <- data.frame(
        issue_num = top,
        cov       = rep(covariate, cum_days_n),
        out       = response,
        day       = 1:cum_days_n,
        pe        = NA,
        lwr       = NA,
        upr       = NA
      )
      
      # fill in point estimate and 95% intervals, then invert logit transform
      for (estimate in c("irf", "Lower", "Upper")) {
        cov_irf_est <- as.data.frame(
          var_irfs_cum[[estimate]][[covariate]]
        )[[response]]
        
        # invert logit and re-center (subtract 0.5) on probability scale
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


# DATA WRANGLING FOR PLOT
#===============================================================================
# - keep only IRFs among MCs, party supporters, and public groups
irf_plot <- irf_data %>%
  filter(
    cov %in% c("dem", "rep", "pubdem", "pubrep", "public", "random"),
    out %in% c("dem", "rep", "pubdem", "pubrep", "public", "random")
  )

# - remove within-group effects and public→public / political→political effects
#   by classifying each group as "pol" or "pub" and dropping same-type pairs
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

# - merge human-readable topic labels
pa2our_tomerge <- pa2our %>%
  dplyr::select("our_", "our_topic_label") %>%
  rename(issue_num = our_, label = our_topic_label)

pa2our_tomerge$issue_num <- as.character(pa2our_tomerge$issue_num)
irf_plot$issue_num       <- as.character(irf_plot$issue_num)

irf_plot <- left_join(irf_plot, pa2our_tomerge)

# - give readable labels to covariate and outcome groups
irf_plot$out <- recode(
  irf_plot$out,
  `dem`    = "Democrats\nin Congress",
  `rep`    = "Republicans\nin Congress",
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

# - set factor order for facets (outcome) and colors (covariate)
irf_plot$out <- factor(
  irf_plot$out,
  levels = c(
    "Democrats\nin Congress",
    "Republicans\nin Congress",
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

# - keep only 15-day cumulative effects and drop IRFs whose CI crosses 0
plot_db <- irf_plot %>%
  filter(day == 15) %>%
  arrange(out, cov, pe) %>%
  mutate(label = factor(label, levels = unique(label))) %>%
  filter(sign(lwr) == sign(upr))

# At this point, 'plot_db' contains only cross-group effects at 15 days
# where the 95% CI does not cross zero.


# OUTPUT -- FIGURE 4
#===============================================================================
png("figure4.png", width = 1600, height = 1400)

ggplot(
  plot_db %>%
    mutate(
      # rescale effects: 10 percentage point increase in covariate attention,
      # outcome measured in percentage-point change
      pe  = (pe  * 100) / 10,
      lwr = (lwr * 100) / 10,
      upr = (upr * 100) / 10
    ),
  aes(x = label, y = pe, ymin = lwr, ymax = upr)
) +
  geom_pointrange(aes(col = cov), alpha = 0.4, size = 1.05) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab("\nThe effect of a 10 percentage point increase in attention by the covariate group, measured in percentage point change") +
  scale_color_manual(
    "",
    values = c(
      "blue", "red",
      "blue4", "red4",
      "gray50", "orange2"
    )
  ) +
  theme(
    panel.background  = element_blank(),
    panel.grid.major  = element_line(colour = "gray90", linetype = "solid"),
    axis.text.x       = element_text(size = 16),
    axis.text.y       = element_text(size = 16),
    strip.text        = element_text(size = 16),
    panel.border      = element_rect(colour = "black", fill = FALSE),
    strip.background  = element_rect(colour = "black"),
    axis.title        = element_text(size = 14),
    legend.text       = element_text(size = 14, margin = margin(t = 20), vjust = 5)
  )

dev.off()
